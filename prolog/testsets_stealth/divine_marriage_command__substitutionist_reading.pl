% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Post-Manifesto Monogamy Mandate (Substitutionist Reading)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   After the 1890 Manifesto, the church's standing arrangement requires
 *   monogamy as doctrine, treats post-Manifesto plural marriage as apostasy
 *   rather than civil disobedience, and enforces the standard through temple
 *   interviews, disciplinary councils, and excommunication. This story
 *   instantiates the SUBSTITUTIONIST READING of the divine_marriage_command
 *   kernel: the Manifesto is new revelation that supersedes the prior
 *   command, and institutional legitimacy depends on that framing holding.
 *   The epsilon referent is the standing post-1890 monogamy-enforcement
 *   arrangement itself, assessed by this reading's own lights — which grants
 *   the revelation-framing prima facie legitimacy while still registering the
 *   real costs borne by holdouts, the transition-generation families, and
 *   members who cannot square the record with the teaching. KEY AGENTS (by
 *   structural relationship): - first_presidency_and_apostles: Agenda setter
 *   (institutional/identity_locked) — administers the standard and guards the
 *   revelation-framing - lds_church_institution: Primary beneficiary
 *   (institutional/constrained) — survival, restored property, legal
 *   alignment - monogamous_member_majority: Secondary beneficiary
 *   (moderate/constrained) — doctrinal clarity at low daily cost -
 *   fundamentalist_plural_marriage_practitioners: Primary target
 *   (organized/identity_locked) — excommunication -
 *   plural_wives_transition_generation: Historical target (powerless/trapped)
 *   — bore the household reconstitution - conscientious_dissenting_members:
 *   Diffuse target (moderate/constrained) — epistemic conformity cost -
 *   member_history_scholars: Excluded voice (moderate/constrained) —
 *   documentation barred from the teaching corridor - secular_judiciary:
 *   Analytical observer (institutional/analytical) — adjudicates edge
 *   incidents from outside. Claim and metrics are authored independently: the
 *   claimed type is what the structure looks like from the authoring seat;
 *   the metrics describe observed operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.42).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Post-Manifesto Monogamy Mandate (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, '253d8b52-8d8d-4983-a36b-60bdc78abc8a').
narrative_ontology:cs_kernel_codification('253d8b52-8d8d-4983-a36b-60bdc78abc8a', fixed_text).
narrative_ontology:cs_authority_grounding('253d8b52-8d8d-4983-a36b-60bdc78abc8a', extraction).
narrative_ontology:cs_interpretation_layer_present('253d8b52-8d8d-4983-a36b-60bdc78abc8a').
narrative_ontology:cs_reading_relation('253d8b52-8d8d-4983-a36b-60bdc78abc8a', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('253d8b52-8d8d-4983-a36b-60bdc78abc8a', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('253d8b52-8d8d-4983-a36b-60bdc78abc8a', foundational, manifesto_constitutes_binding_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_binding_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('253d8b52-8d8d-4983-a36b-60bdc78abc8a', manifesto_constitutes_binding_new_revelation, theological).
narrative_ontology:cs_axiom('253d8b52-8d8d-4983-a36b-60bdc78abc8a', secondary, prior_plural_marriage_command_superseded).
narrative_ontology:cs_axiom_status(prior_plural_marriage_command_superseded, holdable).
narrative_ontology:cs_axiom_grounding('253d8b52-8d8d-4983-a36b-60bdc78abc8a', prior_plural_marriage_command_superseded, theological).
narrative_ontology:cs_reference_frame('253d8b52-8d8d-4983-a36b-60bdc78abc8a', living_oracle_progressive_supersession).
narrative_ontology:cs_drift_state('253d8b52-8d8d-4983-a36b-60bdc78abc8a', contemporary_documentary_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('253d8b52-8d8d-4983-a36b-60bdc78abc8a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, lds_church_institution).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_member_majority).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_plural_marriage_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, plural_wives_transition_generation).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, conscientious_dissenting_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, manifesto_revelation_status).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, living_prophet_doctrine).
narrative_ontology:constraint_vindicates(divine_marriage_command__substitutionist_reading, progressive_revelation_supersession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the interpretations that define the marriage standard, approve disciplinary councils for members who contract plural marriages after the Manifesto, and sign the official declarations. Their personal authority rests on the claim that the Manifesto came by revelation; revising that framing would undercut the office they hold, so the account is not something they can adjust without spending their own legitimacy. They answer to no internal electorate.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, first_presidency_and_apostles, agenda_setter,
    institutional, generational, identity_locked, global).

% Holds the temples, the membership rolls, and the legal standing the 1890 settlement restored. Compliance brought back confiscated property, enabled Utah statehood, and ended the pursuit of its officers; the monogamy standard keeps the institution aligned with the marriage law of every country where it operates. Its doctrine cannot be relocated piecemeal — reopening the Manifesto's status would put in question everything built on top of it.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, lds_church_institution, beneficiary,
    institutional, civilizational, constrained, global).

% Marry under a single clear standard that matches civil law everywhere, receive temple ordinances without complication, and inherit a stable family doctrine taught consistently from childhood. They pay tithing and answer conformity questions in interviews; leaving would cost them community and often family ties, so most stay, and the standard asks little of them day to day.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_member_majority, beneficiary,
    moderate, biographical, constrained, global).

% Continue to covenant plural marriage as an eternal principle they believe was never rescinded. The mainline church disciplines them out: excommunication strips temple ordinances, priesthood standing, and sealing rights. They have built parallel congregations in Utah, Arizona, and northern Mexico, but their identity is fused with the practice — abandoning plural marriage would dissolve the community's reason for being, while keeping it costs them membership in the parent church.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_plural_marriage_practitioners, payer,
    organized, generational, identity_locked, regional).

% The women married under the pre-1890 command, and their children. When the Manifesto landed, households had to be reconstituted around one legally recognized wife; other wives lost recognized status, and families split or went underground. No forum existed in which the standing of their marriages was negotiable — the decision was announced, and they absorbed the rearrangement.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, plural_wives_transition_generation, payer,
    powerless, biographical, trapped, national).

% Members who study the church's own historical record and find the pure-revelation account strained. Their choice set is shaped for them: affirm the official framing in temple interviews, keep doubts private, or walk away from covenants, community, and often family. No sanctioned middle position leaves the question open.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, conscientious_dissenting_members, payer,
    moderate, biographical, constrained, global).

% Academics inside the membership who document the political genesis of the Manifesto, the post-1890 plural marriages performed with elite approval, and the distance between the canonical text of Official Declaration 1 and the diaries behind it. Publishing this work has carried ecclesiastical consequences for member-scholars, notably the 1993 disciplinary actions, so the findings circulate mostly outside the institution's teaching corridors; the offices that control curriculum do not seat them.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, member_history_scholars, excluded,
    moderate, civilizational, constrained, continental).

% Courts and agencies that adjudicate the boundary incidents: tax status of fundamentalist offshoots, welfare and custody interventions in plural-marriage compounds, religious-liberty claims. They see the whole structure from outside, rule on its edges, and take no part in deciding what the doctrine means internally.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, secular_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__substitutionist_reading, lds_church_institution).
narrative_ontology:fixing_cost_class(divine_marriage_command__substitutionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one authoritative marriage standard that matches the civil law of every jurisdiction the church operates in, resolved the 1890 crisis of disincorporation, seized property, and imprisoned leadership, and gives a worldwide membership a common, teachable family doctrine. Whatever else it does, it settles the collision between a revealed marriage command and sovereign law.
% TRANSFER_FUNCTION: Moves doctrinal assent and behavioral conformity from members to the institution; moves legitimacy, restored property, and legal security to the institution; moved the cost of household reconstitution onto the plural-marriage families of the transition generation; and continues to move the price of open disagreement onto dissenters and fundamentalists through discipline and excommunication.
% ABSENT_VOICES: Member historians who can document the coercion context are not seated in any body that decides what the Manifesto means; descendants of plural marriages carry the transition's costs without a forum; fundamentalist believers who read the same canon differently are expelled rather than answered. Unanimity inside the frame is partly an artifact of who was never given a seat. (Commentary-grade: informs consensus-provenance, not classification.)
% DISAPPEARANCE_RATIONALE: If the monogamy mandate and its enforcement vanished overnight, the post-1890 settlement unravels: temple-interview standards lose their object, the fundamentalist position is retroactively vindicated, the living-prophet doctrine absorbs the damage of a revelation left unexplained, and the institution's alignment with civil marriage law worldwide would need rebuilding from scratch.
% FOUNDING_PROBLEM: In 1890 the church faced extinction over plural marriage: Congress had disincorporated it and seized its property, its leaders were in hiding or prison, Utah statehood was blocked, and federal prosecution of plural marriage was escalating. The Manifesto answered how a revealed command survives when the sovereign forbids it.
% FOUNDING_PROBLEM_CORROBORATION: Federal statutes, court records, and congressional testimony from 1862-1890 independently document the coercion the Manifesto answered — corroboration from entirely outside the benefiting parties. Whether the problem is still live divides by seat: the institution teaches the Manifesto closed a completed trial; historians note the legal problem died in 1890 while the doctrinal question did not; fundamentalist bodies attest the underlying command was never rescinded. No neutral arbiter exists.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.58: within this reading's own lights most of the arrangement is legitimate religious authority, so epsilon is bounded — but it concentrates real costs on excommunicated practitioners, on families dissolved at the transition, and on the standing demand that members affirm the revelation-framing against their own archive. Suppression ends at 0.42: the enforcement machinery is real (interview screening, councils, excommunication) but relaxed after dissent externalized into separate denominations. Theater ends at 0.52: the earliest period ran a wide gap between public abandonment and privately approved post-1890 marriages; enforcement narrowed that gap mid-century; the modern period widens it again as official framing retains the pure-revelation account while the institution's own essays concede the political context. All three series run on one shared eight-point grid (1890, 1904, 1920, 1935, 1953, 1978, 2013, 2020) so no metric is sampled against another's end-state. The trajectories are non-monotonic by design: extractiveness and suppression rise together through the enforcement build-up (Second Manifesto 1904, the purges of the 1930s-50s) and fall together as dissent exits the books; theater is U-shaped, high at the accommodation, low at peak enforcement, rising again as framing-maintenance outlasts the events it frames.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the leadership seat the arrangement is sacred duty and continuity — the Manifesto is what the office received, and administering it is fidelity. From the institution seat it is survival won: property, statehood, legal peace. From the member-majority seat it is ordinary religious life at negligible daily cost. From the fundamentalist seat it is betrayal and expulsion — the same texts read faithfully, punished. From the transition-generation seat it is loss administered without consultation. From the scholar seat it is a widening gap between record and curriculum. The engine computes per-seat classifications from power, exit, and role data; the divergence between the leadership's computed type and the fundamentalists' computed type is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the institution sits nearest the beneficiary end (collects legitimacy, property, and the tithing base the settlement secured); the member majority sits mildly beneficiary (clarity and legal security against a light conformity toll). Victim declarations drive high directionality: fundamentalist practitioners sit nearest the full-target end, amplified by identity_lock — their exit from the practice dissolves the community's constituting commitment, so they bear the standard's full force with no arbitrage path; the transition generation sat at full-target while trapped, with no forum; dissenting members sit mid-high, paying an epistemic toll scaled by how much of the record they engage. The leadership seat derives near-beneficiary but not zero: it administers and collects authority, yet bears the framing burden personally — its identity-lock cuts both ways. Excluded scholars feed no directionality: they are in neither array, which is itself the finding their seat records.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional extinction over plural marriage) is authentically contested rather than dead: the legal emergency ended in 1890, but whether the doctrinal question it answered is closed is precisely what the three readings dispute. Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer finds no dead-mandate-plus-rearrangement zombie signature — the arrangement still performs load-bearing work (church-state settlement, worldwide legal alignment, boundary maintenance). The tangled_rope claim is what prevents mislabeling in both directions: reading this as pure coordination erases the excommunicated and the dissolved households; reading it as pure extraction erases the genuine collective problem it solved — a revealed command colliding with a sovereign prohibition, at existential stakes. Both facts are structural, and the classification keeps both visible. Mandatrophy is not declared resolved: the mandate has changed function (crisis-resolution to identity-boundary maintenance) without outliving function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the substitutionist reading of the divine_marriage_command kernel — the Manifesto as new revelation superseding the prior command. How would the constraint''s structure change under the continuationist reading (prudential suspension, doctrine intact) or the coercion-visibility reading (acknowledged survival accommodation)?',
    'Not resolvable by data alone: the dispute turns on whether a prophetic reversal counts as revelation, decided within each tradition''s own epistemology. Corpus-level resolution comes from classifying all three sibling stories and comparing victim sets, epsilon, and computed types.',
    'Under the continuationist reading the victim set shifts — post-Manifesto practitioners become the faithful and the institution becomes the coerced party. Under the coercion-visibility reading the revelation-framing drops out, measured theater climbs sharply, and the arrangement trends toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the divine-marriage kernel this constraint instantiates is irreducible from inside the frame.').

omega_variable(
    revelation_framing_vs_coercion_genesis,
    'Does the Manifesto''s authority actually rest on new revelation, or does the institutional framing maintain revelation-status over a documented coercion genesis?',
    'Side-by-side reading of Woodruff''s contemporaneous diaries against the canonical text of Official Declaration 1, the record of post-1890 plural marriages performed with elite approval, and the institution''s own later essays conceding the political context.',
    'If coercion-genesis is conceded at the level of doctrine, the substitutionist reading loses its warrant, theater_ratio climbs toward piton territory, and the sibling coercion-visibility reading inherits the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_framing_vs_coercion_genesis, empirical, 'Revelation-status of the Manifesto versus documented accommodation genesis.').

omega_variable(
    epistemic_conformity_internalization,
    'Is member acceptance of the revelation-framing settled conviction, or conformity maintained by the social cost of visible doubt?',
    'Compare belief trajectories of lifelong members raised after enforcement relaxed against adult converts, and track belief stability across the post-1970s decline in disciplinary intensity.',
    'If internalized, the conformity demand travels with members after enforcement relaxes — explaining why measured enforcement fell while conformity held, and implying the scalar suppression understates the lived force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_conformity_internalization, empirical, 'Structural versus internalized mechanism sustaining doctrinal conformity.').

omega_variable(
    enforcement_decay_via_externalization,
    'Did enforcement intensity fall because the arrangement stabilized, or because dissenters exited into separate fundamentalist denominations, moving the conflict off the mainline''s books?',
    'Correlate mainline disciplinary caseloads with fundamentalist denomination growth curves, and examine jurisdictions and episodes (Short Creek 1953) where the exit path was restricted.',
    'If decay rode on the exit valve, closing it re-ratchets suppression — the arrangement''s calm is contingent on an open door, not settled consent, and the falling suppression series would date stabilization too early.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_via_externalization, empirical, 'Whether falling enforcement reflects stabilization or export of dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__substitutionist_reading, theater_ratio, 1904, 0.4).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__substitutionist_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__substitutionist_reading, theater_ratio, 1935, 0.25).
narrative_ontology:measurement(divi_tr_t1953, divine_marriage_command__substitutionist_reading, theater_ratio, 1953, 0.28).
narrative_ontology:measurement(divi_tr_t1978, divine_marriage_command__substitutionist_reading, theater_ratio, 1978, 0.33).
narrative_ontology:measurement(divi_tr_t2013, divine_marriage_command__substitutionist_reading, theater_ratio, 2013, 0.48).
narrative_ontology:measurement(divi_tr_t2020, divine_marriage_command__substitutionist_reading, theater_ratio, 2020, 0.52).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__substitutionist_reading, base_extractiveness, 1904, 0.62).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__substitutionist_reading, base_extractiveness, 1920, 0.66).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__substitutionist_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(divi_be_t1953, divine_marriage_command__substitutionist_reading, base_extractiveness, 1953, 0.7).
narrative_ontology:measurement(divi_be_t1978, divine_marriage_command__substitutionist_reading, base_extractiveness, 1978, 0.64).
narrative_ontology:measurement(divi_be_t2013, divine_marriage_command__substitutionist_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(divi_be_t2020, divine_marriage_command__substitutionist_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__substitutionist_reading, suppression_requirement, 1904, 0.55).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__substitutionist_reading, suppression_requirement, 1920, 0.65).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__substitutionist_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement(divi_su_t1953, divine_marriage_command__substitutionist_reading, suppression_requirement, 1953, 0.75).
narrative_ontology:measurement(divi_su_t1978, divine_marriage_command__substitutionist_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(divi_su_t2013, divine_marriage_command__substitutionist_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement(divi_su_t2020, divine_marriage_command__substitutionist_reading, suppression_requirement, 2020, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the church's marriage doctrine' covers three structurally distinct constraints — one per reading of the divine_marriage_command kernel. Each story carries its own epsilon, beneficiaries, and victims; they share a referent (the post-1890 arrangement) but disagree on what the Manifesto IS, which flips the victim set. Linked as a constraint family via affects_constraints. Direction of pressure: the substitutionist frame is the official one and structurally shapes the coercion-visibility reading's operating environment (what its advocates can access and what speaking costs), while logically ruling the continuationist reading out within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
