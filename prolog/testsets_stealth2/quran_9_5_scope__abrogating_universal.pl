% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Abrogating-Universal Reading of the Sword Verse: Standing Offensive Jihad Mandate
 *   domain: religious/jurisprudential/political_theology
 *
 * SUMMARY:
 *   A doctrinal regime held by its adherents as revealed law: Quran 9:5
 *   cancels every earlier verse restraining war, and war against unsubmitted
 *   non-Muslims becomes a perpetual communal obligation, lawful to open
 *   without prior injury, ending only at conversion or submission.
 *   Enforcement runs through a juristic establishment that certifies the
 *   abrogation, licenses campaigns, divides spoils, and disciplines dissent.
 *   Benefit flows to the warrior class and the spoils economy that follows
 *   it; cost falls on the unsubmitted, on tributary scriptuary communities
 *   whose protection stays revocable, and on internal advocates of
 *   coexistence, who face heresy and apostasy sanction. Claimed type and
 *   metrics are authored independently: the claim states snare; the metrics
 *   describe near-total extractiveness, heavy suppression, and a theater hump
 *   during dormancy eras. The engine computes each seat's type from the
 *   structural data; where a computed type diverges from the claim, that
 *   divergence is the measurement.
 *
 * KEY AGENTS:
 *   - - expansionist_warrior_class: primary beneficiary (organized/constrained) — fights under the standing mandate; collects spoils, land, captives, rank
 *   - - doctrine_enforcing_jurists: agenda-setter (institutional/identity_locked) — certifies the abrogation, licenses campaigns, disciplines dissent; authority fused with the mandate
 *   - - spoils_economy_recipients: secondary beneficiary (moderate/mobile) — finances and supplies campaigns, absorbs captured land and labor
 *   - - ordinary_believer_community: subsidized participant (organized/constrained) — receives solidarity and economic opening; pays levies and blood
 *   - - non_submitting_polytheists: primary target (powerless/trapped) — faces the conversion-submission-sword ultimatum; first strike lawful against them
 *   - - non_submitting_scriptuary_communities: conditional target (moderate/constrained) — tribute buys revocable suspension of hostilities
 *   - - dissenting_coexistence_advocates: internal target (moderate/identity_locked) — heresy and apostasy exposure silences restraint arguments
 *   - - rival_reading_jurists: excluded seat (institutional/constrained) — methodological objections ruled out before they are heard
 *   - - comparative_hermeneutics_scholars: analytical observer (analytical/analytical) — documents the structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.95).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.9).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.95).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Abrogating-Universal Reading of the Sword Verse: Standing Offensive Jihad Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/jurisprudential/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'fa89acf7-51b4-4715-a0bb-3f9f7aef6626').
narrative_ontology:cs_kernel_codification('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', fixed_text).
narrative_ontology:cs_authority_grounding('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', lineage).
narrative_ontology:cs_interpretation_layer_present('fa89acf7-51b4-4715-a0bb-3f9f7aef6626').
narrative_ontology:cs_reading_relation('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', foundational, nasikh_cancellation_of_peace_verses).
narrative_ontology:cs_axiom_status(nasikh_cancellation_of_peace_verses, holdable).
narrative_ontology:cs_axiom_grounding('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', nasikh_cancellation_of_peace_verses, theological).
narrative_ontology:cs_axiom('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', foundational, offensive_jihad_perpetual_collective_obligation).
narrative_ontology:cs_axiom_status(offensive_jihad_perpetual_collective_obligation, holdable).
narrative_ontology:cs_axiom_grounding('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', offensive_jihad_perpetual_collective_obligation, theological).
narrative_ontology:cs_axiom('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', secondary, no_indefinite_peace_with_unsubmitted).
narrative_ontology:cs_axiom_status(no_indefinite_peace_with_unsubmitted, holdable).
narrative_ontology:cs_axiom_grounding('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', no_indefinite_peace_with_unsubmitted, theological).
narrative_ontology:cs_reference_frame('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', perpetual_universal_war_mandate).
narrative_ontology:cs_drift_state('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', contemporary_mass_movement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fa89acf7-51b4-4715-a0bb-3f9f7aef6626', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_warrior_class).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, spoils_economy_recipients).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, doctrine_enforcing_jurists).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, ordinary_believer_community).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_submitting_polytheists).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_submitting_scriptuary_communities).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, dissenting_coexistence_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, ordinary_believer_community).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, nasikh_abrogation_methodology).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, finality_of_sword_verse_revelation).
narrative_ontology:constraint_vindicates(quran_9_5_scope__abrogating_universal, bipolar_world_division_dar_al_islam_dar_al_harb).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fights under the standing mandate the doctrine proclaims. Campaign yields spoils, land, captives, and rank distributed by fixed shares; declining the call when the mandate is invoked brings censure and loss of standing. Livelihood and honor are bound to continued expansion; a durable peace removes the economy they live from.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_warrior_class, beneficiary,
    organized, biographical, constrained, continental).

% Scholar-jurists who certify the abrogation claim, issue opinions licensing campaigns and dividing spoils, train judges for conquered districts, and rule on the treatment of surrendered populations. Their authority over law, education, and appointment flows from custody of the mandate; a jurist who denied the abrogation would forfeit his chair, his income, and his place in the transmission chain. The institution has become its function.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, doctrine_enforcing_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Traders, horse-breeders, arms-makers, and financiers who supply campaigns and absorb the land, movable wealth, and captive labor that campaigns produce. Their markets swell when the frontier advances and contract when it stalls. They do not fight; they provision and monetize those who do.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, spoils_economy_recipients, beneficiary,
    moderate, biographical, mobile, regional).

% Households that receive solidarity, meaning, and economic opening from the expanding order — new land, new markets, religious prestige. They also supply sons to the ranks, pay the levies that fund campaigns, and live under the reprisal risk that expansion invites. Leaving the community is socially and legally catastrophic, so participation is less a choice than a condition.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, ordinary_believer_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, ordinary_believer_community, payer).

% Communities outside the scriptural religions who face a standing ultimatum: conversion, submission to subordinate status where it is offered, or the sword. Opening hostilities against them is lawful without prior injury; truces, where granted at all, run only until the community judges itself able to resume. Flight, resistance, and concealment are the only exits, each carrying death or destitution.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_submitting_polytheists, payer,
    powerless, immediate, trapped, regional).

% Jewish, Christian, and other scriptuary populations. The doctrine marks them as targets unless they accept subordinate tribute-paying status; even then the standing mandate keeps their protection conditional and revocable, their worship and persons legally vulnerable, and their testimony subordinate. Payment suspends hostilities; it purchases no membership and no permanence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_submitting_scriptuary_communities, payer,
    moderate, biographical, constrained, continental).

% Muslim teachers, poets, and officials who urge treaty-keeping, mercy, or restraint. Under the doctrine their counsel is not merely mistaken but a denial of revealed obligation. They face charges of heresy or apostasy, loss of position, imprisonment, or execution; many fall silent, and the silence is then cited as consent. Their faith binds them to the community whose law silences them.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, dissenting_coexistence_advocates, payer,
    moderate, biographical, identity_locked, global).

% Jurists in the same broader tradition who read the verse as tied to its seventh-century occasion and deny that it cancels the peaceful verses. Inside this framework they hold no seat: their methodological objections are ruled out in advance as ignorance or bad faith, their schools' rulings are overridden unheard, and their students are disciplined. They argue from outside the adjudication they seek to enter.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, rival_reading_jurists, excluded,
    institutional, generational, constrained, global).

% Academic scholars of Islamic law, tafsir, and comparative hermeneutics who reconstruct the verse's occasions of revelation, the history of the abrogation debate, and the doctrine's career across empires and movements. They hold no standing inside the framework and collect nothing from its operation; they document it.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, comparative_hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_warrior_class).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the believer community's war effort under a single standing command structure: settles who may fight, against whom, when truces lapse, and how conquest proceeds — independent of any particular grievance or provocation, so the war effort never waits on a fresh cause.
% TRANSFER_FUNCTION: Moves life, property, territory, and religious allegiance from non-submitting populations to the expanding polity; moves spoils, land, and captive labor to fighters and the spoils economy; moves interpretive authority, appointments, and social standing to the juristic establishment that administers the mandate; moves internal dissent into silence under heresy and apostasy sanction.
% ABSENT_VOICES: The targeted communities have no standing — their consent is definitionally irrelevant until submission. Rival-reading jurists are excluded from adjudication, their objections ruled out in advance. Treaty partners' voices void whenever the mandate is invoked. All three would locate the fatal error in the abrogation premise itself; none is present where the doctrine is applied.
% DISAPPEARANCE_RATIONALE: Overnight removal: every unsubmitted community leaves the target set; standing ultimata lapse; spoils economies and campaign financing dissolve; the juristic establishment loses its authority warrant; expansionist movements must rebuild legitimacy on grievance or governance alone. Millions currently marked as lawful targets exit the target set at once — the arrangement is load-bearing for its holders and fate-bearing for its targets.
% FOUNDING_PROBLEM: After the Medinan polity's consolidation, the believer community needed a war authority that would not lapse with each peace: a standing, grievance-independent legal basis for expansion, and a settled rule for when treaties with polytheist tribes could be set aside. The arrangement was built to make expansion a permanent communal obligation rather than a response to injury.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: academic histories of Islamic law (e.g., Majid Khadduri's studies of the siyar literature, Wael Hallaq's institutional histories) attest that the doctrine operated as standing law across classical and post-classical jurisprudence; chronicles from targeted populations — Armenian, Byzantine, South Asian — attest its application from the receiving end. Within the tradition, jurists outside the holding movements corroborate the founding problem's framing while disputing its permanence. No attesting source sits wholly outside the tradition; the sharpest external attestations come from the targets' own records.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.95, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-total (0.95) because the mandate reaches life, property, territory, and allegiance themselves, and is by design grievance-independent — no conduct by the target population suspends it short of submission. Suppression is high (0.90) and is a raw structural property, unscaled by power or scope: the arrangement must suppress rival readings inside the tradition, silence coexistence advocacy inside the community, and hold tributary populations in revocable subordination. Theater_ratio (0.40) traces a hump rather than a trend: low while campaigns run (900-1400), peaking in the colonial-era dormancy (0.60 at 1850) when maintenance turned juridical-recitational, falling back as modern movements regained operational capacity (0.40 at 2025) while many proclaimers still lack it. Accessibility_collapse (0.78) is high but below natural-law grade: once the abrogation premise is accepted, coexistence alternatives collapse almost completely inside the framework — yet accepting the premise itself is the contested step, and the sibling readings persist outside it. Resistance (0.80) is correspondingly massive: targeted populations fought, rival schools objected, and modern states and reform movements contest the doctrine openly. The measurement series run on one shared time grid — all three metrics authored at all six points — so no end-state value leaks backward into earlier rows. Coalition note: powerless polytheist communities repeatedly achieved coalition defense, but the arrangement's design counters it by splitting targets through sequential separate submissions — each submitting group exits the target set individually, degrading coalition formation; the suppression figure prices that divide-and-submit mechanism. Receipt surface: gains demonstrably accrue to the warrior class (fixed spoils shares, land, captives), so gain_flow names that seat rather than 'diffuse'; fixing is prohibitive because for the holder coalition abandoning the mandate dissolves its legitimacy warrant, while outsiders lack any standing to fix it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the juristic seat the arrangement is divine law administered faithfully — an experience of order and office. From the unsubmitted polytheist seat it is a standing death-or-submission ultimatum with first strike already authorized — the maximal-target experience. From the ordinary believer seat it is subsidized solidarity with conscription and reprisal costs — near-symmetric. From the dissenting advocate's seat it is a trap that punishes the very mercy the tradition elsewhere commands. The engine computes these per-seat classifications from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the warrior class, spoils recipients, jurists, and ordinary believers sit toward the beneficiary end (low d, damped or inverted effective extraction), with the jurists additionally locked by institutional identity — their authority IS the mandate, so exit is unthinkable regardless of arithmetic. Victim declarations drive the targets: unsubmitted polytheists sit nearest the full-target end (trapped exit, no arbitrage), scriptuary communities slightly inside them (tribute purchases a costly partial exit), and dissenting advocates high despite being insiders, because the arrangement's costs to them (silencing, heresy exposure) are what their victimhood consists of. Scope amplification applies at the continental-global scales the enforcement network actually operates.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is designed perpetual — no sunset clause, no completion condition short of universal submission — so the founding problem (grievance-independent legitimation of expansion) remains live whenever capacity exists, as the 2025 reactivation demonstrates. Mandatrophy risk concentrates in dormancy eras, where maintenance turns theatrical (theater_ratio 0.60 at 1850); but reactivation shows the function was dormant, not dead, so the honest verdict is a live mandate with oscillating operation, not resolved mandatrophy — mandatrophy_resolved is left unset. The classification guards against two opposite mislabels: calling pure coordination what is mandate-plus-capture (the coordination function is real but subordinate — it organizes the taking), and calling the dormancy eras proof of inertial death (the piton signature) when the record shows reactivation. Fixing_cost 'prohibitive' plus named-seat capture keeps this in the captured cell rather than the neglect cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the abrogating_universal reading of kernel quran_9_5_scope — what structurally changes if a sibling reading (contextual_defensive, progressive_synthesis) is adopted instead?',
    'Adoption of an alternative reading by the movement''s own authoritative bodies (binding juristic councils reversing the abrogation claim), or displacement of the holding movement by a rival whose founding commitments instantiate a sibling reading.',
    'Under contextual_defensive the target set shrinks to actual treaty-breakers and first strike loses authorization (structure moves toward coordination-with-friction); under progressive_synthesis the standing mandate lapses entirely (target set empties of standing targets; at most transitional residue). Either sibling adoption rewrites the victim set, the beneficiary structure, and the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling adoption rewrites victim set and authorization scope.').

omega_variable(
    abrogation_claim_textual_validity,
    'Does the nasikh relation asserted between 9:5 and the pre-existing peaceful verses hold under the tradition''s own abrogation methodology — does 9:5 actually cancel them, or does its reported occasion (treaty-breaking polytheist tribes at the pilgrimage season) bind the verse to that context?',
    'Chronological reconstruction of revelation (asbab al-nuzul reports), exhaustive tally of the verses the claim must cancel, and cross-school examination of abrogation conditions (explicit textual cancellation versus inference).',
    'If the abrogation fails methodologically, the universal mandate loses its textual foundation and this constraint collapses into the contextual reading''s shape; if it holds, the standing-obligation structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_claim_textual_validity, empirical, 'Textual-methodological validity of the abrogation premise.').

omega_variable(
    operational_vs_nominal_maintenance,
    'In eras when no holding movement possesses campaign capacity, is the doctrine operative law awaiting capacity, or nominally maintained performance?',
    'Count operational invocations (campaign licensing, spoils adjudication, ultimatum delivery) versus purely rhetorical recitation across the interval''s later phases.',
    'A high nominal share drives theater_ratio upward and opens degradation-toward-inertia during dormancy; reactivation events reset it — the 1650-2025 theater hump records exactly this oscillation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_nominal_maintenance, empirical, 'Operational versus performative maintenance across dormancy and reactivation phases.').

omega_variable(
    gain_capture_concentration,
    'Do the doctrine''s gains concentrate in identifiable seats (warrior elite, juristic establishment) or diffuse across the whole believer community?',
    'Trace spoils shares, land grants, tax farming, and appointment records across campaigns; compare fighter and commoner wealth trajectories.',
    'Concentration confirms the receipt surface as authored (named-seat capture); genuine diffusion would support a hybrid reading with broad beneficiary participation and a larger genuine coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_concentration, empirical, 'Whether gains concentrate in movement elites or diffuse across believers.').

omega_variable(
    coexistence_suppression_mechanism,
    'Is the suppression of coexistence advocacy structural (legal penalties, career destruction, execution) or internalized (advocates preemptively silence themselves, having fused piety with the mandate)?',
    'Post-lift speech trajectories: where enforcement capacity collapses (conquest reversal, regime change), do restraint arguments reappear quickly (structural suppression) or stay absent for generations (internalized)?',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after enforcement decay; the scalar suppression understates the arrangement''s grip on its own community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_suppression_mechanism, empirical, 'Structural versus internalized suppression of dissenting coexistence advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t900, quran_9_5_scope__abrogating_universal, theater_ratio, 900, 0.1).
narrative_ontology:measurement_basis(qura_tr_t900, observed).
narrative_ontology:measurement(qura_tr_t1150, quran_9_5_scope__abrogating_universal, theater_ratio, 1150, 0.12).
narrative_ontology:measurement_basis(qura_tr_t1150, observed).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__abrogating_universal, theater_ratio, 1400, 0.15).
narrative_ontology:measurement_basis(qura_tr_t1400, observed).
narrative_ontology:measurement(qura_tr_t1650, quran_9_5_scope__abrogating_universal, theater_ratio, 1650, 0.35).
narrative_ontology:measurement_basis(qura_tr_t1650, observed).
narrative_ontology:measurement(qura_tr_t1850, quran_9_5_scope__abrogating_universal, theater_ratio, 1850, 0.6).
narrative_ontology:measurement_basis(qura_tr_t1850, observed).
narrative_ontology:measurement(qura_tr_t2025, quran_9_5_scope__abrogating_universal, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(qura_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t900, quran_9_5_scope__abrogating_universal, base_extractiveness, 900, 0.84).
narrative_ontology:measurement_basis(qura_be_t900, observed).
narrative_ontology:measurement(qura_be_t1150, quran_9_5_scope__abrogating_universal, base_extractiveness, 1150, 0.87).
narrative_ontology:measurement_basis(qura_be_t1150, observed).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__abrogating_universal, base_extractiveness, 1400, 0.88).
narrative_ontology:measurement_basis(qura_be_t1400, observed).
narrative_ontology:measurement(qura_be_t1650, quran_9_5_scope__abrogating_universal, base_extractiveness, 1650, 0.86).
narrative_ontology:measurement_basis(qura_be_t1650, observed).
narrative_ontology:measurement(qura_be_t1850, quran_9_5_scope__abrogating_universal, base_extractiveness, 1850, 0.83).
narrative_ontology:measurement_basis(qura_be_t1850, observed).
narrative_ontology:measurement(qura_be_t2025, quran_9_5_scope__abrogating_universal, base_extractiveness, 2025, 0.95).
narrative_ontology:measurement_basis(qura_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t900, quran_9_5_scope__abrogating_universal, suppression_requirement, 900, 0.45).
narrative_ontology:measurement_basis(qura_su_t900, observed).
narrative_ontology:measurement(qura_su_t1150, quran_9_5_scope__abrogating_universal, suppression_requirement, 1150, 0.55).
narrative_ontology:measurement_basis(qura_su_t1150, observed).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__abrogating_universal, suppression_requirement, 1400, 0.65).
narrative_ontology:measurement_basis(qura_su_t1400, observed).
narrative_ontology:measurement(qura_su_t1650, quran_9_5_scope__abrogating_universal, suppression_requirement, 1650, 0.72).
narrative_ontology:measurement_basis(qura_su_t1650, observed).
narrative_ontology:measurement(qura_su_t1850, quran_9_5_scope__abrogating_universal, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement_basis(qura_su_t1850, observed).
narrative_ontology:measurement(qura_su_t2025, quran_9_5_scope__abrogating_universal, suppression_requirement, 2025, 0.9).
narrative_ontology:measurement_basis(qura_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Sword Verse doctrine' decomposes into three structurally distinct constraints — this file (abrogating_universal), quran_9_5_scope__contextual_defensive, and quran_9_5_scope__progressive_synthesis — with different epsilon, different victim sets, and different classifications. The abrogation claim is upstream: whichever way it resolves determines the downstream constraint's shape, and this reading cites the abrogation as textual warrant for everything downstream. Family links run through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
