% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter — Secular-Democratic Reading (Civilian Supremacy Mandate)
 *   domain: constitutional/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   A post-revolutionary charter, ratified by a multi-party consensus
 *   commission after the ouster of the prior regime, is read here through the
 *   secular-democratic lens: the text mandates secular democratic
 *   institutions and subordinates the armed forces to elected civilian
 *   authority. This file instantiates ONE reading of the contested kernel
 *   july_charter_sovereign_legitimacy; the guided-nationalism and
 *   military-custodian readings are separate constraint stories with their
 *   own epsilon values and victim sets, linked through the network block.
 *   Under this reading the arrangement delivers a genuine coordination good —
 *   rule-bound power transfer, minority protection, unified command — while
 *   imposing concentrated costs on two identifiable seats: the Islamist
 *   party, whose participation is delimited, and the officer corps, whose
 *   autonomous political authority is stripped. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED as tangled_rope while the metrics
 *   describe enforced coordination with substantial asymmetric extraction;
 *   the engine measures the divergence per seat.
 *
 * KEY AGENTS:
 *   - national_consensus_commission: agenda setter (institutional/arbitrage) — administers the constituent process and owns the implementation sequence
 *   - secular_democratic_parties: primary beneficiary (organized/mobile) — gains competitive ground and drafting power; bears defensive costs
 *   - religious_minorities: protected beneficiary (powerless/constrained) — receives formal equal citizenship whose value depends on enforcement
 *   - jamaat_e_islami: primary target (organized/identity_locked) — delimited participation; cannot exit by repositioning without dissolving
 *   - military_autonomous_authority: second target (powerful/constrained) — loses custodial prerogatives while retaining corporate benefits
 *   - islamist_voter_base: diffuse target (powerless/constrained) — retains franchise but loses proportionate voice
 *   - islamist_clerical_networks: excluded seat (organized/trapped) — holds a rival constitutional account, never seated
 *   - supreme_court_appellate_bench: analytical observer (institutional/analytical) — its docket records which reading consolidates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter — Secular-Democratic Reading (Civilian Supremacy Mandate)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '3b8c8b48-4054-48a0-bd58-d9002edb9901').
narrative_ontology:cs_kernel_codification('3b8c8b48-4054-48a0-bd58-d9002edb9901', fixed_text).
narrative_ontology:cs_authority_grounding('3b8c8b48-4054-48a0-bd58-d9002edb9901', distributed).
narrative_ontology:cs_reading_relation('3b8c8b48-4054-48a0-bd58-d9002edb9901', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('3b8c8b48-4054-48a0-bd58-d9002edb9901', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('3b8c8b48-4054-48a0-bd58-d9002edb9901', foundational, secular_procedural_sovereignty).
narrative_ontology:cs_axiom_status(secular_procedural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3b8c8b48-4054-48a0-bd58-d9002edb9901', secular_procedural_sovereignty, conventional).
narrative_ontology:cs_axiom('3b8c8b48-4054-48a0-bd58-d9002edb9901', foundational, civilian_supremacy_inviolable).
narrative_ontology:cs_axiom_status(civilian_supremacy_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('3b8c8b48-4054-48a0-bd58-d9002edb9901', civilian_supremacy_inviolable, deontological).
narrative_ontology:cs_axiom('3b8c8b48-4054-48a0-bd58-d9002edb9901', secondary, no_religious_test_for_office).
narrative_ontology:cs_axiom_status(no_religious_test_for_office, holdable).
narrative_ontology:cs_axiom_grounding('3b8c8b48-4054-48a0-bd58-d9002edb9901', no_religious_test_for_office, conventional).
narrative_ontology:cs_reference_frame('3b8c8b48-4054-48a0-bd58-d9002edb9901', secular_procedural_republic).
narrative_ontology:cs_drift_state('3b8c8b48-4054-48a0-bd58-d9002edb9901', post_charter_implementation_period, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3b8c8b48-4054-48a0-bd58-d9002edb9901', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_civil_society).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, professional_bureaucratic_class).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_voter_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the post-revolutionary constituent process: brokers party signatures on the charter text, drafts the implementation statutes that give the secular-democratic provisions operational force, and sequences registration review and command-directive reforms. Its authority exists only while the settlement it brokered holds; if the rival readings capture the implementation process, its role dissolves.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, national_consensus_commission, agenda_setter,
    institutional, biographical, arbitrage, national).

% Contest elections under rules they helped write and gain competitive ground as a major rival's participation is delimited by registration review. They staff the committees that translate the charter's civilian-command clauses into statute. Their costs are defensive: holding the settlement against military public-doctrine campaigns and Islamist street mobilization consumes agenda space and forces uncomfortable policing of former allies.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, agenda_setter).

% Bar associations, university faculties, human-rights organizations, and election-monitoring bodies supply the legitimacy labor the new order runs on: litigation, observation, documentation of abuses under the prior regimes. Procedural openings — information access, standing rules, court access — expand their operating room relative to both custodial and religious-nationalist arrangements.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_civil_society, beneficiary,
    moderate, biographical, mobile, regional).

% Hindu, Buddhist, Christian, and Ahmadiyya communities receive formal equal citizenship under a frame that refuses to rank political standing by faith. Their protection in practice depends on enforcement against sectarian violence and on land-and-property tribunals functioning; the historical exit available to them has been emigration, which the settlement is meant to make unnecessary rather than easy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Career civil servants, judges, and security-service professionals gain predictable promotion ladders and insulated tenure under rule-bound institutions. Under custodial arrangements their posts turned over with each intervention; under a religious-legitimacy frame their appointments became confessionally screened. They carry the daily administrative burden of implementing the charter's statutes.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, professional_bureaucratic_class, beneficiary,
    moderate, generational, constrained, national).

% The Islamist party faces registration delimitation, leadership exposure to wartime-collaboration and sedition proceedings, and exclusion from the charter's consensus tables. Its cadre's commitments are constitutive: converting to secular politics would dissolve the organization as itself, so exit from the frame is not available as repositioning. It contests through constitutional litigation, alliance-building with centrist parties, and mass mobilization, while its welfare and educational wings continue operating under tightening scrutiny.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, identity_locked, national).

% The officer corps' institutional claim to independent political guardianship — intervention precedent, budget lines outside legislative detail, trial jurisdiction over its own, and a self-image as final arbiter of national crises — is what the civilian-command clauses strip. The institution retains substantial corporate benefits under the new order: guaranteed funding, legal personality, professional autonomy in non-political spheres. It resists through public doctrine statements, slow-walking confirmation of civilian appointees, and quiet non-compliance, while an outright rupture would forfeit the legitimacy the revolutionary moment currently denies it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    powerful, generational, constrained, national).

% Millions of voters whose preferred political vehicle is delimited retain the franchise for remaining parties and unhindered private worship, but experience the settlement as a ruling that their identity's political expression sits outside constitutional respectability. Their choices are absorption into centrist coalitions, abstention, or underground organizing — none of which returns voice proportional to their numbers.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_voter_base, payer,
    powerless, biographical, constrained, regional).

% Madrasa networks and pulpit authorities outside formal party structures hold a developed account of the constitution's proper religious foundations and would press it in any constituent forum that seated them. The consensus process did not seat them; their influence now runs through sermons, informal arbitration, and pressure on registered parties rather than through the charter's own channels.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_clerical_networks, excluded,
    organized, generational, trapped, national).

% Adjudicates the registration challenges, command-order reviews, and basic-structure claims through which the charter's meaning is being fixed. Its docket composition — which readings arrive as justiciable controversies and which it declines to hear — is the clearest public record of which reading of the founding text is consolidating.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, supreme_court_appellate_bench, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-revolutionary collective-action problem: establishing rule-bound channels for transferring power (elections, courts, legislative oversight of the budget and appointments) so that regime change does not collapse into factional seizure or military re-intervention, and giving the revolutionary coalition's heterogeneous factions a shared procedural framework to negotiate inside.
% TRANSFER_FUNCTION: Moves political authority away from extra-constitutional holders — command authority from the officer corps to elected civilians, agenda-setting legitimacy from religious movements to registered procedural parties — and concentrates recognition, committee seats, and statutory drafting power in the secular-democratic coalition that authored the settlement.
% ABSENT_VOICES: Islamist clerical networks and the delimited party's grassroots were never seated in the consensus process; senior officers committed to custodial self-conception participated only defensively. Rural pious constituencies whose preferred settlement is guided nationalism were represented indirectly, through parties that signed strategically. Each would object that the unanimity recorded in the charter reflects who was in the room, not agreement on the text's meaning.
% DISAPPEARANCE_RATIONALE: If the secular-democratic mandate vanished overnight, the officer corps would reclaim custodial prerogatives within the first crisis, the delimited party would re-enter competition on religious-legitimacy grounds, minority-protection guarantees would lapse to whatever local majorities tolerated, and the revolutionary coalition would fragment into factional seizure — the entire post-revolutionary settlement rearranges around whichever extra-procedural actor moves first.
% FOUNDING_PROBLEM: A state born of a liberation war whose post-independence history cycled between civilian authoritarianism and military rule, with the religion-versus-secularism axis producing repeated constitutional crises: the charter was built to break both capture modes — military guardianship and religious-nationalist sovereignty — permanently, by making procedure and civilian command the only legitimate sources of political authority.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problems are attested from outside the benefiting parties: minority-rights organizations and regional election-monitoring missions document the sectarian-violence and transfer-of-power failures; comparative civil-military scholarship on the region attests the coup-cycle pattern; and the targeted seats themselves corroborate the problems while disputing the remedy — the officer corps publicly invokes instability (prescribing custodianship), and the delimited party invokes moral crisis (prescribing religious law). Corroboration of the founding problem is robust; corroboration of this reading's remedy is confined to the coalition that authored it.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.62: the coordination delivered is real (peaceful-transfer machinery, minority guarantees, single command line), but two seats pay concentrated, identity-level costs — a major party's political existence delimited, a powerful institution's founding self-conception stripped — which is more than coordination overhead. Suppression is 0.62 as a raw structural property (unscaled by power or scope; the engine scales only extractiveness): registration tribunals, prosecution exposure, financing scrutiny, and command directives are load-bearing, because both targeted seats reject the frame rather than merely bearing it. Theater is 0.22 — the institutions mostly function, but a growing share of civilian-supremacy activity is ceremonial performance over an informally influential officer corps, hence the slowly rising series. Accessibility_collapse is 0.38: the rival readings remain live political programs and the delimited party can rebrand, so alternatives persist rather than collapsing. Resistance is 0.72: litigation, street mobilization, public-doctrine campaigns, and quiet non-compliance are constant. The temporal series run on one shared grid (months since charter signature, T0–T36); points through T12 are observed, later points projected. The suppression_requirement series is authored because the story's tracked dynamic IS enforcement-capacity build-out — tribunals stood up, directives issued — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the commission's and the secular parties' positions the charter is founding work: the coordination they built, defended at real cost. From the delimited party's position the same text is a ruling that its constituency's identity sits outside constitutional respectability; from the officer corps' position it is a demotion dressed as reform. Same-level lateral dynamics matter: the secular parties and the Islamist party are both organized national parties of comparable reach, differentiated not by global standing but by which side of the frame their identity occupies and by exit — the secular parties can lose an election and continue, while the Islamist party's exit is identity-fused. A tacit strange-bedfellow alignment between the two targeted seats (military restraint exchanged for softened registration review) is the coalition possibility the settlement's designers fear most.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the secular parties, urban civil society, the bureaucratic class, and the minorities; victim declarations drive high directionality for the Islamist party, the officer corps' autonomous authority, and the Islamist voter base. One override is authored: the derivation would place the military near the full-target end (~0.95) from its victim-only declaration, but the institution retains substantial corporate benefits under the new order — guaranteed funding, legal personality, professional autonomy in non-political domains — so its effective directionality is corrected downward to 0.78 at the powerful atom (no other stakeholder shares that atom). The Islamist voter base is left to derivation: for that seat the arrangement is nearly pure delimitation, and its powerlessness plus constrained exit correctly amplify effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents mislabeling in both directions. Calling the arrangement a pure rope erases the concentrated losses of the delimited party and the stripped officer corps — losses large enough that both seats actively contest the frame rather than merely bearing it. Calling it a snare erases the coordination good that even the targeted seats partially consume (courts the party litigates in, a command structure the officer corps relies on professionally). The founding problem — the coup-and-crisis cycle with religion as the recurring constitutional axis — remains live, so no mandatrophy resolution is declared: the arrangement's mandate has not outlived its function, and the absence of any sunset clause is consistent with a settlement claiming permanence rather than transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the july_charter_sovereign_legitimacy kernel consolidates — and how would the victim set and effective extraction shift under each sibling reading?',
    'Track appellate-bench basic-structure holdings, amendment politics, and which seats the implementation statutes actually bind; the reading whose exclusions acquire working enforcement machinery is the one consolidating.',
    'Under the military_custodian_reading the victim set flips — civilian parties become the constrained seats and this file''s beneficiaries become targets; under the guided_nationalism_reading victims shift to secularists and religious minorities. Per-seat classification recomputes accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: one charter text, three readings, divergent victim sets and epsilon values.').

omega_variable(
    civilian_supremacy_consolidation,
    'Will the civilian-command provisions consolidate into settled practice through the first full electoral cycle, or decay into ceremonial supremacy over a military that remains informally decisive?',
    'Observe budget-oversight compliance, confirmation fights over defense appointments, and whether the officer corps complies with or litigates command directives.',
    'Consolidation pulls the arrangement toward stabilized coordination; decay drives the theater ratio upward and drifts the operative type toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_supremacy_consolidation, empirical, 'Whether civilian supremacy becomes practice or performance.').

omega_variable(
    exclusion_permanence,
    'Is the delimitation of the Islamist party a transitional-justice measure with a defined endpoint, or a permanent structural exclusion of a political identity?',
    'Examine statutory sunset language, registration-review timelines, and whether re-registration conditions are satisfiable in principle by the delimited organization.',
    'A credible endpoint recasts the exclusion as transitional support with a terminus; permanence deepens the measured extraction borne by the party and its base and raises drift risk toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_permanence, conceptual, 'Whether the political-Islam exclusion is transitional or permanent.').

omega_variable(
    minority_protection_gap,
    'Does formal secular equality deliver actual physical and legal protection to religious minorities, or does a persistent gap separate the frame from enforcement?',
    'Compare sectarian-incident rates, case-resolution statistics, and restitution outcomes before and after charter implementation.',
    'A wide gap shrinks the realized subsidy flowing to the declared minority beneficiary seat, shifting the apparent asymmetry further toward the enforcing coalition and raising its effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_gap, empirical, 'Gap between secular-equality guarantee and minority protection in practice.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the pressure experienced by Islamist political actors wholly structural (registration law, prosecution exposure, financing controls) or partly internalized (self-censorship exceeding legal requirement)?',
    'Post-amnesty speech patterns: if pulpit and campus discourse remains narrowed after legal barriers are lifted or relaxed, an internalized component is confirmed.',
    'Internalized pressure travels with the affected seats after legal reform, raising effective suppression above what the structural instruments alone would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized component of the pressure on Islamist political expression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcsl_secular_democratic_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(jcsl_secular_democratic_tr_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 36, 0.22).

% Extraction over time
narrative_ontology:measurement(jcsl_secular_democratic_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jcsl_secular_democratic_be_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(jcsl_secular_democratic_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(jcsl_secular_democratic_be_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(jcsl_secular_democratic_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(jcsl_secular_democratic_be_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(jcsl_secular_democratic_be_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 36, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jcsl_secular_democratic_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jcsl_secular_democratic_su_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(jcsl_secular_democratic_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(jcsl_secular_democratic_su_t18, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(jcsl_secular_democratic_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(jcsl_secular_democratic_su_t30, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(jcsl_secular_democratic_su_t36, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'the July Charter's settlement of sovereign legitimacy' is a single colloquial label covering three structurally distinct constraints — one per reading of the kernel. This file (secular-democratic reading) authors epsilon 0.62 with victims jamaat_e_islami, military_autonomous_authority, and islamist_voter_base; the guided-nationalism sibling authors a different epsilon with secularists and religious minorities among its victims; the military-custodian sibling authors a different epsilon with civilian parties among its victims. The readings share a ratification event but not a constraint: measuring one with another's observable changes epsilon, which is the signature of distinct constraints. Edges run from this file to both siblings because this reading sets the institutional baseline (registered-party universe, command structure) that the sibling readings must amend or capture to operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
