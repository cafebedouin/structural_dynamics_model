% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Sovereign Right of Border Exclusion (Territorial-Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel
 *   border_legitimacy: the sovereignty_reading, under which border authority
 *   derives from territorial sovereignty and the state holds a legitimate
 *   right to exclude. Per the epsilon-invariance discipline, the two sibling
 *   readings (freedom_of_movement_reading, humanitarian_obligation_reading)
 *   are separate constraint stories with their own epsilon, victim sets, and
 *   classifications; they are linked only through network edges. The referent
 *   of every metric here is the standing arrangement under contest — the
 *   existing admission-control regime operated by destination states —
 *   assessed by this reading's own lights. That assessment is distinctive:
 *   the reading holds the core act of exclusion to be a legitimate exercise
 *   of self-government, so it does not read the arrangement as wholesale
 *   usurpation (the freedom_of_movement reading would); yet it concedes that
 *   the regime's operation imposes heavy, measurable burdens on those outside
 *   the boundary, and that parts of the enforcement apparatus exceed what
 *   membership maintenance requires. Hence a high-but-discounted epsilon, a
 *   victim set containing every excluded class (per the expected structural
 *   delta), and a claimed type of tangled_rope: a genuine coordination
 *   function (bounded democratic membership) fused with asymmetric extraction
 *   (outsiders pay so that insiders benefit), held together by continuous
 *   enforcement. Claim and metrics are independent authored facts; the engine
 *   computes per-seat types from the structural data. KEY AGENTS (by
 *   structural relationship): - receiving_state_government: Agenda setter
 *   (institutional/arbitrage) — writes and administers admission rules -
 *   border_enforcement_establishment: Operational agenda setter and budgetary
 *   beneficiary (powerful/identity_locked) — executes and escalates
 *   enforcement - citizen_electorate: Primary beneficiary
 *   (organized/constrained) — collects wage-floor and welfare-pool protection
 *   - excluded_migrants: Primary target (powerless/trapped) — bears denial of
 *   entry and route dangers - rejected_asylum_seekers: Target with a
 *   recognized but narrowing claim channel (powerless/trapped) -
 *   undocumented_workers_in_shadows: Interior target (powerless/trapped) —
 *   status-based exploitability - transnational_families: Split-position
 *   payers (moderate/constrained) - origin_state_governments: Excluded
 *   inter-institutional voice (institutional/constrained) -
 *   international_human_rights_bodies: Analytical observer
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.64).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Sovereign Right of Border Exclusion (Territorial-Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '5f508bf7-4142-4dce-b90a-e049b4f5e828').
narrative_ontology:cs_kernel_codification('5f508bf7-4142-4dce-b90a-e049b4f5e828', formalized).
narrative_ontology:cs_authority_grounding('5f508bf7-4142-4dce-b90a-e049b4f5e828', lineage).
narrative_ontology:cs_interpretation_layer_present('5f508bf7-4142-4dce-b90a-e049b4f5e828').
narrative_ontology:cs_reading_relation('5f508bf7-4142-4dce-b90a-e049b4f5e828', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f508bf7-4142-4dce-b90a-e049b4f5e828', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('5f508bf7-4142-4dce-b90a-e049b4f5e828', foundational, territorial_authority_carries_admission_discretion).
narrative_ontology:cs_axiom_status(territorial_authority_carries_admission_discretion, holdable).
narrative_ontology:cs_axiom_grounding('5f508bf7-4142-4dce-b90a-e049b4f5e828', territorial_authority_carries_admission_discretion, conventional).
narrative_ontology:cs_axiom('5f508bf7-4142-4dce-b90a-e049b4f5e828', foundational, democratic_self_government_requires_bounded_membership).
narrative_ontology:cs_axiom_status(democratic_self_government_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('5f508bf7-4142-4dce-b90a-e049b4f5e828', democratic_self_government_requires_bounded_membership, deontological).
narrative_ontology:cs_reference_frame('5f508bf7-4142-4dce-b90a-e049b4f5e828', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('5f508bf7-4142-4dce-b90a-e049b4f5e828', contemporary_global_mobility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5f508bf7-4142-4dce-b90a-e049b4f5e828', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_electorate).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, border_enforcement_establishment).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, receiving_state_government).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, undocumented_workers_in_shadows).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, transnational_families).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, westphalian_non_intervention_principle).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, bounded_democratic_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets admission categories, visa rules, and removal priorities; funds and directs the enforcement apparatus; defends the admission regime in courts and diplomacy. Gains electoral support from visible control and retains discretion over who joins the polity. Its costs are enforcement budgets and periodic humanitarian criticism. Exit from the arrangement is meaningless — it writes the rules.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Patrols, detains, processes, and removes. Its budget, staffing, procurement contracts, and institutional mission expand with each intensification of control; career paths and agency identity are built around the enforcement mission. It lobbies for expanded mandates and measures success in apprehension statistics. Reassignment to any other function would amount to dissolution of the institution as it currently exists.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, border_enforcement_establishment, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, border_enforcement_establishment, agenda_setter).

% Votes on the governments that set admission policy; enjoys protected wage floors in covered sectors, a welfare pool closed to newcomers, and membership in a self-governing community whose composition it can influence. Pays for enforcement through taxation and absorbs some labor shortages in uncovered sectors. Leaving would forfeit the membership advantages, so staying is the default.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_electorate, beneficiary,
    organized, generational, constrained, national).

% Seek entry for work, safety, or family and are barred by visa categories they rarely qualify for. They face physical barriers, interdiction at sea and land, and removal if caught; irregular routes carry mortality, debts to smugglers, and years of precarity. For most there is no legal channel at all, and waiting lists for the few that exist run decades.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution or collapse and present claims at borders or consulates; many are screened out, returned to transit states, or left in protracted encampments. They hold a recognized legal category but face narrowing definitions, fast-track procedures, and externalized processing that keeps them far from the territory they ask to enter.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, regional).

% Live and work inside the territory without status after entry was denied or expired. Employers can pay below-market wages and dismiss without recourse because reporting violations invites removal. Legalization paths are narrow or politically stalled; returning home often means returning to the conditions that drove them out.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, undocumented_workers_in_shadows, payer,
    powerless, biographical, trapped, national).

% Span the boundary — some members inside with status, kin outside without it. Family-reunification channels exist but are capped, slow, and income-tested; separations last years or become permanent. They navigate both sides of the regime and bear the emotional and financial costs of the gap between them.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, transnational_families, payer,
    moderate, biographical, constrained, continental).

% Receive remittance income and manage return flows but sit outside the admission conversations of destination states. They object to harsh treatment of their nationals abroad and to skill losses, yet lack leverage beyond diplomatic protest and readmission bargains struck under aid conditionality.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, origin_state_governments, excluded,
    institutional, generational, constrained, national).

% Monitor compliance with refugee and human-rights treaties, publish findings, and litigate test cases. Their rulings constrain enforcement methods at the margin but not the underlying admission discretion. They document the human costs the enforcement record generates.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, citizen_electorate).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates membership in political communities: fixes who may join the labor force, the welfare pool, and the electorate, so that members can run redistributive institutions and collective self-government over a composition they control.
% TRANSFER_FUNCTION: Moves access — to territory, labor markets, safety, and family life — from non-members to members; moves the costs of enforcement onto migrants themselves (route mortality, smuggler debts, detention time) and the fiscal costs onto destination-state taxpayers; moves remittance income outward to origin economies as a side-flow.
% ABSENT_VOICES: The excluded themselves: would-be migrants barred by rules they had no vote in writing, and origin-state governments managing the consequences without a seat in destination-state admission politics. International human-rights bodies hold a monitoring seat but no vote. Unanimity in destination-state politics arises partly because the people bound by the rules are constitutionally outside the constituency.
% DISAPPEARANCE_RATIONALE: Overnight abolition of admission control would trigger large-scale population movement toward wealthy labor markets, rapid wage and rent adjustment in destination cities, emergency redesign of welfare and voting rules, remittance surges and origin-country labor shocks, and acute political crisis in receiving states — the membership architecture of the state system would reorganize within years.
% FOUNDING_PROBLEM: After the collapse of dynastic and imperial orders, political communities needed a way to govern who belongs: to constitute a bounded people capable of democratic self-rule and mutual obligation, and to decide admission to it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: migration economists and demographers document the membership-composition pressures; UNHCR and UN DESA data attest the scale of displacement and movement; origin-state diplomats and refugee-law scholars confirm the governance problem is unresolved. Many of the same sources dispute that territorial sovereignty alone is the right resolution — corroboration of the problem, not of this reading's answer.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.64 at interval end) but discounted below what a rejectionist reading would assign: the sovereignty reading counts the core denial of entry as priced authority and locates the unjustified residue in enforcement excess — endangerment-based deterrence routing, prolonged detention, externalized processing that shifts suffering onto transit states, and the status precarity that makes interior labor exploitable. Suppression (0.72) is a raw structural property, unscaled by power or scope: walls, patrols, carrier sanctions, and removal threats are the machinery that holds the boundary, and it has ratcheted monotonically across the interval (1990s corridor build-ups, post-2001 securitization, 2015-era externalization) rather than oscillating — election cycles produce short fluctuations around the trend, not reversals, so no cyclical pattern is claimed and the suppression_requirement series tracks genuine enforcement-capacity growth. Theater (0.30) is real but a minority share: symbolic wall segments and publicized removal flights serve electorate sentiment alongside functional patrol and docket work. Accessibility_collapse sits mid-range (0.45): alternatives persist — irregular routes, humanitarian channels, regional free-movement zones — but each is narrowed or criminalized. Resistance (0.55) reflects sustained counter-mobilization: sanctuary networks, caravan formation, and strategic litigation; the powerless payer seats are not without coalition potential, and part of the measured resistance is exactly that coalition activity. All three series run on one shared eight-point grid; every tracked metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the agenda-setter seat (receiving_state_government) and its enforcement arm, the arrangement is legitimate self-government it administers — coordination whose costs it accepts. From the payer seats, the same structure is a hard barrier with lethal margins. Two same-level contrasts sharpen the divergence: excluded_migrants and rejected_asylum_seekers hold equal nominal powerlessness but different exit options — the asylum channel is a recognized (if narrowing) legal route, the economic-migrant channel is none — so their computed positions differ despite identical formal standing. And citizen_electorate experiences the arrangement as protective while paying its fiscal costs, placing it nearer the beneficiary end than its tax burden alone would suggest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for citizen_electorate, border_enforcement_establishment, and receiving_state_government; victim declarations drive high d for the four payer groups, amplified by trapped exits (no legal channel, removal threat, sunk migration debts) and, for the enforcement establishment, identity_locked exit — the agency has become its mission and cannot be reassigned without dissolving it. Spatial scope amplifies verification difficulty: the regime operates nationally but its targets sit globally, and externalized corridors push the coercive surface beyond easy observation. No directionality overrides were needed: the derivation chain from beneficiary/victim declarations plus exit options reproduces the intended positions, including the dual-positioned enforcement establishment (budgetary beneficiary, operational agenda-setter).
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — how bounded political communities govern membership — is live, corroborated from outside the benefiting parties, and the arrangement still performs its function. The classification disciplines both failure modes: reading the arrangement as pure extraction (snare) erases the real coordination function that democratic membership requires; reading it as pure coordination (rope) erases who pays for it. The theater_ratio series is the early-warning instrument for the opposite decay: if the founding problem ever died (full regional free movement, radically transformed mobility economics) while enforcement continued, the arrangement would persist as performance — theater climbing past function, with the status-dead/world-rearranges mismatch flagging a zombie signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the border_legitimacy kernel (reading: sovereignty_reading). What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Authoring the sibling files: freedom_of_movement_reading would move the entire excluded population into the victim set and raise epsilon toward the maximum over the same referent; humanitarian_obligation_reading would shrink the victim set to refused protection-seekers and discount epsilon for economically motivated exclusion. The disagreement is located in a single structural element: whether admission discretion is a sovereign prerogative or a presumptively restricted one.',
    'This file''s classification is stable under its own reading; cross-reading comparison is valid only family-wide, never by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and the disagreement locus are recorded here rather than in invented fields.').

omega_variable(
    justified_authority_extraction_margin,
    'How much of the measured burden on the excluded does proportionality analysis attribute to the minimum enforcement that membership maintenance requires, versus excess (endangerment-based deterrence, prolonged detention, externalized suffering)?',
    'Minimum-necessary enforcement studies comparing outcomes across states with similar membership goals but different method intensity; litigation records on proportionality of enforcement methods.',
    'If most of the burden is proportionate, epsilon falls toward the coordination-cost floor and the type drifts rope-ward; if excess dominates, epsilon rises and the snare boundary comes into view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justified_authority_extraction_margin, empirical, 'The margin between necessary boundary maintenance and enforcement excess — this reading''s own discount on epsilon.').

omega_variable(
    tacit_consent_of_the_excluded,
    'Can the excluded be said to accept the membership regime — via hypothetical contract, associative obligation, or the boundary''s mere existence — or is consent structurally impossible for the unrepresented?',
    'Political-theoretical analysis plus direct elicitation of excluded populations'' own normative attitudes toward the arrangements that bar them.',
    'If consent holds, the transfer reads as an agreed price of membership and the payer seats'' directionalities soften; if not, the transfer is imposition and the extraction reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_consent_of_the_excluded, conceptual, 'Whether the payer seats'' position is consented or imposed — the classic vulnerability of the sovereignty defense.').

omega_variable(
    deterrence_functionality_vs_symbol,
    'Is the enforcement surplus beyond functional control actually deterring crossings, or performing resolve for the electorate?',
    'Elasticity estimates of crossing rates against enforcement-intensity spikes; discontinuity analysis around symbolic build-outs such as wall segments and publicized removal operations.',
    'Feeds the theater_ratio trajectory: symbol-driven escalation predicts continued theater growth without effect on flows; functional escalation predicts the opposite pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_functionality_vs_symbol, empirical, 'Functional versus performative share of enforcement escalation.').

omega_variable(
    externalized_coercion_visibility,
    'Does externalizing enforcement to transit and third states reduce the regime''s coercive surface, or relocate it beyond the observation that feeds these metrics?',
    'Comparative mortality, detention, and pushback records along externalized versus domestically enforced corridors.',
    'If relocated, measured suppression understates the regime''s true coercive surface and the victim set extends into transit-state populations; if genuinely reduced, externalization is de-escalation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_coercion_visibility, empirical, 'Whether externalization hides or shrinks the coercive surface.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__sovereignty_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__sovereignty_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__sovereignty_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__sovereignty_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__sovereignty_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t35, border_legitimacy__sovereignty_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement_basis(bord_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__sovereignty_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__sovereignty_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__sovereignty_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__sovereignty_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t35, border_legitimacy__sovereignty_reading, base_extractiveness, 35, 0.64).
narrative_ontology:measurement_basis(bord_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__sovereignty_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__sovereignty_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__sovereignty_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__sovereignty_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__sovereignty_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__sovereignty_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t35, border_legitimacy__sovereignty_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(bord_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border legitimacy' covers three structurally distinct claims, instantiated as three readings of one kernel. This file is the sovereignty reading (epsilon high but discounted by the reading's acceptance of core exclusion; victim set = all excluded classes). The freedom_of_movement reading shares the referent and authors near-maximal epsilon; the humanitarian_obligation reading narrows the victim set to refused protection-seekers. Direction of influence: the sovereignty reading is the operative law in most jurisdictions, so it structurally shapes the width of the humanitarian exception (influences edge); none of the three logically forecloses another within a single party's framework. Family members link via affects_constraints; epsilon values are never averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
