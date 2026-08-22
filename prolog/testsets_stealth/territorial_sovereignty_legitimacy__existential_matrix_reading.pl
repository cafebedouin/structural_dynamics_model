% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential-Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political theory/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'territorial_sovereignty_legitimacy': the existential-matrix reading,
 *   which holds that sovereignty legitimacy is not primarily juridical but
 *   existential — each people requires territorial control as a precondition
 *   for collective survival and identity expression, making the conflict
 *   fundamentally zero-sum regardless of legal or historical argument. The
 *   standing arrangement under contest (and the epsilon referent) is the
 *   governance of the territorial conflict under this frame: two peoples
 *   locked in exclusive-control logic, with juridical processes treated as
 *   epiphenomenal and compromise frameworks structurally unstable. The
 *   claim/metric gap is deliberate: the reading is CLAIMED as tangled_rope
 *   (it retains a genuine survival-coordination core) while the authored
 *   metrics describe heavily enforced, substantially extractive operation —
 *   the engine measures that divergence. Per the epsilon-invariance
 *   principle, the colloquial label 'sovereignty legitimacy' decomposes into
 *   three structurally distinct readings (covenant-continuity,
 *   existential-matrix, self-determination), each with its own epsilon,
 *   beneficiary structure, and classification; this file is one member of
 *   that family, linked to the others via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - hardline_security_establishments: agenda-setting administrator (institutional/identity_locked) — produces the threat frame both societies live inside; partially consumed by what it administers
 *   - dominant_demographic_military_bloc: primary beneficiary (powerful/arbitrage) — collects dominance dividends; holds exit routes others lack
 *   - diaspora_identity_investors: secondary beneficiary (powerful/mobile) — funds and affirms the frame from zero-risk distance
 *   - civilian_populations_both_sides: primary target (organized/trapped) — supplies conscripts, casualties, taxes; barred from cross-community coalition-building by both establishments
 *   - compromise_advocates_peacemakers: secondary target (moderate/identity_locked) — pays in careers, ostracism, and lives for keeping alternatives alive
 *   - international_juridical_bodies: analytical observer (institutional/analytical) — generates the juridical output the frame declares epiphenomenal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.78).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.82).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential-Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political theory/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '7029f235-0e18-45a3-a755-797caa1f580c').
narrative_ontology:cs_kernel_codification('7029f235-0e18-45a3-a755-797caa1f580c', distributed).
narrative_ontology:cs_authority_grounding('7029f235-0e18-45a3-a755-797caa1f580c', practice).
narrative_ontology:cs_interpretation_layer_present('7029f235-0e18-45a3-a755-797caa1f580c').
narrative_ontology:cs_reading_relation('7029f235-0e18-45a3-a755-797caa1f580c', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7029f235-0e18-45a3-a755-797caa1f580c', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_axiom('7029f235-0e18-45a3-a755-797caa1f580c', foundational, territorial_control_precondition_collective_survival).
narrative_ontology:cs_axiom_status(territorial_control_precondition_collective_survival, holdable).
narrative_ontology:cs_axiom_grounding('7029f235-0e18-45a3-a755-797caa1f580c', territorial_control_precondition_collective_survival, empirically_contingent).
narrative_ontology:cs_axiom('7029f235-0e18-45a3-a755-797caa1f580c', foundational, zero_sum_conflict_immunity_to_juridical_settlement).
narrative_ontology:cs_axiom_status(zero_sum_conflict_immunity_to_juridical_settlement, holdable).
narrative_ontology:cs_axiom_grounding('7029f235-0e18-45a3-a755-797caa1f580c', zero_sum_conflict_immunity_to_juridical_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('7029f235-0e18-45a3-a755-797caa1f580c', existential_territorial_necessity).
narrative_ontology:cs_drift_state('7029f235-0e18-45a3-a755-797caa1f580c', post_oslo_collapse_contemporary, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('7029f235-0e18-45a3-a755-797caa1f580c', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_security_establishments).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_demographic_military_bloc).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_identity_investors).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_both_sides).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_advocates_peacemakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military and intelligence leaderships on both sides produce the threat assessments, educational curricula, commemorative calendars, and emergency powers through which each society reads the other. Their budgets, career ladders, and veto over diplomacy depend on permanent mobilization; standing the frame down would dissolve the institutional self they have built across careers. They also bury their own dead in the conflicts the frame organizes, which is part of why they cannot treat it as optional.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_security_establishments, agenda_setter,
    institutional, generational, identity_locked, regional).

% Whichever community currently holds military superiority and demographic momentum collects the arrangement's dividends: expanded territorial control, international deference, and the ability to dictate or indefinitely stall settlement terms. Its members retain exit routes unavailable to others — emigration, enclave withdrawal, foreign residency — so rising costs push them outward rather than forcing renegotiation.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_demographic_military_bloc, beneficiary,
    powerful, generational, arbitrage, regional).

% Diaspora donors, lobby organizations, and identity communities fund hardline politics from jurisdictions where they bear none of the daily risk. The struggle supplies meaning, belonging, and a ready-made collective identity at a distance. Their financial exit is trivial and their exposure to the conflict's physical costs approaches zero.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_identity_investors, beneficiary,
    powerful, generational, mobile, global).

% Ordinary families on both sides supply the conscripts, the casualties, the displaced, and the taxes. Movement between the two polities is barred, emigration means abandoning home, kin, and graves, and each community's internal politics punishes open tiredness with the struggle. They hold votes and stage protests but rarely set the agenda; cross-community organizing that treats the other side as neighbors rather than enemies draws sanction from both establishments at once.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_both_sides, payer,
    organized, generational, trapped, regional).

% Negotiators, joint civil-society organizers, and politicians who campaign on territorial trade-offs pay in career destruction, social ostracism, and occasionally assassination — the killing of Rabin being the canonical case. Their professional and moral identity is bound to the peacemaking project, so exiting into quiet private life means abandoning who they are; meanwhile the frame's supporters cite their very presence as evidence that openness invites attack.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_advocates_peacemakers, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_advocates_peacemakers, excluded).

% The UN, the ICJ, and mediating states keep producing resolutions, advisory opinions, and peace plans premised on the assumption that legal settlement can allocate the territory. The existential frame reads their output as noise over the real driver; they continue because conceding that law cannot reach this conflict would gut their own reason for operating in the arena.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_juridical_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_security_establishments).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates each community's survival mobilization: shared threat assessment, military preparedness, social cohesion under siege conditions, and a common account of why territorial control cannot be relinquished. Whatever else it does, it solves the real collective-action problem of keeping a frightened population unified, armed, and vigilant.
% TRANSFER_FUNCTION: Moves political authority, budget share, and veto power toward whoever administers permanent mobilization (the security establishments of both sides) and toward whichever bloc currently holds dominance; moves political space away from compromise advocates; moves the costs of perpetual conflict — casualties, displacement, taxation, foreclosed futures — onto the civilian populations of both sides.
% ABSENT_VOICES: Advocates of arrangements that transcend exclusive sovereignty altogether — binationalists, federalists, confederal planners — are nearly absent from the conversation the frame structures, since the frame defines the choice space as 'which people controls the territory,' not 'whether exclusive control is the right unit.' Ordinary residents who want normalcy are present demographically but filtered out of agenda-setting by elite threat curation; they surface mainly in civil-society margins and international forums.
% DISAPPEARANCE_RATIONALE: If the existential frame vanished overnight, the security establishments would lose their organizing warrant, compromise advocates would regain agenda access, and settlement architectures now dismissed as naively juridical would reopen — a major rearrangement. But the parties dispute the baseline: the establishments attest that the underlying dangers are real and would regenerate the same logic within a generation, while the mediation community and much comparative scholarship attest that the frame itself is a principal driver, and that its removal (as in Franco-German or Northern Irish analogues) changes what becomes possible.
% FOUNDING_PROBLEM: Collective survival under conditions where juridical protection had demonstrably failed: the founding generation concluded, after catastrophic violence and the collapse of legal guarantees, that only territorial control held by one's own people secured existence, and that legal and historical arguments could not be relied upon to do so.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: historians independently document the failure of juridical protection in the founding era, and international-relations scholarship corroborates the reality of security-dilemma dynamics and the founding traumas. No external source attests the frame's strong form — that the conflict is permanently zero-sum and immune to legal settlement; that claim is maintained chiefly by the establishments whose authority it underwrites, and is disputed by comparative peace research and the mediation community.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the frame converts a contestable political question into an apparently non-negotiable survival fact, transferring authority and resources to administrators of permanent mobilization while foreclosing the compromise option-space. Suppression is higher still (0.82) and is a raw structural property, unscaled by power or scope: the frame's persistence requires actively suppressing alternative framings — securitized discourse, curricula, ostracism of compromisers, occasional assassination — not merely passive acceptance. Suppression here is roughly half structural (legal and political exclusion of alternatives) and half internalized (generational transmission of fear, commemorative infrastructure that makes doubt feel like betrayal); the internalized share is flagged in the doctrine-versus-condition omega. Theater ratio is moderate (0.53) and rising: a growing share of the frame's activity is ritualized threat display, commemorative mobilization, and juridical argumentation that the frame itself declares epiphenomenal — performance over a substrate it insists is unaffected by performance. Accessibility collapse is 0.62: once the frame is accepted, compromise alternatives collapse almost entirely (conceding territory equals accepting vulnerability equals the unthinkable), but they never fully vanish — peace movements, joint organizations, and periodic negotiations keep partial option-space alive. Resistance is 0.58: real, recurring, and punished, but never sufficient to break the frame. The measurement series run on one shared eight-point time grid (t in years since 1948) covering all three tracked metrics. The series are CYCLICAL, not monotonic: the 1979 Egyptian treaty and the 1993 Oslo opening produced visible dips in extraction and enforcement (t31, t45), followed by sharp rebounds after the Oslo collapse (t52 onward). Critically, the oscillation is itself an extraction mechanism — intermittent reinforcement: each cycle of hope, negotiation, violence, and vindication discredits compromisers further and ratchets the frame tighter, so the troughs get shallower (0.54 at t45 vs 0.66 at t31) and the peaks higher (0.78 at t76 vs 0.75 at t0). Coalition check: the civilian populations are nominally 'organized' and a cross-community coalition (joint veterans' movements, bereaved-parent circles) is the structurally available escape, but the frame's enforcement machinery sanctions exactly such coalitions from both directions simultaneously, which is why coalition potential remains unrealized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute different types from identical structure. From the security establishments' position the frame is tragic necessity — protective coordination they did not choose and maintain at the cost of their own dead; from the trapped civilian and compromised advocate positions the same structure operates as enforced foreclosure of political possibility, administered by the very seats that profit from its permanence. The diaspora seat experiences a fourth version: identity subsidy at zero risk. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the dominant bloc and diaspora investors (arbitrage-grade exit pushes the diaspora seat nearest the full-beneficiary end). Victim declarations drive high d for civilian populations (trapped, identity-bound — near full-target) and compromise advocates (identity_locked — also near full-target despite lower power). The one override corrects the administering seat: the derivation reads hardline_security_establishments as near-pure beneficiaries (agenda_setter + beneficiary declarations), but structurally they are partially captured by the frame they enforce — their children serve, their authority dies with the frame, and they cannot stand it down without institutional self-dissolution. The override raises institutional-power d to 0.30 to register that partial capture. Note the schema keys overrides by power atom, not by agent: the collateral effect on the international juridical bodies (also institutional) is immaterial because they occupy the analytical seat outside the extraction circuit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabelings. Reading the frame as pure tragedy — an inevitable feature of intercommunal existence, a natural law of peoples — would naturalize it and hide the identifiable beneficiaries and suppressed alternatives; declaring beneficiaries and enforcement keeps that naturalization visible as a claim rather than a fact. Conversely, reading it as pure extraction would erase the genuine coordination core: the founding traumas were real, the threats are recurrently validated, and a besieged population's survival mobilization solves a real collective-action problem. Tangled rope holds both truths. On mandatrophy proper: the founding problem (survival without reliable juridical protection) remains LIVE — corroborated by independent history — so no mandatrophy resolution is declared; but the frame's scope has quietly atrophied relative to its founding scale (current threats are an order of magnitude smaller than 1948-scale annihilation, yet the frame operates unchanged), which is recorded as contested rather than resolved and left to the temporal record to adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (existential_matrix_reading) of the territorial_sovereignty_legitimacy kernel; how would the classification shift if instantiated instead under the covenant_continuity_reading or the self_determination_reading?',
    'Compare against the sibling stories'' compiled classifications: the covenant reading relocates beneficiaries toward religious-nationalist institutions and grounds legitimacy in lineage; the self-determination reading flips the victim/beneficiary structure toward the Arab population''s majority claims and grounds legitimacy in a juridical principle. Divergence localizes the dispute in the ground-of-legitimacy element itself.',
    'Under the covenant reading, enforcement is doctrinal-transmissive rather than securitized and the victim set narrows; under the self-determination reading, the current dominance-holder becomes the primary target rather than the primary beneficiary. The zero-sum permanence claim — unique to this reading — is what generates the high suppression and the foreclosed-compromise structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings of the sovereignty-legitimacy kernel; sibling readings instantiate different constraints with different epsilon and beneficiary structure.').

omega_variable(
    doctrine_vs_underlying_condition,
    'Is the zero-sum dynamic a property of the underlying situation (genuinely incompatible survival requirements) or a property manufactured and sustained by the doctrine itself?',
    'Comparative analysis of resolved analogues (Franco-German reconciliation, Northern Ireland) combined with natural experiments where the frame weakened (the 1979 treaty window, the Oslo window): if compromise survived those windows until exogenous violence broke it, the doctrine is doing causal work; if every opening collapsed for reasons independent of the frame, the underlying condition dominates.',
    'If manufactured, the coordination story is substantially cover and the constraint sits nearer the pure-extraction end than the authored claim concedes; if genuine, a larger share of measured extraction is tragic coordination cost and the frame''s persistence is better explained by the condition than by its administrators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_underlying_condition, empirical, 'Whether the zero-sum substrate is real or doctrine-manufactured — the load-bearing uncertainty for the tangled-rope claim.').

omega_variable(
    dominance_capture_rotation,
    'The expected structural delta names the beneficiary as ''whichever side achieves demographic/military dominance'' — is the capturing seat stable or does it rotate across regime shifts?',
    'Longitudinal tracking of which seats absorb the arrangement''s gains across dominance transitions (pre- and post-1967, pre- and post-Oslo, post-2024): stable capture by one side''s establishment supports a pure-extraction reading; rotation in which each population alternately coordinates-and-pays supports the hybrid reading.',
    'Rotation strengthens the tangled-rope claim (both peoples are inside the same structure, alternately subsidized and extracted-from); stable capture would indicate the frame functions as one side''s instrument wearing a symmetrical mask.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominance_capture_rotation, empirical, 'Whether the beneficiary seat rotates with dominance or is fixed — determines whether the frame is symmetrical or masked-asymmetrical.').

omega_variable(
    zero_sum_falsifiability,
    'Is the zero-sum permanence premise falsifiable within the frame, given that peace-process failure counts as vindication and periods of calm count as enemy deception?',
    'Specify in-advance falsifiers the frame would have to accept (e.g., a negotiated territorial settlement surviving leadership turnover on both sides for a defined period) and test whether the frame''s interpreters admit them or reinterpret them as confirmation.',
    'If the premise is unfalsifiable as operated, the theater_ratio understates performative content, the frame decouples from evidence, and the constraint drifts toward inertial-theatrical maintenance — classification should be revisited on that trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_falsifiability, conceptual, 'Falsifiability asymmetry: whether the frame can lose on the evidence, or only win.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_existential_matrix_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tsl_existential_matrix_tr_t19, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 19, 0.38).
narrative_ontology:measurement(tsl_existential_matrix_tr_t31, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 31, 0.42).
narrative_ontology:measurement(tsl_existential_matrix_tr_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 45, 0.48).
narrative_ontology:measurement(tsl_existential_matrix_tr_t52, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 52, 0.4).
narrative_ontology:measurement(tsl_existential_matrix_tr_t62, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 62, 0.46).
narrative_ontology:measurement(tsl_existential_matrix_tr_t70, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 70, 0.5).
narrative_ontology:measurement(tsl_existential_matrix_tr_t76, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 76, 0.53).

% Extraction over time
narrative_ontology:measurement(tsl_existential_matrix_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(tsl_existential_matrix_be_t19, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 19, 0.79).
narrative_ontology:measurement(tsl_existential_matrix_be_t31, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 31, 0.66).
narrative_ontology:measurement(tsl_existential_matrix_be_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(tsl_existential_matrix_be_t52, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 52, 0.71).
narrative_ontology:measurement(tsl_existential_matrix_be_t62, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 62, 0.75).
narrative_ontology:measurement(tsl_existential_matrix_be_t70, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 70, 0.77).
narrative_ontology:measurement(tsl_existential_matrix_be_t76, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 76, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tsl_existential_matrix_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tsl_existential_matrix_su_t19, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 19, 0.72).
narrative_ontology:measurement(tsl_existential_matrix_su_t31, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 31, 0.64).
narrative_ontology:measurement(tsl_existential_matrix_su_t45, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(tsl_existential_matrix_su_t52, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 52, 0.73).
narrative_ontology:measurement(tsl_existential_matrix_su_t62, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 62, 0.78).
narrative_ontology:measurement(tsl_existential_matrix_su_t70, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 70, 0.8).
narrative_ontology:measurement(tsl_existential_matrix_su_t76, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 76, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, two_state_compromise_framework).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'sovereignty legitimacy' into three structurally distinct readings per the epsilon-invariance principle: covenant_continuity_reading (lineage/theological ground, transmissive enforcement), existential_matrix_reading (this file; existential ground, securitized enforcement, foreclosed compromise), and self_determination_reading (juridical-principle ground, majoritarian victim/beneficiary structure). The upstream readings are typically cited AS legitimacy claims that this reading deflates as epiphenomenal, so this reading structurally influences both siblings' operating environments (it drains persuasive force from their instruments) without being derivable from them. The two_state_compromise_framework edge records this reading's distinctive downstream prediction: compromise architectures are structurally unstable under the existential frame because accepting them equals accepting vulnerability. Each family member carries its own epsilon, beneficiaries, and claimed type; no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
