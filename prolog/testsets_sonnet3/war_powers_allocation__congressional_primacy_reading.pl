% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story instantiates the congressional-primacy reading of the war
 *   powers allocation kernel: that military force beyond immediate
 *   self-defense requires explicit prior congressional authorization as a
 *   matter of constitutional necessity, not merely political prudence. Under
 *   this reading, the standing pattern of executive-initiated,
 *   congressionally-unratified deployments (Korea forward through
 *   contemporary drone and expeditionary campaigns) constitutes an extractive
 *   bypass of the legislature's constitutionally assigned war-initiation
 *   power. The coordination function this reading identifies (preventing snap
 *   unilateral war) is real, but the standing arrangement it critiques has,
 *   in this reading's own assessment, been substantially captured by
 *   executive practice that treats notification and post-hoc funding votes as
 *   a substitute for prior authorization. ε is authored high because THIS
 *   reading holds the standing arrangement to be substantially extractive of
 *   congressional and public authority — not because unilateral action is
 *   efficient, but because the reading holds that efficiency is not the
 *   constitutional test.
 *
 * KEY AGENTS:
 *   - executive_branch_war_council: agenda_setter/beneficiary (institutional/arbitrage) — initiates and sustains deployments, captures decision speed
 *   - congress_war_power: payer (institutional/constrained) — holds the formal power, bears the erosion of its exercise
 *   - standing_military_command_structure: beneficiary (institutional/constrained) — executes under streamlined executive authorization
 *   - public_deliberative_check: payer, non-agent (powerless/trapped) — diffuse democratic interest in prior debate
 *   - deployed_service_members: excluded (powerless/trapped) — bear risk, no voice in the authorization dispute
 *   - constitutional_scholarship_community: observer (analytical/analytical) — assesses the pattern against text and history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'f159b857-a1e2-455f-8ed2-d8807d44403d').
narrative_ontology:cs_kernel_codification('f159b857-a1e2-455f-8ed2-d8807d44403d', fixed_text).
narrative_ontology:cs_authority_grounding('f159b857-a1e2-455f-8ed2-d8807d44403d', lineage).
narrative_ontology:cs_interpretation_layer_present('f159b857-a1e2-455f-8ed2-d8807d44403d').
narrative_ontology:cs_reading_relation('f159b857-a1e2-455f-8ed2-d8807d44403d', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('f159b857-a1e2-455f-8ed2-d8807d44403d', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('f159b857-a1e2-455f-8ed2-d8807d44403d', foundational, declare_war_clause_binds_prior_to_sustained_force).
narrative_ontology:cs_axiom_status(declare_war_clause_binds_prior_to_sustained_force, holdable).
narrative_ontology:cs_axiom_grounding('f159b857-a1e2-455f-8ed2-d8807d44403d', declare_war_clause_binds_prior_to_sustained_force, conventional).
narrative_ontology:cs_axiom('f159b857-a1e2-455f-8ed2-d8807d44403d', secondary, commander_in_chief_power_excludes_war_initiation).
narrative_ontology:cs_axiom_status(commander_in_chief_power_excludes_war_initiation, holdable).
narrative_ontology:cs_axiom_grounding('f159b857-a1e2-455f-8ed2-d8807d44403d', commander_in_chief_power_excludes_war_initiation, conventional).
narrative_ontology:cs_reference_frame('f159b857-a1e2-455f-8ed2-d8807d44403d', founding_era_deliberative_war_initiation).
narrative_ontology:cs_drift_state('f159b857-a1e2-455f-8ed2-d8807d44403d', post_war_powers_resolution_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f159b857-a1e2-455f-8ed2-d8807d44403d', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch_war_council).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, standing_military_command_structure).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_war_power).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, public_deliberative_check).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, declare_war_clause_supremacy).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, collective_deliberation_before_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military deployments citing commander-in-chief authority, national security urgency, and operational secrecy. Controls the tempo and framing of engagements, often presenting force as already underway before Congress can convene meaningful debate. Benefits from speed and discretion that congressional authorization would constrain.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch_war_council, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch_war_council, beneficiary).

% Holds the constitutional Declare War and appropriations powers but is structurally bypassed once forces are committed — reversing an ongoing deployment carries political and institutional costs that authorizing it upfront would not have. Its formal check becomes a ratification exercise after the fact rather than a prior authorization gate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_war_power, payer,
    institutional, generational, constrained, national).

% Executes deployments authorized through executive channels rather than statute; benefits from streamlined chain-of-command decision-making that does not wait on legislative timelines, but bears no direct extraction cost under this reading.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, standing_military_command_structure, beneficiary,
    institutional, biographical, constrained, global).

% The collective democratic interest in having sustained military commitments debated and authorized by elected representatives before troops are committed. This interest is diffuse and non-institutional but is what the constitutional design was meant to protect; it is diminished whenever force proceeds without the deliberative process running first.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, public_deliberative_check, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__congressional_primacy_reading, public_deliberative_check).

% Bear the direct physical risk of engagements whose legal authorization is contested. Have no voice in whether the deployment proceeds through the congressional or executive channel; their interest in a settled authorization framework is real but they are not parties to the constitutional dispute.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deployed_service_members, excluded,
    powerless, immediate, trapped, global).

% Analyzes the historical pattern of authorization bypass against the constitutional text and founding-era debates, without a direct stake in any particular deployment's outcome.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_scholarship_community, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the constitutional design coordinates a genuine collective-action problem: preventing a single office from committing the nation to sustained bloodshed without the broader political consensus that a legislative vote requires, and preventing snap deployments from outrunning public deliberation.
% TRANSFER_FUNCTION: Moves decision authority over sustained military force from the collective legislative body (constitutionally assigned) to the executive when authorization is bypassed — transferring both the formal war-initiation power and the political accountability that would otherwise attach to a recorded congressional vote.
% ABSENT_VOICES: Deployed service members bear the operational risk of contested-authorization engagements but have no standing to litigate or vote on the authorization question. Foreign populations in theaters of unilateral action are entirely outside the domestic constitutional conversation despite being most materially affected.
% DISAPPEARANCE_RATIONALE: If the congressional-authorization requirement were understood to have fully disappeared as binding practice, the executive branch would treat prior authorization as advisory rather than necessary — a world already partially realized in the post-1973 pattern of contested deployments. Congress disputes that this has happened; the executive branch's practice suggests otherwise. Whether the world 'rearranges' turns on whether one credits the constitutional text or the accumulated practice as the operative baseline.
% FOUNDING_PROBLEM: The framers sought to prevent a monarch-style unilateral war-initiation power, having witnessed the British crown's ability to commit the nation to war without parliamentary consent; the Declare War Clause and appropriations power were designed to require the branch closest to the people to authorize sustained bloodshed before it began.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative-government scholars outside both branches (not benefiting from either reading) attest that the founding-era concern with unchecked executive war-initiation remains empirically live — cited repeatedly in War Powers Resolution debates, congressional hearings following Vietnam, Kosovo, Libya, and Syria deployments, and in dissenting opinions from federal judges declining to reach the merits of war-powers suits on justiciability grounds rather than disputing the underlying constitutional concern.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, contested).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.35 to 0.68) tracking the accumulating pattern of unauthorized or thinly-authorized deployments since the War Powers Resolution's passage — each unrebuked instance normalizes the next. Suppression is authored high (0.72) because this reading holds that inherent-authority claims must be actively suppressed as constitutionally illegitimate for the coordination function to hold; the suppression is aimed at delegitimizing the sibling readings' premises, not merely at foreign adversaries. Theater ratio is moderate (0.4) reflecting the substantial genuine constitutional debate and litigation activity alongside performative congressional resolutions that condemn deployments without exercising the appropriations leverage that would actually stop them.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the structural beneficiary under this reading: it captures decision speed and discretion at the expense of the branch constitutionally assigned the war-initiation power. Congress is the primary victim — not because it lacks formal power, but because the practical mechanics of reversing an ongoing deployment (political cost of 'abandoning troops in the field,' appropriations riders becoming the only leverage point) convert its prior-authorization power into a weak, after-the-fact ratification role. The standing military command structure benefits operationally from streamlined authorization but is not itself an extractive agent — it operates within whatever authorization channel it is given.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy in one direction and risks it in another: the founding problem (unchecked unilateral war-initiation) remains empirically live per outside constitutional-historian corroboration, so classifying the constitutional requirement itself as an obsolete mandate would be wrong. But the specific enforcement mechanism this reading is measuring — the practical operation of congressional check — has drifted toward theatrical ratification. The tangled_rope classification captures this: genuine coordination function (preventing monarchical war power) persists at the level of constitutional design, while the executive-branch capture of the actual authorization channel constitutes ongoing asymmetric extraction requiring active enforcement (in the form of continued suppression of the sibling inherent-authority reading) to sustain the imbalance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_bypass_versus_constitutional_settlement,
    'Is the accumulated pattern of executive-initiated deployments without prior congressional authorization a series of ongoing constitutional violations (this reading''s premise), or has repeated executive practice and congressional acquiescence effected a de facto constitutional settlement that has altered the operative allocation of war powers?',
    'A definitive Supreme Court ruling on a war-powers merits question (courts have consistently avoided this via political-question and standing doctrines), or a sustained multi-decade congressional reassertion (e.g., successful invocation of the War Powers Resolution''s automatic withdrawal provision) that would test whether the formal power still has practical teeth.',
    'If de facto settlement is found, this reading''s classification of executive action as ''extraction'' becomes historically anachronistic — the standing arrangement would be better read as an evolved rope under the functional_accommodation or inherent_executive readings. If no settlement is found and violation is confirmed, the tangled_rope/high-suppression classification is vindicated and the drift toward snare intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_bypass_versus_constitutional_settlement, conceptual, 'Whether the practice constitutes ongoing violation or an unacknowledged constitutional amendment by practice.').

omega_variable(
    congressional_self_disempowerment,
    'To what extent is Congress''s diminished war-powers check a product of executive extraction versus Congress''s own institutional choice to avoid politically costly authorization votes (deniability-seeking)?',
    'Analysis of congressional voting patterns and rhetoric around authorization votes — do members express desire for a vote that leadership withholds, or does leadership actively avoid scheduling votes members do not want to be forced to take?',
    'If Congress is complicit in avoiding the vote for its own political cover, the victim classification for congress_war_power is complicated — the branch may be a co-architect of its own bypass rather than a pure victim, which would shift some of the extraction analysis toward a coordination failure rather than a unilateral executive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_self_disempowerment, empirical, 'Whether congressional weakness is imposed or self-selected.').

omega_variable(
    kernel_framing_alternative,
    'Is the kernel more accurately framed as the constitutional text itself (Declare War Clause vs. Commander-in-Chief Clause), or as the layered practice-tradition of War Powers Resolution compliance/non-compliance that has accreted since 1973?',
    'Compare classification outcomes treating the kernel as (a) the 1787 text alone versus (b) the text plus 50 years of War Powers Resolution invocation practice as the operative kernel.',
    'Framing (a) treats all deviations as violations of a fixed 18th-century text; framing (b) would treat the WPR''s consultation/reporting requirements (rather than prior authorization) as the settled contemporary kernel, which would substantially lower this reading''s ε since consultation-and-report compliance is much higher than prior-authorization compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Alternative kernel framings (founding text vs. accreted post-1973 practice) that would change the extraction measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_powers_allocation kernel. congressional_primacy_reading (this story) treats any sustained force beyond immediate defense as requiring prior authorization and classifies bypass as extraction from Congress (tangled_rope, ε=0.68). inherent_executive_reading treats commander-in-chief authority as sufficient on its own and would classify the same historical pattern as legitimate coordination (expected low ε, rope-leaning). functional_accommodation_reading treats the authorization requirement as context-dependent (imminent threat vs. prolonged campaign) and would classify the pattern as partially extractive depending on operational duration (expected moderate ε, scaffold-or-tangled-rope-leaning). All three share the same underlying historical deployment record but assign it structurally different beneficiary/victim sets and different ε values because they hold different foundational axioms about where war-initiation authority constitutionally sits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
