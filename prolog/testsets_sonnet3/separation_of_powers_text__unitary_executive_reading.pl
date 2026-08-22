% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Article II Vesting Clause — Unitary Executive Reading
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This story instantiates the unitary executive reading of the
 *   separation-of-powers kernel: the Article II Vesting Clause ('The
 *   executive Power shall be vested in a President') is read as committing
 *   all executive power, without exception or subdivision, to the President
 *   alone, such that any statutory insulation of an executive officer from
 *   at-will presidential removal is a per se constitutional violation. This
 *   reading has moved from academic fringe (1930s-1980s) to a doctrine with
 *   real litigation traction (Seila Law v. CFPB 2020, Collins v. Yellen
 *   2021), steadily eroding but not yet overruling Humphrey's Executor
 *   (1935). The theater ratio falls over time as the doctrine moves from
 *   rhetorical assertion toward operative litigation success — less
 *   performance, more actual removal-power exercise.
 *
 * KEY AGENTS:
 *   - sitting_president: Primary beneficiary (institutional/arbitrage) — gains removal power and centralized control
 *   - federal_trade_commission, national_labor_relations_board, federal_reserve_board: Primary targets (organized-institutional/trapped-constrained) — their founding insulation is recast as constitutional defect
 *   - civil_service_administrative_law_judges: Most exposed individual targets (powerless/trapped) — no institutional weight to resist reclassification
 *   - congress: Excluded institutional designer whose statutory choices are displaced by the doctrine
 *   - federal_judiciary: Analytical observer and actual arbiter of the doctrine's reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.62).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.58).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Article II Vesting Clause — Unitary Executive Reading").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'c05c8051-2609-4363-9917-99632ab53d3b').
narrative_ontology:cs_kernel_codification('c05c8051-2609-4363-9917-99632ab53d3b', fixed_text).
narrative_ontology:cs_authority_grounding('c05c8051-2609-4363-9917-99632ab53d3b', lineage).
narrative_ontology:cs_interpretation_layer_present('c05c8051-2609-4363-9917-99632ab53d3b').
narrative_ontology:cs_reading_relation('c05c8051-2609-4363-9917-99632ab53d3b', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c05c8051-2609-4363-9917-99632ab53d3b', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('c05c8051-2609-4363-9917-99632ab53d3b', foundational, executive_power_is_constitutionally_indivisible).
narrative_ontology:cs_axiom_status(executive_power_is_constitutionally_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('c05c8051-2609-4363-9917-99632ab53d3b', executive_power_is_constitutionally_indivisible, conventional).
narrative_ontology:cs_axiom('c05c8051-2609-4363-9917-99632ab53d3b', secondary, removal_power_is_absolute_and_unreviewable).
narrative_ontology:cs_axiom_status(removal_power_is_absolute_and_unreviewable, holdable).
narrative_ontology:cs_axiom_grounding('c05c8051-2609-4363-9917-99632ab53d3b', removal_power_is_absolute_and_unreviewable, instrumental).
narrative_ontology:cs_reference_frame('c05c8051-2609-4363-9917-99632ab53d3b', decision_of_1789_removal_practice).
narrative_ontology:cs_drift_state('c05c8051-2609-4363-9917-99632ab53d3b', post_seila_law_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c05c8051-2609-4363-9917-99632ab53d3b', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_office_lawyers).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, unitary_executive_scholars).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_trade_commission).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_reserve_board).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, civil_service_administrative_law_judges).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, vesting_clause_grants_indivisible_executive_power).
narrative_ontology:constraint_vindicates(separation_of_powers_text__unitary_executive_reading, removal_power_is_inherent_to_execution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims plenary removal power over all executive officers on the theory that the Vesting Clause commits all executive power, undivided, to a single office. Uses this reading to justify removing agency heads without cause, directing agency enforcement priorities, and centralizing rulemaking review through OMB/OIRA. Benefits directly from every expansion of removal authority and every agency reclassified as merely executive rather than independent.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, sitting_president, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, sitting_president, beneficiary).

% Office of Legal Counsel and White House Counsel staff who build the litigation and legal-opinion architecture supporting unitary executive claims. Career and institutional prestige are tied to the doctrine's success; they can move between administrations and academia regardless of outcome.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_office_lawyers, beneficiary,
    institutional, biographical, mobile, national).

% Legal academics and think-tank figures whose scholarly reputations and influence are built on the unitary executive theory's doctrinal ascendance. They gain citations, clerkship pipelines, and judicial appointments when courts adopt their reading; the theory's rejection would not personally ruin them but would diminish their influence.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, unitary_executive_scholars, beneficiary,
    moderate, generational, mobile, national).

% A multi-member independent commission with for-cause removal protections for its commissioners, created by Congress to enforce antitrust and consumer protection law with insulation from day-to-day presidential politics. Under this reading, its removal protections are unconstitutional infringements on executive power; commissioners face potential removal at will, and the agency's independence — the entire premise of its design — is treated as a constitutional violation rather than a feature.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_trade_commission, payer,
    organized, biographical, trapped, national).

% Adjudicates labor disputes through members with for-cause removal protection, designed to insulate labor policy from partisan capture by either management-aligned or union-aligned administrations. This reading treats that insulation as itself the constitutional defect, not the safeguard it was designed to be.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, national_labor_relations_board, payer,
    organized, biographical, trapped, national).

% Sets monetary policy with statutory removal protections specifically intended to buffer interest-rate decisions from electoral-cycle pressure. Under strict unitary executive logic its governors' insulation is suspect; the Fed has somewhat more institutional weight to resist than smaller agencies but is not immune to the doctrine's logical reach, and markets price in the risk of politicized monetary policy if the reading prevails fully.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_reserve_board, payer,
    institutional, generational, constrained, national).

% ALJs within agencies adjudicate disputes with statutory tenure protections meant to ensure impartiality against the agency's own enforcement staff. Under this reading, their double for-cause removal insulation is unconstitutional; they have no leverage to resist reclassification and depend entirely on Congress or courts to preserve any protection at all.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, civil_service_administrative_law_judges, payer,
    powerless, biographical, trapped, national).

% Created the independent agencies by statute, deliberately designing removal protections as a check on presidential overreach and a guarantee of technocratic continuity across administrations. This reading treats congressional structural choices as constitutionally irrelevant when they conflict with the Vesting Clause's asserted indivisibility — Congress's institutional design judgment is displaced rather than engaged.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, excluded,
    institutional, generational, constrained, national).

% Adjudicates removal-power and agency-structure disputes case by case (Humphrey's Executor, Seila Law, Collins v. Yellen). Increasingly receptive to unitary executive arguments in recent decisions, but has not fully overruled Humphrey's Executor; sits as the actual arbiter of how far this reading's reach extends into surviving multi-member independent agencies.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, sitting_president).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, clear line of political accountability for the entire executive branch: voters elect one President, and every officer exercising executive power answers, in principle, to that one office — solving the problem of diffuse, unaccountable bureaucratic power exercised by no one voters can remove.
% TRANSFER_FUNCTION: Moves authority over enforcement priorities, personnel removal, and regulatory direction from insulated multi-member agency boards and their statutory tenure protections to the President and the executive office directly, at the expense of the specific insulation Congress built into each agency's founding statute.
% ABSENT_VOICES: The independent agencies' own institutional histories and the empirical record on regulatory capture avoided through insulation are largely absent from the doctrinal argument, which proceeds from constitutional text and structure rather than from the administrative-law record the agencies were built to address. Congress, as the drafter of the removal protections being invalidated, is treated as constitutionally irrelevant to its own institutional design choices.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading were abandoned entirely, existing independent agency structures would retain their for-cause removal protections without further litigation risk, commissioners and board members could act with insulation from at-will removal, and monetary policy, labor adjudication, and antitrust enforcement would continue operating on the multi-member, staggered-term model Congress designed — a substantial portion of the modern administrative state's institutional architecture depends on this reading NOT prevailing.
% FOUNDING_PROBLEM: The Framers sought to prevent a fragmented executive that would let no single officer be held accountable for the faithful execution of the laws, and to avoid a plural executive council model (rejected at the Constitutional Convention) that could produce deadlock or diffuse responsibility for abuses of power.
% FOUNDING_PROBLEM_CORROBORATION: Unitary executive proponents (executive branch lawyers, sitting Presidents of both parties when it suits their interests) attest the accountability problem remains fully live and unresolved by modern agency design. Independent, non-beneficiary corroboration is thinner: administrative law scholars outside the unitary executive camp and several sitting federal judges (including in Humphrey's Executor's continued, if narrowed, vitality) attest that Congress's post-1935 innovation of insulated expert agencies solved a different and equally real problem — regulatory capture and politicized enforcement — that the unitary reading's proponents do not treat as symmetrically weighty.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily (0.20 to 0.62) tracking the doctrine's litigation success — as courts increasingly credit unitary executive arguments (Seila Law, Collins), the reading extracts real institutional independence from previously insulated agencies rather than remaining a purely academic position. Suppression rises correspondingly (0.15 to 0.58) as the doctrine moves from argument to enforceable removal actions and OIRA centralization, actively foreclosing the alternative institutional design Congress chose. Theater ratio declines modestly because the doctrine increasingly operates through real removals and real litigation rather than symbolic assertion. Metrics track the doctrine's own trajectory as this reading's proponents would describe its success, not a hostile external measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting President is the paradigm beneficiary: every extension of removal power is a direct institutional and political gain, and the office rotates between parties so it is not one administration's private benefit but a standing beneficiary role. Independent agencies and their staff (commissioners, ALJs) are targets: their entire designed function — insulation from at-will removal — is what the reading declares unconstitutional. Civil service ALJs sit at the extraction extreme (powerless, trapped) with no institutional bulk to resist; the Federal Reserve, despite formally the same target status, has enough market-systemic weight that a directionality override toward moderated extraction is arguably justified but not applied here to keep the reading's structural logic uniform across agencies — this is itself worth noting as an omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diffuse, unaccountable executive fragmentation) is genuinely contestable as still-live: proponents can point to real accountability gaps in modern agency governance. But Congress's 1935-forward answer to a DIFFERENT founding problem (regulatory capture, politicized enforcement) is treated by this reading as constitutionally irrelevant rather than as a competing value the Constitution also permits Congress to pursue. This is the tangled rope signature: genuine coordination function (accountability) bundled with asymmetric extraction (the specific institutional designs Congress built to solve capture and technocratic continuity problems are declared unconstitutional, not merely policy-disfavored) — this reading does not merely disagree with agency independence, it forecloses Congress's power to choose it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vesting_clause_indivisibility_question,
    'Does the Vesting Clause''s grant of ''the executive Power'' to a single President textually and historically require that ALL executive power be exercised at the President''s unreviewable pleasure, or does it establish only a default that Congress may modify for specific offices where insulation serves a legitimate structural purpose?',
    'Historical evidence from the Decision of 1789 debates, the structure of the original cabinet, and 18th-century administrative practice regarding officer removal; this is a live originalist methodological dispute that the current Supreme Court has not fully resolved (Seila Law preserved Humphrey''s Executor for multi-member expert bodies while narrowing it elsewhere).',
    'If the indivisibility reading is historically correct, more independent agencies fall; if Congress retained a modification power for specific structural purposes, Humphrey''s Executor survives largely intact and this reading''s practical reach narrows substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vesting_clause_indivisibility_question, conceptual, 'Whether Article II textually commands strict removal indivisibility or permits congressional carve-outs.').

omega_variable(
    committer_kernel_reading_disagreement,
    'This story is one of three readings (formalist, functionalist, unitary_executive) of the same separation_of_powers_text kernel. Where exactly does the disagreement locate — in the text itself, in historical practice, or in the weighting of competing structural values (accountability vs. capture-resistance)?',
    'Not empirically resolvable in the ordinary sense; this is a live jurisprudential dispute among originalist, structuralist, and functionalist methodologies with no neutral arbiter beyond continued judicial and scholarly contest.',
    'The unitary_executive_reading and formalist_reading share methodological commitments (textualism, strict boundaries) but disagree on WHICH boundary is strict (removal vs. delegation); functionalist_reading rejects the premise that any boundary must be strict at all. A shift in dominant judicial methodology would reallocate which reading''s beneficiary/victim structure becomes operative law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_disagreement, conceptual, 'Locating the exact site of disagreement among the three kernel readings.').

omega_variable(
    federal_reserve_override_question,
    'Should the Federal Reserve''s directionality be overridden upward (toward beneficiary) relative to the other independent agencies, given its greater institutional weight and market-systemic importance to resist full removal-at-will exposure?',
    'Track actual litigation outcomes: Collins v. Yellen (2021) already exempted the Fed''s removal question from full resolution due to its unique structure; watch for future cases directly testing Fed governor removal.',
    'If courts continue treating the Fed as structurally distinct, an override would be warranted; if the unitary executive logic is applied uniformly, no override is warranted and the Fed sits identically to the FTC/NLRB in the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_reserve_override_question, empirical, 'Whether the Federal Reserve''s structural weight should shift its authored directionality relative to smaller independent agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1935, 0.5).
narrative_ontology:measurement_basis(sepa_tr_t1935, observed).
narrative_ontology:measurement(sepa_tr_t1970, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1970, 0.48).
narrative_ontology:measurement_basis(sepa_tr_t1970, observed).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement_basis(sepa_tr_t1990, observed).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(sepa_tr_t2010, observed).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t2020, observed).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(sepa_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1935, 0.2).
narrative_ontology:measurement_basis(sepa_be_t1935, observed).
narrative_ontology:measurement(sepa_be_t1970, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(sepa_be_t1970, observed).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement_basis(sepa_be_t1990, observed).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement_basis(sepa_be_t2010, observed).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(sepa_be_t2020, observed).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(sepa_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1935, 0.15).
narrative_ontology:measurement_basis(sepa_su_t1935, observed).
narrative_ontology:measurement(sepa_su_t1970, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement_basis(sepa_su_t1970, observed).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement_basis(sepa_su_t1990, observed).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(sepa_su_t2010, observed).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(sepa_su_t2020, observed).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(sepa_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.1).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the separation_of_powers_text kernel, decomposed per the epsilon-invariance principle because the same constitutional text yields structurally distinct claims with different epsilon values, different beneficiary/victim sets, and different classifications depending on interpretive methodology. formalist_reading targets agency RULEMAKING authority (nondelegation); unitary_executive_reading (this story) targets agency INDEPENDENCE from removal; functionalist_reading denies either targeting is constitutionally required. All three link to each other via affects_constraints because judicial adoption of one measurably changes the litigation environment and legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
