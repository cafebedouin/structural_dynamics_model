% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Debt Ceiling as Constitutionally Void Constraint (14th Amendment Sec. 4 Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the CONSTITUTIONAL NULLITY reading of the
 *   statutory debt ceiling kernel: the claim that the 14th Amendment's
 *   Section 4 ('the validity of the public debt of the United States,
 *   authorized by law...shall not be questioned') renders the statutory
 *   ceiling void whenever it conflicts with debt already authorized through
 *   the appropriations process. On this reading the ceiling is not a live
 *   legal constraint at all — it is a piece of statutory theater that
 *   Congress continues to perform (raising it periodically) and that Treasury
 *   has, in every crisis to date, treated as though binding, but which this
 *   reading holds has no valid claim against the constitutional duty to honor
 *   appropriated obligations. This is a Mountain claim: the constraint is
 *   asserted to have effectively zero legal force, comparable to a statute
 *   purporting to override a constitutional guarantee. It is distinguished
 *   sharply from its sibling readings — the coordination_scaffold_reading
 *   (which treats the ceiling as a genuine, functioning procedural device)
 *   and the extraction_snare_reading (which treats it as a weaponized
 *   extraction mechanism for legislative minorities) — which are separate
 *   constraint stories with their own ε values, not measurement variants of
 *   this one.
 *
 * KEY AGENTS:
 *   - executive_branch_treasury: primary claimed beneficiary of the nullity reading (institutional/arbitrage) — freed from binding constraint
 *   - congress_ceiling_statute_drafters: continues ceremonial enactment (institutional/constrained) — performs a vote this reading holds is legally inert when triggered
 *   - federal_bondholders_and_credit_markets: benefits from removed default risk (organized/mobile)
 *   - constitutional_law_scholars_and_courts: analytical observers of an unresolved justiciability question
 *   - legislative_minority_factions: excluded — their leverage strategy is premised on a legal claim this reading rejects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.04).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.08).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Debt Ceiling as Constitutionally Void Constraint (14th Amendment Sec. 4 Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, '8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3').
narrative_ontology:cs_kernel_codification('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', fixed_text).
narrative_ontology:cs_authority_grounding('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', lineage).
narrative_ontology:cs_interpretation_layer_present('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3').
narrative_ontology:cs_reading_relation('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', statutory_debt_ceiling__extraction_snare_reading, forecloses).
narrative_ontology:cs_axiom('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', foundational, section_four_supersedes_conflicting_statute).
narrative_ontology:cs_axiom_status(section_four_supersedes_conflicting_statute, holdable).
narrative_ontology:cs_axiom_grounding('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', section_four_supersedes_conflicting_statute, conventional).
narrative_ontology:cs_axiom('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', secondary, appropriated_debt_authorization_is_self_validating).
narrative_ontology:cs_axiom_status(appropriated_debt_authorization_is_self_validating, holdable).
narrative_ontology:cs_axiom_grounding('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', appropriated_debt_authorization_is_self_validating, conventional).
narrative_ontology:cs_reference_frame('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', constitutional_supremacy_over_conflicting_statute).
narrative_ontology:cs_drift_state('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', post_2011_debt_ceiling_crises_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('8e7cb0d4-dd97-400e-bdcf-0b440fdbaeb3', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, executive_branch_treasury).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, federal_bondholders_and_credit_markets).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_over_statute).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_four_self_executing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, Treasury's constitutional duty to service the public debt (Section 4) legally overrides the statutory ceiling the moment the two conflict; the ceiling has no valid claim to bind Treasury's borrowing once appropriations have been enacted. Treasury benefits from this reading because it dissolves the periodic threat of enforced default and restores unconstrained execution of congressionally appropriated spending. In this reading Treasury COULD simply continue issuing debt to meet obligations Congress already created, treating ceiling votes as inoperative theater rather than binding law.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, executive_branch_treasury, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, executive_branch_treasury, agenda_setter).

% Continues to pass and periodically 'raise' the statutory ceiling as though it were binding, performing votes that this reading holds have no operative legal force once they conflict with the Section 4 duty to honor validly authorized debt. Congress's institutional self-conception depends on the ceiling mattering; under the nullity reading its repeated votes are ceremonial confirmation of a constraint that a court applying Section 4 would not enforce against Treasury.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_ceiling_statute_drafters, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress_ceiling_statute_drafters, excluded).

% Holders of U.S. Treasury securities and the broader credit market benefit from any reading that removes the recurring possibility of a technical default triggered by ceiling brinkmanship. Under the nullity reading, the constitutional debt-servicing duty forecloses that scenario entirely, stabilizing valuation of the world's reference risk-free asset regardless of legislative gridlock.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_bondholders_and_credit_markets, beneficiary,
    organized, biographical, mobile, global).

% Assess whether Section 4's text ('the validity of the public debt...shall not be questioned') is self-executing against a conflicting statute, or merely an interpretive canon with no justiciable remedy. No court has definitively ruled on the nullity claim; scholars disagree sharply on whether a live case-or-controversy could even reach the merits, since a President invoking the reading pre-empts the confrontation that would generate standing.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_law_scholars_and_courts, observer,
    analytical, civilizational, analytical, national).

% Factions that use ceiling votes as leverage for unrelated policy concessions have no voice in this reading's framework — if the constraint is void, their leverage evaporates entirely. They are structurally excluded from this reading's account because the reading treats their entire strategic use of the ceiling as premised on a legal error.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, legislative_minority_factions, excluded,
    organized, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading — the ceiling performs no coordination function because it has no binding legal force once it conflicts with the Section 4 duty to honor appropriated, validly-authorized public debt. The genuine coordination problem (aligning spending authorizations with borrowing authority) is fully solved upstream by the appropriations process itself; the ceiling adds a second veto point that this reading holds is constitutionally inoperative when triggered.
% TRANSFER_FUNCTION: Under a genuine nullity, nothing is transferred by the constraint's operation because it does not operate — Treasury borrows as appropriations require. What IS transferred, on this reading, is political risk and negotiating leverage away from legislative minorities and toward the executive and bondholders, since the threat that gave minorities leverage is revealed as legally empty.
% ABSENT_VOICES: Legislative minority factions who rely on the ceiling as leverage have no standing in this reading's account — their strategic interest in the ceiling's continued vitality is treated as resting on a legal mistake, not as a competing legitimate claim. A future litigant with concrete injury from a Treasury default-avoidance action might also be absent, since the reading's practical effect is to prevent the controversy that would generate a plaintiff.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling were formally repealed tomorrow, under this reading almost nothing would change in substance — Treasury already possesses (per this reading) the constitutional authority to service appropriated debt regardless of the ceiling's text. The ceremonial votes would simply stop; credit markets would price the removal of theatrical brinkmanship risk but the underlying fiscal mechanics (appropriations drive borrowing) are unaltered because, on this reading, they were never actually gated by the ceiling to begin with.
% FOUNDING_PROBLEM: The ceiling itself was originally built (1917, later consolidated in 1939) to give Treasury administrative flexibility to issue debt without a separate congressional vote for every bond issuance, replacing case-by-case authorization. The nullity reading holds that this original coordinating problem was fully mooted by later developments and that Section 4, ratified in 1868, structurally forecloses any subsequent statute from making that flexibility mechanism into a binding veto over debt already authorized through appropriations.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside any branch that benefits from the reading (e.g., academic commentary invoking Section 4's text and drafting history, including work cited in the 2011 and 2023 crises by legal historians with no Treasury or congressional affiliation) attest that the original coordination purpose of the ceiling has been supplanted by its use as a confrontation device, and that the Section 4 argument exists independently of any particular administration's political interest in deploying it. No neutral corroborator affirms that the ceiling still serves its 1917 coordination function as originally designed.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.04) because under this reading the constraint has no operative legal force to extract anything through — there is no toll being collected, because the gate this reading asserts is unlocked. Suppression is authored low (0.08) because no one is coerced by a void constraint; the only 'suppression' present is the residual social pressure to treat the ceiling as though it binds, which is closer to convention than coercion. Theater ratio is authored HIGH and rising (0.10 in 1917 to 0.82 in 2025) precisely because this reading's central empirical claim is that the ceiling's persistence is now almost entirely performative — Congress enacts 'raises,' the President negotiates as if bound, media covers 'brinkmanship,' and none of it is, on this reading, legally necessary. Accessibility collapse is low (0.15): the constitutional alternative (invoke Section 4, keep issuing debt) is understood and has been publicly proposed by legal scholars and even sitting officials, so alternatives have NOT collapsed — the constraint's apparent bindingness persists despite the alternative being visible, which is itself part of what makes the theater-ratio reading credible. Resistance is moderate (0.35): the nullity argument is actively resisted by those who prefer the constraint be treated as binding (both those who benefit from ceiling brinkmanship and those with genuine constitutional-avoidance concerns about untested doctrine).
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and bondholders are coded as beneficiaries of this reading because a successful nullity claim removes recurring default risk that harms both institutional flexibility and credit-market stability. Congress sits in a genuinely divided seat: as an institution it is coded agenda_setter (it authors the ceiling statute and could repeal it) but its interest in the reading is ambivalent — the reading strips it of a mechanism, so parts of Congress (leadership seeking governing stability) may quietly welcome the nullity as a pressure valve while other factions (using the ceiling as leverage) are effectively excluded and opposed. Legislative minority factions are coded excluded/payer-in-effect rather than beneficiary because the entire value of their position depends on the ceiling being treated as binding; the nullity reading, if adopted, extracts nothing from them structurally but does remove a strategic asset they currently hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'dead' — the original 1917/1939 administrative-convenience rationale (letting Treasury issue debt without per-bond congressional votes) has been fully superseded first by routine 'raise' legislation and then by repeated use as a confrontation device, which is a different function entirely from the founding one. This is the diagnostic case for mandatrophy: the disappearance_verdict is authored 'world_unchanged' (nothing substantively changes if the ceiling vanishes, on this reading, because Treasury already has the constitutional footing to proceed) while the founding_problem_status is 'dead' — that MISMATCH (dead + world_unchanged, not dead + world_rearranges) is coherent here specifically because the nullity reading's whole claim is that the arrangement was ALREADY functionally hollowed out; there is no live capture to flag because the reading asserts the capture already failed constitutionally, it just hasn't been formally noticed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_four_self_executing_ambiguity,
    'Is 14th Amendment Section 4 self-executing against a conflicting appropriations-vs-ceiling statute, such that Treasury could lawfully ignore the ceiling, or is it merely an interpretive canon with no independent justiciable remedy absent an Act of Congress or judicial enforcement mechanism?',
    'A live constitutional test case would resolve this — e.g., Treasury invoking Section 4 during an actual ceiling-appropriations conflict and a resulting suit reaching a merits ruling from a court with standing to hear it. To date every crisis (2011, 2013, 2023) has been resolved by legislative action before the confrontation was forced, leaving the doctrine untested.',
    'If Section 4 is confirmed self-executing, the nullity reading is vindicated and the ceiling''s legal force collapses entirely for future crises. If courts hold it requires further legislative or judicial implementation, the nullity reading fails and the coordination_scaffold_reading or extraction_snare_reading becomes the operative account of the same textual kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(section_four_self_executing_ambiguity, conceptual, 'Whether Section 4 is self-executing or merely interpretive, unresolved by any binding precedent.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the debt ceiling kernel supports three structurally distinct and empirically incompatible readings (nullity, scaffold, snare), which reading best describes the ceiling''s ACTUAL current operation, as opposed to its theoretically available legal status?',
    'Track which reading historical actors (Presidents, Treasury Secretaries, Congressional leadership) actually act under during a live crisis — do they treat the ceiling as void (never yet observed), as a genuine coordination step (routine raises without brinkmanship), or as extraction leverage (2011, 2013, 2023 patterns)? Empirical behavior across crises is the resolving signal, distinct from the legal question in the first omega.',
    'If actors never in practice invoke the nullity reading even when facing genuine default, that behavioral fact is strong evidence the extraction_snare_reading or coordination_scaffold_reading better describes the constraint''s operative reality regardless of the nullity reading''s legal soundness — a doctrine that is legally available but never exercised functions differently from one that is exercised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Which of the three sibling readings best matches observed institutional behavior across debt-ceiling crises.').

omega_variable(
    fsm_beneficiary_ambiguity,
    'Does declaring executive_branch_treasury and bondholders as ''beneficiaries'' of a Mountain-claimed constraint indicate this is actually a false summit — i.e., is the ''constitutional nullity'' framing itself partly a constructed legal argument advanced because it benefits the executive branch''s flexibility and bondholder stability, rather than a pure discovery of pre-existing constitutional fact?',
    'Compare the historical timeline of when the Section 4 nullity argument gained prominence (largely post-2011) against the interests of the parties advancing it; assess whether the argument''s academic pedigree predates or postdates the political convenience of the reading for the executive branch.',
    'If the nullity argument''s rise closely tracks executive-branch political convenience rather than independent constitutional scholarship, this Mountain claim should be reclassified toward tangled_rope via the false_summit_mountain signature — a legal argument serving as cover for expanded executive fiscal discretion rather than a genuine natural-law-style constitutional limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_beneficiary_ambiguity, conceptual, 'Whether the nullity reading is genuine constitutional discovery or a constructed argument benefiting the executive branch and credit markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1917, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(stat_tr_t1979, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1979, 0.35).
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.72).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2013, 0.78).
narrative_ontology:measurement(stat_tr_t2023, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2023, 0.8).
narrative_ontology:measurement(stat_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.82).

% Extraction over time
narrative_ontology:measurement(stat_be_t1917, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1917, 0.02).
narrative_ontology:measurement(stat_be_t1979, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1979, 0.03).
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.03).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.04).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2013, 0.04).
narrative_ontology:measurement(stat_be_t2023, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2023, 0.04).
narrative_ontology:measurement(stat_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.03).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the statutory_debt_ceiling kernel, decomposed per the ε-invariance principle because the natural-language concept 'the debt ceiling' conflates structurally incompatible legal and empirical claims. constitutional_nullity_reading (this story) claims near-zero extraction because the constraint is legally void when triggered; coordination_scaffold_reading claims low extraction with a genuine, functioning coordination purpose; extraction_snare_reading claims substantial extraction via weaponized default-threat leverage. All three cannot share one ε because they make incompatible claims about the same textual kernel's actual legal force and operative function. Each is linked to the other two via affects_constraints; whichever reading gains institutional traction (e.g., a future Section 4 test case) would structurally influence the persistence and credibility of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
