% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Coordinate Construction: Dispersed Interpretive Authority Across Branches
 *   domain: constitutional_law_political_theory
 *
 * SUMMARY:
 *   This story instantiates the coordinate construction reading of the
 *   constitutional interpretive authority kernel: no branch of government
 *   holds final say over constitutional meaning, and the constitution's
 *   operative content is constructed through an ongoing process of
 *   interbranch dialogue, appointment politics, budgetary leverage, and
 *   occasional direct confrontation (court-packing threats,
 *   jurisdiction-stripping bills, non-enforcement of rulings). This is
 *   distinct from the judicial_supremacy_reading (courts hold final say via
 *   judicial review) and the parliamentary_supremacy_reading (the elected
 *   legislature holds final say and courts cannot void its acts) — those are
 *   separate constraint stories with their own ε values, victim sets, and
 *   classifications. Coordinate construction is neither of those: it is a
 *   claim about persistent, structural indeterminacy of final authority,
 *   resolved only through political contestation over time rather than by any
 *   single adjudicative act.
 *
 * KEY AGENTS:
 *   - elected_legislative_majorities: institutional beneficiary/agenda_setter — retains leverage to contest adverse rulings via statute, appropriations, and appointments
 *   - executive_branch_officials: institutional beneficiary/agenda_setter — operative interpretive power via enforcement discretion
 *   - durable_political_coalitions: organized beneficiary — patience and organization rewarded across cycles
 *   - constitutional_minorities: powerless payer — dependent on provisional judicial protection with no guarantee of durability
 *   - litigants_seeking_durable_rights_settlement: moderate payer — pay in diminished practical value of judicial victories
 *   - judicial_branch: institutional agenda_setter/beneficiary — one voice among several, not final
 *   - constitutional_scholars_and_historians: analytical observer — assesses the empirical pattern of dispersed authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.38).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.3).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Coordinate Construction: Dispersed Interpretive Authority Across Branches").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law_political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'de738100-1381-4d64-a578-70bfff260df4').
narrative_ontology:cs_kernel_codification('de738100-1381-4d64-a578-70bfff260df4', distributed).
narrative_ontology:cs_authority_grounding('de738100-1381-4d64-a578-70bfff260df4', distributed).
narrative_ontology:cs_reading_relation('de738100-1381-4d64-a578-70bfff260df4', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('de738100-1381-4d64-a578-70bfff260df4', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('de738100-1381-4d64-a578-70bfff260df4', foundational, no_branch_holds_dispositive_interpretive_finality).
narrative_ontology:cs_axiom_status(no_branch_holds_dispositive_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('de738100-1381-4d64-a578-70bfff260df4', no_branch_holds_dispositive_interpretive_finality, conventional).
narrative_ontology:cs_axiom('de738100-1381-4d64-a578-70bfff260df4', foundational, constitutional_meaning_settles_through_sustained_political_contestation_not_singular_adjudication).
narrative_ontology:cs_axiom_status(constitutional_meaning_settles_through_sustained_political_contestation_not_singular_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('de738100-1381-4d64-a578-70bfff260df4', constitutional_meaning_settles_through_sustained_political_contestation_not_singular_adjudication, empirically_contingent).
narrative_ontology:cs_reference_frame('de738100-1381-4d64-a578-70bfff260df4', founding_era_unresolved_allocation_of_interpretive_finality).
narrative_ontology:cs_drift_state('de738100-1381-4d64-a578-70bfff260df4', contemporary_era_of_intensified_judicial_appointment_conflict, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('de738100-1381-4d64-a578-70bfff260df4', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch_officials).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, durable_political_coalitions).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, litigants_seeking_durable_rights_settlement).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, interbranch_losers_of_the_moment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can advance statutory readings of contested constitutional questions, control appropriations that constrain how other branches act on their own interpretations, and shape the judiciary's composition through appointment and confirmation. Under coordinate construction, a legislative majority's constitutional view is never simply overruled and set aside — it remains a live input that can outlast an adverse court ruling by amendment, jurisdiction-stripping, or attrition.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislative_majorities, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislative_majorities, agenda_setter).

% Issue executive interpretations of constitutional authority (war powers, enforcement discretion, signing statements) that function as operative law unless and until contested and reversed through political or judicial channels that themselves take time and require coalition strength to invoke.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch_officials, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch_officials, agenda_setter).

% Coalitions that can sustain political power across multiple election cycles are positioned to eventually reshape constitutional meaning through appointments, statutes, and constitutional amendment even after losing a specific judicial contest. Their patience and organizational capacity is rewarded by a system where no single ruling is final.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, durable_political_coalitions, beneficiary,
    organized, generational, mobile, national).

% Depend on courts to recognize and hold a rights claim against majoritarian institutions, but under coordinate construction any favorable ruling remains provisional — subject to erosion via legislative circumvention, executive non-enforcement, or a reconstituted judiciary. They lack the organizational patience or resources to fight the same battle across successive institutional venues indefinitely.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% Bring a case expecting that a favorable judicial resolution will settle the question, but discover that under coordinate construction the ruling is one move in an ongoing contest rather than a final word — the practical value of winning is diminished by the near-certainty of political counter-mobilization to reverse or hollow out the result.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, litigants_seeking_durable_rights_settlement, payer,
    moderate, biographical, constrained, national).

% Whichever branch or faction currently holds the losing constitutional position bears the practical costs of contested authority — delay, uncertainty, and the burden of remobilizing political resources to reopen a settled-seeming question, with no guaranteed venue that will render a final, binding answer.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, interbranch_losers_of_the_moment, payer,
    moderate, immediate, constrained, national).

% Issues rulings that carry substantial but not final weight; must anticipate legislative and executive responses, its own legitimacy resting partly on other branches' willingness to comply. Benefits from being one of several interpretive voices rather than bearing sole responsibility for constitutional meaning, but cannot guarantee its own rulings will stick.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, beneficiary).

% Study the pattern of interbranch contestation as evidence for or against the coordinate construction thesis, drawing on historical episodes (court-packing threats, jurisdiction-stripping proposals, non-enforcement of rulings) to assess whether authority is genuinely dispersed or whether one branch has de facto final say despite formal appearances.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars_and_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving genuine disagreement about constitutional meaning among coordinate branches without requiring any one branch to be permanently subordinated to another's judgment — legitimacy is built iteratively through repeated political engagement rather than assigned once and fixed.
% TRANSFER_FUNCTION: Moves the practical burden of achieving durable constitutional settlement from a single authoritative forum onto whichever party can sustain political mobilization longest; shifts risk and cost onto those without resources to fight across successive institutional venues (courts, then legislatures, then elections, then courts again).
% ABSENT_VOICES: Individuals and minority groups whose rights claims depend on a single, final, insulated-from-majoritarian-politics ruling are structurally disadvantaged by a system that treats every ruling as provisional; they are present as litigants but their preference for finality has no institutional channel under this reading.
% DISAPPEARANCE_RATIONALE: If coordinate construction were abandoned overnight in favor of a single final interpretive authority, the entire pattern of legislative override attempts, court-packing threats, jurisdiction-stripping proposals, and non-enforcement postures would lose their rationale — political actors would redirect all resources toward capturing whichever single institution now held final say, and constitutional politics would reorganize around that one battleground.
% FOUNDING_PROBLEM: Early constitutional designers faced the problem of allocating interpretive authority among branches without creating either an unaccountable judicial oligarchy or an unconstrained legislative/executive majority able to define the limits of its own power.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside any single branch's interest attest that founding-era debates left interpretive finality genuinely unresolved and that subsequent practice (not settled doctrine) determined how much deference courts receive in any given period; political scientists studying interbranch conflict corroborate that the dispersed-authority pattern persists empirically even where judicial supremacy is asserted rhetorically.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).
:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the coordinate construction arrangement genuinely solves a real coordination problem (avoiding capture of constitutional meaning by any single branch) while also systematically transferring the cost of interpretive instability onto those least able to sustain multi-venue political struggle — chiefly constitutional minorities and one-shot litigants. Suppression is comparatively low (0.30) because no single mechanism forcibly forecloses alternative readings; the constraint persists through distributed political friction rather than concentrated coercion. Theater ratio is modest but rising (0.18 to 0.28) reflecting a documented pattern: as interbranch conflict escalates, some interpretive contestation (symbolic non-enforcement statements, performative confirmation battles) increasingly serves signaling functions rather than substantive resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a durable, well-resourced political coalition, coordinate construction looks like a rope: a fair, iterative process that legitimately allows contestation of judicial overreach. From the seat of a constitutional minority or a single rights claimant, the same structure looks closer to a snare: a promise of legal protection that dissolves under sustained political pressure with no final venue of appeal. The engine computes these divergent seat-level classifications from the same structural data — the coordinate construction reading does not average or resolve this gap, it is the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majorities, executive officials, and durable coalitions are beneficiaries because coordinate construction preserves their capacity to contest and eventually reverse unfavorable rulings — their structural position gives them low d (near-beneficiary). Constitutional minorities and rights-seeking litigants are targets: they depend on judicial finality that the arrangement structurally denies them, giving them high d (near-target). The judicial branch occupies an intermediate position — it sets agenda through rulings but cannot guarantee compliance, so its directionality is closer to symmetric than either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to allocate interpretive authority without creating either judicial oligarchy or unconstrained majoritarianism — remains genuinely live rather than resolved and abandoned; this is why founding_problem_status is 'contested' rather than 'dead.' Classifying this as tangled_rope rather than snare prevents mislabeling a structure that does perform real coordination work (preventing any single branch from permanently capturing constitutional meaning) as pure extraction; it also prevents mislabeling it as a pure rope by acknowledging the asymmetric cost borne by minorities and one-shot litigants who cannot outlast the contestation cycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_dispersal_vs_de_facto_hierarchy,
    'Is interpretive authority genuinely dispersed across branches over the long run, or does one branch (typically the judiciary, sometimes the executive in crisis periods) hold de facto final say most of the time, with coordinate construction describing only the rare contested episodes?',
    'Longitudinal empirical study of how often judicial rulings on constitutional questions are effectively overridden, ignored, or hollowed out by the other branches versus how often they stand as practically final for the relevant political generation.',
    'If de facto hierarchy dominates, this reading is closer to a disguised version of judicial_supremacy_reading and coordinate construction functions mainly as legitimating rhetoric for episodes when the dominant branch is challenged; if genuine dispersal dominates, the reading is descriptively accurate and its extraction profile (falling on minorities and one-shot litigants) is the real structural cost of avoiding permanent capture by any branch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_dispersal_vs_de_facto_hierarchy, empirical, 'Whether coordinate construction describes the norm or only exceptional contested episodes.').

omega_variable(
    committer_structure_kernel_disagreement_location,
    'Where exactly does this reading''s disagreement with judicial_supremacy_reading and parliamentary_supremacy_reading locate structurally — is it a disagreement about WHO should hold final authority, or a denial that ''final authority'' is a coherent or achievable state at all?',
    'Careful reconstruction of founding-era and contemporary theoretical arguments: does the coordinate construction position argue for a specific alternative allocation of finality, or does it deny finality is achievable in principle given the structural incentives of a multi-branch system?',
    'If the disagreement is about who should hold finality, coordinate_construction_reading could in principle converge with either sibling reading under different institutional configurations (influences relation is apt). If the disagreement is a denial that finality is achievable at all, the reading structurally forecloses both siblings'' core premise that some single locus of authority exists or should exist — this bears directly on whether the reading_relations below should be forecloses rather than coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_disagreement_location, conceptual, 'Whether coordinate construction is a rival allocation claim or a denial that any single-locus claim can be true.').

omega_variable(
    instability_tolerance_as_extraction_mechanism,
    'Is the higher tolerance for interpretive instability under this reading itself an extraction mechanism (systematically favoring resourced, patient actors who can outlast contestation cycles) or a neutral structural feature that happens to have this distributive effect as a side consequence?',
    'Comparative analysis against jurisdictions or historical periods with more settled finality doctrines: do minority rights claims fare measurably worse under coordinate-construction-type systems controlling for other factors?',
    'If instability tolerance functions as an extraction mechanism, the tangled_rope classification is well-grounded and the victim declarations here are structurally central rather than incidental; if it is a side effect, the extraction score may be overstated relative to the genuine coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instability_tolerance_as_extraction_mechanism, conceptual, 'Whether interpretive instability is a designed-in cost-shifting mechanism or an incidental byproduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 50, 0.37).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 60, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'final constitutional interpretive authority' per the ε-invariance principle. judicial_supremacy_reading and parliamentary_supremacy_reading assign finality to a single branch and would show different beneficiary/victim structures and likely different ε (judicial_supremacy_reading plausibly lower ε for rights-claimants, higher for legislative majorities; parliamentary_supremacy_reading the inverse). All three are linked here; each carries its own ε and classification and should not be averaged or reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
