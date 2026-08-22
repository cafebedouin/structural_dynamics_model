% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Fixation of Constitutional Meaning — Judicial Binding to Ratification-Era Public Understanding
 *   domain: legal/political/philosophical
 *
 * SUMMARY:
 *   In the American constitutional order, an operative discipline requires
 *   that each provision of the federal Constitution bear the meaning its
 *   ratification-generation public attached to it, and that judges deciding
 *   constitutional questions rest their holdings on historical evidence —
 *   founding-era dictionaries, corpora, drafting records, early public
 *   practice — rather than on contemporary moral conviction. Contemporary
 *   circumstances remain relevant to applying fixed meaning (a modern
 *   punishment is assessed under a fixed concept of cruelty using present
 *   facts) but not to discovering what the text means. The discipline is
 *   administered from the bench, screened for at confirmation, reproduced
 *   through a dedicated academic literature, and enforced through the
 *   legitimacy costs of departing from it. Claim and metrics are authored
 *   independently: the claimed type reflects the structure I believe true — a
 *   genuine coordination function joined to asymmetric, actively enforced
 *   extraction — while the metrics describe the arrangement's actual
 *   operation as the record shows it.
 *
 * KEY AGENTS:
 *   - - originalist_judges: Agenda-setting administrator (institutional/identity_locked) — applies and enforces the historical-evidence discipline from the bench
 *   - - counter_majoritarian_constraint_advocates: Primary beneficiary (organized/mobile) — collects legitimacy, appointments, and agenda control
 *   - - elected_branch_institutions: Secondary beneficiary (institutional/constrained) — recovers policy space courts decline to occupy
 *   - - rights_claimants_without_historical_support: Primary target (powerless/trapped) — bears the cost of claims closed for want of founding-era analogues
 *   - - contemporary_democratic_majorities: Diffuse target (organized/constrained) — bound to ratification-era settlements short of Article V
 *   - - dissenting_non_originalist_judges: Suppressed minority seat (powerful/identity_locked) — methodology ruled out of majority opinions
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the full structure from outside the enforcement loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.64).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.77).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Fixation of Constitutional Meaning — Judicial Binding to Ratification-Era Public Understanding").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "legal/political/philosophical").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'c95c5b08-1412-46f0-85a2-08da1e18e1c7').
narrative_ontology:cs_kernel_codification('c95c5b08-1412-46f0-85a2-08da1e18e1c7', fixed_text).
narrative_ontology:cs_authority_grounding('c95c5b08-1412-46f0-85a2-08da1e18e1c7', lineage).
narrative_ontology:cs_interpretation_layer_present('c95c5b08-1412-46f0-85a2-08da1e18e1c7').
narrative_ontology:cs_reading_relation('c95c5b08-1412-46f0-85a2-08da1e18e1c7', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c95c5b08-1412-46f0-85a2-08da1e18e1c7', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('c95c5b08-1412-46f0-85a2-08da1e18e1c7', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('c95c5b08-1412-46f0-85a2-08da1e18e1c7', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('c95c5b08-1412-46f0-85a2-08da1e18e1c7', secondary, constitutional_change_requires_article_v_amendment).
narrative_ontology:cs_axiom_status(constitutional_change_requires_article_v_amendment, holdable).
narrative_ontology:cs_axiom_grounding('c95c5b08-1412-46f0-85a2-08da1e18e1c7', constitutional_change_requires_article_v_amendment, conventional).
narrative_ontology:cs_reference_frame('c95c5b08-1412-46f0-85a2-08da1e18e1c7', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('c95c5b08-1412-46f0-85a2-08da1e18e1c7', contemporary_consolidation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c95c5b08-1412-46f0-85a2-08da1e18e1c7', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, elected_branch_institutions).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, contemporary_democratic_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, senate_confirmation_gatekeepers).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, written_constitution_supremacy).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, popular_sovereignty_ratification_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on federal courts and decide constitutional questions by researching what the ratified text's words meant to the public at enactment — founding-era dictionaries, corpora, drafting records, early practice. Their votes in close cases turn on the quality of historical evidence. Leaving the methodology mid-career would mean repudiating the opinions, scholarship, and professional alliances that constitute their judicial identity; movement within the same method is the realistic mobility.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, biographical, identity_locked, national).

% Scholars, advocacy networks, and litigators who argued for decades that unelected judges should apply the law's fixed content rather than their own moral views. The methodology's ascent delivered them clerkship pipelines, judicial seats, law-review influence, and agenda control over which cases get heard. Their commitment is chosen rather than imposed; they can and do redirect attention between causes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Congress, presidents, and state legislatures regain policy room wherever courts decline to locate unwritten limits in old text; they simultaneously lose the option of relying on courts to update entrenched rules. Their levers are ordinary legislation and judicial appointments, both shaped by the methodology's requirements.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, elected_branch_institutions, beneficiary,
    institutional, biographical, constrained, national).

% Litigants claiming protections — due-process liberties, equal treatment, extensions of enumerated rights — whose claims lack close founding-era analogues. They must prevail inside courts that discount contemporary evidence of hardship or consensus; losing closes the claim nationwide, and there is no other forum in which a constitutional right can be established.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support, payer,
    powerless, biographical, trapped, national).

% Present-day voting coalitions that want to revise entrenched settlements — campaign finance, firearms regulation, executive power — and find that judicial updating is off the table. Their sole formal route is the Article V supermajority gauntlet, which has succeeded twenty-seven times in roughly 235 years; everything short of that leaves ratification-era choices standing.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, contemporary_democratic_majorities, payer,
    organized, generational, constrained, national).

% Judges who read the clauses as enduring principles adapted to present circumstances. Under the governing methodology their approach is out of bounds in majority opinions; they write dissents, contest historical records they regard as incomplete, and cannot adopt the rival method without severing the intellectual identity that brought them to the bench.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, dissenting_non_originalist_judges, excluded,
    powerful, biographical, identity_locked, national).

% Senators who screen nominees for demonstrated commitment to the historical method; much of the arrangement's enforcement happens here, before any case arrives, through personnel selection. Their leverage depends on coalition maintenance and electoral timing, which caps how far they can push in any given cycle.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, senate_confirmation_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, senate_confirmation_gatekeepers, beneficiary).

% Academics comparing how other democracies fix or evolve constitutional meaning. They watch the American arrangement from outside its enforcement loop, publish assessments of its coherence and costs, and are read by no seat under any obligation to act.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective problem: how a durable written constitution can hold unelected judges to a standard citizens can know in advance. Fixation makes constitutional content publicly ascertainable across generations, protects reliance interests built on settled text, and routes formal change through a single supermajoritarian channel instead of case-by-case judicial revision.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges and present-day coalitions to the ratification-era electorate; moves wins in constitutional litigation from claimants without founding-era analogues to governments defending historical practice; and delivers legitimacy rents — appointments, clerkships, scholarly authority — to the advocates of judicial restraint.
% ABSENT_VOICES: Those with the most at stake were absent at the source: the ratification electorate excluded women, the enslaved, and the propertyless, so the public meaning that binds everyone was fixed by a narrow franchise, and their descendants enter the system only as litigants bound by evidence from an electorate that never counted them. Non-originalist judges hold seats but their method is ruled out of majority opinions. Both groups would object that consensus around fixation was manufactured by exclusion; this is commentary-grade, not correction-grade.
% DISAPPEARANCE_RATIONALE: If the fixation requirement vanished overnight, constitutional adjudication would immediately reorganize around rival methods; decades of precedent justified solely by historical evidence would lose their warrant structure; confirmation politics, law-school curricula, and litigation strategy would all shift within a few appointment cycles.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: by what right do unelected, life-tenured judges invalidate the acts of elected majorities? The arrangement was built to answer that judges enforce the fixed command of the sovereign people rather than their own values, converting judicial review from rule by judges into rule by law.
% FOUNDING_PROBLEM_CORROBORATION: The legitimacy problem itself is corroborated from outside the benefiting parties: the non-originalist academy (the Bickel lineage of scholarship) concedes the counter-majoritarian difficulty is real while disputing that fixation solves it, and political scientists document the arrangement's counter-majoritarian operations empirically. No source outside the originalist movement attests that fixation is the correct solution; the solution's adequacy is attested only by its beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.64 at interval end) because the win-bar for rights claimants is set by evidence availability rather than merit: a claim with no founding-era analogue fails regardless of its present force, and the history-only mode adopted in recent Second Amendment jurisprudence raised that bar measurably. Suppression is higher still (0.77) because persistence depends on actively disqualifying rival methods — confirmation screening, certiorari filtering, and legitimacy sanctions against non-historical reasoning — not on participant preference; suppression is authored as a raw structural property and is deliberately left unscaled, since only extractiveness is scaled by directionality and scope downstream. Theater is moderate (0.39) and rising: the historical enterprise involves real scholarly labor, but the share of activity that performs neutrality — methodological preambles, selective citation — grows as the stakes of each case grow. Accessibility collapse is low-moderate (0.42) because rival approaches remain openly practiced in academia, state courts, and dissents; alternatives are marginalized, not eliminated. Resistance (0.58) is correspondingly sustained and organized. The measurement series run on one shared time grid (points 0, 10, 20, 30, 40, 50 of a 1975–2025 interval) so every tracked metric is authored at every examined time point; the trajectories are monotonic consolidation rather than cyclical — the arrangement tightened as it moved from academic theory to governing doctrine, with no reconciliation phase to model.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently. From the trapped claimant's position the arrangement operates as near-total closure: one forum, one admissible evidence class, nationwide effect on loss. From the advocate's position it is genuine discipline: predictable law, judicial humility, protection of democratic choice. The administrator seat sits between — real methodological labor, plus a legitimacy subsidy from being able to say 'the law, not I, decides.' Coalition potential among the powerless victims is structurally weak: harms arrive case-by-case, each claimant's loss is particularized, and the class lacks a standing forum in which to aggregate. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: counter_majoritarian_constraint_advocates (mobile exit, organized power) derive near the full-beneficiary end; elected_branch_institutions derive mildly beneficiary-side, since their gain is incidental policy space rather than collected revenue. Victim declarations drive the opposite pole: rights_claimants_without_historical_support (trapped, powerless) derive nearest the full-target end, amplified by their inability to exit the forum; contemporary_democratic_majorities derive high-but-not-maximal, since Article V remains a costly but real exit. The administrator seats (originalist_judges, senate_confirmation_gatekeepers) derive near-symmetric with a slight beneficiary tilt — the arrangement subsidizes their legitimacy claim without paying them directly. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat the scenario contains, and the derivation chain produces the right relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the counter-majoritarian difficulty — is contested, not dead: the legitimacy anxiety originalism answered is still acknowledged across the non-originlist academy even by those who reject the fix. Because status is contested rather than dead, the mismatch consumer should not fire the zombie flag here; the arrangement's persistence tracks a live (if disputed) function. The tangled_rope claim is what prevents mislabeling in both directions: reading the arrangement as pure coordination (the movement's self-description) would erase the claimants who pay for its stability; reading it as pure extraction (its fiercest critics' description) would erase the real discipline it imposes on judges of every methodological allegiance, including its beneficiaries' opponents. The prohibitive fixing-cost assessment matters diagnostically: whoever could abandon the method (the current bench) bears maximal legitimacy cost from repudiating its own invested doctrine, so the arrangement is not transient neglect awaiting cheap repair — but the named receipt seat keeps it from the inertial, nobody-profits profile as well.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the us_constitution_meaning kernel (originalist_reading); what would change structurally if a sibling reading governed instead?',
    'Join on kernel_id across the compiled sibling stories: living_constitutionalist_reading shifts the victim set toward those harmed by frozen application and lowers suppression of evolving outcomes; positivist_reading relocates authority to enactment procedure and removes historical evidence as the binding input.',
    'ε, beneficiary structure, and per-seat classifications are reading-indexed; cross-reading comparison must join on kernel_id, never pool the stories as measurements of one thing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: this file is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    ratification_meaning_determinacy,
    'Is ratification-era public meaning determinate enough to bind judges, or does irreducible historiographical indeterminacy leave judges choosing among plausible histories in practice?',
    'Corpus-linguistic replication studies and blinded panels of professional historians scoring whether multiple equally plausible founding-era meanings exist for the clauses that generate recurring litigation.',
    'If indeterminacy is high, the measured suppression of non-historical outcomes is substantially theatrical — judges select among histories rather than obey one — and the arrangement drifts toward performative maintenance with rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratification_meaning_determinacy, empirical, 'Determinacy of the historical evidence base the discipline presupposes.').

omega_variable(
    dead_hand_authorization_status,
    'Does binding present generations to ratification-era meaning constitute valid popular-sovereign authorization or illegitimate dead-hand rule?',
    'Not resolvable by evidence alone; turns on commitments about intergenerational obligation and democratic consent. Tracked observationally by amendment-rate trends and shifts in public endorsement of judicial method.',
    'If dead-hand, the victim set expands to all contemporary majorities and effective extraction rises toward pure-extraction territory; if authorized, part of the measured burden is consented coordination cost rather than imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_authorization_status, preference, 'Normative status of intergenerational binding across a changed franchise.').

omega_variable(
    law_office_history_selectivity,
    'Do judges applying the historical method conduct genuine inquiry or motivated selection of congenial sources?',
    'Blind coding of majority-opinion historical analysis against party alignment of the outcome, benchmarked against professional historians'' assessments of the same records.',
    'High selectivity inflates theater_ratio above functional levels and supports reclassification pressure toward extraction-dominated operation; low selectivity strengthens the coordination framing and the claimed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_office_history_selectivity, empirical, 'Sincerity of the historical enterprise inside adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscm_orig_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(uscm_orig_tr_t0, observed).
narrative_ontology:measurement(uscm_orig_tr_t10, us_constitution_meaning__originalist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(uscm_orig_tr_t10, observed).
narrative_ontology:measurement(uscm_orig_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(uscm_orig_tr_t20, observed).
narrative_ontology:measurement(uscm_orig_tr_t30, us_constitution_meaning__originalist_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(uscm_orig_tr_t30, observed).
narrative_ontology:measurement(uscm_orig_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(uscm_orig_tr_t40, observed).
narrative_ontology:measurement(uscm_orig_tr_t50, us_constitution_meaning__originalist_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(uscm_orig_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(uscm_orig_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(uscm_orig_be_t0, observed).
narrative_ontology:measurement(uscm_orig_be_t10, us_constitution_meaning__originalist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(uscm_orig_be_t10, observed).
narrative_ontology:measurement(uscm_orig_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(uscm_orig_be_t20, observed).
narrative_ontology:measurement(uscm_orig_be_t30, us_constitution_meaning__originalist_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(uscm_orig_be_t30, observed).
narrative_ontology:measurement(uscm_orig_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(uscm_orig_be_t40, observed).
narrative_ontology:measurement(uscm_orig_be_t50, us_constitution_meaning__originalist_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(uscm_orig_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(uscm_orig_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(uscm_orig_su_t0, observed).
narrative_ontology:measurement(uscm_orig_su_t10, us_constitution_meaning__originalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(uscm_orig_su_t10, observed).
narrative_ontology:measurement(uscm_orig_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(uscm_orig_su_t20, observed).
narrative_ontology:measurement(uscm_orig_su_t30, us_constitution_meaning__originalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(uscm_orig_su_t30, observed).
narrative_ontology:measurement(uscm_orig_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(uscm_orig_su_t40, observed).
narrative_ontology:measurement(uscm_orig_su_t50, us_constitution_meaning__originalist_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(uscm_orig_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how the Constitution means' conflates three structurally distinct claims (per the ε-invariance principle): fixation of meaning at ratification (this story), evolutionary application of enduring principles (living_constitutionalist_reading), and procedural-source validity (positivist_reading). Each is a separate file with its own ε, beneficiaries, and victims. Edges run from this reading outward because fixation rhetoric supplies the legitimacy conditions under which the sibling readings compete: challenges to fixation attack the premise the other readings must argue against. Values differ by reading — this reading authors high suppression of non-historical outcomes and a victim set of historically unsupported claimants; the living reading would author lower suppression and a different victim set (those harmed by frozen application); the positivist reading relocates the contested good from meaning to validity altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
