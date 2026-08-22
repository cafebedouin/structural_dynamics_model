% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the national-primacy reading of Article 17
 *   complementarity: national courts are presumptively adequate, the ICC
 *   bears the burden to prove inadmissibility (a sham proceeding or genuine
 *   collapse), and this presumption favors state sovereignty over
 *   international oversight. Under this reading, proceedings that are genuine
 *   in form but weak or captured in substance still satisfy Article 17, which
 *   coordinates real value (avoiding permanent international override of
 *   functioning domestic courts) while also creating asymmetric extraction —
 *   victims in states with hollow-but-formal proceedings are placed outside
 *   ICC reach, and the cost of that placement falls disproportionately on the
 *   powerless. The sibling reading (international_oversight_reading) treats
 *   the same text as an accountability trigger with a broadly-construed
 *   'unwilling or unable' standard; that is a different constraint with a
 *   different ε, authored separately and linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.58).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.51).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'c8c9586d-3f5c-407a-9bf6-9b1c731ed793').
narrative_ontology:cs_kernel_codification('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', formalized).
narrative_ontology:cs_authority_grounding('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', distributed).
narrative_ontology:cs_reading_relation('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', foundational, domestic_proceeding_presumptively_adequate).
narrative_ontology:cs_axiom_status(domestic_proceeding_presumptively_adequate, holdable).
narrative_ontology:cs_axiom_grounding('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', domestic_proceeding_presumptively_adequate, conventional).
narrative_ontology:cs_axiom('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', foundational, burden_of_proof_rests_on_icc).
narrative_ontology:cs_axiom_status(burden_of_proof_rests_on_icc, holdable).
narrative_ontology:cs_axiom_grounding('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', burden_of_proof_rests_on_icc, conventional).
narrative_ontology:cs_reference_frame('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', rome_statute_sovereignty_bargain).
narrative_ontology:cs_drift_state('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', post_libya_kenya_admissibility_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8c9586d-3f5c-407a-9bf6-9b1c731ed793', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries_of_powerful_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_governments).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, domestic_military_and_political_elites).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, atrocity_victims_in_weak_but_nominally_functioning_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, witnesses_facing_domestic_intimidation).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, civil_society_actors_pursuing_international_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, office_of_the_prosecutor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct domestic investigations and prosecutions, however limited, that satisfy Article 17's presumptive-adequacy standard and thereby foreclose ICC jurisdiction over their nationals. Control the pace, scope, and outcome of proceedings while the presumption of adequacy shifts the burden onto the ICC to prove a sham.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries_of_powerful_states, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries_of_powerful_states, agenda_setter).

% Cite complementarity as validation that international courts cannot override domestic legal processes absent proof of total collapse. Use even minimal or slow-moving domestic proceedings as a jurisdictional shield, and shape diplomatic pressure on the ICC's Office of the Prosecutor to defer.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_governments, beneficiary,
    institutional, generational, arbitrage, global).

% Face domestic proceedings they can influence, delay, or direct toward acquittal or minor charges, all of which count as genuine activity under the national-primacy standard and remove them from ICC exposure regardless of proceeding quality.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, domestic_military_and_political_elites, beneficiary,
    powerful, biographical, mobile, national).

% Suffer harms investigated, if at all, by domestic systems captured or intimidated by the same power structures implicated in the harm. Because proceedings exist on paper and are not a total sham, the high inadmissibility threshold keeps their cases outside ICC reach even when domestic justice is functionally hollow.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, atrocity_victims_in_weak_but_nominally_functioning_states, payer,
    powerless, biographical, trapped, local).

% Must testify, if at all, within the same domestic system controlled by those they would testify against. The national-primacy reading treats the existence of a domestic forum as adequacy regardless of whether witnesses can safely participate in it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, witnesses_facing_domestic_intimidation, payer,
    powerless, immediate, trapped, local).

% Document abuses and petition the ICC to find domestic proceedings inadequate, but under this reading their evidence must overcome a strong presumption of adequacy; they are not the ones who set the inadmissibility standard and their findings rarely meet the sham-proof burden.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, civil_society_actors_pursuing_international_accountability, excluded,
    moderate, generational, constrained, global).

% Bears the burden under this reading to affirmatively demonstrate that national proceedings are unwilling or unable in a manner amounting to sham, a high evidentiary bar that consumes prosecutorial resources and depends on state cooperation the same states can withhold.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, office_of_the_prosecutor, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, office_of_the_prosecutor, payer).

% Adjudicate admissibility challenges applying the presumptive-adequacy standard; their rulings are shaped by, and in turn reinforce, the national-primacy reading's high threshold for finding proceedings genuinely inadequate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_judges, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the ICC from displacing functioning domestic legal systems, preserving state sovereignty over criminal jurisdiction and avoiding a permanent international court that overrides national courts whenever it disagrees with their outcomes or pace.
% TRANSFER_FUNCTION: Moves accountability leverage from victims and international oversight bodies to national governments and their judiciaries; the cost of unresolved impunity is transferred to victims and witnesses in states whose domestic proceedings are genuine-in-form but weak-in-substance.
% ABSENT_VOICES: Victims and witnesses inside the affected states are structurally distant from admissibility litigation, which occurs between the Office of the Prosecutor and state counsel; civil society accountability groups can submit information but do not control the sham-proof burden they must help meet.
% DISAPPEARANCE_RATIONALE: If the national-primacy reading's high inadmissibility threshold disappeared, the ICC's Office of the Prosecutor could assert jurisdiction on a lower showing of domestic inadequacy, materially expanding the pool of admissible cases and reallocating leverage from sovereignty-maximizing states toward international oversight bodies; states currently shielded by minimal domestic proceedings would lose that shield.
% FOUNDING_PROBLEM: The Rome Statute's drafters needed to secure broad state ratification for a permanent international criminal court; without a strong guarantee that functioning domestic systems would be respected, sovereignty-sensitive states would not have joined, and complementarity was built to make ICC jurisdiction the exception, not the rule.
% FOUNDING_PROBLEM_CORROBORATION: States and their delegations at Rome, and subsequent state parties invoking Article 17 in admissibility challenges, attest the sovereignty-protection function remains live and necessary. Independent monitoring bodies, victims' counsel in ICC proceedings, and scholars studying admissibility rulings (e.g. in the Libya and Kenya situations) attest from outside the state-party beneficiary set that the presumptive-adequacy standard has been used to shield proceedings that are genuine in form but produce no meaningful accountability, suggesting the founding problem of respecting genuine domestic justice has been extended to cover its opposite.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but moderate transfer: the presumption of adequacy does genuine coordination work (respecting sovereignty, incentivizing ratification and domestic capacity-building) while also shielding elites whose proceedings are performative. Suppression (0.51) is lower than a pure snare because the mechanism does not forcibly prevent victims from pursuing domestic remedies — it simply raises the evidentiary bar for ICC intervention, which functions as passive exclusion rather than active coercion. Theater ratio (0.42) captures that some domestic proceedings shielded under this reading are substantially performative — charges filed, minimal investigation, no real prospect of conviction — while others are genuinely under-resourced but sincere. All three temporal series share the 0-5-10-15-20-25 grid; extraction and theater both drift upward as the jurisprudence of successive admissibility rulings (Libya, Kenya, Darfur-adjacent situations) progressively raises the practical bar for finding a sham, hardening the presumption into something closer to an evidentiary wall.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a sovereignty-maximizing state or a domestic judiciary, this reading looks like principled deference to functioning institutions — a rope preventing an unaccountable international body from second-guessing legitimate domestic processes. From the seat of a trapped victim or intimidated witness in a state whose proceedings are formally extant but substantively captured, the same structural arrangement operates as an extraction mechanism: the presumption of adequacy is the wall that keeps their case out of any forum capable of delivering accountability. The engine computes these as different per-seat classifications from the same structural data; this divergence is exactly what the tangled_rope type is meant to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries of powerful states and sovereignty-maximizing governments sit near the full-beneficiary end: they set or heavily influence the inadmissibility standard's application and collect the benefit of retained jurisdiction and reduced international scrutiny. Domestic elites facing proceedings they can steer benefit similarly, with mobile exit options if proceedings turn genuinely adverse. Atrocity victims and intimidated witnesses sit near the full-target end: trapped exit options, no capacity to independently trigger ICC jurisdiction, and structurally dependent on the same domestic system implicated in their harm. Civil society accountability actors are excluded rather than positioned as targets or beneficiaries — they attempt to supply the evidence needed to rebut the presumption but do not control the admissibility standard itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state sovereignty over criminal jurisdiction, without which Rome Statute ratification would have failed) remains partly live — many states genuinely value non-interference in functioning domestic systems. But the founding-problem status is authored as contested because the presumptive-adequacy standard, applied to proceedings that are genuine-in-form but hollow-in-substance, extends the sovereignty-protection function well past cases where domestic justice actually functions. The tangled_rope classification (rather than pure snare) reflects that genuine coordination value persists for the states and cases where domestic proceedings really are adequate — the extraction is asymmetric and layered onto real coordination, not free-floating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sham_threshold_indeterminacy,
    'Where does ''genuine but weak'' domestic proceeding shade into ''sham'' under the national-primacy reading''s own standard, and who gets to draw that line in contested cases?',
    'Comparative analysis of ICC Pre-Trial Chamber admissibility rulings (Libya, Kenya, Darfur situations) to identify the operative evidentiary threshold actually applied versus the threshold as stated in Article 17 jurisprudence.',
    'If the applied threshold is functionally near-total-collapse, the national-primacy reading is close to structurally foreclosing ICC jurisdiction outside failed-state scenarios, sharpening the tangled_rope''s extractive component; if the threshold is genuinely responsive to proceeding quality, the coordination function is stronger than the extractive one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_threshold_indeterminacy, empirical, 'Where the sham/genuine line actually falls in practice under this reading.').

omega_variable(
    reading_choice_as_framing_artifact,
    'Is the national-primacy reading and its sibling (international_oversight_reading) genuinely two different legal interpretations of Article 17, or is the apparent divergence produced by which admissibility cases each reading''s proponents foreground (near-total-collapse cases vs. hollow-but-formal cases)?',
    'Systematic review of state-party submissions and OTP admissibility briefs across all contested admissibility challenges to date, coded for which threshold language each brief actually argues for, independent of outcome.',
    'If the divergence is primarily rhetorical rather than doctrinal, the two-story decomposition should be revisited toward a single constraint with contested threshold as an omega rather than two constraints; if the divergence tracks genuinely different legal argument structures (burden allocation, standard of proof), the two-constraint decomposition is the correct model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_as_framing_artifact, conceptual, 'Whether the kernel''s two readings reflect genuine doctrinal divergence or selective case emphasis.').

omega_variable(
    state_cooperation_leverage_ambiguity,
    'Does prioritizing state cooperation under this reading produce net accountability gains over time (states build genuine domestic capacity to avoid ICC referral) or net accountability losses (states learn the minimum performative threshold and calibrate proceedings to just clear it)?',
    'Longitudinal tracking of domestic proceeding quality (conviction rates, sentence severity, judicial independence indicators) in states that have successfully asserted Article 17 inadmissibility, compared to matched states without such challenges.',
    'Capacity-building trajectory would support the rope-leaning coordination reading; calibration-to-minimum trajectory would support the extraction-heavy reading and predict rising theater_ratio, consistent with the authored upward drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_cooperation_leverage_ambiguity, empirical, 'Whether state cooperation incentives produce genuine capacity or performative minimums.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__national_primacy_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__national_primacy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__national_primacy_reading, suppression_requirement, 25, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This constraint and article_17_complementarity__international_oversight_reading decompose the natural-language concept 'Article 17 complementarity' into two structurally distinct readings of the same kernel, per the ε-invariance principle. This story (national_primacy_reading) authors ε=0.58 with a restricted victim set (complete judicial collapse cases) and beneficiaries centered on sovereignty-maximizing states and national judiciaries. The sibling authors a different ε reflecting a broader 'unwilling or unable' construction and an expanded victim set reaching victor's-justice and elite-shielding scenarios. Neither story averages over the other; each is a complete, independently classifiable constraint linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
