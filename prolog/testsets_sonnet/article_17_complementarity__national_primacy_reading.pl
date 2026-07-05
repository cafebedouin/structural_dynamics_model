% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the national-primacy reading of Article 17
 *   complementarity: national courts are presumptively adequate, and the ICC
 *   bears the burden of proving inadmissibility (state unwillingness or
 *   inability). This is deliberately ONE of two structurally distinct
 *   constraints emerging from the same textual kernel — the sibling story
 *   (international_oversight_reading) treats complementarity as an
 *   accountability-trigger with a low threshold favoring ICC intervention
 *   against sham proceedings. The two readings produce different beneficiary
 *   sets, different victim sets, and different effective extraction levels
 *   from the identical text, which is why they are authored as separate
 *   constraint stories rather than one story with a measurement parameter.
 *
 * KEY AGENTS:
 *   - national_judiciaries: primary beneficiary/agenda_setter (institutional/arbitrage) — controls whether cases ever reach the ICC
 *   - sovereignty_maximizing_states: beneficiary (powerful/arbitrage) — shielded from ICC intervention by nominal domestic process
 *   - victims_of_incomplete_national_proceedings: primary target (powerless/trapped) — bear the cost of the high inadmissibility threshold
 *   - icc_office_of_the_prosecutor: payer/agenda_setter (institutional/constrained) — bears the evidentiary burden the reading assigns
 *   - civil_society_accountability_advocates: excluded (organized/constrained) — documents deficiencies without standing to compel review
 *   - international_legal_scholarship: analytical observer — tracks doctrinal drift across admissibility jurisprudence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.42).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.38).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '7789c96e-4d07-4f85-a277-99e1d1edc5af').
narrative_ontology:cs_kernel_codification('7789c96e-4d07-4f85-a277-99e1d1edc5af', fixed_text).
narrative_ontology:cs_authority_grounding('7789c96e-4d07-4f85-a277-99e1d1edc5af', practice).
narrative_ontology:cs_interpretation_layer_present('7789c96e-4d07-4f85-a277-99e1d1edc5af').
narrative_ontology:cs_reading_relation('7789c96e-4d07-4f85-a277-99e1d1edc5af', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('7789c96e-4d07-4f85-a277-99e1d1edc5af', foundational, sovereignty_presumption_of_adequacy).
narrative_ontology:cs_axiom_status(sovereignty_presumption_of_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('7789c96e-4d07-4f85-a277-99e1d1edc5af', sovereignty_presumption_of_adequacy, conventional).
narrative_ontology:cs_axiom('7789c96e-4d07-4f85-a277-99e1d1edc5af', foundational, icc_bears_inadmissibility_burden).
narrative_ontology:cs_axiom_status(icc_bears_inadmissibility_burden, holdable).
narrative_ontology:cs_axiom_grounding('7789c96e-4d07-4f85-a277-99e1d1edc5af', icc_bears_inadmissibility_burden, conventional).
narrative_ontology:cs_reference_frame('7789c96e-4d07-4f85-a277-99e1d1edc5af', rome_statute_ratification_compromise).
narrative_ontology:cs_drift_state('7789c96e-4d07-4f85-a277-99e1d1edc5af', post_kenya_libya_admissibility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7789c96e-4d07-4f85-a277-99e1d1edc5af', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, state_security_apparatuses).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_incomplete_national_proceedings).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, civil_society_accountability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct domestic investigations and prosecutions that, once opened, presumptively satisfy Article 17 and block ICC admissibility regardless of proceeding quality, so long as it is not demonstrably a sham. Controls the pace, scope, and charging decisions of the proceeding, which determines whether the case ever reaches the ICC. Retains primary jurisdiction as the default state of affairs.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary).

% Invoke domestic proceedings, however limited, to preclude ICC jurisdiction over their nationals or territory. Bear no cost from the reading beyond occasionally having to open a token investigation. Benefit from a high inadmissibility threshold that makes ICC intervention rare and burdensome for the Court to establish.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    powerful, generational, arbitrage, national).

% Live in states where investigations exist on paper, or proceed slowly, or produce lenient outcomes, but do not rise to the demonstrable-sham threshold the reading requires for ICC admissibility. Have no forum beyond the domestic system that produced the outcome they are contesting. Cannot compel ICC review because the burden sits with the Court to prove inadequacy, not with them to prove adequacy failed.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_incomplete_national_proceedings, payer,
    powerless, biographical, trapped, local).

% Document proceeding deficiencies — charge sub-selection, evidentiary gaps, prosecutorial reluctance — and petition for ICC intervention, but the presumption of adequacy means their evidence must clear a high bar to shift the burden onto the state. Frequently operate under domestic legal or physical risk for the documentation itself.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, civil_society_accountability_advocates, excluded,
    organized, biographical, constrained, national).

% Must affirmatively demonstrate that national proceedings are unwilling or unable genuinely to prosecute before admissibility is granted — the reverse of a presumption against sovereignty. Expends significant investigative and diplomatic capital establishing sham status, and risks state non-cooperation retaliation for pursuing admissibility challenges that states experience as sovereignty violations.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, agenda_setter).

% Analyzes admissibility jurisprudence across cases to assess whether the national-primacy reading produces consistent deference to weak-but-genuine proceedings or is being used to shield elite impunity. Has no enforcement role but shapes doctrinal interpretation over time.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the ICC from becoming a court of first instance that displaces functioning domestic legal systems, preserving state investment in national judicial capacity and avoiding a permanent standing threat that could be wielded to override domestic due process for political ends.
% TRANSFER_FUNCTION: Moves the burden of proof from the state (to demonstrate its proceedings are adequate) to the ICC (to demonstrate they are a sham), which in practice moves accountability risk away from states with even nominal domestic proceedings and onto victims who must rely on those same proceedings for redress.
% ABSENT_VOICES: Victims and civil society groups who experience the domestic proceeding as inadequate have no direct standing to trigger admissibility review; their documentation must pass through the Office of the Prosecutor's own threshold-clearing analysis, and their objections are not dispositive even when well-founded.
% DISAPPEARANCE_RATIONALE: If the national-primacy reading were abandoned in favor of a lower admissibility threshold, states currently shielded by nominal proceedings would face materially higher ICC exposure, prosecutorial strategy toward domestic sham detection would change, and states would likely alter cooperation patterns with the Court — either engaging more genuinely with domestic prosecution or withdrawing cooperation entirely.
% FOUNDING_PROBLEM: The Rome Statute needed a jurisdictional principle that would secure broad state ratification by guaranteeing the ICC would not supplant national courts, since without complementarity many states would not have joined the Court at all.
% FOUNDING_PROBLEM_CORROBORATION: States and national judiciaries attest the founding problem — the risk of an overreaching international court eroding sovereign judicial authority — remains live and requires continued deference. Independent legal scholars and past ICC prosecutors, from outside the beneficiary set, attest that the founding problem of gaining state buy-in has been substantially achieved (near-universal ratification predates most contested admissibility rulings) and that the high threshold now functions primarily to shield weak proceedings rather than to protect functioning ones.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).
:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than severe: the reading has a genuine coordination function (preserving state buy-in to the Rome Statute regime, respecting functioning domestic systems) that is not pure cover — many national proceedings this reading protects are in fact adequate. But the reading also produces asymmetric extraction: victims in states with weak-but-genuine proceedings — investigations that exist, move slowly, and under-charge without being demonstrable shams — fall outside ICC reach entirely, and that population bears a real cost the beneficiaries do not. Suppression is lower than extraction (0.38) because the mechanism operates through evidentiary and procedural burden-shifting rather than direct coercion; the 'suppression' here is largely the structural difficulty of proving a negative (state unwillingness) from outside the state. Theater ratio is moderate (0.31) and rising, reflecting a documented pattern of states opening investigations specifically to trigger the Article 17 presumption without intending genuine prosecution — the theater is the investigation itself, staged for admissibility purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states sit near the full-beneficiary end: the presumption of adequacy is a structural subsidy that requires no effort beyond opening a file. Victims of incomplete proceedings sit near the full-target end: trapped in the jurisdiction whose proceeding failed them, with no standing to compel review and a burden-of-proof structure that works against them by design. The ICC Prosecutor's office is a payer in an unusual sense — an institutional actor whose institutional mission is undermined by the very burden structure it operates under, but which retains institutional exit options (declining to open a case, or choosing different admissibility strategies) that individual victims lack entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   Complementarity's founding problem (state buy-in for a fledgling international court) is largely solved — ratification is near-universal and has been for two decades. The national-primacy reading's continued high threshold is therefore best read as R5-contested: the arrangement persists past the point its founding justification cleanly applies, but it has NOT become pure zombie mandate, because the coordination function (respecting genuine domestic proceedings, avoiding an overreaching supranational court) remains partially live wherever domestic systems are actually functional. This is why the classification lands as tangled_rope rather than snare: the coordination story is not merely cover, but neither is it clean — victims of the gap between 'not a sham' and 'genuinely adequate' bear a real, structurally produced cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_sham_line,
    'Where does a domestic proceeding cross from ''weak but genuine'' (inadmissible under this reading) into ''demonstrable sham'' (admissible)? Is there a principled line, or is the distinction manipulable by states with sophisticated legal counsel?',
    'Comparative analysis of ICC Pre-Trial Chamber admissibility rulings across cases (Kenya, Libya, Colombia referrals) to identify whether the sham threshold has produced consistent, predictable outcomes or has been shaped ad hoc by state cooperation leverage.',
    'If the line is principled and consistently applied, the reading functions closer to genuine coordination with acceptable cost. If the line is manipulable, sophisticated states can perform minimal compliance theater indefinitely, converting the tangled_rope into something closer to a snare for the victim population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_sham_line, empirical, 'Whether the sham/genuine distinction is a stable doctrinal line or a manipulable threshold.').

omega_variable(
    kernel_reading_contest_location,
    'The Rome Statute text does not specify which party bears the burden of proof at admissibility, nor does it define ''genuinely'' with precision — this ambiguity is exactly where the national_primacy_reading and international_oversight_reading diverge. Is this textual gap better resolved by judicial interpretation over time, or does it represent an irreducible drafting compromise that permanently supports both readings as live options?',
    'Track whether ICC Appeals Chamber jurisprudence converges toward one burden-allocation standard over multiple admissibility cycles, or continues to produce chamber-dependent outcomes.',
    'Convergence toward one reading would effectively foreclose the other as a live doctrinal position, even though both remain textually defensible today. No convergence confirms this is a genuine, permanent kernel-level contest rather than a temporary interpretive gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether the national-primacy and international-oversight readings will converge or remain permanently coexisting doctrinal positions.').

omega_variable(
    investigation_theater_measurement,
    'Is the rising theater_ratio (investigations opened primarily to trigger the Article 17 presumption) a genuine trend or an artifact of increased scrutiny/documentation by civil society over the same period?',
    'Compare investigation-opening timing relative to ICC referral announcements across a larger case sample, controlling for documentation intensity by monitoring organizations.',
    'If theater is genuinely rising, the national-primacy reading''s coordination function is eroding faster than the metrics suggest. If it is a documentation artifact, the underlying rate may be stable and previously under-observed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(investigation_theater_measurement, empirical, 'Whether rising measured theater reflects a real trend or improved observation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(arti_tr_t4, article_17_complementarity__national_primacy_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(arti_tr_t8, article_17_complementarity__national_primacy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(arti_tr_t12, article_17_complementarity__national_primacy_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(arti_tr_t16, article_17_complementarity__national_primacy_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(arti_tr_t24, article_17_complementarity__national_primacy_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(arti_be_t4, article_17_complementarity__national_primacy_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(arti_be_t8, article_17_complementarity__national_primacy_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(arti_be_t12, article_17_complementarity__national_primacy_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(arti_be_t16, article_17_complementarity__national_primacy_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(arti_be_t24, article_17_complementarity__national_primacy_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(arti_su_t4, article_17_complementarity__national_primacy_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(arti_su_t8, article_17_complementarity__national_primacy_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(arti_su_t12, article_17_complementarity__national_primacy_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(arti_su_t16, article_17_complementarity__national_primacy_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(arti_su_t24, article_17_complementarity__national_primacy_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).

% DUAL FORMULATION NOTE:
% This story and article_17_complementarity__international_oversight_reading decompose a single textual kernel (Rome Statute Article 17) into two structurally distinct constraints per the ε-invariance principle. This reading (national_primacy) has lower ε (0.42, moderate) because its high admissibility threshold restricts the victim class to complete judicial collapse and preserves a genuine sovereignty-coordination function. The sibling reading (international_oversight) is expected to carry higher ε because a low admissibility threshold widens the victim class to any case of plausible elite protection while shrinking the sovereignty-shielding benefit. The two are not the same constraint viewed from different angles — they are different burden-of-proof doctrines that different ICC chambers, states, and advocacy communities have actually argued for as competing interpretations of the same clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
