% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity as International Oversight Trigger
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the international_oversight_reading of
 *   the Article 17 complementarity kernel. Under this reading,
 *   complementarity is not a sovereignty shield but an accountability
 *   trigger: the ICC acts as a guardian against impunity when states fail to
 *   prosecute genuinely, interpreting 'unwilling or unable' broadly to
 *   capture victor's justice, sham proceedings, and elite immunity. The
 *   structural delta from the national_primacy_reading is a low admissibility
 *   threshold, intensified state cooperation demands, and an expanded victim
 *   set that includes symbolic prosecution scenarios. The constraint
 *   coordinates the international community's response to atrocities while
 *   asymmetrically extracting sovereignty from states under scrutiny.
 *
 * KEY AGENTS:
 *   - icc_prosecutor_and_chambers: Agenda-setter (institutional/global) â interprets admissibility and enforces the oversight function
 *   - atrocity_victims: Primary beneficiary (powerless/trapped) â receives accountability when domestic systems fail
 *   - state_sovereignty_claimants: Primary payer (institutional/constrained) â bears the sovereignty cost of external oversight
 *   - targeted_political_elites: Secondary payer (powerful/constrained) â loses impunity through broad admissibility interpretation
 *   - non_party_powerful_states: Excluded seat (institutional/arbitrage) â evades jurisdiction while shaping political constraints on the Court
 *   - human_rights_advocates: Analytical observer (organized/analytical) â monitors and petitions for intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.72).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.8).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity as International Oversight Trigger").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '2d22ae64-2b02-443d-94ee-d74f3bfbd4fe').
narrative_ontology:cs_kernel_codification('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', formalized).
narrative_ontology:cs_authority_grounding('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', lineage).
narrative_ontology:cs_interpretation_layer_present('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe').
narrative_ontology:cs_reading_relation('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', foundational, international_guardianship_over_sovereign_impunity).
narrative_ontology:cs_axiom_status(international_guardianship_over_sovereign_impunity, holdable).
narrative_ontology:cs_axiom_grounding('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', international_guardianship_over_sovereign_impunity, conventional).
narrative_ontology:cs_axiom('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', foundational, low_threshold_admissibility).
narrative_ontology:cs_axiom_status(low_threshold_admissibility, holdable).
narrative_ontology:cs_axiom_grounding('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', low_threshold_admissibility, conventional).
narrative_ontology:cs_reference_frame('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', rome_statute_sovereignty_balance).
narrative_ontology:cs_drift_state('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', post_kenya_afghanistan_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d22ae64-2b02-443d-94ee-d74f3bfbd4fe', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_victims).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, state_sovereignty_claimants).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, targeted_political_elites).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, international_oversight_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Evaluates whether national proceedings are genuine under Article 17; opens investigations when states fail to act; seeks arrest warrants and state cooperation; interprets 'unwilling or unable' through preliminary rulings and admissibility decisions.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutor_and_chambers, agenda_setter,
    institutional, generational, constrained, global).

% Survivors of mass atrocities in states where domestic courts are captured, non-existent, or complicit; they petition the ICC and rely on its intervention as their primary accountability forum.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_victims, beneficiary,
    powerless, biographical, trapped, local).

% States party to the Rome Statute that assert exclusive jurisdiction over crimes on their territory; they must defend the genuineness of their proceedings against ICC admissibility challenges and surrender suspects upon request.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, state_sovereignty_claimants, payer,
    institutional, generational, constrained, national).

% Senior state officials and military leaders who previously enjoyed de facto immunity from domestic prosecution; they now face ICC indictment if national proceedings are deemed shielding tactics.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, targeted_political_elites, payer,
    powerful, biographical, constrained, national).

% Major powers outside the Rome Statute framework who actively oppose ICC jurisdiction over their nationals and territories; they use political and economic leverage to shield allies and deter referrals while remaining outside the Court's reach.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, non_party_powerful_states, excluded,
    institutional, generational, arbitrage, global).

% International and local NGOs that monitor domestic trials, document obstruction, and lobby the Office of the Prosecutor to open preliminary examinations when national accountability fails.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures accountability for international crimes when national legal systems are unwilling or unable to prosecute genuinely, solving the collective-action problem of state impunity for powerful actors.
% TRANSFER_FUNCTION: Transfers prosecutorial authority and legitimacy from domestic jurisdictions to the ICC when domestic proceedings are deemed non-genuine; transfers impunity-risk from elites to international legal scrutiny.
% ABSENT_VOICES: Non-party powerful states and their allies are structurally excluded from the accountability framework they nevertheless politically influence. Domestic judicial systems in targeted states are heard only when defending their own genuineness, not when framing the admissibility standard itself.
% DISAPPEARANCE_RATIONALE: If the ICC oversight function vanished, states would resume full sovereign control over atrocity prosecutions; victims in failed or complicit states would lose their external accountability backstop; the Rome Statute architecture would collapse into pure state consent.
% FOUNDING_PROBLEM: Post-Cold War atrocities revealed that states routinely shield powerful perpetrators from domestic accountability, creating an accountability gap in international law.
% FOUNDING_PROBLEM_CORROBORATION: Victims' groups and human rights organizations from outside state power structures corroborate that impunity persists. Some former prosecutors and international law scholars attest the problem is partially solved but now complicated by selectivity and politicization. State governments benefiting from sovereignty retention argue the problem is manageable domestically.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the broad interpretation of 'unwilling or unable' systematically displaces domestic procedural autonomy, transferring prosecutorial authority to The Hague. Suppression (0.80) is high because the regime depends on active enforcement: arrest warrants, state cooperation demands, and non-compliance referrals to the ASP or Security Council. Theater ratio (0.45) reflects growing performative distance between the Court's universalist claims and its selective docket, where powerful non-parties remain untouched. Accessibility collapse (0.60) captures the overshadowing of hybrid and regional alternatives by the ICC's monopoly on legitimacy. Resistance (0.75) is high due to African state withdrawals, US sanctions threats, and Russian hostility. The claim is tangled_rope because a genuine coordination function (atrocity accountability) is inseparable from asymmetric sovereignty extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the victim seat in a failed state, the constraint is a lifeline against absolute impunity; from the seat of a state sovereignty claimant, it is an external usurpation of constitutional authority; from the ICC seat, it is the necessary price of a functioning international criminal regime. The engine computes these divergences from the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Atrocity victims are structural beneficiaries (low d): the constraint subsidizes their access to justice. State sovereignty claimants and targeted elites are structural targets (high d): the constraint extracts their jurisdictional autonomy and impunity. The ICC prosecutor sits near symmetric but slightly toward beneficiary: it gains institutional power from the broad reading, yet remains procedurally constrained by the Statute. Non-party powerful states have arbitrage-grade exit, pushing their effective d toward the beneficiary end despite their vocal opposition, because they can evade the constraint's reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâstate impunity for mass atrocitiesâremains live in many jurisdictions, preventing a pure snare classification. However, the broad interpretation has drifted from the original sovereignty-balance frame, and the selective enforcement pattern means the constraint's coordination function is unevenly distributed. Classifying it as tangled_rope rather than rope prevents masking the sovereignty extraction; classifying it as tangled_rope rather than snare prevents ignoring the real accountability coordination it provides to victims without domestic recourse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the international oversight reading foreclose the national primacy reading within a single interpretive framework, or can both coexist as live legal doctrines?',
    'Jurisprudential analysis of ICC Chambers decisions to identify whether any single judgment has held both a strong sovereignty presumption and a low admissibility threshold simultaneously.',
    'If foreclosed, the kernel is structurally bifurcated into two incompatible regimes; if coexistent, the constraint''s classification remains reading-dependent and seat-divergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sibling reading logical relationship').

omega_variable(
    selective_enforcement_power_asymmetry,
    'To what extent does the ICC''s admissibility practice correlate with the geopolitical power of the target state rather than the gravity of the alleged crimes?',
    'Regression analysis of OTP situation selection and Pre-Trial Chamber admissibility rulings against state power indices and alliance structures.',
    'If strong correlation exists, the constraint''s victim set is power-asymmetric, amplifying effective extraction against weaker states and subsidizing powerful non-parties, which would push the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_power_asymmetry, empirical, 'Power bias in admissibility practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_17_c_tr_t0, article_17_complementarity__international_oversight_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(article_17_c_tr_t5, article_17_complementarity__international_oversight_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(article_17_c_tr_t10, article_17_complementarity__international_oversight_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(article_17_c_tr_t15, article_17_complementarity__international_oversight_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(article_17_c_tr_t22, article_17_complementarity__international_oversight_reading, theater_ratio, 22, 0.45).

% Extraction over time
narrative_ontology:measurement(article_17_c_be_t0, article_17_complementarity__international_oversight_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(article_17_c_be_t5, article_17_complementarity__international_oversight_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(article_17_c_be_t10, article_17_complementarity__international_oversight_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(article_17_c_be_t15, article_17_complementarity__international_oversight_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(article_17_c_be_t22, article_17_complementarity__international_oversight_reading, base_extractiveness, 22, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(article_17_c_su_t0, article_17_complementarity__international_oversight_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(article_17_c_su_t5, article_17_complementarity__international_oversight_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(article_17_c_su_t10, article_17_complementarity__international_oversight_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(article_17_c_su_t15, article_17_complementarity__international_oversight_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(article_17_c_su_t22, article_17_complementarity__international_oversight_reading, suppression_requirement, 22, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling reading instantiate two structurally distinct interpretations of the Article 17 complementarity kernel: international oversight (broad admissibility, low threshold) versus national primacy (sovereignty presumption, high threshold). They share the same referent (Article 17) but have different epsilon values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
