% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statute as Incumbent Rent Extraction
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This story authors the rent-seeking-suppression reading of the
 *   licensing-statute kernel: statutory credential requirements are read,
 *   from this seat, as an incumbent-capture mechanism whose
 *   consumer-protection language has decoupled from its harm-prevention
 *   effect and now primarily restricts labor supply to sustain practitioner
 *   income and board/association institutional revenue. The reading's
 *   referent is the standing licensing arrangement AS THIS READING SEES IT —
 *   high extraction, high suppression, moderate-to-high theater — not the
 *   alternative arrangement (open entry with voluntary certification) this
 *   reading would prefer. Two sibling constraints exist for the same
 *   underlying statute: public_safety_coordination (reads the same statute as
 *   a genuine competence-verification rope) and graduated_access_filter
 *   (reads it as a class-sorting mechanism). Each is a separate story with
 *   its own stable epsilon; they are linked via network only, never merged
 *   into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.78).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statute as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '33f4a5a4-af95-479d-b6ea-e6fc1af24ace').
narrative_ontology:cs_kernel_codification('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', formalized).
narrative_ontology:cs_authority_grounding('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', extraction).
narrative_ontology:cs_interpretation_layer_present('33f4a5a4-af95-479d-b6ea-e6fc1af24ace').
narrative_ontology:cs_reading_relation('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', foundational, credentialing_function_has_decoupled_from_harm_evidence).
narrative_ontology:cs_axiom_status(credentialing_function_has_decoupled_from_harm_evidence, holdable).
narrative_ontology:cs_axiom_grounding('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', credentialing_function_has_decoupled_from_harm_evidence, empirically_contingent).
narrative_ontology:cs_axiom('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', secondary, incumbent_board_capture_is_the_operative_mechanism).
narrative_ontology:cs_axiom_status(incumbent_board_capture_is_the_operative_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', incumbent_board_capture_is_the_operative_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', consumer_protection_founding_charter).
narrative_ontology:cs_drift_state('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', contemporary_licensing_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33f4a5a4-af95-479d-b6ea-e6fc1af24ace', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbyists).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, low_income_career_changers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already hold the credential and sit on or fund the boards that write continuing-education and entry requirements. Every new barrier raised against entrants raises the market-clearing price for their own services while costing them nothing further, since they are grandfathered in. Face essentially no downside from tightening the statute.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners, beneficiary,
    organized, biographical, arbitrage, regional).

% Sets examination content, required hours, reciprocity rules, and disciplinary process. Board seats are disproportionately filled by incumbent practitioners nominated by trade associations. Justifies each new requirement in the language of consumer protection while the requirements track association lobbying priorities more closely than documented harm rates.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_licensing_boards, agenda_setter,
    institutional, generational, arbitrage, regional).

% Draft model legislation for state boards, testify at rulemaking hearings, and fund reelection campaigns of legislators who sit on relevant committees. Their membership dues are effectively a return on their success at narrowing the entrant pipeline.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbyists, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, trade_association_lobbyists, beneficiary).

% Must complete costly, often duplicative coursework and pass exams with content only loosely tied to job tasks, then pay recurring renewal fees. Many are working-class or mid-career changers without capital to absorb years of unpaid training; some abandon the field entirely or work informally at legal risk.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants, payer,
    powerless, biographical, constrained, regional).

% Pay higher prices produced by the artificially thinned supply of licensed providers, particularly in underserved rural and low-income areas where the practitioner shortage is most acute. Have no visibility into which requirements protect them versus which merely raise price.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_of_licensed_services, payer,
    powerless, immediate, constrained, regional).

% Frequently possess out-of-state licenses or equivalent informal competence but face non-reciprocal requirements that force them to restart training. Lack the savings buffer to survive a multi-year re-credentialing process, effectively excluding them from the licensed trade regardless of underlying skill.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, low_income_career_changers, payer,
    powerless, biographical, trapped, regional).

% Vote on licensing statute text, typically deferring to board and association-drafted language absent organized opposition. Occasionally hold sunset review hearings but rarely act against incumbent lobbying without an external triggering scandal.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislators, observer,
    institutional, biographical, analytical, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, state_legislators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_licensed_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves a real informational asymmetry — consumers cannot easily verify practitioner competence before purchase — by certifying a minimum skill floor. This reading holds that the certification function is a cover story riding on top of a scarcity-generation function that has become the statute's actual operative purpose.
% TRANSFER_FUNCTION: Moves income from prospective entrants, consumers, and low-income career changers to incumbent practitioners and the associations that represent them, via the wedge between the licensed-supply-constrained price and the price a competitive, competence-verified market would produce.
% ABSENT_VOICES: Prospective entrants and out-of-state credentialed workers are almost never represented on the boards that set the requirements that exclude them; consumers paying the markup are diffuse and unorganized relative to the concentrated incumbent lobby that shows up at every rulemaking hearing.
% DISAPPEARANCE_RATIONALE: If the statute vanished, entry would broaden rapidly, prices in the licensed trade would fall toward a competitive equilibrium, incumbent practitioners would lose their supply-restriction rents, and boards and associations funded by license fees would lose their institutional revenue base — a substantial rearrangement of the sector's income distribution.
% FOUNDING_PROBLEM: Historically framed as protecting consumers from incompetent or fraudulent practitioners in trades where harm from bad service is hard to detect before the fact.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists (using cross-state comparisons of licensed vs. unlicensed practice of the same occupation) and antitrust regulators (FTC occupational licensing studies) attest that measured harm-reduction from many licensing regimes is small or undetectable relative to the price effects, supporting the reading that the founding problem is now substantially a cover story; the boards and associations themselves are the primary voices asserting the founding problem remains fully live.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because the wedge between licensed-supply-constrained prices and competitive-market prices, documented across comparable occupations that vary in licensing intensity across states, tracks board/association lobbying success more than harm-rate evidence. Suppression (0.72) reflects active statutory enforcement — unlicensed practice is criminalized or civilly penalized, not merely disfavored. Theater ratio (0.58) is authored above the midpoint because a substantial and rising share of licensing board activity (continuing-education mandates, reciprocity denial, exam content unrelated to job tasks) functions as barrier-maintenance rather than competence verification; the theater ratio trajectory rises across the interval as boards layer additional procedural requirements onto a core credential that has not changed. All three temporal series share one time grid (T=0,8,16,24,32,40) per the alignment rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners and the boards/associations they staff are the structural beneficiaries: exit is available to them at arbitrage grade (they already hold the credential and can relocate or specialize freely) and no cost of new requirements falls on them. Prospective entrants, consumers, and especially low-income career changers are structural targets: entrants and career changers face constrained-to-trapped exit (sunk training costs, geographic immobility, absence of reciprocity), and consumers face constrained exit because licensed-service markets typically offer no unlicensed substitute due to the same statute's suppression of alternatives. This directionality difference — arbitrage-grade incumbents versus trapped entrants — is what produces the seat divergence the engine will compute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine information asymmetry about practitioner competence) is authored as contested rather than flatly dead, because some baseline competence floor plausibly remains valuable; the classification as snare (not tangled_rope) reflects the reading's judgment that the coordination component, if it still exists, no longer explains the observed level and trajectory of extraction and suppression — the requirements have decoupled from harm evidence and now track incumbent lobbying success. This is precisely the mandatrophy pattern: a mandate (protect consumers) has been retained procedurally while its actual operation serves a different, unstated function (restrict supply).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_rate_vs_rent_evidence,
    'Do cross-jurisdictional comparisons of harm rates in licensed versus unlicensed practice of the same occupation support a genuine competence-protection function, or do they show licensing intensity correlates with price effects but not harm-reduction effects?',
    'Meta-analysis of FTC and academic occupational licensing studies comparing states/occupations with varying licensing stringency, controlling for practice-area risk, to isolate the harm-reduction coefficient from the price coefficient.',
    'If harm-reduction effects are negligible while price effects are large and robust, this substantially corroborates the rent_seeking_suppression reading over the public_safety_coordination reading for the same statute. If harm-reduction effects are substantial, the public_safety_coordination reading''s epsilon would need reassessment, though this story''s epsilon (authored under the rent-seeking reading''s own lights) would not itself change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_rate_vs_rent_evidence, empirical, 'Whether documented harm-rate evidence supports the competence-protection story or only the price-effect story.').

omega_variable(
    which_reading_the_boards_endorse,
    'Is the licensing_statute_mandate kernel better modeled as a single contested claim with three competing readings, or does the statutory text itself encode enough of the rent-seeking function (e.g. explicit grandfathering, non-reciprocity clauses) that the rent_seeking_suppression reading is the textually dominant one rather than merely one interpretation among equals?',
    'Textual and legislative-history analysis of specific statutory provisions (grandfather clauses, reciprocity denial, exam-content-to-task-analysis correlation) to assess whether the statute''s own design features are more consistent with scarcity-generation than with the other two readings.',
    'If statutory design features are found to be strongly scarcity-oriented (e.g., grandfathering with no competence retest, denial of facially-equivalent out-of-state credentials), this reading''s claim to be describing the statute''s actual operative function — not merely one contestable interpretation — strengthens. If design features are neutral, all three readings remain equally defensible interpretations of the same ambiguous text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_boards_endorse, conceptual, 'Whether the rent-seeking reading describes the statute''s actual dominant function or is one of several equally plausible interpretive framings.').

omega_variable(
    board_capture_degree,
    'To what degree are licensing board appointments genuinely captured by the incumbent practitioners and associations they regulate, versus representing a broader public-interest constituency?',
    'Analysis of board composition rules and appointment histories across jurisdictions: proportion of seats reserved for or filled by licensed incumbents versus public/consumer members, and voting pattern analysis on rule changes that expand versus restrict entry.',
    'High capture degree strengthens the beneficiary attribution to boards themselves (rather than boards being neutral administrators); low capture degree would suggest board agenda-setting is less directly self-interested than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_degree, empirical, 'Degree of incumbent capture of licensing board composition and decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 8, 0.33).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 16, 0.41).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 24, 0.48).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 32, 0.54).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.1).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the licensing_statute_mandate kernel, decomposed per the epsilon-invariance principle because the same statutory text produces structurally distinct epsilon values depending on which claim is evaluated: public_safety_coordination reads the statute as a genuine competence floor (low epsilon, mountain/rope-flavored), graduated_access_filter reads it as a class-stratification mechanism (distinct victim structure organized by prior resource access), and this story reads it as incumbent rent extraction (high epsilon, snare). All three share the same underlying statutory kernel but are authored as separate constraints with independent metrics, per DP-001 and the BGS worked example.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
