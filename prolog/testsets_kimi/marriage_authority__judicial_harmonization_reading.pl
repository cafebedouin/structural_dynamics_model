% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority via Constitutional Floor Imposition
 *   domain: legal/constitutional/comparative_family_law
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_harmonization_reading of the
 *   marriage_authority kernel. In contexts of legal pluralism with
 *   politically blocked legislative reform, the Supreme Court imposes
 *   constitutional floors on personal law codes governing marriage through
 *   case-by-case review. The mechanism bypasses both communal norm-generation
 *   and legislative codification, concentrating interpretive authority in the
 *   judiciary. It is presented as a transitional scaffold toward
 *   constitutional convergence, yet it lacks formal statutory sunset and
 *   risks entrenching judicial supremacy indefinitely.
 *
 * KEY AGENTS:
 *   - supreme_court_judiciary: Agenda-setter (institutional/constrained) â expands authority by imposing constitutional floors; primary seat where institutional extraction accrues.
 *   - individual_petitioners: Beneficiary (moderate/mobile) â gain rights protections via constitutional litigation against discriminatory personal law.
 *   - religious_communal_authorities: Primary target (organized/constrained) â bear loss of autonomous norm-generation and enforcement over marriage.
 *   - union_legislature: Secondary target (institutional/constrained) â displaced from law-making primacy in family law by judicial harmonization.
 *   - legal_academics: Analytical observer (analytical/analytical) â tracks doctrinal drift and institutional capture dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.48).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.55).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority via Constitutional Floor Imposition").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'b1859460-cb4c-4125-a574-4fd5e16360d4').
narrative_ontology:cs_kernel_codification('b1859460-cb4c-4125-a574-4fd5e16360d4', formalized).
narrative_ontology:cs_authority_grounding('b1859460-cb4c-4125-a574-4fd5e16360d4', lineage).
narrative_ontology:cs_interpretation_layer_present('b1859460-cb4c-4125-a574-4fd5e16360d4').
narrative_ontology:cs_reading_relation('b1859460-cb4c-4125-a574-4fd5e16360d4', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('b1859460-cb4c-4125-a574-4fd5e16360d4', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('b1859460-cb4c-4125-a574-4fd5e16360d4', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1859460-cb4c-4125-a574-4fd5e16360d4', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('b1859460-cb4c-4125-a574-4fd5e16360d4', foundational, constitutional_floor_judicial_imposition).
narrative_ontology:cs_axiom_status(constitutional_floor_judicial_imposition, holdable).
narrative_ontology:cs_axiom_grounding('b1859460-cb4c-4125-a574-4fd5e16360d4', constitutional_floor_judicial_imposition, conventional).
narrative_ontology:cs_axiom('b1859460-cb4c-4125-a574-4fd5e16360d4', foundational, harmonization_without_legislation).
narrative_ontology:cs_axiom_status(harmonization_without_legislation, holdable).
narrative_ontology:cs_axiom_grounding('b1859460-cb4c-4125-a574-4fd5e16360d4', harmonization_without_legislation, instrumental).
narrative_ontology:cs_reference_frame('b1859460-cb4c-4125-a574-4fd5e16360d4', constitutional_supremacy_framework).
narrative_ontology:cs_drift_state('b1859460-cb4c-4125-a574-4fd5e16360d4', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1859460-cb4c-4125-a574-4fd5e16360d4', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, individual_petitioners).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, religious_communal_authorities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, union_legislature).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, basic_structure_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expands its institutional authority by articulating and enforcing constitutional floors that override personal law codes in marriage matters on a case-by-case basis. Derives legitimacy from constitutional supremacy and basic structure doctrine. Cannot exit this function without fundamentally altering the constitutional scheme and its own role within it.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Invoke constitutional remedies to challenge personal law provisions that discriminate in marriage and family relations. Benefit directly when courts impose constitutional floors that override restrictive communal norms. Can exit by choosing not to litigate, though social costs within their communities may constrain this option.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, individual_petitioners, beneficiary,
    moderate, biographical, mobile, national).

% Lose normative authority over marriage regulation as courts override personal law provisions with constitutional floors. Bear the cost of diminished autonomy and reduced ability to enforce community-specific marriage norms. Can lobby politically or mobilize socially but lack structural exit from the constitutional review framework.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_communal_authorities, payer,
    organized, generational, constrained, national).

% Loses law-making primacy in family law to judicial constitutional interpretation. Bears the cost of legislative atrophy in a major legal domain. Could theoretically fix the constraint by enacting a comprehensive Uniform Civil Code, but political deadlock and coalition costs make this prohibitively difficult.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, union_legislature, payer,
    institutional, generational, constrained, national).

% Document and critique the accretion of judicial authority over personal law. Analyze the tension between constitutional universalism and legal pluralism. Neither collect from nor pay into the constraint; their position is external and analytical.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legal_academics, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transitional pathway to harmonize divergent personal law codes with constitutional equality guarantees when formal legislative codification is blocked by political deadlock.
% TRANSFER_FUNCTION: Transfers normative authority over marriage regulation from religious communal traditions and the legislature to the Supreme Court; transfers rights protections to individual petitioners who successfully challenge personal law provisions.
% ABSENT_VOICES: Religious minority women who might oppose both communal patriarchy and majoritarian constitutionalism are rarely centered as interlocutors; the legislature is structurally sidelined in this convergence pathway despite possessing formal authority to enact a Uniform Civil Code.
% DISAPPEARANCE_RATIONALE: If the judicial constitutional floor mechanism vanished, personal law codes would reassert full autonomy in marriage regulation, communal authorities would regain normative primacy, the legislature would face altered pressure regarding UCC enactment, and individual petitioners would lose a direct rights enforcement channelâfamily law would reorganize around either legislative action or communal fragmentation.
% FOUNDING_PROBLEM: Political deadlock preventing legislative enactment of a Uniform Civil Code while personal law codes perpetuate gender inequality and intra-community inequities in marriage regulation.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations and public interest litigators attest the problem from outside the judiciary. Communal authorities and legislative members dispute both the characterization and the judicial solution. Academic constitutional scholars provide external analysis confirming political deadlock but remain divided on whether judicial harmonization is the appropriate remedy.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.48) because the judiciary gains significant institutional authority and agenda-setting power, though the extraction is not monetary. Suppression (0.55) reflects the active override of personal law alternatives and legislative bypass. Theater_ratio (0.30) captures the moderate gap between expansive constitutional rhetoric and uneven ground-level implementation. Accessibility_collapse (0.75) is high because once a constitutional floor is announced, alternative communal norms in that domain lose legal validity. Resistance (0.60) reflects sustained pushback from communal authorities and legislative elites. The measurement series track the maturation of this mechanism from cautious early review to assertive contemporary constitutionalism on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary experiences this arrangement as legitimate constitutional guardianship and transitional coordination toward rights-protective harmonization. Communal authorities experience it as institutional dispossession and cultural erosion. The legislature experiences it as constitutional encroachment on democratic law-making. These divergent seat perceptions arise from the same structural facts: the transfer of marriage norm-generation from plural communal and legislative sources to a centralized judicial forum.
 *
 * DIRECTIONALITY LOGIC:
 *   The supreme_court_judiciary and individual_petitioners are declared beneficiaries, receiving low directionality and damped effective extraction; the judiciary collects institutional authority and petitioners collect rights protections. Religious_communal_authorities and union_legislature are declared victims, receiving high directionality and amplified effective extraction; they bear the costs of lost autonomy and displaced legislative primacy respectively. The engine will compute divergent per-seat classifications from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as scaffold rather than snare or tangled_rope depends on the transitional justification: the mechanism is warranted by the transition to harmonization, not the steady state. However, if the transition never completes and judicial authority becomes self-perpetuating, the constraint risks mandatrophy into a piton or snare. The authored theater_ratio and the omegas documenting sunset ambiguity are designed to flag this drift. The claim/metric independence is maintained: the claimed type is scaffold, but the metrics honestly describe a moderately extractive, actively enforced arrangement with significant resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_harmonization_sunset,
    'Is the judicial harmonization mechanism structurally self-sunsetting upon future legislative codification, or does it entrench permanent judicial supremacy over personal law regardless of legislative action?',
    'Observation of judicial behavior following legislative enactment in analogous domainsâwhether courts defer to the new legislative scheme or continue parallel constitutional review.',
    'If permanent, the scaffold classification fails and the constraint is better modeled as a tangled_rope or snare extracting institutional power from the legislature. If transitional, the scaffold claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_harmonization_sunset, conceptual, 'Whether judicial harmonization lacks a genuine sunset or has become self-perpetuating.').

omega_variable(
    mechanism_vs_normative_reading,
    'Does the judicial harmonization reading constitute an independent normative commitment, or is it merely an institutional mechanism parasitic on the gender-rights or secularist readings?',
    'Examination of judicial opinions for independent doctrinal justification (structural constitutionalism, basic structure) versus derivative equality-based or secularist reasoning.',
    'If parasitic, it should be classified as a downstream effect of other constraints rather than a standalone constraint with independent epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_vs_normative_reading, conceptual, 'Whether judicial harmonization is an independent reading or a mechanism parasitic on others.').

omega_variable(
    communal_autonomy_collapse,
    'To what extent does judicial constitutional floor imposition actually collapse alternatives for communal authorities, versus merely shifting their norm-generation to areas outside marriage?',
    'Empirical mapping of personal law code provisions struck down versus remaining autonomy space in family law and adjacent domains.',
    'High collapse supports higher extraction and suppression metrics; low collapse suggests the constraint is more symbolic than structurally extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_autonomy_collapse, empirical, 'Actual degree of alternative collapse for communal authorities under judicial review.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(marr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(marr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(marr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(marr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(marr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(marr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel, decomposed from the colloquial label 'marriage authority' which conflates communal autonomy, secularist legislative supremacy, gender-equality judicial expansion, federalist millet pluralism, and judicial harmonization mechanisms. Each reading carries a distinct epsilon, stakeholder structure, and classification; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
