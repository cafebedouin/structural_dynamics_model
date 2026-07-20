% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional/law_enforcement/civil_rights
 *
 * SUMMARY:
 *   This constraint story models the qualified immunity doctrine from the
 *   constitutional fidelity reading: the doctrine is a judicial fabrication
 *   without constitutional or statutory authorization, illegitimate
 *   regardless of policy outcomes. Key agents are identified by structural
 *   relationship to the constraint. The federal judiciary is the primary
 *   beneficiary of institutional power expansion, while both civil rights
 *   plaintiffs and law enforcement officers are denied a legitimate legal
 *   framework. The story is authored as a snare because the protective
 *   coordination narrative is cover for pure extraction of judicial
 *   authority. Claim and metrics are independently authored: the claim is
 *   snare; the metrics describe high extraction, high suppression, and
 *   significant theater in the application of the clearly established law
 *   test.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setter and beneficiary (institutional/arbitrage) â controls and profits from the doctrine
 *   - civil_rights_plaintiffs: Primary target (powerless/constrained) â denied constitutional remedies
 *   - law_enforcement_officers: Secondary target (moderate/constrained) â denied legitimate statutory framework, operates in judge-made ambiguity
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â documents the doctrine's lack of textual foundation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.72).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.85).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional/law_enforcement/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '5de83a7f-f208-4792-86db-415ab476232b').
narrative_ontology:cs_kernel_codification('5de83a7f-f208-4792-86db-415ab476232b', implicit).
narrative_ontology:cs_authority_grounding('5de83a7f-f208-4792-86db-415ab476232b', extraction).
narrative_ontology:cs_interpretation_layer_present('5de83a7f-f208-4792-86db-415ab476232b').
narrative_ontology:cs_reading_relation('5de83a7f-f208-4792-86db-415ab476232b', qualified_immunity_doctrine__protective_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('5de83a7f-f208-4792-86db-415ab476232b', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('5de83a7f-f208-4792-86db-415ab476232b', foundational, unauthorized_judicial_doctrine_is_void).
narrative_ontology:cs_axiom_status(unauthorized_judicial_doctrine_is_void, holdable).
narrative_ontology:cs_axiom_grounding('5de83a7f-f208-4792-86db-415ab476232b', unauthorized_judicial_doctrine_is_void, conventional).
narrative_ontology:cs_axiom('5de83a7f-f208-4792-86db-415ab476232b', foundational, constitutional_remedies_are_nonwaivable_by_common_law_fiat).
narrative_ontology:cs_axiom_status(constitutional_remedies_are_nonwaivable_by_common_law_fiat, holdable).
narrative_ontology:cs_axiom_grounding('5de83a7f-f208-4792-86db-415ab476232b', constitutional_remedies_are_nonwaivable_by_common_law_fiat, deontological).
narrative_ontology:cs_reference_frame('5de83a7f-f208-4792-86db-415ab476232b', constitutional_textual_order).
narrative_ontology:cs_drift_state('5de83a7f-f208-4792-86db-415ab476232b', contemporary_qualified_immunity_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5de83a7f-f208-4792-86db-415ab476232b', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created the qualified immunity doctrine in Pierson v. Ray (1967) and expanded it through Harlow v. Fitzgerald (1982) and subsequent precedents. Controls the clearly established law test and applies it to dismiss constitutional claims. Derives institutional agenda-setting power and discretion from maintaining a common-law immunity framework without statutory or constitutional authorization.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Individuals alleging violations of constitutional rights by government officials. Their claims are dismissed at the motion-to-dismiss or summary-judgment stage when courts find the asserted right was not clearly established at the time of conduct. They bear the cost of denied remedy, denied trial, and the judicial signal that constitutional violations may proceed unchecked.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_plaintiffs, payer,
    powerless, biographical, constrained, national).

% Government officials whose conduct is challenged in Section 1983 and Bivens actions. While shielded from personal financial liability by the doctrine, they operate within a judge-made framework that denies them the clarity of statutory or constitutional rules defining lawful conduct boundaries. They bear the cost of legal ambiguity and the erosion of public legitimacy that attends unremedied constitutional violations.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, payer,
    moderate, biographical, constrained, national).

% Legal academics and public-law scholars who document the doctrine's absence from the text and history of Section 1983 and the Constitution. They publish originalist and textualist critiques demonstrating the doctrine's invention in 1967, but exercise no control over its application.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None â the doctrine purports to coordinate the balance between constitutional accountability and official discretion, but from this reading's perspective it solves no genuine coordination problem. The protective function is cover for a judicial power expansion that operates without constitutional or statutory authorization.
% TRANSFER_FUNCTION: Transfers agenda-setting authority over the scope of constitutional remedies from the legislative process and litigants to the federal judiciary; transfers the material and dignitary costs of unconstitutional government conduct from offending officers to victims by foreclosing trial and remedy.
% ABSENT_VOICES: Civil rights plaintiffs dismissed on immunity grounds are excluded from the remedial framework; legislative majorities that would codify liability standards are sidelined by judicial preemption; law enforcement officers who would prefer clear statutory rules to judge-made ambiguity have no seat in the doctrine's maintenance.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, pending constitutional tort cases would proceed to discovery and trial, the federal judiciary would lose its gatekeeping discretion over Section 1983 and Bivens actions, officers would face personal liability exposure or statutory indemnification regimes, and Congress would be compelled to legislate standards for official liability â the inter-branch distribution of power and the practical availability of constitutional redress would reorganize.
% FOUNDING_PROBLEM: To shield government officials from the chilling effect of personal liability for discretionary actions performed in good faith, preserving effective government operations; first judicially constructed in Pierson v. Ray (1967) and later transformed into an objective clearly established law test in Harlow v. Fitzgerald (1982).
% FOUNDING_PROBLEM_CORROBORATION: Originalist and textualist scholars and dissenting Supreme Court justices attest from outside the judiciary's beneficiary seat that the doctrine lacks historical foundation; legislative proposals to abolish qualified immunity corroborate that the problem is now addressed through institutional power maintenance rather than the original protective rationale.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the doctrine systematically denies plaintiffs trial and remedy while depriving officers of clear rules. Suppression is very high (0.85) because the constraint persists through judicial enforcement of the clearly established law barrier and the absence of legislative override. Theater ratio is elevated (0.58) because courts perform elaborate multi-step analyses that functionally serve to protect judicial discretion rather than implement a statutory or constitutional mandate. Accessibility collapse is high (0.82) because alternative remedial pathways have been judicially narrowed. Resistance is substantial (0.70) due to sustained scholarly critique and periodic judicial dissents. The measurement series tracks the doctrine's intensification from Harlow (1982) to the present on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this constraint as a legitimate exercise of common-law adjudication preserving government function; civil rights plaintiffs experience it as an absolute bar to remedy; law enforcement officers experience ambiguous protection that substitutes judicial discretion for statutory clarity. The engine computes divergent per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is declared as beneficiary (agenda_setter with arbitrage exit), producing a low directionality value â the constraint subsidizes its institutional power. Civil rights plaintiffs and law enforcement officers are declared as victims (payers with constrained exit), producing high directionality values â the constraint extracts from them by denying a legitimate legal framework. No override is needed because the structural derivation matches the reading's intended relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's original mandate â protecting good-faith official discretion â has outlived its legitimate function and now persists as a mechanism of judicial power accumulation. The protective scaffold reading would classify this as a scaffold or tangled rope (coordination with officer protection). The constitutional fidelity reading rejects that framing: the mandate is dead, the coordination story is cover, and the residual structure is a snare extracting institutional authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_power_vs_officer_impunity,
    'Is the primary extraction beneficiary the federal judiciary (institutional power expansion) or law enforcement officers (impunity from liability)?',
    'Comparative analysis of who controls the doctrine''s evolution and who captures the structural gains â judicial discretion over docket control versus officer financial protection.',
    'If officers are the primary beneficiaries, the constraint aligns with the accountability_void_reading; if the judiciary is the primary beneficiary, the snare classification rests on judicial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_power_vs_officer_impunity, conceptual, 'Beneficiary ambiguity between judiciary and officers').

omega_variable(
    common_law_immunity_historicity,
    'Did the 1871 Civil Rights Act preserve or abolish common-law immunities, and does historical practice support judicial creation of new immunities?',
    'Archival and legal-historical research into the 42nd Congress''s understanding of Section 1983 and the common-law status of official immunities in 1871.',
    'If common-law immunities were not preserved, the constitutional_fidelity reading is strengthened; if they were, the doctrine may have textual grounding that undermines the illegitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_immunity_historicity, empirical, 'Historical basis for judicial immunity doctrine').

omega_variable(
    legislative_vs_judicial_fix_path,
    'Can qualified immunity be fixed more cheaply by congressional statute or by judicial overruling, and does the fix path matter for classification?',
    'Analysis of legislative vote counts, judicial docket composition, and institutional incentives to maintain or overturn the doctrine.',
    'If only the judiciary can fix it and they refuse, the constraint exhibits self-protective extraction; if Congress could cheaply fix it but chooses not to, the extraction is better described as legislative abdication than judicial snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_vs_judicial_fix_path, preference, 'Institutional path dependency of doctrine reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qid_cfr_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qid_cfr_tr_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(qid_cfr_tr_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(qid_cfr_tr_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(qid_cfr_tr_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 42, 0.58).

% Extraction over time
narrative_ontology:measurement(qid_cfr_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(qid_cfr_be_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(qid_cfr_be_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(qid_cfr_be_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(qid_cfr_be_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 42, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qid_cfr_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(qid_cfr_su_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(qid_cfr_su_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(qid_cfr_su_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(qid_cfr_su_t42, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 42, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.1).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, accountability_void_reading).

% DUAL FORMULATION NOTE:
% The natural-language label qualified immunity conflates three structurally distinct constraints: protective_scaffold_reading (coordination function for officer discretion), accountability_void_reading (extraction function for impunity), and constitutional_fidelity_reading (illegitimacy through lack of textual authorization). Each has distinct epsilon values, beneficiary/victim structures, and institutional dynamics. They are modeled as separate stories linked in a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
