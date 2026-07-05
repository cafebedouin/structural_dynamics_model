% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   Qualified immunity shields government officials, most visibly police
 *   officers, from personal-capacity civil liability under Section 1983
 *   unless the plaintiff shows the official violated a constitutional right
 *   that was 'clearly established' by closely analogous precedent at the time
 *   of the conduct. The protective-scaffold reading holds that this doctrine
 *   solves a genuine coordination problem: without it, officers making
 *   split-second decisions in ambiguous legal terrain would face personal
 *   financial ruin for good-faith errors, chilling the vigorous exercise of
 *   lawful authority and driving qualified candidates from the profession.
 *   This reading acknowledges the doctrine also externalizes real costs onto
 *   constitutional-violation survivors who are denied remedy when no
 *   sufficiently similar precedent exists — hence tangled_rope rather than
 *   pure rope: there is a genuine coordination function (protecting
 *   good-faith discretion) operating through the same mechanism that produces
 *   asymmetric extraction (uncompensated rights violations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.55).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Vigorous Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '54c4725a-f372-4988-b65a-42fa57d6cabf').
narrative_ontology:cs_kernel_codification('54c4725a-f372-4988-b65a-42fa57d6cabf', distributed).
narrative_ontology:cs_authority_grounding('54c4725a-f372-4988-b65a-42fa57d6cabf', practice).
narrative_ontology:cs_interpretation_layer_present('54c4725a-f372-4988-b65a-42fa57d6cabf').
narrative_ontology:cs_reading_relation('54c4725a-f372-4988-b65a-42fa57d6cabf', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('54c4725a-f372-4988-b65a-42fa57d6cabf', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('54c4725a-f372-4988-b65a-42fa57d6cabf', foundational, good_faith_discretion_requires_liability_shield).
narrative_ontology:cs_axiom_status(good_faith_discretion_requires_liability_shield, holdable).
narrative_ontology:cs_axiom_grounding('54c4725a-f372-4988-b65a-42fa57d6cabf', good_faith_discretion_requires_liability_shield, instrumental).
narrative_ontology:cs_axiom('54c4725a-f372-4988-b65a-42fa57d6cabf', secondary, fair_notice_justifies_precedent_specificity_requirement).
narrative_ontology:cs_axiom_status(fair_notice_justifies_precedent_specificity_requirement, holdable).
narrative_ontology:cs_axiom_grounding('54c4725a-f372-4988-b65a-42fa57d6cabf', fair_notice_justifies_precedent_specificity_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('54c4725a-f372-4988-b65a-42fa57d6cabf', harlow_fair_notice_standard).
narrative_ontology:cs_drift_state('54c4725a-f372-4988-b65a-42fa57d6cabf', post_pearson_discretionary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54c4725a-f372-4988-b65a-42fa57d6cabf', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make split-second decisions in ambiguous, high-pressure encounters. Immunity shields them from personal financial liability and from most civil suits unless their conduct violated a 'clearly established' right, so they can act decisively without needing to consult a lawyer mid-encounter. They did not design the doctrine but structure their conduct and risk tolerance around its protection.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    moderate, biographical, constrained, national).

% Fund police departments, indemnify officers in most cases, and lobby (through associations and litigation) to preserve the doctrine because it caps municipal liability exposure and makes recruiting and retaining officers easier. They administer the departments the doctrine protects and could push for reform but bear little of the doctrine's direct cost.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, agenda_setter).

% Advocate aggressively for preserving and expanding qualified immunity protections in legislatures and courts, framing them as essential to officer welfare and recruitment. They shape the political conditions under which the doctrine persists or narrows.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, agenda_setter).

% Have suffered an actual constitutional violation (excessive force, unlawful search, wrongful arrest) but must show that a prior case with materially similar facts already established the right was violated. Absent an on-point precedent, their claim is dismissed regardless of the violation's severity — they bear the full cost of the doctrine's error-protection function with no recourse against the individual officer.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Applies and refines the 'clearly established law' standard case by case, exercising substantial discretion in defining precedent specificity. Judges can resolve the constitutional merits first or skip directly to the immunity question, which shapes how much precedent accumulates over time. This discretion is the primary mechanism by which the doctrine's scope expands or contracts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Represent survivors in Section 1983 suits and bear the practical burden of overcoming the immunity defense. They are consulted in academic and advocacy fora but have no seat in the judicial or legislative processes that set or revise the doctrine's boundaries; their objections are documented in briefs and law review articles rather than incorporated into the doctrine's design.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigators, excluded,
    moderate, biographical, constrained, national).

% Study the doctrine's origins, drift, and empirical effects on officer behavior and litigation outcomes, producing the evidentiary record that legislatures and courts sometimes draw on but are not obligated to follow.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shields officers from personal liability for reasonable, good-faith split-second judgment calls in ambiguous legal terrain, so that the threat of after-the-fact litigation does not paralyze policing decisions or drive qualified candidates out of the profession.
% TRANSFER_FUNCTION: Moves the cost of erroneous or ambiguous-law constitutional violations away from the individual officer and, in practice, largely away from the municipality as well (through the dismissal of claims lacking on-point precedent) onto the person whose rights were violated, who is left without a remedy.
% ABSENT_VOICES: Civil rights litigators and the survivors they represent participate as parties in individual suits but have no institutional role in setting the 'clearly established' standard itself; that standard is set entirely by appellate courts applying precedent, with legislatures largely deferring to judicial doctrine rather than codifying a remedy structure.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers and municipalities would face expanded exposure to personal-capacity suits, insurance and indemnification arrangements would need to be renegotiated, departments would likely adopt more conservative use-of-force and search practices to reduce liability, and civil rights litigation volume and settlement rates would rise sharply — the entire risk-allocation architecture of policing and municipal liability would reorganize.
% FOUNDING_PROBLEM: Courts sought to prevent the threat of personal liability from discouraging officers from exercising discretion in fast-moving, legally ambiguous encounters, and to avoid subjecting officials to damages for violating rights that were not yet clearly defined at the time they acted.
% FOUNDING_PROBLEM_CORROBORATION: Police unions, municipal risk managers, and some judges attest the founding problem remains live — officers still face split-second, legally ambiguous decisions. Independent empirical research (e.g., studies of indemnification practices showing officers rarely pay personally even without immunity, and studies of 'clearly established' doctrine's increasingly narrow application) from legal scholars outside the beneficiary set corroborates that the doctrine's actual operation has drifted well past the original fair-notice rationale into something closer to categorical immunity, casting doubt on whether the founding problem still requires this specific remedy-denial mechanism to solve it.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.48) rather than low or severe: the doctrine has a real coordination rationale (officers should not personally bankrupt themselves for reasonable errors in unsettled law), so it is not pure extraction, but the accumulating body of case law narrowing 'clearly established' to require near-identical prior facts has measurably increased the share of meritorious claims dismissed without merits review — hence extraction and suppression both drift upward over the measured interval as the doctrine's application hardened. Theater ratio is modest (0.28): the doctrine still performs its stated fair-notice function in a substantial share of cases, but a growing fraction of its operation defends categorical dismissal rather than genuine fair-notice concerns, which is what the rising theater trajectory tracks. Suppression is authored as a raw structural property: it reflects how completely the doctrine forecloses relief regardless of the officer's actual state of mind, not how heavily any single party enforces it.
 *
 * PERSPECTIVAL GAP:
 *   From the officer and municipal seats, the doctrine is experienced as a necessary and stabilizing protection — this is precisely the protective-scaffold reading's claim, and the engine should be expected to compute something close to a coordination-favorable seat perception for those agents given their low derived directionality. From the survivor seat, the identical structure computes as extraction: a constitutional violation occurred, was real, and produced no remedy, purely because of a precedent-specificity technicality. This divergence is exactly what a tangled_rope classification is built to hold — both readings of the lived experience are structurally accurate from their respective seats, and neither is more real than the other. This is not the same divergence as the accountability_void or constitutional_fidelity readings, which are different constraints entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers, municipalities, and police unions are declared beneficiaries because the doctrine's operation — even under the protective-scaffold reading — reliably reduces their exposure and administrative burden; this places their directionality near the beneficiary end. Constitutional-violation survivors are declared victims because the same mechanism that protects good-faith discretion also denies them remedy when precedent is not sufficiently on-point, regardless of the severity of the violation; their directionality sits near the full-target end, amplified by their trapped exit options (there is no alternative forum once the federal claim is dismissed on immunity grounds). The federal judiciary occupies an agenda-setting seat with analytical exit — it does not personally benefit or pay, but its discretion in defining precedent specificity is the primary lever determining how the extraction/coordination balance shifts over time.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fair notice: officials should not be liable for violating rights not yet clearly defined) remains partially live — legal ambiguity in fast-moving encounters is a real and recurring feature of policing. But the doctrine's actual operation, per the R5 corroboration, has drifted toward requiring near-identical precedent rather than genuine fair notice, and empirical indemnification research shows officers rarely bear personal costs even without immunity — suggesting the mechanism increasingly protects municipal budgets and insurance arrangements rather than individual officer decision-making under genuine uncertainty. This drift is a live contested question, not a settled fact, which is why founding_problem_status is authored as 'contested' rather than 'dead': the protective-scaffold reading maintains the founding rationale is still substantially operative, even as the doctrine's boundary-drawing has hardened in ways that increasingly separate remedy-denial from genuine notice concerns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_qualified_immunity,
    'Is qualified immunity better characterized as (a) a necessary protective scaffold for good-faith discretion, (b) a systematic accountability void guaranteeing impunity, or (c) a judicially fabricated doctrine lacking any constitutional or statutory authorization?',
    'This ambiguity is resolved by decomposition, not by evidence internal to any one reading: each reading is authored as a separate constraint story (accountability_void_reading, constitutional_fidelity_reading, protective_scaffold_reading) linked via network.affects_constraints. This omega documents that the present file speaks only for the protective-scaffold reading and does not adjudicate among the three.',
    'Adopting a different reading changes the beneficiary/victim structure, the claimed type, and the ε value entirely — the accountability_void_reading would authorize a much higher extractiveness and likely a snare classification; the constitutional_fidelity_reading would emphasize the doctrine''s lack of textual authorization independent of its policy effects, likely also snare-adjacent or scaffold-without-sunset. This story''s classification is valid only for the protective-scaffold framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_qualified_immunity, conceptual, 'Which of three competing readings of the qualified immunity kernel best characterizes the doctrine; this story instantiates only the protective-scaffold reading.').

omega_variable(
    clearly_established_discretion_scope,
    'How much of the ''clearly established law'' standard''s narrowing over time reflects genuine accumulation of case-specific ambiguity versus a judicial policy preference for shielding officials from suit?',
    'Empirical coding of circuit court qualified immunity opinions over multiple decades, comparing precedent-specificity requirements against contemporaneous constitutional merits determinations (Pearson v. Callahan''s discretion to skip the merits question is a key variable).',
    'If the narrowing primarily reflects genuine legal ambiguity, the moderate extractiveness score is well-calibrated and the coordination function is doing real work. If it primarily reflects a judicial policy preference independent of actual notice concerns, the extractiveness figure understates the doctrine''s true drift toward the accountability_void reading over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_discretion_scope, empirical, 'Whether the doctrine''s narrowing precedent-specificity requirement reflects genuine ambiguity or policy preference.').

omega_variable(
    officer_beneficiary_versus_institutional_beneficiary,
    'Do individual officers meaningfully benefit from qualified immunity given that indemnification research shows they are rarely held personally liable even absent the doctrine, or is the true beneficiary the municipal insurance and budget structure rather than the officer as an individual?',
    'Comparative study of jurisdictions with and without robust indemnification practices, tracking whether officer behavior or recruitment differs measurably based on personal exposure versus institutional exposure.',
    'If officers are largely indemnified regardless, the beneficiary declaration for law_enforcement_officers should shift toward municipal_governments as the primary structural beneficiary, which would adjust directionality for the officer seat toward a more symmetric position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_beneficiary_versus_institutional_beneficiary, empirical, 'Whether individual officers or the municipal indemnification structure is the true structural beneficiary of the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1982, 0.14).
narrative_ontology:measurement(qual_tr_t1997, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1982, 0.3).
narrative_ontology:measurement(qual_be_t1997, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2009, 0.43).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2017, 0.46).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1982, 0.38).
narrative_ontology:measurement(qual_su_t1997, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1997, 0.45).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2017, 0.53).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the qualified_immunity_doctrine kernel. protective_scaffold_reading (this file) treats the doctrine as tangled_rope: genuine coordination function (shielding good-faith discretion) plus asymmetric extraction (uncompensated rights violations), moderate ε. accountability_void_reading treats the identical case law as near-pure extraction with a much higher ε and likely a snare classification. constitutional_fidelity_reading treats the doctrine's illegitimacy as flowing from its lack of textual/statutory authorization independent of whether its policy consequences are good or bad. All three share the same underlying case law and precedent structure but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε values, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
