% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity — Protective Scaffold Reading
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story instantiates the PROTECTIVE SCAFFOLD READING of the qualified
 *   immunity kernel: the doctrine as a transitional protection meant to
 *   balance officer initiative against victim remedy, which has gradually
 *   atrophied into a near-absolute bar. The reading acknowledges genuine
 *   coordination function (protecting officers from bad-faith suits in
 *   genuinely novel situations) but documents how judicial application has
 *   externalized litigation costs to constitutional violation survivors. The
 *   scaffold was meant to sunset as constitutional law developed; instead,
 *   the 'clearly established' test became a ratchet that prevents new rights
 *   from ever becoming clearly established because courts dismiss cases
 *   without reaching the merits — the constitutional question is never
 *   resolved, so the right never becomes clearly established, so the next
 *   case is dismissed. This is the doctrinal feedback loop that converts
 *   scaffold into snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.42).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.38).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, scaffold).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity — Protective Scaffold Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:has_sunset_clause(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'f8317aa5-eb9b-47fe-9c1f-8098d01893a1').
narrative_ontology:cs_kernel_codification('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', fixed_text).
narrative_ontology:cs_authority_grounding('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', lineage).
narrative_ontology:cs_interpretation_layer_present('f8317aa5-eb9b-47fe-9c1f-8098d01893a1').
narrative_ontology:cs_reading_relation('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', foundational, officer_initiative_requires_liability_protection).
narrative_ontology:cs_axiom_status(officer_initiative_requires_liability_protection, holdable).
narrative_ontology:cs_axiom_grounding('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', officer_initiative_requires_liability_protection, instrumental).
narrative_ontology:cs_axiom('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', foundational, clearly_established_test_balances_initiative_and_accountability).
narrative_ontology:cs_axiom_status(clearly_established_test_balances_initiative_and_accountability, holdable).
narrative_ontology:cs_axiom_grounding('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', clearly_established_test_balances_initiative_and_accountability, conventional).
narrative_ontology:cs_reference_frame('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', harlow_balanced_qualified_immunity).
narrative_ontology:cs_drift_state('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', post_pearson_callahan_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8317aa5-eb9b-47fe-9c1f-8098d01893a1', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_departments).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_insurers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive immunity from personal liability for constitutional violations unless the right was 'clearly established' in prior precedent. The doctrine shields them from the financial and professional ruin of meritless suits. Their professional identity fuses with the legal protection — leaving policing or losing immunity both feel like existential threats to who they are as officers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    organized, biographical, identity_locked, national).

% Set training, policy, and supervision standards that interact with the 'clearly established' test. Benefit from the doctrine by avoiding indemnification costs and preserving recruitment. Can lobby for legislative reform but also benefit from judicial doctrine they do not control.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_departments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_departments, beneficiary).

% Underwrite police liability risk. The immunity doctrine dramatically reduces their exposure by filtering out claims before discovery. They price premiums assuming the doctrine's protection; if it narrowed, their costs would rise and they would pass them to municipalities.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_insurers, beneficiary,
    powerful, biographical, mobile, national).

% Bear the full cost of constitutional violations when immunity bars their claims. They experience the violation, lose the remedy, and have no alternative path to compensation. Their exit is structural — the courthouse door is closed by the doctrine itself.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, immediate, trapped, national).

% Bring Section 1983 claims that are dismissed at summary judgment because no prior case has 'clearly established' the right in the same factual configuration. They invest resources in litigation that the doctrine renders futile. Some pivot to legislative advocacy, but the judicial barrier remains.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigants, excluded).

% Administer the 'clearly established' test case by case. Their discretion in defining the requisite specificity of precedent determines the doctrine's effective scope. They can narrow or expand the scaffold through incremental rulings, but institutional inertia and stare decisis constrain rapid change.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Created the doctrine in Harlow v. Fitzgerald (1982) and can modify or abolish it. Has repeatedly declined to revisit it despite criticism from across the ideological spectrum. Its inaction functions as ongoing ratification of the scaffold.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Has statutory authority to abrogate or modify qualified immunity under Section 1983 but has not acted despite repeated legislative proposals. Political dynamics — police union influence, 'soft on crime' framing — keep the issue off the floor. Their exclusion is structural: the doctrine persists because they do not act.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, congress, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable legal framework that allows officers to make split-second decisions in volatile situations without paralyzing fear of personal financial ruin from litigation over novel or ambiguous constitutional questions. Solves the coordination problem of aligning officer initiative with constitutional boundaries when the law is unsettled.
% TRANSFER_FUNCTION: Transfers the cost of constitutional violations from individual officers and their employers (who would pay judgments or settlements) to the victims of those violations (who receive no remedy when immunity applies). The transfer is not monetary but remedial: the victim's right to compensation is extinguished to preserve the officer's immunity.
% ABSENT_VOICES: Victims of constitutional violations whose cases are dismissed before discovery — they never reach a jury, never tell their story in court, and have no organized lobby. Also absent: officers who would welcome clearer constitutional boundaries and accountability mechanisms but cannot speak against union and institutional orthodoxy.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers would face personal liability exposure for constitutional violations, municipalities would face higher indemnification costs, insurers would raise premiums or withdraw, and police departments would rapidly adopt clearer use-of-force policies and training to define 'clearly established' rights prospectively. The litigation floodgates would open initially, then settle as constitutional boundaries sharpened through adjudication rather than being preemptively blocked.
% FOUNDING_PROBLEM: Post-Monell (1978), municipalities could be sued for constitutional violations, but officers themselves had absolute immunity. Harlow replaced absolute immunity with qualified immunity to balance two concerns: (1) officers needed protection from harassing litigation that would deter vigorous enforcement, and (2) victims needed a path to vindicate clearly established rights. The 'clearly established' test was meant to be a workable standard, not a near-absolute bar.
% FOUNDING_PROBLEM_CORROBORATION: The protective scaffold reading is attested by police unions, the DOJ (across administrations), and judicial majorities who cite Harlow's policy rationale. The accountability void reading is attested by civil rights organizations (ACLU, NAACP LDF), the Cato Institute (libertarian), and Justices Sotomayor and Thomas in dissent — ideologically diverse critics who agree the doctrine has become a near-absolute bar. The constitutional fidelity reading is attested by originalist scholars (Will Baude, Jud Campbell) who argue the doctrine has no basis in the text or history of Section 1983. Corroboration for the shifted-function critique comes from outside the benefiting parties.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).
:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate — not zero (the coordination function is real, officers do face frivolous suits) but substantial (the cost transfer to victims is systematic and growing). Suppression (0.38) is moderate — the doctrine does not use physical force but structurally suppresses remedies by closing the courthouse door at summary judgment. Theater ratio (0.28) is significant — the 'clearly established' test performs the appearance of a legal standard while functioning as a procedural trap. Accessibility collapse (0.62) is high — once a plaintiff understands the test, alternatives (legislative reform, Supreme Court reversal) are practically inaccessible. Resistance (0.55) is substantial — the doctrine faces sustained critique from left, right, and center but persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (officers, departments, insurers) experience the constraint as genuine coordination — a necessary protection that enables their function. The payer seats (victims, litigants) experience it as extraction — a structural bar to remedy that externalizes the cost of constitutional violations onto them. The agenda_setter seats (courts) sit in tension: they administer a test they know produces perverse outcomes but feel constrained by precedent and separation-of-powers concerns. The engine computes this divergence from the declared structural data; the protective scaffold reading claims the coordination function is primary, while the accountability void reading claims the extraction is primary. Both are structurally true from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers are beneficiaries (d ~ 0.2): they receive immunity, their professional identity is fused with it (identity_locked exit), and they would lose the most if it were abolished. Municipal insurers are beneficiaries (d ~ 0.1): they capture reduced risk but can exit by repricing or withdrawing. Police departments are dual-positioned agenda_setters/beneficiaries (d ~ 0.25): they administer policy that interacts with the doctrine and benefit from cost avoidance. Constitutional violation survivors are full targets (d ~ 0.95): trapped, powerless, bearing the full cost with no remedy. Civil rights litigants are payers (d ~ 0.8): constrained exit (can pivot to legislation but the judicial barrier remains). Federal courts and the Supreme Court are analytical/institutional agenda_setters with analytical exit — they observe and could change the structure but face stare decisis and institutional inertia. Congress is excluded but powerful — their inaction sustains the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine was founded as a scaffold — a transitional balance that would evolve as constitutional law developed. The founding problem (balancing officer initiative against victim remedy in an unsettled legal landscape) is contested: the coordination need persists but the extraction has grown far beyond the scaffold's design. The mandate has atrophied because the 'clearly established' ratchet prevents the very legal development the scaffold presupposed. The doctrine now persists through judicial inertia and political gridlock, not because the founding problem remains in its original form. This is mandatrophy: the arrangement outlived its function but the cost of fixing it (judicial reversal or congressional action) exceeds what any single actor bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_to_snare_transition_point,
    'At what point did the qualified immunity scaffold complete its transition into a snare — or has it not yet?',
    'Longitudinal analysis of dismissal rates at summary judgment pre- and post-Pearson v. Callahan (2009), which allowed courts to skip the constitutional question and dismiss solely on ''not clearly established.'' Track whether courts increasingly decline to decide constitutional questions, preventing rights from ever becoming ''clearly established.''',
    'If the transition is complete, the constraint is a snare with a scaffold cover story. If incomplete, it remains a degraded scaffold with residual coordination function. The classification hinges on whether the coordination function is vestigial or operational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_to_snare_transition_point, empirical, 'Whether the protective scaffold has fully atrophied into an accountability snare').

omega_variable(
    bad_faith_litigation_magnitude,
    'What is the actual volume and impact of bad-faith litigation against officers, independent of the immunity doctrine''s filtering effect?',
    'Empirical study of Section 1983 filings: meritorious vs. frivolous claims, dismissal rates pre-discovery, and officer time/cost burden in jurisdictions with varying immunity standards. Compare to other professions with similar public-contact roles but no qualified immunity.',
    'If bad-faith litigation is minimal, the coordination justification is largely pretextual and the doctrine is predominantly extractive. If substantial, the scaffold reading''s founding problem remains live and the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bad_faith_litigation_magnitude, empirical, 'Whether the doctrine''s stated justification (deterring frivolous suits) corresponds to empirical reality').

omega_variable(
    committer_frame_kernel_decomposition,
    'Is the qualified immunity doctrine a single constraint with contested readings, or three structurally distinct constraints sharing a label?',
    'Apply the epsilon-invariance test: if the protective scaffold reading, accountability void reading, and constitutional fidelity reading author materially different extractiveness values for the same doctrine, they are different constraints. The protective scaffold reading authors ε ≈ 0.42; the accountability void reading would author ε ≈ 0.75; the constitutional fidelity reading authors ε as undefined (the doctrine should not exist). The wide divergence confirms decomposition.',
    'Confirms the kernel decomposition is analytically necessary, not merely rhetorical. Each reading gets its own constraint story with its own ε, beneficiaries, victims, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_decomposition, conceptual, 'Structural validation of the kernel decomposition into three constraint stories').

omega_variable(
    congressional_inaction_as_structural_feature,
    'Is Congress''s failure to act on qualified immunity a contingent political fact or a structural feature of the constraint''s persistence?',
    'Analyze whether the doctrine''s design — specifically, that it shields the very actors (police unions, municipalities) who lobby Congress — creates a self-reinforcing loop where the beneficiaries of the doctrine are also the gatekeepers of legislative reform.',
    'If structural, the constraint includes Congress as a captured agenda_setter whose exclusion is endogenous. If contingent, Congress is a genuine excluded party that could act but has not yet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_inaction_as_structural_feature, conceptual, 'Whether legislative gridlock is a bug or a feature of the qualified immunity constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(qual_tr_t1990, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(qual_tr_t2000, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(qual_tr_t2015, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(qual_tr_t2020, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1982, 0.15).
narrative_ontology:measurement(qual_be_t1990, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(qual_be_t2000, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(qual_be_t2015, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(qual_be_t2020, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2020, 0.41).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(qual_su_t1990, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(qual_su_t2000, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2009, 0.33).
narrative_ontology:measurement(qual_su_t2015, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(qual_su_t2020, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2020, 0.37).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__protective_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, section_1983_litigation_structure).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, police_union_collective_bargaining).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, municipal_indemnification_practice).

% DUAL FORMULATION NOTE:
% This story is one of three in the qualified_immunity_doctrine constraint family. The protective scaffold reading (this file) claims ε ≈ 0.42 with genuine but degraded coordination function. The accountability_void_reading claims ε ≈ 0.75 with coordination as pretext. The constitutional_fidelity_reading claims the doctrine is illegitimate ab initio — its ε is undefined because the constraint should not exist. All three share the same kernel (qualified_immunity_doctrine) but instantiate different constraints with different structural data. They are linked via affects_constraints and should be analyzed as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, institutional, 0.2).
constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, organized, 0.2).
constraint_indexing:directionality_override(qualified_immunity_doctrine__protective_scaffold_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
