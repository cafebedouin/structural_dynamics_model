% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Positivist Reading of US Constitutional Validity: Formal Enactment as Sole Ground
 *   domain: constitutional_law/legal_philosophy/interpretive_theory
 *
 * SUMMARY:
 *   This constraint story captures the positivist reading of the US
 *   Constitution: constitutional validity derives exclusively from formal
 *   enactment procedures (Article V amendment, legislative passage,
 *   hierarchical precedent) and not from moral content, natural law, or
 *   historical meaning. It is one of three live readings of the kernel
 *   'us_constitution_text'. The positivist reading presents itself as a
 *   coordination mechanism (a Rope) that stabilizes law by tethering validity
 *   to source, not substance. But it extracts from substantive justice
 *   claimants whose moral claims lack formal enactment — a structural
 *   asymmetry that the engine will measure via per-seat classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.22).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.35).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Positivist Reading of US Constitutional Validity: Formal Enactment as Sole Ground").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '3183260f-0802-453b-bdb3-211dcaff562b').
narrative_ontology:cs_kernel_codification('3183260f-0802-453b-bdb3-211dcaff562b', formalized).
narrative_ontology:cs_authority_grounding('3183260f-0802-453b-bdb3-211dcaff562b', lineage).
narrative_ontology:cs_interpretation_layer_present('3183260f-0802-453b-bdb3-211dcaff562b').
narrative_ontology:cs_reading_relation('3183260f-0802-453b-bdb3-211dcaff562b', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3183260f-0802-453b-bdb3-211dcaff562b', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('3183260f-0802-453b-bdb3-211dcaff562b', foundational, formal_enactment_sole_validity_ground).
narrative_ontology:cs_axiom_status(formal_enactment_sole_validity_ground, holdable).
narrative_ontology:cs_axiom_grounding('3183260f-0802-453b-bdb3-211dcaff562b', formal_enactment_sole_validity_ground, conventional).
narrative_ontology:cs_axiom('3183260f-0802-453b-bdb3-211dcaff562b', foundational, judicial_moral_reasoning_invalid_as_law).
narrative_ontology:cs_axiom_status(judicial_moral_reasoning_invalid_as_law, holdable).
narrative_ontology:cs_axiom_grounding('3183260f-0802-453b-bdb3-211dcaff562b', judicial_moral_reasoning_invalid_as_law, conventional).
narrative_ontology:cs_reference_frame('3183260f-0802-453b-bdb3-211dcaff562b', legal_positivist_rule_of_recognition).
narrative_ontology:cs_drift_state('3183260f-0802-453b-bdb3-211dcaff562b', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3183260f-0802-453b-bdb3-211dcaff562b', '2026-08-24T12:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_institutions).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legislative_authorities).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legal_practitioners).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, state_officials_relying_on_predictability).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, marginalized_groups_seeking_recognition).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, rights_advocates_without_formal_enactment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, congress_and_state_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legal_practitioners_and_scholars).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, lower_court_judges).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, rule_of_law_as_procedural_predictability).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, institutional_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, separation_of_powers_via_article_v).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the Constitution through a formal validity lens; their opinions constitute binding precedent. They benefit from the constraint's predictability and institutional authority, but are bound by its procedural discipline — they cannot import moral reasoning as a validity condition without breaking the reading's internal logic. Exit means leaving the bench or writing dissents that do not change the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, supreme_court_justices, beneficiary).

% Apply Supreme Court precedent and formal enactment criteria; constrained by hierarchy. They bear the cost of suppressing equitable or moral reasoning in hard cases, but benefit from clear decision-rules that insulate them from political criticism.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, lower_court_judges, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, lower_court_judges, payer).

% Hold the formal amendment power (Article V) and ordinary legislative power; the constraint validates their enactments as the exclusive source of constitutional validity. They benefit from a clear boundary between their lawmaking authority and judicial moral review. Exit is mobile: they can amend the Constitution or pass new statutes within the existing framework.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, congress_and_state_legislatures, beneficiary,
    institutional, generational, mobile, national).

% Operate within a predictable, rule-based interpretive framework; their professional expertise and advisory value depend on stable formal criteria. They benefit from the constraint's clarity and teachability. Exit is mobile: they can shift to other interpretive frameworks in academic work, though professional practice demands compliance.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_practitioners_and_scholars, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, legal_practitioners_and_scholars, observer).

% Police, prosecutors, administrators, regulators — they need stable rules to enforce and administer. The constraint gives them a fixed reference point: valid law is what was enacted. They benefit from reduced decision-uncertainty. Exit is constrained: their role requires applying the law as the courts say it is.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, state_officials_relying_on_predictability, beneficiary,
    organized, biographical, constrained, national).

% Litigants and communities arguing that constitutional meaning should protect dignity, equality, or liberty beyond what the enacted text provides. They bear the cost of the constraint's formalism: their claims are dismissed as judicially unenforceable without formal enactment. Exit is trapped: they cannot access the formal amendment process (Article V's supermajority thresholds) and have no alternative forum where moral validity suffices.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_claimants, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, substantive_justice_claimants, excluded).

% Groups whose rights claims depend on evolving moral understanding (e.g., LGBTQ+ rights, reproductive autonomy, voting rights restoration) rather than historical enactment. They pay the highest extraction: the constraint treats their exclusion as legally unremarkable until formal enactment occurs. Exit is trapped: identity-locked to the polity, excluded from the supermajority coalition needed for Article V.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, marginalized_groups_seeking_recognition, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, marginalized_groups_seeking_recognition, excluded).

% Civil rights organizations, public interest lawyers, advocacy networks — they invest resources in moral-validity arguments that the constraint structurally excludes. They bear opportunity costs and strategic frustration. Exit is constrained: they can pivot to legislative lobbying or state constitutional amendment campaigns, but the federal constraint remains the primary obstacle.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, rights_advocates_without_formal_enactment, payer,
    moderate, biographical, constrained, national).

% Hold a competing reading (originalist_reading) that also rejects moral validity but grounds constraint in historical meaning rather than formal enactment. They observe the positivist constraint from a rival interpretive seat; their coexistence creates the kernel's interpretive contest. Exit is analytical: they engage the dispute as intellectual combatants.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, originalist_judges_and_scholars, observer,
    institutional, generational, analytical, national).

% Hold a competing reading (living_constitutionalist_reading) that affirms moral validity and evolutionary interpretation. They observe the positivist constraint as the primary antagonist to their position. Exit is analytical: they engage the dispute as intellectual combatants.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, living_constitutionalist_judges_and_scholars, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, determinate, publicly verifiable criterion for constitutional validity — formal enactment — that coordinates judicial decision-making, legislative drafting, and citizen expectation around a shared rule of recognition. Solves the coordination problem of 'what counts as law' without requiring agreement on moral truth.
% TRANSFER_FUNCTION: Transfers interpretive authority and decision-outcome control from moral-reasoning actors (judges, advocates, communities) to formal-enactment actors (legislatures, supermajorities, Article V ratifiers). Moves the power to say 'this is constitutional' from outcome-validity to source-validity.
% ABSENT_VOICES: Future generations who will inherit the constraint's exclusions; non-citizens subject to US constitutional authority without representation in Article V; the politically disorganized poor who cannot access the supermajority amendment process. They are not in the room when validity is decided.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, judges would immediately import moral reasoning as a validity condition (living constitutionalism) or historical meaning (originalism) — the interpretive vacuum would be filled by the sibling readings. Legislative supremacy would be challenged; rights litigation would shift forums and strategies. The entire institutional settlement around 'what the Constitution is' would reorganize.
% FOUNDING_PROBLEM: Post-Civil War and Progressive Era chaos: competing judicial philosophies (Lochner-era substantive due process, natural law adjudication) produced unpredictable, politically contested outcomes. The positivist reading emerged as a disciplinary constraint: bind judges to the text and the amendment process, stabilize the rule of law, depoliticize the courts.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists (Hart, Raz) and process theorists (Ely, Bickel) attest the founding problem is live: judicial discretion without formal criteria remains a threat to democratic legitimacy. Originalists and living constitutionalists attest the problem is dead or transformed: the constraint now serves to entrench status quo power by excluding moral claims that lack supermajority access. No neutral arbiter corroborates; the dispute is the kernel's contest.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).
:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.22) because the constraint's primary operation is coordinative: it gives judges, lawyers, and officials a shared rule of recognition. Suppression (0.35) is higher because the constraint actively excludes moral-validity arguments from legal force — not by persuasion but by institutional rule (stare decisis, hierarchy, Article V gatekeeping). Theater ratio (0.18) is low but rising: formalist rhetoric increasingly covers outcome-driven reasoning. Accessibility collapse (0.72) is high because once you accept the rule of recognition, alternative validity grounds are legally invisible. Resistance (0.28) is moderate: living constitutionalist and originalist readings persist as live competitors.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is a Rope: a genuine coordination solution that prevents judicial tyranny. From the payer seat (marginalized groups), it is a Snare: a formally neutral rule that structurally excludes their claims because they cannot access the supermajority enactment process. The engine computes this divergence from the structural data — the claim (Rope) and the metrics (moderate extraction, real suppression) are authored independently.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda-setters (justices, lower judges) sit near symmetric: they both enforce and are bound by the constraint. Institutional beneficiaries (legislatures, state officials) have d near 0.0 — the constraint subsidizes their authority. Payers (substantive justice claimants, marginalized groups) have d near 1.0 — they bear the exclusion. The identity_locked exit of marginalized groups (trapped in the polity, excluded from Article V) amplifies their effective extraction. Rival interpretive camps (originalists, living constitutionalists) are observers with analytical exit — they contest the constraint but do not pay its extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial unpredictability, Lochner-era chaos) is contested: positivists say it's live; critics say the constraint now extracts by freezing out moral claims that the amendment process will never adopt. The constraint is not a Piton — its coordination function (rule of recognition) is actively maintained and genuinely used. But it is not a pure Rope either — the extraction from trapped claimants is real and asymmetric. The engine's per-seat classification will reveal whether the payer seats experience it as Snare or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_kernel_identity,
    'Is the positivist reading of the US Constitution a distinct constraint from the originalist and living constitutionalist readings, or a measurement variant of a single constraint?',
    'Apply the ε-invariance test: if the three readings produce stably different ε values, different beneficiary/victim structures, and different stakeholder seat classifications when assessed from their own lights, they are distinct constraints. Decompose if ε differs by >0.15 across readings.',
    'If distinct, each reading gets its own constraint story with independent classification. If a single constraint, the kernel is the constraint and readings are merely observer perspectives — the framework would need a kernel-level classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivist_reading_kernel_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (per DP-001 ε-invariance) or observer perspectives on one constraint.').

omega_variable(
    moral_exclusion_as_extraction,
    'Does the positivist constraint''s exclusion of moral-validity claims constitute extraction (transfer from claimants to enactment-holders) or merely the absence of a benefit?',
    'Compare the counterfactual: if moral claims were valid, claimants would win legal protections; enactment-holders would lose legislative freedom. The difference is a transfer. Measure the value of the forgone protections and the legislative freedom retained.',
    'If extraction, ε is higher and payer seats experience Snare/Tangled Rope dynamics. If mere absence, ε is lower and the constraint is closer to pure Rope. Affects classification of marginalized_groups_seeking_recognition and substantive_justice_claimants seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_exclusion_as_extraction, conceptual, 'Whether formalist exclusion of moral claims is an active transfer (extraction) or a neutral baseline.').

omega_variable(
    article_v_access_as_exit_modulation,
    'Does the formal availability of Article V amendment (however difficult) mean marginalized groups have ''constrained'' rather than ''trapped'' exit?',
    'Empirical: has any marginalized group successfully used Article V to secure rights the positivist reading excluded? Count: 0 (13th, 14th, 15th, 19th Amendments were Reconstruction/Progressive supermajorities, not marginalized-group-led). The supermajority threshold structurally requires coalition with the excluding power.',
    'If exit is ''trapped'', d → 1.0 for marginalized groups, χ amplifies to Snare levels. If ''constrained'', d ~0.75, χ lower. Directly affects per-seat classification for the most vulnerable payer seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_v_access_as_exit_modulation, empirical, 'Whether Article V''s theoretical availability modulates exit for groups that cannot realistically access it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(positivist_reading_tr_t1868, us_constitution_text__positivist_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(positivist_reading_tr_t1900, us_constitution_text__positivist_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(positivist_reading_tr_t1937, us_constitution_text__positivist_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(positivist_reading_tr_t1954, us_constitution_text__positivist_reading, theater_ratio, 1954, 0.14).
narrative_ontology:measurement(positivist_reading_tr_t1973, us_constitution_text__positivist_reading, theater_ratio, 1973, 0.16).
narrative_ontology:measurement(positivist_reading_tr_t2000, us_constitution_text__positivist_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(positivist_reading_tr_t2024, us_constitution_text__positivist_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(positivist_reading_be_t1868, us_constitution_text__positivist_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement(positivist_reading_be_t1900, us_constitution_text__positivist_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(positivist_reading_be_t1937, us_constitution_text__positivist_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(positivist_reading_be_t1954, us_constitution_text__positivist_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(positivist_reading_be_t1973, us_constitution_text__positivist_reading, base_extractiveness, 1973, 0.2).
narrative_ontology:measurement(positivist_reading_be_t2000, us_constitution_text__positivist_reading, base_extractiveness, 2000, 0.21).
narrative_ontology:measurement(positivist_reading_be_t2024, us_constitution_text__positivist_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(positivist_reading_su_t1868, us_constitution_text__positivist_reading, suppression_requirement, 1868, 0.25).
narrative_ontology:measurement(positivist_reading_su_t1900, us_constitution_text__positivist_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(positivist_reading_su_t1937, us_constitution_text__positivist_reading, suppression_requirement, 1937, 0.38).
narrative_ontology:measurement(positivist_reading_su_t1954, us_constitution_text__positivist_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(positivist_reading_su_t1973, us_constitution_text__positivist_reading, suppression_requirement, 1973, 0.33).
narrative_ontology:measurement(positivist_reading_su_t2000, us_constitution_text__positivist_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(positivist_reading_su_t2024, us_constitution_text__positivist_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This is the positivist_reading of the us_constitution_text kernel. The originalist_reading instantiates a constraint with ε ≈ 0.18 (lower extraction, historical meaning as coordination), beneficiaries = originalist_judges/scholars + textualist_legislators, victims = living_constitutionalist_claimants. The living_constitutionalist_reading instantiates a constraint with ε ≈ 0.35 (higher extraction, moral evolution as coordination), beneficiaries = progressive_judges/rights_advocates, victims = originalist_legislators/textualist_judges. The three readings form a constraint family: each claims the kernel, each has distinct ε and structural asymmetries. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__positivist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, moderate, 0.7).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, organized, 0.3).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
