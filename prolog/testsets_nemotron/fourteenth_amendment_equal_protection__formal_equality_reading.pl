% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection formal equality doctrine (strict scrutiny for explicit racial classification)
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The formal equality reading of the Equal Protection Clause holds that
 *   explicit state racial classification is presumptively unconstitutional
 *   and survives only under strict scrutiny. This reading emerged from Bakke
 *   (1978) through Croson (1989), Adarand (1995), Parents Involved (2007),
 *   and SFFA (2023), progressively tightening the constraint on
 *   race-conscious state action. The constraint coordinates judicial review
 *   around a single standard (genuine coordination) but asymmetrically
 *   extracts from state corrective programs and the communities they serve
 *   (extraction). It requires active enforcement — courts must police the
 *   boundary between permissible and impermissible classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.52).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection formal equality doctrine (strict scrutiny for explicit racial classification)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'f403c97b-127e-4b45-a8c8-bef0c14b9074').
narrative_ontology:cs_kernel_codification('f403c97b-127e-4b45-a8c8-bef0c14b9074', fixed_text).
narrative_ontology:cs_authority_grounding('f403c97b-127e-4b45-a8c8-bef0c14b9074', lineage).
narrative_ontology:cs_interpretation_layer_present('f403c97b-127e-4b45-a8c8-bef0c14b9074').
narrative_ontology:cs_reading_relation('f403c97b-127e-4b45-a8c8-bef0c14b9074', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('f403c97b-127e-4b45-a8c8-bef0c14b9074', foundational, racial_classification_presumptively_invalid).
narrative_ontology:cs_axiom_status(racial_classification_presumptively_invalid, holdable).
narrative_ontology:cs_axiom_grounding('f403c97b-127e-4b45-a8c8-bef0c14b9074', racial_classification_presumptively_invalid, deontological).
narrative_ontology:cs_axiom('f403c97b-127e-4b45-a8c8-bef0c14b9074', foundational, strict_scrutiny_as_categorical_gate).
narrative_ontology:cs_axiom_status(strict_scrutiny_as_categorical_gate, holdable).
narrative_ontology:cs_axiom_grounding('f403c97b-127e-4b45-a8c8-bef0c14b9074', strict_scrutiny_as_categorical_gate, conventional).
narrative_ontology:cs_axiom('f403c97b-127e-4b45-a8c8-bef0c14b9074', secondary, remediation_not_compelling_interest_per_se).
narrative_ontology:cs_axiom_status(remediation_not_compelling_interest_per_se, holdable).
narrative_ontology:cs_axiom_grounding('f403c97b-127e-4b45-a8c8-bef0c14b9074', remediation_not_compelling_interest_per_se, conventional).
narrative_ontology:cs_reference_frame('f403c97b-127e-4b45-a8c8-bef0c14b9074', bakke_strict_scrutiny_compromise).
narrative_ontology:cs_drift_state('f403c97b-127e-4b45-a8c8-bef0c14b9074', sfia_2023, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f403c97b-127e-4b45-a8c8-bef0c14b9074', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, white_petitioners_opposing_race_conscious_remedies).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism_advocates).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_actors_preferring_formal_neutrality).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_action_programs).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_enforcement_initiatives).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, disparate_impact_remediation_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_actors_preferring_formal_neutrality).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_colorblindness_principle).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_as_default_for_racial_classification).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, government_race_neutrality_as_constitutional_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and applies the formal equality reading through precedent (Bakke, Croson, Adarand, Parents Involved, SFFA). Sets the doctrinal rules that constrain state race-conscious action. Their institutional position is secure; exit from the constraint means doctrinal reversal, not personal consequence.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court_majority_formal_equality, agenda_setter,
    institutional, generational, arbitrage, national).

% Individual plaintiffs (Bakke, Hopwood, Blum-backed petitioners) who challenge affirmative action, voting rights remedies, contracting set-asides. They benefit from the constraint's restriction on race-conscious state action. They can exit by not litigating, but the constraint's existence creates the legal opportunity.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, white_petitioners_opposing_race_conscious_remedies, beneficiary,
    organized, biographical, mobile, national).

% Legal organizations, scholars, and political actors who advance formal equality as the constitutional ideal. They benefit intellectually and politically from the constraint's dominance. Their exit would be abandoning the frame — possible but costly to identity and institutional position.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Universities, employers, government agencies that prefer clear formal-neutrality rules over the complexity of race-conscious compliance. They benefit from doctrinal predictability but pay compliance costs when the constraint forces them to dismantle existing diversity programs. Exit means accepting race-conscious mandates from other jurisdictions or political branches.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_actors_preferring_formal_neutrality, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, institutional_actors_preferring_formal_neutrality, payer).

% Race-conscious admissions, contracting, voting rights remedies, and school desegregation orders that must satisfy strict scrutiny. They bear the cost of the constraint: heightened justification burden, narrow tailoring requirements, periodic re-justification. Exit means abandoning corrective goals or restructuring as formally neutral proxies (often less effective).
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_corrective_action_programs, payer,
    organized, biographical, constrained, national).

% DOJ Civil Rights Division pattern-or-practice suits, EEOC systemic enforcement, VRA Section 5 preclearance (while operative). The formal equality reading treats these as suspect state racial classification. They bear enforcement restriction costs. Exit means shifting to disparate-impact-only frameworks or private enforcement.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_enforcement_initiatives, payer,
    organized, biographical, constrained, national).

% Title VII, Title VI, FHA disparate impact liability regimes. They occupy a contested space: formally neutral on their face but race-conscious in operation and justification. The formal equality reading's logic threatens their legitimacy. Exit means narrowing to intentional-discrimination-only enforcement.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, disparate_impact_remediation_frameworks, payer,
    moderate, biographical, constrained, national).

% Racial minority communities, women, LGBTQ+ populations who experience structural hierarchy but are not formal parties to the doctrinal debate. Their interests are represented indirectly (if at all) through state corrective programs that the constraint restricts. They cannot exit the hierarchy; the constraint treats their situation as pre-constitutional background.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, communities_subject_to_hierarchy, excluded,
    powerless, generational, trapped, national).

% Advance the sibling reading (anti-caste) that Equal Protection requires dismantling hierarchy. They analyze the formal equality constraint from outside its frame, documenting its extraction from corrective programs. Their exit is analytical — they can adopt a different frame but the constraint's legal force remains.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_scholars_and_litigators, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable doctrinal rule for when the state may use racial classifications: only under strict scrutiny for compelling interests. Coordinates judicial review, legislative drafting, and institutional compliance around a single standard.
% TRANSFER_FUNCTION: Moves the burden of justification onto any state actor using explicit racial classification; moves the power to invalidate race-conscious programs from political branches to courts; moves the cost of remediation from state corrective programs to the communities those programs would serve.
% ABSENT_VOICES: Communities subject to structural hierarchy (excluded stakeholders) — they would object to treating existing inequality as pre-constitutional background, but they are not parties to the doctrinal framework that makes that move. Also absent: the original understanding of the Fourteenth Amendment's framers regarding race-conscious remediation, which is contested historical terrain.
% DISAPPEARANCE_RATIONALE: If the formal equality constraint vanished overnight, strict scrutiny would no longer be the default for racial classification; states could pursue race-conscious remediation without heightened judicial barrier; affirmative enforcement programs would expand; the center of gravity in equal protection doctrine would shift to anti-caste principles. The legal landscape would reorganize substantially.
% FOUNDING_PROBLEM: Post-Civil Rights Act backlash: how to constrain state racial classification after Brown and the Civil Rights Movement without foreclosing all remediation. The formal equality reading emerged as a compromise — strict scrutiny as a gate, not a wall — but hardened into a near-categorical bar.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the constraint's own beneficiaries (colorblind constitutionalists) as still live: they argue racial classification remains dangerous regardless of intent. Anti-caste scholars (outside the beneficiary set) corroborate that the problem was never a stable compromise but a contested settlement that has shifted toward categorical prohibition. No consensus exists.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint does not extract from all parties equally — it subsidizes formal neutrality while burdening race-conscious remediation. Suppression (0.52) is moderate: the constraint operates through judicial doctrine, not direct coercion, but its effect is to suppress a whole category of state action. Theater ratio (0.22) is low-moderate: the doctrinal framework has real analytical structure, but the 'compelling interest' inquiry increasingly performs neutrality while producing categorical results. Accessibility collapse (0.61) and resistance (0.58) are both elevated — alternatives (anti-caste reading, intermediate scrutiny frameworks) persist but are marginalized in doctrine; resistance comes from dissenting justices, scholars, and political branches.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Court majority), the constraint is genuine coordination: a stable, neutral principle for a pluralistic society. From the payer seats (corrective programs), it is asymmetric extraction: their constitutional legitimacy is uniquely burdened. From the excluded seat (hierarchy-subjected communities), it is structural suppression: their reality is rendered constitutionally invisible. The engine computes these divergences from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Court majority (agenda_setter, institutional, arbitrage exit) sits at the beneficiary end — it controls the constraint and faces no personal cost. White petitioners and colorblind advocates (beneficiaries, organized, mobile exit) benefit from the constraint's restriction on race-conscious action. Institutional actors preferring formal neutrality (beneficiary/payer, institutional, constrained exit) gain predictability but lose programmatic flexibility. State corrective programs, affirmative enforcement, and disparate impact frameworks (payers, organized/moderate, constrained exit) bear the constraint's costs directly. Communities subject to hierarchy (excluded, powerless, trapped) experience the constraint as backgrounding their structural condition — they cannot exit the hierarchy the constraint treats as pre-constitutional.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (constraining post-Brown racial classification) was live in 1978. By 2023, the problem has mutated: the constraint no longer gates race-conscious remediation — it nearly forecloses it. The mandate (prevent invidious classification) has atrophied into a tool against remediation. This is not pure mandatrophy (the constraint still polices invidious classification) but a drifted mandate where the extraction from corrective action far exceeds the coordination benefit. The formal equality reading now functions as a tangled rope where the coordination story (neutral principle) covers extraction from anti-hierarchy programs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_ambiguity,
    'Does the formal equality reading logically foreclose the anti-caste reading, or do they coexist as competing frameworks within constitutional law?',
    'Track whether a single doctrinal framework (e.g., a Court opinion) can simultaneously apply strict scrutiny to invidious classification AND authorize race-conscious remediation without contradiction. If no opinion sustains both, foreclosure is plausible.',
    'If forecloses, the kernel has a genuine logical split — adopting one reading excludes the other within any coherent framework. If coexists_with, both remain live options for different institutional actors, and the constraint family reflects persistent doctrinal contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the two readings of the Fourteenth Amendment kernel are logically mutually exclusive or simultaneously holdable.').

omega_variable(
    compelling_interest_coherence,
    'Is ''compelling interest'' a coherent doctrinal category, or has it become a performative gate that categorically rejects race-conscious remediation while nominally leaving the door open?',
    'Empirical survey of strict scrutiny outcomes: if race-conscious remediation programs almost never survive strict scrutiny while other compelling interests (national security, etc.) regularly do, the category is performing extraction, not coordination.',
    'If the compelling interest inquiry is performative, the constraint''s theater ratio is understated and its extractiveness is higher — the coordination function is a cover for categorical prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_coherence, empirical, 'Whether the strict scrutiny framework operates as genuine coordination or as a theatrical barrier to remediation.').

omega_variable(
    pre_constitutional_background_assumption,
    'Is the treatment of structural inequality as ''pre-constitutional background'' a defensible constitutional interpretation, or is it an ideological move that extracts from remediation by definitional fiat?',
    'Historical analysis of the Fourteenth Amendment''s framing and ratification: did the framers understand the Amendment to authorize race-conscious remediation? Compare with the anti-caste reading''s historical evidence.',
    'If the background assumption is ideological rather than historical, the constraint''s extractiveness is structural — it defines the constitutional problem in a way that excludes the remedy by construction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pre_constitutional_background_assumption, conceptual, 'Whether the formal equality reading''s treatment of existing hierarchy as constitutionally inert is historically grounded or ideologically constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(four_tr_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(four_tr_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(four_tr_t2009, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2009, 0.21).
narrative_ontology:measurement(four_tr_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2016, 0.23).
narrative_ontology:measurement(four_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.25).
narrative_ontology:measurement(four_be_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1989, 0.32).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(four_be_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(four_be_t2009, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2009, 0.37).
narrative_ontology:measurement(four_be_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2016, 0.39).
narrative_ontology:measurement(four_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(four_su_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1989, 0.48).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(four_su_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(four_su_t2009, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(four_su_t2016, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2016, 0.53).
narrative_ontology:measurement(four_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__formal_equality_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, title_vii_disparate_impact_framework).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, voting_rights_act_section_2_enforcement).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, affirmative_action_doctrine_post_bakke).

% DUAL FORMULATION NOTE:
% This constraint and its sibling anti_caste_reading form a kernel family decomposing the 'Equal Protection' label. This reading (formal equality) has lower ε (0.38) because it constrains state action asymmetrically but provides genuine coordination (stable doctrinal standard). The sibling reading would have higher ε for race-conscious programs but lower ε for hierarchy-subjected communities. They are linked via affects_constraints because the formal equality reading's doctrinal dominance structurally constrains the operational space for anti-caste enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, institutional, 0.15).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, organized, 0.35).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, moderate, 0.55).
constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__formal_equality_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
