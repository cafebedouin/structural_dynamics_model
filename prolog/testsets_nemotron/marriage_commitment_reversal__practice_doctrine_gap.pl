% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Principle: Doctrine Preserved, Practice Suspended
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   Following the 1890 Manifesto, the institutional leadership suspended
 *   public compliance with Section 132 (marriage commitment principle) while
 *   preserving it in doctrine. This created a structural ambiguity: the
 *   principle remained canonically valid but was not enforced in practice,
 *   enabling ~200+ marriages (1890-1904) in jurisdictions where they were
 *   claimed legal. The arrangement functioned as a dual-track legitimation
 *   system — doctrine for the faithful, practice for the state. This reading
 *   treats the gap itself as the constraint: institutional survival purchased
 *   at the cost of membership clarity, with the general membership
 *   experiencing betrayal/bewilderment and fundamentalists experiencing
 *   schism.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda_setter/beneficiary (institutional/identity_locked) — administers the ambiguity, extracts survival value
 *   - general_membership: payer (moderate/constrained) — bears interpretive costs of doctrine-practice gap
 *   - fundamentalist_faction: payer/victim (organized/identity_locked) — experiences schism, bears cost of principled resistance
 *   - pragmatic_membership: beneficiary (moderate/mobile) — gains institutional continuity without doctrinal rupture
 *   - federal_authorities: observer (institutional/analytical) — applies external pressure, monitors compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.72).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.58).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Principle: Doctrine Preserved, Practice Suspended").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '751dec05-9b46-43bb-b623-7d9cb234d63a').
narrative_ontology:cs_kernel_codification('751dec05-9b46-43bb-b623-7d9cb234d63a', fixed_text).
narrative_ontology:cs_authority_grounding('751dec05-9b46-43bb-b623-7d9cb234d63a', lineage).
narrative_ontology:cs_interpretation_layer_present('751dec05-9b46-43bb-b623-7d9cb234d63a').
narrative_ontology:cs_reading_relation('751dec05-9b46-43bb-b623-7d9cb234d63a', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('751dec05-9b46-43bb-b623-7d9cb234d63a', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('751dec05-9b46-43bb-b623-7d9cb234d63a', foundational, doctrine_practice_separation_permissible).
narrative_ontology:cs_axiom_status(doctrine_practice_separation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('751dec05-9b46-43bb-b623-7d9cb234d63a', doctrine_practice_separation_permissible, conventional).
narrative_ontology:cs_axiom('751dec05-9b46-43bb-b623-7d9cb234d63a', foundational, institutional_survival_supersedes_doctrinal_clarity).
narrative_ontology:cs_axiom_status(institutional_survival_supersedes_doctrinal_clarity, holdable).
narrative_ontology:cs_axiom_grounding('751dec05-9b46-43bb-b623-7d9cb234d63a', institutional_survival_supersedes_doctrinal_clarity, instrumental).
narrative_ontology:cs_reference_frame('751dec05-9b46-43bb-b623-7d9cb234d63a', canonical_section_132_binding_practice).
narrative_ontology:cs_drift_state('751dec05-9b46-43bb-b623-7d9cb234d63a', post_1890_manifesto, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('751dec05-9b46-43bb-b623-7d9cb234d63a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, pragmatic_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_faction).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival_via_ambiguity).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, dual_track_legitimation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine-practice gap: publicly affirms Section 132 while privately allowing practice suspension. Extracts institutional survival and flexibility from the ambiguity. Identity-locked — the leadership's legitimacy is fused with the institution's continuity; exit would dissolve their role.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, beneficiary).

% Experiences the doctrine-practice gap as betrayal and bewilderment. Taught the principle is eternal, observes leadership suspending it. Bears interpretive labor costs: reconciling canon with practice, managing social friction. Exit is constrained — community, family, and identity ties make leaving costly but not impossible.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, constrained, national).

% Experiences the gap as schism-inducing betrayal. Maintains that Section 132 is binding practice, not merely doctrine. Bears extreme costs: excommunication, social ostracization, loss of community, formation of breakaway groups. Identity-locked — their self-concept is constituted through fidelity to the principle; exit from the principle is exit from self.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_faction, payer,
    organized, biographical, identity_locked, national).

% Gains institutional continuity without doctrinal rupture. Accepts the ambiguity as pragmatic adaptation. Benefits from statehood, reduced persecution, institutional stability. Mobile exit — can leave for other denominations or secular life with moderate cost; identity not fused to the principle.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, pragmatic_membership, beneficiary,
    moderate, biographical, mobile, national).

% Applies external coercion (Edmunds Act, disfranchisement, property seizure) that creates the pressure for practice suspension. Monitors compliance with 1890 Manifesto and 1904 Second Manifesto. Does not extract from the ambiguity — imposes the constraint that makes ambiguity adaptive for the institution.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional survival under existential federal threat by creating a dual-track system: doctrine preserved for internal legitimacy, practice suspended for external compliance. Solves the coordination problem of maintaining institutional continuity when the core practice is criminalized.
% TRANSFER_FUNCTION: Moves membership clarity and interpretive certainty from general membership and fundamentalists to institutional leadership. Leadership gains flexibility and survival; membership pays in betrayal/bewilderment (general) and schism (fundamentalists). ~200+ marriages (1890-1904) represent the extraction of practice from doctrine's shadow.
% ABSENT_VOICES: Women in plural marriages — their experience of the doctrine-practice gap (protection withdrawn, status ambiguous, children's legitimacy questioned) is structurally excluded from the leadership's calculus. Dissenting apostles (e.g., Matthias Cowley, John W. Taylor) — their objections to post-Manifesto marriages were overruled; they were not in the room when the ambiguity was administered.
% DISAPPEARANCE_RATIONALE: If the doctrine-practice gap vanished overnight (either full enforcement or full doctrinal revision), the institutional structure would fundamentally rearrange: full enforcement invites federal destruction; full revision triggers fundamentalist schism and legitimacy crisis. The ambiguity IS the load-bearing structure.
% FOUNDING_PROBLEM: Federal criminalization of the marriage commitment principle (Section 132) threatened institutional destruction: property seizure, leadership imprisonment, disfranchisement of membership, loss of territorial governance.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the federal threat remained live through 1904 (Smoot hearings, continued prosecution). Federal authorities and independent historians attest the existential threat substantially receded after 1896 statehood and 1898 amnesty — the ambiguity persisted because it served institutional flexibility, not because the founding problem remained acute. The Smoot hearings (1904-1907) corroborate the shifted function: the Senate investigated whether the ambiguity was a genuine cessation or a cover for continued practice.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.72) from the ambiguity itself: the institution extracts interpretive labor and loyalty from membership while avoiding the costs of either full enforcement (federal destruction) or full doctrinal revision (schism). Theater ratio (0.45) is substantial: public performance of doctrine preservation masks the suspension of practice. Suppression (0.58) is moderate — federal coercion provides the enforcement backbone, but internal suppression of dissent (excommunication of fundamentalists) is real. Accessibility collapse (0.62) reflects that alternatives (exit, schism, doctrinal revision) are structurally narrowed but not eliminated. Resistance (0.38) is present but fragmented: fundamentalist resistance is high but minority; general membership resistance is diffuse bewilderment rather than organized opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits at d≈0.15 (beneficiary — ambiguity enables survival and flexibility). Pragmatic membership at d≈0.35 (mild beneficiary — gains continuity, pays some interpretive cost). General membership at d≈0.65 (target — bears betrayal/bewilderment, constrained exit). Fundamentalist faction at d≈0.85 (high target — identity_locked, schism cost extreme). Federal authorities are analytical observers (d≈0.5 symmetric) — they impose the external constraint but do not extract from the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal destruction threat) was live in 1890. By 1904, with statehood achieved and federal pressure shifted, the mandate had atrophied — yet the ambiguity persisted because it now served institutional flexibility. This is mandatrophy: the constraint outlived its founding justification and became a tool for dual-track legitimation. The claim of tangled_rope captures the genuine coordination function (institutional survival under threat) AND the asymmetric extraction (membership clarity sacrificed for leadership flexibility).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the practice-doctrine gap a distinct structural constraint, or merely a descriptive artifact of the other two readings?',
    'If the gap produces independent extractive dynamics (200+ marriages, membership betrayal, fundamentalist schism) not reducible to either revelation or coercion alone, it is a distinct constraint with its own ε.',
    'If distinct, this reading has independent classification (tangled_rope) and its own stakeholder structure. If artifact, it collapses into the other readings'' classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the practice-doctrine gap is a structurally independent constraint').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (federal coercion) or internalized (identity-fused loyalty preventing exit)?',
    'Post-exit suppression trajectory: if former fundamentalists and dissidents continue to experience interpretive coercion after schism/exit, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s extraction persists beyond its formal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the membership').

omega_variable(
    beneficiary_boundary,
    'Does ''pragmatic_membership'' genuinely benefit, or is it a constructed category that masks universal extraction?',
    'Compare exit behaviors: if pragmatic members show arbitrage-grade exit when ambiguity resolves (1904 Second Manifesto), they were beneficiaries. If they resist resolution, they were identity-locked targets.',
    'If pragmatic membership are actually targets, the constraint is snare (no genuine beneficiaries). If genuine beneficiaries exist, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary, empirical, 'Whether the pragmatic membership category represents real beneficiaries or masked extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(marr_tr_t1885, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1885, 0.22).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.38).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1895, 0.44).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1900, 0.48).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.42).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1910, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(marr_be_t1885, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1885, 0.42).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1895, 0.71).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1900, 0.74).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.68).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1910, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1880, 0.45).
narrative_ontology:measurement(marr_su_t1885, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1885, 0.52).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1895, 0.62).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.55).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1910, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is the practice-doctrine-gap reading of the marriage_commitment_reversal kernel. The endogenous_reinterpretation_reading frames the reversal as internal divine revelation (Woodruff's vision); the exogenous_override_reading frames it as federal coercion without doctrinal change. This reading isolates the structural gap between doctrine and practice as the constraint itself — the ambiguity that enables dual-track legitimation and ~200 marriages 1890-1904. The three readings have different ε values and different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, organized, 0.85).
constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, moderate, 0.65).
constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
