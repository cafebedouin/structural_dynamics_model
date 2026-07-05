% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection as Strict Anti-Classification (Colorblind Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint isolates the strict anti-classification reading of the
 *   equal protection commitment — the position traced to Justice Harlan's
 *   dissent in Plessy v. Ferguson, that the Constitution is 'color-blind' and
 *   forbids any state use of racial classification. Applied to contemporary
 *   admissions and allocation disputes, this reading treats race-conscious
 *   remedial or diversity-oriented programs as themselves the constitutional
 *   harm, regardless of remedial intent. This is a distinct constraint from
 *   the remedial reading (which permits race-conscious dismantling of caste
 *   structures) and the diversity reading (which permits race as one factor
 *   for educational diversity) — the three readings have different
 *   beneficiary/victim structures and different ε values and must not be
 *   merged or averaged. Under this reading specifically, denied Asian and
 *   white applicants enter the beneficiary set and race-conscious programs
 *   and their intended beneficiaries enter the victim set.
 *
 * KEY AGENTS:
 *   - denied_asian_applicants: primary beneficiary (moderate/constrained) — gains cause of action and admission-weight redistribution
 *   - denied_white_applicants: primary beneficiary (moderate/constrained) — same structural position
 *   - colorblind_legal_movement: agenda_setter (organized/mobile) — administers and expands the doctrine
 *   - race_conscious_admissions_programs: primary payer (institutional/constrained) — loses operative legal tool
 *   - underrepresented_minority_applicants: primary payer (powerless/trapped) — bears diffuse admission-rate cost
 *   - civil_rights_historians_and_scholars: excluded — documents caste-effect record treated as doctrinally irrelevant
 *   - reviewing_courts: analytical observer — adjudicates between competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection as Strict Anti-Classification (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'e5cfb7e4-d7f1-453f-a41c-880167d61ef5').
narrative_ontology:cs_kernel_codification('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', fixed_text).
narrative_ontology:cs_authority_grounding('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', lineage).
narrative_ontology:cs_interpretation_layer_present('e5cfb7e4-d7f1-453f-a41c-880167d61ef5').
narrative_ontology:cs_reading_relation('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', foundational, classification_itself_is_the_constitutional_injury).
narrative_ontology:cs_axiom_status(classification_itself_is_the_constitutional_injury, holdable).
narrative_ontology:cs_axiom_grounding('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', classification_itself_is_the_constitutional_injury, deontological).
narrative_ontology:cs_axiom('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', foundational, purpose_of_classification_is_constitutionally_irrelevant).
narrative_ontology:cs_axiom_status(purpose_of_classification_is_constitutionally_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', purpose_of_classification_is_constitutionally_irrelevant, conventional).
narrative_ontology:cs_reference_frame('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', harlan_dissent_categorical_prohibition).
narrative_ontology:cs_drift_state('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', post_affirmative_action_litigation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e5cfb7e4-d7f1-453f-a41c-880167d61ef5', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, denied_asian_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, denied_white_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, diversity_officers_and_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who allege they were denied admission to selective institutions because race-conscious policies weighted their applications differently than they would have been weighted under a strict colorblind rule. The colorblind reading gives them a cause of action and, if adopted, forecloses the specific mechanism they attribute their denial to.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, denied_asian_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Applicants who similarly allege injury from race-conscious weighting. Under the colorblind reading their claim is structurally identical to any other racial-classification claim regardless of which race is disadvantaged by the specific program.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, denied_white_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Litigators, legal foundations, and jurists who advance and administer the anti-classification principle through strategic litigation, amicus strategy, and judicial appointment advocacy. They set the doctrinal agenda this reading operationalizes and benefit institutionally (funding, precedent, prestige) from its adoption and expansion.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_legal_movement, agenda_setter,
    organized, generational, mobile, national).

% University and public-agency admissions offices that used race as one factor among many. Under this reading their entire operating framework becomes presumptively unconstitutional; they must redesign intake criteria, absorb litigation risk, and forfeit the tool they used to pursue compositional goals. Exit means either abandoning diversity goals or engaging in costly proxy-variable redesign.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs, payer,
    institutional, biographical, constrained, national).

% Applicants from groups previously considered as one factor favoring admission under race-conscious programs. Under the colorblind reading, the tool that accounted for the ongoing effects of historical and structural disadvantage is removed; they bear a diffuse but real cost in reduced admission rates at selective institutions, with no individualized remedy available.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants, payer,
    powerless, biographical, trapped, national).

% Institutional staff whose professional function was built around administering race-conscious criteria. The colorblind reading eliminates the legal basis for their core function, forcing role redefinition or elimination, and exposes them to personal liability risk in some enforcement regimes.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_officers_and_administrators, payer,
    moderate, biographical, constrained, national).

% Scholars who document the historical function of race-conscious remedy as counteracting durable caste effects. Their empirical record of ongoing structural disadvantage is treated by this reading as constitutionally irrelevant once any racial classification is used, regardless of remedial purpose — their voice is doctrinally excluded rather than substantively rebutted.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, civil_rights_historians_and_scholars, excluded,
    moderate, civilizational, analytical, national).

% Federal and state courts that adjudicate whether a given program's use of race triggers strict scrutiny and whether the anti-classification principle or a competing reading controls the outcome. They apply whichever reading currently commands a doctrinal majority.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, reviewing_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule — no racial classification by the state, ever — that avoids case-by-case litigation over which uses of race are benign versus invidious, and forecloses the risk that any race-based sorting mechanism (once legitimated for remedial purposes) could later be repurposed for subordination.
% TRANSFER_FUNCTION: Removes the specific admissions or allocation weight that race-conscious programs assigned to underrepresented applicants and reallocates the resulting seats/positions toward the applicant pool that would have prevailed under race-blind criteria — disproportionately Asian and white applicants in the studied institutional contexts — while removing race-conscious administrative capacity from institutions.
% ABSENT_VOICES: Communities whose current underrepresentation is attributed to durable structural effects of historical exclusion are not heard as parties with a continuing claim under this reading — their disadvantage is treated as either resolved or constitutionally non-cognizable once framed as requiring a racial classification to remedy. Civil rights historians documenting ongoing caste effects are cited by rival readings but structurally irrelevant to this one's core test.
% DISAPPEARANCE_RATIONALE: If the colorblind reading were displaced by a rival reading, institutions could resume or expand race-conscious admissions and allocation criteria; litigation strategies built around anti-classification claims would lose their doctrinal footing; the current wave of admissions lawsuits would need to be reframed or would collapse; downstream statutory and administrative-law analogues (federal contracting set-asides, redistricting doctrine) that lean on the anti-classification principle would also be destabilized.
% FOUNDING_PROBLEM: Built to prevent the state from ever again constructing or entrenching a racial caste system by using race as a sorting criterion — Harlan's dissent in Plessy targeted the Court's willingness to let states classify by race for any purpose, arguing the Constitution 'neither knows nor tolerates classes among citizens.'
% FOUNDING_PROBLEM_CORROBORATION: The colorblind legal movement attests the founding problem is fully live and generalizable to any racial classification, benign or invidious. Civil rights historians and several sitting and former jurists outside that movement attest that Harlan's original target was subordinating classification specifically, and that applying the same rule to remedial classification inverts the founding purpose — that dissenting corroboration comes from outside the reading's own beneficiary set and is documented in dissenting opinions and historical scholarship, not merely asserted by the parties who would lose from the reading's adoption.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set moderate-high (0.42 at 2024) per the expected structural delta: under this reading the classification itself is treated as the harm, meaning any race-conscious program — however carefully tailored — is presumptively suspect, which transfers real institutional capacity and admission outcomes away from race-conscious programs and their intended beneficiaries. Suppression (0.48) reflects the doctrine's use of strict scrutiny as an active disabling mechanism against alternative institutional designs, not merely a background rule. Theater ratio (0.28) is moderate because much of the doctrine's operation is genuinely dispositive (programs are actually struck down, not merely performatively reviewed), but some compliance activity (proxy-variable redesign, race-neutral-in-form programs with race-conscious effects) is adopted precisely to perform colorblindness while preserving some compositional goals. Accessibility collapse (0.4) and resistance (0.62) reflect that this is a contested doctrinal reading, not settled natural law: real alternatives (the remedial and diversity readings) persist as live doctrine in other periods and jurisdictions, and resistance from affected institutions and communities is substantial and ongoing.
 *
 * PERSPECTIVAL GAP:
 *   From the colorblind legal movement's seat, this reading is a principled, minimal, rule-like coordination mechanism (close to a rope) — a bright-line rule preventing any future racial hierarchy. From the seat of race-conscious institutions and underrepresented applicants, the same doctrine operates as an actively enforced extraction of a policy tool developed specifically to counteract measurable, persistent disadvantage — closer to tangled_rope or snare depending on how one weighs the coordination function against the asymmetric cost. The engine's per-seat computation is expected to diverge sharply between the agenda_setter/beneficiary seats and the payer seats; this divergence is the analytical payload, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Denied Asian and white applicants are structural beneficiaries of this specific reading — the doctrine directly vindicates their claimed injury and reallocates admission weight toward them. The colorblind legal movement is the agenda-setting beneficiary, gaining doctrinal victories, litigation funding, and precedent. Race-conscious admissions programs and diversity administrators are direct payers — their operative tool and professional function are eliminated. Underrepresented minority applicants are payers with the least mobility (trapped exit) because the removed mechanism was specifically designed to counteract disadvantages they cannot individually litigate around.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status divergence is central here: the colorblind movement treats the founding problem (preventing state-constructed racial caste) as still fully live and best solved by prohibiting ALL racial classification. Outside corroborators (dissenting jurists, historians) attest that Harlan's target was specifically subordinating classification, and that the problem the doctrine was built to solve (dismantling caste) is arguably still live in a form this reading no longer addresses — meaning the doctrine may have drifted from remedying caste to prohibiting remedy, which is exactly the founding-problem status the mismatch-only consumer is built to flag (status=contested + disappearance=world_rearranges signals a live capture-or-genealogy-drift question, not a settled mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harlan_original_scope_ambiguity,
    'Did Harlan''s ''color-blind'' dissent target all state racial classification categorically, or specifically classification used to entrench a subordinating caste system?',
    'Close historical-textual analysis of the full Plessy dissent alongside Harlan''s other opinions and the immediate post-Reconstruction legal context in which it was written; comparison with how contemporaneous jurists and advocates read the opinion.',
    'If Harlan''s target was subordination specifically, applying the colorblind rule to remedial race-conscious measures inverts rather than extends the original commitment — supporting the reading that this constraint is a constructed doctrinal extension rather than a direct descendant. If the categorical reading is correct, the extension is faithful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harlan_original_scope_ambiguity, conceptual, 'Whether the anti-classification principle is a faithful extension of or an inversion of its founding source.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does this reading currently command doctrinal ascendancy over the remedial and diversity readings — genuine constitutional discovery, or the relative organizational and litigation capacity of the colorblind legal movement compared to institutions defending race-conscious programs?',
    'Comparative institutional analysis of litigation funding, amicus mobilization, and judicial appointment strategy across the three reading-communities over the interval; tracking doctrinal shifts against changes in court composition versus changes in underlying social fact.',
    'If ascendancy tracks organizational capacity rather than changed underlying facts about caste conditions, this reading''s current dominance is better modeled as a captured doctrinal outcome than a settled constitutional truth — raising the effective ε further. If it tracks genuinely changed social conditions (e.g., caste effects meaningfully diminished), the reading''s expansion is more defensible on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading''s ascendancy reflects genuine doctrinal discovery or asymmetric organizational capacity between reading-communities.').

omega_variable(
    classification_as_harm_versus_effect_as_harm,
    'Is the constitutional harm the act of racial classification itself (this reading''s premise), or the disparate effect/subordination that classification produces or remedies (the siblings'' premise)?',
    'No empirical resolution is possible — this is the doctrinal fork itself. Resolution mechanism is judicial/political: which premise a controlling majority of the Supreme Court adopts in a given era.',
    'This premise IS the axis distinguishing all three sibling readings; whichever premise controls determines which of the three constraint files describes the operative law at a given time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_as_harm_versus_effect_as_harm, preference, 'The foundational premise fork between the colorblind, remedial, and diversity readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.14).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_commitment__colorblind_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.21).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_commitment__colorblind_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__colorblind_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1995, equal_protection_commitment__colorblind_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.33).
narrative_ontology:measurement(equa_be_t2016, equal_protection_commitment__colorblind_reading, base_extractiveness, 2016, 0.37).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__colorblind_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(equa_su_t1995, equal_protection_commitment__colorblind_reading, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(equa_su_t2016, equal_protection_commitment__colorblind_reading, suppression_requirement, 2016, 0.43).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__colorblind_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint, equal_protection_commitment__remedial_reading, and equal_protection_commitment__diversity_reading are three readings of a single contested kernel (the equal protection commitment). Each reading has a distinct beneficiary/victim structure and a distinct ε: the colorblind reading (this file) treats classification itself as the harm (ε ~0.42, moderate-high, victims are race-conscious programs and their intended beneficiaries); the remedial reading treats caste perpetuation as the harm and permits race-conscious dismantling (expected lower ε, different victim set); the diversity reading treats race as a permissible factor among many for a compelling educational interest (expected intermediate ε, narrower victim set limited to strict-numerical-quota contexts). They are linked, not merged, per the ε-invariance principle — do not average their metrics or treat them as the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
