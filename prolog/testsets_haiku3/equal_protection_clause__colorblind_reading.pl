% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause (Colorblind Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The colorblind equal-protection reading asserts that the 14th Amendment
 *   forbids ALL governmental racial classifications, treating individuals as
 *   constitutional subjects whose rights are independent of group membership.
 *   Under this reading, any race-conscious policy — whether remedial,
 *   diversity-seeking, or otherwise — violates equal protection because it
 *   sorts individuals by race. The constraint is claimed as a ROPE (a
 *   coordination rule establishing uniform non-discrimination across all
 *   governmental actors) while the authored extractiveness is very low (0.12)
 *   and suppression minimal (0.08) because the constraint's operative force
 *   is primarily formal rule application, not coercion of a contested
 *   structure. Theater is low-to-moderate (0.18) because the reading's
 *   enforcement is straightforward judicial review against statutory text,
 *   though interpretive contests over originalism versus living
 *   constitutionalism add some performative dimension. The constraint is one
 *   reading of a contested kernel; the remedial and diversity readings
 *   instantiate different constraints from the same constitutional text.
 *
 * KEY AGENTS:
 *   - Individuals as rights-bearers: the beneficiary group — all persons regardless of race, treated as constitutional subjects whose dignity is protected by colorblind governance
 *   - Government actors (federal, state, local): the agenda-setters who enforce colorblindness by ceasing race-conscious policy
 *   - Originalist jurists: institutional agenda-setters who interpret the 14th Amendment to forbid all racial classifications
 *   - Race-conscious program operators: the payers bearing compliance burden and loss of policy flexibility
 *   - Historically subordinated groups: identity-locked payers excluded from the beneficiary frame by the reading's core premises
 *   - Living constitutionalist judges: excluded voices who would interpret equal protection to permit race-consciousness under compelling-interest tests
 *   - Constitutional scholars: observers analyzing the reading's foundations and consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.08).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '09efba71-8e90-4b57-a7a1-c9d9302a4ba5').
narrative_ontology:cs_kernel_codification('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', fixed_text).
narrative_ontology:cs_authority_grounding('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', lineage).
narrative_ontology:cs_interpretation_layer_present('09efba71-8e90-4b57-a7a1-c9d9302a4ba5').
narrative_ontology:cs_reading_relation('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', foundational, race_consciousness_categorically_impermissible).
narrative_ontology:cs_axiom_status(race_consciousness_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', race_consciousness_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', foundational, individual_rights_independent_of_group_membership).
narrative_ontology:cs_axiom_status(individual_rights_independent_of_group_membership, holdable).
narrative_ontology:cs_axiom_grounding('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', individual_rights_independent_of_group_membership, deontological).
narrative_ontology:cs_reference_frame('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', colorblind_equal_protection).
narrative_ontology:cs_drift_state('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', contemporary_post_dobbs, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('09efba71-8e90-4b57-a7a1-c9d9302a4ba5', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individuals_as_rights_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, race_conscious_program_operators).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, historically_subordinated_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a constitutional rule that bars governmental sorting by race. They stand as rights-bearers whose dignity is protected by colorblind governance. The reading guarantees they cannot be reduced to group categories in the distribution of governmental benefits or burdens.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individuals_as_rights_bearers, beneficiary,
    organized, generational, mobile, national).

% Administer colorblindness by ceasing race-conscious policies across admissions, hiring, contracting, and resource allocation. They operate under judicial constraint to apply the colorblind rule. Compliance is clear and bright-line; deviation risks judicial invalidation and reversal.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, government_actors, agenda_setter,
    institutional, generational, constrained, national).

% Interpret and enforce the colorblind reading through judicial opinions and constitutional doctrine. They assert that the 14th Amendment's original public meaning forbade all governmental racial classifications. They defend this reading against living-constitutionalist challenge through legal argument, textual analysis, and historical scholarship.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, originalist_jurists, agenda_setter,
    institutional, generational, analytical, national).

% Designers and administrators of affirmative action, diversity admissions, targeted contracting, and remedial programs must restructure or suspend those programs. They bear the compliance cost and the loss of policy tools. Exiting the constraint means accepting judicial invalidation; adapting to it means redesigning around race-neutral proxies or accepting reduced program effectiveness.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, race_conscious_program_operators, payer,
    organized, generational, constrained, national).

% Cannot access race-conscious remedial or diversity programs because the colorblind reading treats race-consciousness as unconstitutional. They are locked into racial identity but denied the remedy of group-conscious protection. Their interests and testimony are excluded from the reading's beneficiary frame by the reading's core premise.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_groups, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, historically_subordinated_groups, excluded).

% Would interpret equal protection to permit or require race-consciousness under appropriate compelling-interest or remedial tests. They are structurally excluded from the colorblind reading's authority hierarchy — their interpretive claims are treated as error or misdirection by the originalist majority. Their dissents and scholarly work remain on record but do not shape doctrine under the current constitutional regime.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, living_constitutionalist_judges, excluded,
    institutional, generational, trapped, national).

% Advocate for race-conscious remediation and diversity as necessary responses to ongoing racial inequality. They are excluded from the colorblind reading's normative framework, which treats their policy preferences as unconstitutional. They challenge the reading through litigation and public advocacy but operate within a constitutional regime that denies their preferred tools.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_organizations, excluded,
    organized, generational, constrained, national).

% Analyze the colorblind reading's historical foundations, doctrinal implications, and consequences. They produce scholarship that frames judicial reasoning and public debate but do not directly administer the constraint. They serve as intellectual referees in the kernel contest.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, constitutional_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, originalist_jurists).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, judicially enforceable principle binding all governmental actors: race cannot be a permissible classification basis. Solves the coordination problem of what equal protection means in practice — a bright-line rule that eliminates case-by-case interest-balancing and produces predictability across jurisdictions.
% TRANSFER_FUNCTION: Transfers authority over remedial and diversity policy-making from government agencies and educators to courts applying colorblind doctrine. It transfers also the presumption of validity: colorblind policies are presumed constitutional; race-conscious policies are presumed unconstitutional. The flow of authority moves from policy discretion to doctrinal constraint.
% ABSENT_VOICES: Living constitutionalist judges and civil-rights advocates who would interpret equal protection to permit or require race-consciousness are structurally excluded. They have a stake in the constraint's interpretation but are not party to the colorblind reading's authority structure. Historical testimony from persons subjected to slavery and its aftermath is not part of the reading's interpretive method, even though the 14th Amendment was enacted to address slavery's aftermath.
% DISAPPEARANCE_RATIONALE: If the colorblind equal-protection constraint vanished, government would resume authority to design race-conscious remedial, diversity, and targeted policies. Universities would restore affirmative action. Contracting agencies would resume targeted vendor diversity. Legislatures would enact race-conscious anti-poverty and education programs. The policy landscape would reorganize around permitted race-consciousness, and resource allocation in education, employment, and public contracting would shift substantially.
% FOUNDING_PROBLEM: The 14th Amendment (enacted 1868) was enacted to abolish slavery and establish equal citizenship. The colorblind reading asserts that the clause's core meaning forbids governmental racial sorting — treating race-consciousness itself as the constitutional wrong that equality protects against.
% FOUNDING_PROBLEM_CORROBORATION: The current Supreme Court majority (as of 2023, post-Dobbs) and originalist constitutional scholars (Justice Thomas, Justice Scalia's jurisprudence, originalist law professors) attest the founding problem is live and colorblindness is the correct remedy. The remedial-reading tradition (civil-rights scholars, Reconstruction historians, living constitutionalists, critical race theorists) attests the founding problem was substantive equality for formerly enslaved persons and their descendants — a goal colorblindness alone cannot achieve. Independent historical scholarship on the Reconstruction Congress's intent is divided but substantial evidence suggests the framers intended race-conscious remediation, not mere non-discrimination.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The colorblind reading produces very low extractiveness because it operates as a formal, bright-line rule. Governmental actors apply it directly: race cannot be a classification basis. No ambiguity, no coercive extraction from beneficiaries, no complex enforcement machinery beyond judicial review. Accessibility collapse is very high (0.92) because once the reading is understood — that race is categorically foreclosed as a governmental sorting principle — alternatives (permitting race-consciousness, balancing interests, considering group context) are fully collapsed. Resistance is substantial (0.71) because multiple institutional constituencies object: civil-rights advocates arguing remediation requires race-consciousness; educational institutions defending diversity; scholars contesting originalism's interpretive claims. Theater is low because rule application is straightforward, though it rises slightly over the interval as the constraint faces intensifying resistance and originalist jurists invest in explaining and defending the reading against living-constitutionalist challenge. The measurement series shows extractiveness stable at the low end — the constraint's burden on race-conscious program operators is real but does not grow over the interval; the theatrical dimension grows modestly as interpretive contention intensifies but remains a minor component of the enforcement picture. All measurements are authored on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   Originalist jurists and supporters of colorblind equal protection perceive the constraint as a clear, protective rule against governmental discrimination — a genuine rope. Race-conscious program operators and civil-rights advocates perceive it as a constraint that forecloses remedial tools and freezes historical inequality into formalism — a constraint that extracts from those seeking group remedies. Living constitutionalists perceive the constraint as a misreading of the amendment that blocks necessary tools for substantive equality. The engine computes per-seat types from the structural data; the divergence between the colorblind reading's claim (rope) and how targeted seats experience it (as foreclosure) is exactly what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals-as-rights-bearers sit at the beneficiary end (d near 0.0) — the reading guarantees they cannot be sorted by race, treating them as equal constitutional subjects. Government actors sit at symmetric directionality (d ≈ 0.5) — they benefit from a clear rule eliminating discretionary judgment, but they also bear a constraint on their policy tools. Originalist jurists benefit from the reading as it validates their interpretive method; they sit near beneficiary (low d). Race-conscious program operators are the targets (d near 1.0) — the reading forecloses their policy choice. Historically subordinated groups are trapped payers (d ≈ 0.95) — identity-locked into racial identity but unable to access group-conscious remedies because the reading denies race-consciousness as a legitimate tool. The asymmetry is structural: the reading's beneficiary frame (individuals independent of group membership) systematically denies voice to those whose historical group subordination might justify race-consciousness.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading does not present a mandatrophy case. The founding problem (preventing governmental use of race as a sorting principle) remains live, and the reading is actively defended and enforced by the current constitutional majority. The constraint's founding justification has not been abandoned. However, the remedial reading articulates a contention: the founding problem of substantive equality — remedying slavery and its aftermath — cannot be solved by colorblindness alone. This is a kernel contest, not mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_vs_purposive_interpretation,
    'Is equal protection''s core meaning exhausted by the text''s prohibition on racial classifications (formalism), or must interpretation accommodate the Reconstruction-era purpose of remedying slavery and subordination (purposivism)?',
    'Historical scholarship on 14th Amendment drafting intent, comparative analysis of equal-protection doctrine across nations with similar texts but different purposes, and longitudinal study of whether textual colorblindness produces substantive equality or entrenchment.',
    'Textual primacy (originalist reading) supports the colorblind constraint; purposive reading (remedial constraint) would support race-conscious remediation as constitutionally required. The choice between them determines the reading''s legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_vs_purposive_interpretation, conceptual, 'The interpretive method that determines whether colorblindness or remediation is the correct equal protection principle.').

omega_variable(
    formal_vs_substantive_equality,
    'Is formal equality (treating all alike regardless of circumstance) sufficient for constitutional justice, or does substantive equality (addressing actual disparities and their causes) also matter constitutionally?',
    'Empirical observation of whether colorblind policies reduce or entrench racial disparities in education, employment, wealth, and incarceration. Doctrinal analysis of whether the 14th Amendment is exhausted by non-discrimination or includes affirmative equality obligations.',
    'If formal equality suffices, the colorblind reading is constitutionally complete. If substantive equality is required and colorblindness fails to achieve it, the colorblind reading is misdirected and the remedial reading becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, empirical, 'Whether formal colorblindness produces substantive equality or whether remediation is necessary.').

omega_variable(
    individual_vs_group_rights_frames,
    'Are constitutional rights and protections best understood as protecting individuals independent of group membership (individual-rights frame), or must constitutional protection account for the group harms and group remedies required by historical injustice (group-solidarity frame)?',
    'Analysis of the structure and purpose of civil rights law; examination of whether remedy for systematic group harm can be individual-only; historical and normative interrogation of whether individual rights can be fully protected when group-based subordination persists.',
    'The colorblind reading is embedded in the individual-rights frame — race-consciousness violates it because it treats individuals as group members. The remedial reading assumes group solidarity matters constitutionally. The two frames are conceptually distinct; choosing between them is not empirical but foundational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_vs_group_rights_frames, conceptual, 'The foundational framing of constitutional rights as individual or group-indexed.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the colorblind reading logically foreclose the remedial and diversity readings within a single constitutional framework, or do all three remain live interpretive options across a divided polity?',
    'Legal-philosophical analysis: if colorblindness as a constitutional first principle directly contradicts remediation as a first principle, one forecloses the other. If they can coexist as different readings held by different constitutional constituencies, they coexist.',
    'The kernel contest is whether these readings are in logical contradiction or in empirical disagreement. Foreclosure would mean the colorblind reading has eliminated the others from live interpretation; coexistence means they remain competitors. This determines the cs_structure.reading_relations field.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the colorblind reading forecloses or coexists with the remedial and diversity readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__colorblind_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(equa_tr_t5, equal_protection_clause__colorblind_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(equa_tr_t10, equal_protection_clause__colorblind_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t15, equal_protection_clause__colorblind_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(equa_tr_t20, equal_protection_clause__colorblind_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(equa_tr_t25, equal_protection_clause__colorblind_reading, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__colorblind_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(equa_be_t5, equal_protection_clause__colorblind_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(equa_be_t10, equal_protection_clause__colorblind_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(equa_be_t15, equal_protection_clause__colorblind_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(equa_be_t20, equal_protection_clause__colorblind_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(equa_be_t25, equal_protection_clause__colorblind_reading, base_extractiveness, 25, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__colorblind_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(equa_su_t5, equal_protection_clause__colorblind_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement(equa_su_t10, equal_protection_clause__colorblind_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(equa_su_t15, equal_protection_clause__colorblind_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(equa_su_t20, equal_protection_clause__colorblind_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(equa_su_t25, equal_protection_clause__colorblind_reading, suppression_requirement, 25, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.06).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel decomposes into three constraint stories, one per reading. All three share the constitutional text but instantiate different constraints with different ε values, beneficiary/victim structures, and types. The colorblind reading treats race-consciousness as categorically impermissible; the remedial reading treats it as required for substantive equality; the diversity reading treats it as conditionally permissible. They are linked via network.affects_constraints because each reading's interpretive claim influences the others — a colorblind precedent weakens remedial authority, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
