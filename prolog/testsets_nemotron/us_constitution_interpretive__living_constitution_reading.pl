% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Interpretive Reading
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint story captures the 'living constitution' reading of the
 *   U.S. Constitution — the view that constitutional meaning evolves through
 *   judicial interpretation to reflect contemporary values and conditions,
 *   rather than being fixed at ratification. The reading emerged gradually
 *   from the Marshall Court through the Lochner era, the New Deal revolution,
 *   the Warren Court rights expansion, and the substantive due process
 *   lineage (Griswold, Roe, Lawrence, Obergefell). It is contested by
 *   originalism (meaning fixed at ratification) and popular constitutionalism
 *   (meaning shaped by democratic politics, not courts). This story authors
 *   ONLY the living constitution reading as a clean, epsilon-invariant
 *   constraint per Rule 1.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.68).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.55).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Interpretive Reading").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '7c4a7482-af91-41bf-a137-ac7c329577c3').
narrative_ontology:cs_kernel_codification('7c4a7482-af91-41bf-a137-ac7c329577c3', fixed_text).
narrative_ontology:cs_authority_grounding('7c4a7482-af91-41bf-a137-ac7c329577c3', lineage).
narrative_ontology:cs_interpretation_layer_present('7c4a7482-af91-41bf-a137-ac7c329577c3').
narrative_ontology:cs_reading_relation('7c4a7482-af91-41bf-a137-ac7c329577c3', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c4a7482-af91-41bf-a137-ac7c329577c3', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('7c4a7482-af91-41bf-a137-ac7c329577c3', foundational, constitutional_meaning_evolves_with_societal_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_societal_values, holdable).
narrative_ontology:cs_axiom_grounding('7c4a7482-af91-41bf-a137-ac7c329577c3', constitutional_meaning_evolves_with_societal_values, instrumental).
narrative_ontology:cs_axiom('7c4a7482-af91-41bf-a137-ac7c329577c3', foundational, judicial_reasoned_adaptation_is_legitimate_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_reasoned_adaptation_is_legitimate_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7c4a7482-af91-41bf-a137-ac7c329577c3', judicial_reasoned_adaptation_is_legitimate_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('7c4a7482-af91-41bf-a137-ac7c329577c3', living_constitutionalist_framework).
narrative_ontology:cs_drift_state('7c4a7482-af91-41bf-a137-ac7c329577c3', post_dobbs_originalist_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7c4a7482-af91-41bf-a137-ac7c329577c3', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, judicial_institution).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, constrained_by_expanded_federal_reach).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, judicial_review_as_guardian_of_evolving_rights).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, constitutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__living_constitution_reading, unenumerated_rights_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal judiciary, especially the Supreme Court, administers this reading by issuing binding interpretations that expand federal power and recognize unenumerated rights. It collects institutional authority and legitimacy from being the primary expositor of evolving constitutional meaning. Justices are appointed for life, giving them biographical-to-generational time horizons and analytical exit options (they can dissent but cannot leave the institution without ending their judicial role).
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, judicial_institution, agenda_setter,
    institutional, generational, analytical, national).

% Groups seeking recognition of equality rights (racial minorities, women, religious minorities, disability advocates) benefit when courts read equal protection and due process to cover new classifications and substantive rights. They are organized through advocacy organizations and litigation networks. Exit is constrained — they cannot easily abandon constitutional litigation as a strategy without losing the primary vehicle for rights recognition, but they can shift to legislative or state-level strategies at some cost.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    organized, biographical, constrained, national).

% Advocates for abortion access, contraception, and reproductive decision-making benefit from the privacy/dignity line of cases rooted in substantive due process. They are organized through national advocacy groups and litigation funds. Exit is constrained — the constitutional strategy is central to their cause, but state-level protection and legislative strategies provide partial alternatives, especially after Dobbs.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% LGBTQ+ individuals and organizations benefit from the extension of equal protection and due process to sexual orientation and gender identity (Lawrence, Obergefell, Bostock). They are organized through national litigation and advocacy networks. Exit is constrained — constitutional recognition remains the gold standard for nationwide protection, though state-level victories and statutory strategies (Equality Act) offer partial alternatives.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% State governments and officials who argue that expanding federal judicial power displaces state sovereignty in areas traditionally reserved to states (family law, education, criminal procedure, health regulation). They bear the costs of compliance with federal mandates and loss of policy autonomy. They are powerful as institutional actors (state governments) but constrained in exit — they cannot leave the federal system, but can resist through non-cooperation, interposition rhetoric, and judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    powerful, biographical, constrained, regional).

% Judges, scholars, and advocates committed to originalist methodology who experience the living constitution reading as an illegitimate power grab that undermines the rule of law and democratic legitimacy. They bear the cost of seeing their interpretive framework displaced in academia and the judiciary. Exit is identity-locked — their professional identity and intellectual project are constituted by opposition to this reading; abandoning it would mean abandoning their life's work and community.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, generational, identity_locked, national).

% Individuals and entities (businesses, local governments, property owners, religious organizations) subject to expanded federal regulatory power under the Commerce Clause and Section 5 enforcement legislation. They bear compliance costs and loss of local control. Exit is mobile — they can relocate, restructure, or seek exemptions, but cannot escape federal jurisdiction.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constrained_by_expanded_federal_reach, payer,
    moderate, biographical, mobile, national).

% The academic community that analyzes, critiques, and theorizes constitutional interpretation. They do not directly collect from or pay into the constraint but shape the intellectual environment in which it operates. Their exit is analytical — they can shift frameworks without personal cost.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, legal_scholarship, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the constitutional order to adapt to changing moral and social conditions without formal amendment, allowing rights and powers to evolve through reasoned judicial interpretation rather than legislative deadlock or violent rupture.
% TRANSFER_FUNCTION: Moves interpretive authority from the fixed text and original understanding to the contemporary judiciary, transferring the power to define the scope of rights and federal authority from the ratifying generation to current judges. The transfer runs from states and textualist-constrained actors to rights claimants and the judicial institution.
% ABSENT_VOICES: Future generations whose constitutional order is being shaped today without their participation; the ratifying generation whose understanding is displaced; populations in territories and tribal nations whose constitutional status remains unsettled by any reading.
% DISAPPEARANCE_RATIONALE: If the living constitution reading vanished overnight, the judicial institution would lose its primary warrant for recognizing unenumerated rights and expanding federal power; rights claimants would lose their most effective litigation strategy; states and textualists would gain interpretive ground. The constitutional order would rearrange toward originalism or popular constitutionalism as the dominant framework.
% FOUNDING_PROBLEM: The Constitution's text is too sparse and its amendment process too difficult to address novel rights claims and governance challenges in a rapidly changing society; judicial adaptation prevents constitutional obsolescence and provides a peaceful channel for moral progress.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists (Brennan, Dworkin, Strauss) attest the problem remains live — society continues to generate novel rights claims the framers could not anticipate. Originalists (Scalia, Barrett, Whittington) and popular constitutionalists (Kramer, Tushnet) attest the problem is either solved by democratic processes or the solution (judicial updating) is worse than the problem; they corroborate from outside the beneficiary set that the founding problem is contested, not settled.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the reading's transfer of interpretive authority from fixed text to contemporary judges, enabling rights recognition and federal power expansion that the ratifying generation did not authorize. Suppression (0.55) reflects the constraint's dependence on judicial enforcement and the difficulty of exiting its reach — states and textualists cannot opt out of Supreme Court precedent. Theater ratio (0.22) is moderate-low: the coordination function (adaptive governance) is genuine but a growing share of doctrinal complexity serves to manage the reading's own internal tensions rather than solve external problems. Accessibility collapse (0.42) is moderate — alternatives (originalism, popular constitutionalism) remain intellectually and politically viable. Resistance (0.61) is substantial — originalism has captured much of the federal judiciary and popular constitutionalism challenges judicial supremacy from the left.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial institution's seat, the reading is genuine coordination — it solves the problem of constitutional adaptation without amendment gridlock. From states' rights and textualist seats, the same structure operates as enforced extraction — their interpretive preferences are suppressed and their autonomy displaced. From rights claimants' seats, it is conditional coordination — they benefit but remain vulnerable to doctrinal reversal (Dobbs). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst's view that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial institution sits at the beneficiary end (d ~ 0.15): it collects institutional authority and legitimacy from administering the reading. Rights claimants (civil rights, reproductive autonomy, LGBTQ+) are beneficiaries with constrained exit (d ~ 0.3) — they gain substantive protections but depend on the reading's continued dominance. States' rights advocates and the constrained-by-federal-reach are payers with moderate-to-high d (~0.7-0.8) — they bear compliance costs and loss of autonomy with limited exit. Original-meaning textualists are identity-locked payers (d ~ 0.9) — their professional identity is constituted by opposition. The derivation chain from beneficiary/victim + power + exit produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional obsolescence) remains contested — originalists argue democratic processes solve it; living constitutionalists argue courts are necessary. The reading has not resolved into pure extraction (snare) because the coordination function (peaceful adaptation) is still invoked and partially effective. It has not resolved into pure coordination (rope) because the extraction from states and textualists is asymmetric and enforced. Tangled rope is the honest structural assessment: genuine coordination function + asymmetric extraction + active enforcement (judicial supremacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the living constitution reading a distinct constraint from the originalist and popular constitutionalism readings, or are they framings of a single constraint?',
    'Apply the epsilon-invariance test: if measuring the constraint from the living constitution frame yields a different epsilon than measuring from the originalist frame, they are distinct constraints. The structural delta (different beneficiaries, victims, scope) indicates they are distinct.',
    'If distinct, each reading gets its own constraint story with its own epsilon and classification. If the same constraint, a single story with observer-dependent classification would be required — violating epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints per epsilon-invariance.').

omega_variable(
    coordination_extraction_boundary,
    'Is the adaptive governance function (coordination) separable from the judicial power expansion (extraction), or are they inextricably fused?',
    'Counterfactual analysis: if courts recognized unenumerated rights but deferred to Congress on Commerce Clause scope, would the reading still function? Historical comparison with periods of judicial restraint (e.g., post-1937 Commerce Clause deference) versus rights expansion.',
    'If separable, the constraint decomposes into a rope (rights adaptation) and a snare (federal power aggrandizement). If fused, tangled_rope is the correct unitary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    textualist_identity_lock,
    'Is the identity_locked exit for original_meaning_textualists a genuine structural trap or a voluntary professional commitment?',
    'Examine whether textualists who abandon originalism face professional ostracism, loss of appointment prospects, or intellectual community dissolution — versus whether they could transition to living constitutionalist scholarship without career cost.',
    'If genuine identity lock, their directionality is near 1.0 and effective extraction is maximal. If voluntary, their exit is closer to constrained and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_identity_lock, empirical, 'Whether textualist opposition constitutes identity lock or voluntary positioning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t1789, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t1865, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t1937, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t1954, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t1973, us_constitution_interpretive__living_constitution_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_tr_t2026, us_constitution_interpretive__living_constitution_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t1789, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t1865, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1865, 0.25).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t1937, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t1954, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1954, 0.52).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t1973, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 1973, 0.61).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_be_t2026, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t1789, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t1865, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1865, 0.35).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t1937, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t1954, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t1973, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 1973, 0.52).
narrative_ontology:measurement(us_constitution_interpretive__living_constitution_reading_su_t2026, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__living_constitution_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint (living_constitution_reading) is one member of the us_constitution_interpretive constraint family. The originalist_reading and popular_constitutionalism_reading are sibling constraints with different epsilon values, beneficiary/victim structures, and claimed types. The living constitution reading influences both siblings: it creates the legitimacy conditions that originalism reacts against, and it occupies the judicial supremacist position that popular constitutionalism contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
