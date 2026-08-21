% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of constitutional
 *   authority, which asserts that the meaning of the U.S. Constitution is
 *   fixed at the time of its ratification and should be interpreted according
 *   to the original public understanding. This reading claims to provide a
 *   stable, objective legal framework, akin to a natural law. However, its
 *   operational reality involves active judicial enforcement that can be
 *   highly extractive for groups seeking to adapt constitutional rights to
 *   contemporary social conditions. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as a mountain (fixed meaning) while the authored
 *   metrics describe its substantially extractive and suppressive operation,
 *   which the engine will measure as a divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.7).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.8).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, mountain).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).
domain_priors:emerges_naturally(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2').
narrative_ontology:cs_kernel_codification('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', fixed_text).
narrative_ontology:cs_authority_grounding('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', lineage).
narrative_ontology:cs_interpretation_layer_present('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2').
narrative_ontology:cs_reading_relation('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', foundational, original_public_meaning_is_authoritative).
narrative_ontology:cs_axiom_status(original_public_meaning_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', original_public_meaning_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', founding_era_public_understanding).
narrative_ontology:cs_drift_state('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2ada5f0f-d20e-432d-8a6b-9f1c9d4a59c2', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_legal_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, groups_seeking_new_rights).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, social_progressives).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, constitutional_amendment_advocates).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce the fixed meaning of the Constitution as understood at the time of its ratification. Their authority is grounded in this interpretive method, which limits their discretion but also provides a stable basis for their rulings. They actively suppress alternative interpretive methods in their jurisprudence.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Provide the intellectual and theoretical justification for originalism, benefiting from its prominence in judicial appointments and legal discourse. Their careers and influence are often tied to the success of this interpretive method.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_legal_scholars, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of a rigid constitutional interpretation that makes it difficult to recognize unenumerated rights or adapt to social change without the arduous Article V amendment process. Their claims are often denied or delayed by this interpretive framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, groups_seeking_new_rights, payer,
    powerless, biographical, trapped, national).

% Actively resist the originalist interpretation, viewing it as an impediment to social justice and progress. They advocate for alternative interpretive methods and political action to counter its effects, but are constrained by its legal authority.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, social_progressives, payer,
    organized, biographical, constrained, national).

% Offer alternative interpretive theories that emphasize the Constitution's evolving meaning. While they operate within the broader legal academy, their interpretive framework is largely excluded from the originalist judicial methodology, limiting their direct influence on originalist rulings.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_scholars, excluded,
    powerful, generational, mobile, national).

% Are forced to pursue the extremely difficult Article V amendment process to achieve constitutional changes that originalism prevents through interpretation. This makes them payers of the high transaction costs of constitutional change under this reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_amendment_advocates, payer,
    organized, generational, constrained, national).

% The abstract concept that originalism claims as its source of authority. It 'benefits' by being elevated to the ultimate arbiter of constitutional meaning, even as its precise content is debated and constructed by scholars.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, historical_public_understanding, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(constitutional_text_authority__originalist_reading, historical_public_understanding).

% Individuals, corporations, or institutions that benefit from the maintenance of existing legal and social arrangements, which originalism often reinforces by resisting change. They may not actively enforce originalism but gain from its outcomes.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, status_quo_beneficiaries, beneficiary,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal interpretation by anchoring constitutional meaning to a fixed historical point, thereby limiting judicial discretion and ensuring continuity with the founding generation's intent.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary society and evolving values to historical intent and original public understanding, effectively shifting power from present-day majorities to past understandings. It also transfers the burden of constitutional change to the difficult Article V amendment process.
% ABSENT_VOICES: Future generations, whose evolving values and experiences are implicitly excluded from shaping constitutional meaning without formal amendment. Also, those whose historical voices were excluded from the original ratification process (e.g., enslaved people, women, indigenous populations) are not fully represented in the 'original public understanding' that serves as the authority.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, judicial interpretation would immediately become more flexible, allowing for the evolution of rights and powers without formal amendment. The legal landscape would shift dramatically as courts adopted more dynamic interpretive methods, potentially leading to new legal precedents and a reordering of constitutional rights and governmental powers.
% FOUNDING_PROBLEM: To establish a stable, supreme law that limits government power and protects individual liberties, preventing arbitrary rule and ensuring fidelity to the foundational compact, thereby avoiding judicial overreach and political instability.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents cite the Federalist Papers, historical legal texts, and the writings of the framers as corroboration for the founding problem and its ongoing relevance. Critics (e.g., living constitutionalists, some historians) argue that the 'founding problem' itself was interpreted differently by various founders and that the idea of a single, fixed public understanding is anachronistic; independent historical scholarship often highlights the diversity of original intent and the evolving nature of constitutional thought even in the founding era.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(constitutional_text_authority__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constitutional_text_authority__originalist_reading),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because this interpretive method often denies or delays the recognition of rights for certain groups, effectively extracting opportunities for social and legal progress. Suppression is also high (0.8) as it actively dismisses or marginalizes alternative interpretive methods and requires significant effort (e.g., constitutional amendments) to overcome its rigidity. The theater ratio is moderate (0.4) because while there is genuine scholarly and judicial effort to ascertain historical meaning, there is also a performative aspect of 'fidelity' that can mask policy preferences. Accessibility collapse is high (0.85) from the originalist perspective, as it claims to definitively fix meaning, leaving few legitimate interpretive alternatives. Resistance is high (0.7) due to ongoing legal and political challenges from those advocating for more dynamic constitutional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist proponents, this constraint is a neutral, objective 'mountain' that merely uncovers the Constitution's true, fixed meaning. From the perspective of those whose rights are denied or whose social progress is impeded, the same structure operates as a 'snare' or 'tangled rope,' actively enforced to maintain existing power structures and extract costs from those seeking change. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and conservative legal scholars are structural beneficiaries, as their authority and influence are enhanced by this interpretive framework. Groups seeking new rights and social progressives are targets, bearing the costs of denied or delayed rights and the difficulty of achieving constitutional change. Constitutional amendment advocates are also targets, as the originalist reading forces them into a highly demanding process. The 'historical public understanding' is a conceptual beneficiary, as its authority is elevated.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist claim of a fixed, natural-law-like meaning (a 'mountain') serves to obscure any potential mandatrophy. By asserting that the Constitution's meaning is immutable, it resists the idea that its mandate could become obsolete or that its function could drift from coordination to extraction. The classification system, by measuring high extractiveness and suppression against a 'mountain' claim, detects this potential false summit, indicating that the constraint's operation may be more extractive than its stated purpose suggests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the fixed meaning of the Constitution, as asserted by originalism, a genuine natural law or an interpretive construct that benefits identifiable agents?',
    'Analysis of historical evidence regarding the diversity of original intent and the evolution of interpretive methodologies; examination of the political and social outcomes of originalist rulings.',
    'If primarily a construct, the constraint''s ''mountain'' claim is a cover story for a ''tangled_rope'' or ''snare,'' and its legitimacy rests on active enforcement rather than inherent truth. This would trigger a reclassification from mountain to a more extractive type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between inherent constitutional meaning and interpretive choice.').

omega_variable(
    historical_fidelity_vs_judicial_activism,
    'Does the originalist methodology genuinely limit judicial discretion by adhering to historical fidelity, or does it enable a form of judicial activism by selectively interpreting historical evidence to achieve desired policy outcomes?',
    'Empirical studies of judicial decision-making under originalist frameworks, comparing stated methodology with actual outcomes, especially in politically charged cases. Analysis of how ''original public meaning'' is constructed by judges and scholars.',
    'If it enables selective interpretation, the ''judicial restraint'' claim is theatrical, and the constraint''s effective extractiveness and suppression are higher than acknowledged, as it masks policy imposition under the guise of historical objectivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_fidelity_vs_judicial_activism, empirical, 'Whether originalism is truly about restraint or a form of activism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative constitutional interpretations structural (e.g., stare decisis, institutional inertia) or internalized (e.g., ideological commitment to originalism by legal professionals)?',
    'Analysis of legal education curricula, judicial appointment processes, and the career trajectories of legal scholars and judges who deviate from originalist orthodoxy. Post-exit trajectory of legal professionals who leave originalist institutions.',
    'If internalized, the constraint''s effective suppression is higher than the structural measures suggest, as legal actors carry the interpretive framework with them, making genuine alternatives harder to conceive or implement even in less rigid environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of interpretive alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_text_authority__originalist_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text_authority__originalist_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__originalist_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__originalist_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(cons_tr_t2020, constitutional_text_authority__originalist_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_text_authority__originalist_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(cons_be_t1990, constitutional_text_authority__originalist_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__originalist_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__originalist_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(cons_be_t2020, constitutional_text_authority__originalist_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_text_authority__originalist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(cons_su_t1990, constitutional_text_authority__originalist_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__originalist_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__originalist_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(cons_su_t2020, constitutional_text_authority__originalist_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, judicial_review_scope).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_recognition).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_text_authority' kernel. Its ε value differs significantly from sibling readings (living_constitutionalist_reading, positivist_reading) due to its fixed-meaning premise and the resulting extractive outcomes for certain groups. All readings are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
