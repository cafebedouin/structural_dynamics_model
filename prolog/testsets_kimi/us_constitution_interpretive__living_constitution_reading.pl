% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution Judicial Interpretation
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the living_constitution_reading of the
 *   us_constitution_interpretive kernel. Under this reading, constitutional
 *   meaning evolves with societal values and interpretive authority derives
 *   from federal courts' reasoned adaptation of text and precedent to
 *   contemporary conditions. The constraint coordinates national governance
 *   across changing circumstances while simultaneously extracting autonomy
 *   from states, localities, and originalist interpreters who contest the
 *   legitimacy of non-textual evolution. Key agents operate across
 *   institutional, organized, and analytical power levels, with divergent
 *   exit options shaped by their relationship to judicial supremacy.
 *
 * KEY AGENTS:
 *   - Federal judiciary (agenda_setter/institutional/constrained): Administers evolving interpretive authority and enforces it through judicial review
 *   - Civil rights expansion claimants (beneficiary/organized/mobile): Receive constitutional recognition through adaptive equal protection readings
 *   - Reproductive autonomy advocates (beneficiary/organized/mobile): Rely on unenumerated rights recognized via evolving privacy doctrine
 *   - LGBTQ rights claimants (beneficiary/organized/mobile): Obtain shelter under expanding liberty and equality interpretations
 *   - States rights advocates (payer/organized/constrained): Bear loss of reserved authority to federal expansion
 *   - Original meaning textualists (payer/organized/constrained): Marginalized by interpretive method that treats text as open-ended
 *   - State governments (payer/institutional/constrained): Lose regulatory space to preemptive federal judicial interpretations
 *   - Constitutional scholars (observer/analytical/analytical): Document and analyze the interpretive framework without direct enforcement stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.62).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.58).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution Judicial Interpretation").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, 'abf41c52-99c7-46af-8984-994f3c4ce903').
narrative_ontology:cs_kernel_codification('abf41c52-99c7-46af-8984-994f3c4ce903', fixed_text).
narrative_ontology:cs_authority_grounding('abf41c52-99c7-46af-8984-994f3c4ce903', lineage).
narrative_ontology:cs_interpretation_layer_present('abf41c52-99c7-46af-8984-994f3c4ce903').
narrative_ontology:cs_reading_relation('abf41c52-99c7-46af-8984-994f3c4ce903', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('abf41c52-99c7-46af-8984-994f3c4ce903', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('abf41c52-99c7-46af-8984-994f3c4ce903', foundational, constitutional_meaning_adaptive).
narrative_ontology:cs_axiom_status(constitutional_meaning_adaptive, holdable).
narrative_ontology:cs_axiom_grounding('abf41c52-99c7-46af-8984-994f3c4ce903', constitutional_meaning_adaptive, conventional).
narrative_ontology:cs_axiom('abf41c52-99c7-46af-8984-994f3c4ce903', foundational, unenumerated_rights_recognizable).
narrative_ontology:cs_axiom_status(unenumerated_rights_recognizable, holdable).
narrative_ontology:cs_axiom_grounding('abf41c52-99c7-46af-8984-994f3c4ce903', unenumerated_rights_recognizable, deontological).
narrative_ontology:cs_reference_frame('abf41c52-99c7-46af-8984-994f3c4ce903', adaptive_constitutional_order).
narrative_ontology:cs_drift_state('abf41c52-99c7-46af-8984-994f3c4ce903', originalist_resurgence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('abf41c52-99c7-46af-8984-994f3c4ce903', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal courts, culminating in the Supreme Court, exercise authority to interpret the Constitution as embodying evolving principles. They hear challenges to state and federal laws, invalidate those deemed incompatible with contemporary values, and write opinions justifying outcomes through adaptive readings of text and precedent. Judges enjoy life tenure but are bound by the institutional role and its norms.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Groups and individuals seeking constitutional protection against discrimination or disenfranchisement who benefit when courts read equal protection and due process as encompassing evolving social understandings of equality.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    organized, biographical, mobile, national).

% Advocates for bodily autonomy and privacy rights who rely on judicial recognition of unenumerated rights that adapt to modern medical and social conditions, even when constitutional text is silent on the matter.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, mobile, national).

% Individuals and organizations seeking constitutional shelter for sexual orientation and gender identity under evolving equal protection and substantive liberty doctrines.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, mobile, national).

% Advocates and officials defending state regulatory authority and reserved powers under the Tenth Amendment, constrained by federal judicial rulings that expand national power through broad constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Legal scholars, judges, and litigants who argue constitutional meaning was fixed at ratification and who are structurally marginalized when courts treat the text as open-ended and historically detached.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, generational, constrained, national).

% State legislatures and executive agencies whose policy choices in areas like health, safety, and morals are preempted or invalidated by federal courts interpreting the Constitution to authorize broader federal oversight.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, state_governments, payer,
    institutional, biographical, constrained, national).

% Academics who analyze, critique, and document the development of constitutional doctrine without being direct parties to litigation or enforcement.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables constitutional governance to adapt to changing social conditions across centuries without requiring formal amendment for every new context, coordinating stability and change through centralized judicial interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority and policy-making capacity from state legislatures and local majorities to the federal judiciary and to rights-claiming groups; transfers the burden of legal uncertainty and preemption from the national center to dissenting states and localities.
% ABSENT_VOICES: Popular constitutionalists who argue democratic majorities should directly shape constitutional meaning rather than defer to courts; originalist scholars and state officials who dispute the legitimacy of non-textual evolution; future citizens who might prefer a formally amended text over judicially adapted doctrine.
% DISAPPEARANCE_RATIONALE: If courts abandoned evolving interpretation and reverted to fixed original meaning, federal statutory schemes currently justified by broad Commerce Clause readings would face challenge; unenumerated rights like privacy would lose constitutional shelter; states would reclaim regulatory authority over abortion, marriage, and commerce.
% FOUNDING_PROBLEM: How to maintain a written constitution's viability across centuries of unanticipated social, technological, and moral change without making the text so easy to amend that it loses stabilizing force.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians like Bruce Ackerman attest to the problem of inter-generational governance; originalist scholars like Antonin Scalia and Randy Barnett dispute that the founding problem requires evolutionary judicial solutions, arguing the amendment process suffices. State attorneys general and federalism scholars corroborate that the expansion of federal power creates coordination problems the living reading was not originally built to solve.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is moderate-high because the living reading systematically transfers decision-making capacity from state and local actors to the federal judiciary, extracting autonomy through broad Commerce Clause, implied powers, and unenumerated rights doctrines. Suppression (0.58) reflects the active marginalization of originalist methodology via precedent hierarchy and the structural exclusion of non-judicial interpretive voices. Theater ratio (0.30) is moderate-low: the reasoning is often genuine principled adaptation, but it can mask policy preferences in high-salience cases. Accessibility collapse (0.60) captures how originalism becomes legally non-viable once the living framework is accepted institutionally. Resistance (0.72) is high due to sustained originalist and federalist opposition across the interval. Temporal measurements show extractiveness rising through the mid-interval as the Warren and Burger Courts expanded rights, then modestly declining toward the end as originalist critique gained institutional traction, producing a peaked trajectory rather than monotonic drift.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this constraint as necessary governance maintenance â solving the problem of constitutional obsolescence through reasoned adaptation. Rights-claiming beneficiaries experience it as protective coordination. States' rights advocates and originalists experience the identical structure as asymmetric extraction of their autonomy and interpretive legitimacy. The engine computes these divergent seat classifications from the same structural data: low directionality for beneficiaries, high directionality for state governments and textualists, and near-symmetric for the judiciary depending on institutional power effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (civil rights claimants, reproductive autonomy advocates, LGBTQ rights claimants) receive constitutional protection and therefore derive low directionality from the constraint. Victim groups (states rights advocates, original meaning textualists, state governments) bear costs in the form of lost authority and marginalized methodology, yielding high directionality. The federal judiciary sits at an intermediate directionality: it is the administrative seat that wields the extracted power, but it is also bound by institutional norms and does not capture the gains as personal rent. Device users in the example are not present here; the analog is the general citizenry, which is not separately modeled because the extraction is inter-governmental and interpretive rather than direct.
 *
 * MANDATROPHY ANALYSIS:
 *   The living reading was built to solve the founding problem of constitutional obsolescence. That problem remains live in the sense that formal amendment is difficult, so the coordination function has not fully atrophied. However, the constraint risks mandatrophy if judicial adaptation becomes purely instrumental to preferred policy outcomes without reasoned textual grounding. The theater ratio staying below 0.4 across the interval suggests that genuine coordination work still occurs, preventing piton classification, but the peak in theater around the midpoint indicates periods where performative reasoning increased. The divergence between the coordination story and the extraction profile is what makes this a tangled rope rather than a rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_boundary_plasticity,
    'Has the living reading rendered the constitutional text so plastic that it no longer functions as a fixed kernel, or does the text still impose meaningful constraint on judicial adaptation?',
    'Jurisprudential analysis comparing Supreme Court opinions to constitutional text: if outcomes correlate weakly with textual provisions, the kernel has dissolved into pure interpretive discretion.',
    'If the kernel is fully dissolved, the constraint shifts from tangled_rope toward snare (pure judicial power without coordinative text); if text still constrains, it remains a genuine coordination mechanism with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_plasticity, conceptual, 'Whether the constitutional text still constrains the living reading').

omega_variable(
    originalism_living_synthesis_feasibility,
    'Can a single interpretive framework coherently combine original public meaning and living adaptation, or do these sibling readings represent structurally irreconcilable constraints?',
    'Examination of hybrid theories for internal consistency and predictive power in actual judicial decisions.',
    'If synthesis is coherent, the relation to originalist_reading should remain coexists_with or influences; if impossible, foreclosure may be warranted and the kernel decomposes more sharply into distinct constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_living_synthesis_feasibility, conceptual, 'Logical reconcilability of originalist and living readings').

omega_variable(
    judicial_supremacy_exclusion,
    'Does the living reading''s concentration of interpretive authority in the federal judiciary structurally exclude popular democratic constitutional authorship?',
    'Empirical comparison of constitutional change through judicial review versus legislative and popular constitutional movements across the interval.',
    'If popular authorship is structurally excluded, the constraint is more extractive toward democratic majorities than the coordination story suggests; if channels remain open, extraction is limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_exclusion, empirical, 'Whether living constitutionalism monopolizes constitutional meaning-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_interpretive__living_constitution_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_interpretive__living_constitution_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_interpretive__living_constitution_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_interpretive__living_constitution_reading, theater_ratio, 75, 0.34).
narrative_ontology:measurement(us_c_tr_t85, us_constitution_interpretive__living_constitution_reading, theater_ratio, 85, 0.3).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t15, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(us_c_be_t30, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(us_c_be_t45, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(us_c_be_t75, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(us_c_be_t85, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 85, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(us_c_su_t15, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(us_c_su_t30, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(us_c_su_t45, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(us_c_su_t75, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(us_c_su_t85, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 85, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__living_constitution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__living_constitution_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_interpretive kernel. The kernel decomposes into structurally distinct commitments because the same constitutional text supports divergent epsilon profiles: originalist reading (low extraction, fixed meaning), living reading (moderate extraction, adaptive meaning), and popular constitutionalism (diffuse authority, democratic contestation). Each reading has distinct beneficiaries, victims, and directionality structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
