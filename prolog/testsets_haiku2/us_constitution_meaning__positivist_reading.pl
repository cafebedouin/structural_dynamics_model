% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Positivist Constitutional Validity: Formal Procedure over Moral Substance
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the U.S. Constitution grounds constitutional
 *   validity in formal enactment procedures and institutional authority, not
 *   in external moral principles. Under this reading, judges are constrained
 *   to interpret the text and formal amendment process; moral reasoning is
 *   excluded from validity determination by definitional rule. The reading
 *   beneficiaries are institutional stability advocates and ruling coalitions
 *   (it protects their preferred outcomes once entrenched through
 *   appointments or amendments). The reading's victims are substantive
 *   justice claimants whose rights lack explicit textual
 *   grounding—historically excluded groups, LGBTQ+ persons, and others whose
 *   moral claims cannot clear the procedural bar. The constraint is CLAIMED
 *   as tangled_rope: it does coordinate a solution (procedural legitimacy for
 *   judicial interpretation) while simultaneously extracting (locking out
 *   moral reasoning and distributing power to ruling coalitions). This is one
 *   reading of the contested kernel 'us_constitution_meaning'; the
 *   originalist and living-constitutionalist readings are distinct
 *   constraints authoring different beneficiary/victim structures and
 *   different ε values for the same kernel.
 *
 * KEY AGENTS:
 *   - Proceduralist judges: agenda-setters who enforce the positivist reading by constraining their own reasoning to text and procedure
 *   - Institutional stability advocates: beneficiaries who defend proceduralism as essential to rule of law and democratic legitimacy
 *   - Ruling coalitions: beneficiaries and agenda-setters who use appointments and amendment strategy to entrench outcomes under the guise of procedural neutrality
 *   - Substantive justice claimants (especially historically excluded groups): victims systematically foreclosed from constitutional protection by the exclusion of moral reasoning
 *   - Moral philosophers and rights theorists: excluded by definition; would argue proceduralism licenses historical injustice
 *   - Living constitutionalists and originalists: observers holding alternative readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.64).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Positivist Constitutional Validity: Formal Procedure over Moral Substance").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, 'd526d4a3-fc30-47c2-aac9-9caddfe0e9ba').
narrative_ontology:cs_kernel_codification('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', fixed_text).
narrative_ontology:cs_authority_grounding('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', lineage).
narrative_ontology:cs_interpretation_layer_present('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba').
narrative_ontology:cs_reading_relation('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', foundational, procedure_exhausts_legitimacy).
narrative_ontology:cs_axiom_status(procedure_exhausts_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', procedure_exhausts_legitimacy, conventional).
narrative_ontology:cs_axiom('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', foundational, moral_reasoning_excluded_from_validity).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity, holdable).
narrative_ontology:cs_axiom_grounding('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', moral_reasoning_excluded_from_validity, deontological).
narrative_ontology:cs_reference_frame('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', formal_procedural_authority_only).
narrative_ontology:cs_drift_state('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', post_strategic_appointment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d526d4a3-fc30-47c2-aac9-9caddfe0e9ba', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, institutional_stability_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, proceduralist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, ruling_coalitions).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, marginalized_groups_lacking_textual_protection).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, rule_of_law_through_procedure).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, amendment_process_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Support the positivist reading as a doctrine of institutional legitimacy. Argue that constitutional validity resting on formal procedure rather than contested moral principle protects courts from politicization and preserves democratic legitimacy through amendment processes. They benefit from a stable, rule-predictable constitutional framework that does not subject enacted law to moral re-evaluation by each generation of judges.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, institutional_stability_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Enforce the positivist reading by constraining judicial reasoning to textual and procedural grounds, excluding moral or extra-constitutional principle from validity determination. They set the agenda for constitutional interpretation by deciding which arguments count as legitimate. Their position is reinforced by legal education, bar standards, and collegial validation within the judiciary. They claim this constrains judicial discretion; critics argue it licenses outcome-driven reasoning dressed in procedure.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, proceduralist_judges, agenda_setter,
    institutional, biographical, constrained, national).

% Control the judiciary through appointment and leverage the positivist reading to entrench their policy preferences into constitutional doctrine. By winning control of the amendment process (through supermajority coalition-building or through appointment of judges who embrace the reading), they can lock in outcomes that would be harder to justify on moral grounds alone. The reading allows them to claim procedural neutrality while advancing substantive goals.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, ruling_coalitions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, ruling_coalitions, agenda_setter).

% Seek constitutional protection for rights or interests that lack explicit textual grounding in the Constitution. Under the positivist reading, their claims are systematically foreclosed: moral arguments about dignity, equality, or necessity cannot override the absence of textual support. They pay the cost of a constraint that excludes their voice from the conversation by definitional rule.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Experience systematic exclusion from constitutional protection because their ancestors held no political power when the text was ratified. The positivist reading ensures that rights for enslaved people, women, religious minorities, LGBTQ+ persons, and others were not included in the original text and therefore remain constitutionally unprotected unless the super-difficult amendment process succeeds. Their identity as members of a historically excluded class makes exit (moving out of the constitutional system's jurisdiction) structurally impossible.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, marginalized_groups_lacking_textual_protection, payer,
    powerless, generational, identity_locked, national).

% Develop systematic arguments for rights based on moral principle, natural law, or philosophical reasoning about human dignity. The positivist reading excludes them from the constitutional conversation by definition—their arguments are categorized as non-legal, external to the valid determinants of constitutional meaning. They would argue that proceduralism without substance licenses the entrenchment of historical injustices and claims false neutrality while advancing particular (conservative) substantive ends.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_philosophers_and_rights_theorists, excluded,
    moderate, generational, mobile, national).

% Practice a parallel reading: the Constitution's meaning is fixed at ratification, knowable through historical investigation. They share with positivists a commitment to constraining judicial discretion through external anchors (text/history rather than procedure/form), but disagree on what counts as that anchor. In practice, where the amendment process gridlocks, originalism and positivism converge on the same constraint: frozen law.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, originalist_judges, observer,
    institutional, biographical, constrained, national).

% Interpret constitutional principles as living and evolving with social attitudes and circumstances. They directly oppose the positivist reading by insisting that moral reasoning, evolving consensus, and contemporary values inform constitutional meaning. Their presence in the judiciary creates the doctrinal contest that defines this constraint.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_judges, observer,
    institutional, biographical, constrained, national).

% Produces the theoretical and empirical work that contests or supports the positivist reading. They develop competing frameworks (natural law, critical theory, comparative constitutionalism) and analyze whether the reading's operation in practice delivers on its promise of constraining discretion or instead conceals outcome-driven reasoning. Their work shapes how the next generation of judges understands the reading.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_academy, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, ruling_coalitions).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, institutionally legitimate procedure for determining constitutional validity. Instead of each generation or faction re-litigating fundamental moral principles, the positivist reading establishes that validity flows from formal enactment through the procedures the Constitution itself prescribes. This solves the collective-action problem of the constitutional system: how to make binding determinations about law's authority without triggering endless dispute over first principles.
% TRANSFER_FUNCTION: Moves the power to determine what is constitutionally legitimate from moral philosophers, rights advocates, and ordinary democratic citizens into the hands of proceduralist judges and ruling coalitions that control appointments and amendment majorities. Substantive justice claims lacking formal textual support are systematically transferred out of the constitutional conversation and into the category of 'legislative matters' or 'not rights at all'.
% ABSENT_VOICES: Moral philosophers, rights theorists, marginalized groups, and advocates for unenumerated substantive freedoms are excluded by the reading's definition of what counts as a valid constitutional argument. They would object that proceduralism without moral substance licenses the entrenchment of historical injustices and claims false neutrality while advancing particular (conservative) substantive ends. They are kept out by the same rule that defines validity as procedure-dependent rather than principle-dependent.
% DISAPPEARANCE_RATIONALE: If the positivist reading vanished—if courts returned to recognizing moral reasoning, natural rights, and substantive principle as legitimate sources of constitutional meaning—the entire landscape of what is constitutionally protected would shift. Rights and protections for historically excluded groups would become arguable again on moral grounds; the amendment process would be supplemented (not displaced) by judicial reasoning about human dignity; the outcome-determinativeness of controlling judicial appointments would diminish because more doctrinal paths would be open. The constitutional order would rearrange around a different legitimacy criterion.
% FOUNDING_PROBLEM: The Constitution's text does not clearly resolve disputed questions of meaning. Judges must interpret ambiguous language and determine the scope of sparse enumerated rights. The positivist reading was developed to solve the problem: How can judges legitimately interpret the Constitution while respecting democratic authority and preventing judges from substituting their own moral or policy preferences for the text? Answer: Bind interpretation to the text and formal procedures; exclude external moral reasoning.
% FOUNDING_PROBLEM_CORROBORATION: Proceduralist judges, law-and-order conservatives, and institutional-stability advocates attest that the founding problem persists: judicial discretion remains a threat to the rule of law and democratic legitimacy. Living constitutionalists, critical legal scholars, and moral philosophers counter that the 'problem' of judicial discretion is insoluble and that claiming procedural purity while advancing substantive outcomes is self-deception. Outside corroboration comes from comparative constitutional law (other systems use text + principle together) and empirical legal scholarship showing outcome-correlation with judicial appointments (suggesting the 'constraint' is theater). The founding problem status is not independently verified; it is a matter of legal philosophy.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the reading delivers real coordination (stable, predictable interpretation) but also concentrates power in ruling coalitions and excludes entire categories of claims by definitional rule. The coordination benefit is genuine; the extraction cost is borne by those whose moral claims lack textual support. Suppression is moderately high (0.64) because the reading is actively enforced through legal education, bar standards, collegial validation within the judiciary, and exclusion of competing frameworks from legitimate constitutional conversation. Theater ratio is moderate (0.42) because proceduralism does perform real constraint-on-discretion work (judges really do feel bound by text and procedure), but empirical studies show strong correlation between judicial outcomes and appointing president, suggesting outcome-determination beneath the procedural facade. The measurement series show extractiveness rising from 0.38 to 0.55 over the first 40 time units (30-year period), then stabilizing—modeling the period when the reading was gaining dominance through Court appointments and academic influence, then reaching a plateau where it became established doctrine. Theater ratio rises in parallel, modeling the increased disjunction between the reading's claimed constraint on discretion and evidence of outcome-determinativeness as the winning coalition consolidated power.
 *
 * PERSPECTIVAL GAP:
 *   From the proceduralist judge's seat, the reading is a neutral, constraining doctrine that protects rule of law. From the victim's seat (substantive justice claimant, historically excluded group), the same structure appears as enforced exclusion dressed in procedural language. The engine computes per-seat classification divergence from the structural data: the beneficiary and agenda-setter seats will classify the constraint differently from payer and powerless seats because their exit options and power levels are asymmetric. The reading's claim as tangled_rope depends on this divergence—the same structure solves coordination for some while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional stability advocates and proceduralist judges benefit from the reading (it validates their preferred interpretive methodology and claims neutrality for their outcomes), so their directionality is low (~0.2–0.3, closer to beneficiary end). Ruling coalitions benefit most directly (power to control the meaning of the Constitution once appointments or amendments are won), so their directionality is lowest (~0.1). Substantive justice claimants and marginalized groups bear the cost of exclusion (their claims are systematically foreclosed), so their directionality is high (~0.85–0.95, closer to target end). Exit options amplify the asymmetry: institutional actors have arbitrage (they can shift to originalism if positivism becomes politically costly); payers are trapped (substantive justice claims have no alternative constitutional path if this reading forecloses them). Moral philosophers and rights theorists have some mobility (they can publish and advocate outside the judiciary) but are excluded from constitutional conversation inside it. The directionality spread is wide, producing significant per-seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: judges needed a principle for constraining discretion without substituting their own policy preferences for the text. The positivist reading appeared to solve it by binding interpretation to procedure. However, empirical and comparative evidence suggests the problem may have expired or transformed: modern constitutional systems (including comparative democracies) allow both textual constraint AND moral reasoning without collapse into pure judicial discretion. The reading's function has shifted from solving a genuine coordination problem to a pure extraction mechanism—it locks in the outcomes of whatever coalition controls appointments without requiring renewed democratic authorization. This is mandatrophy: the founding problem's solution has outlived the problem and become a vehicle for entrenchment. The reading is sustained by institutional inertia and theater (the appearance that procedure constrains discretion) rather than by any live coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_vs_substance_separability,
    'Can constitutional validity be determined by procedure alone, or does the exclusion of moral reasoning inherently privilege particular substantive outcomes?',
    'Empirical study of outcomes under positivism vs. alternative frameworks (originalism, living constitutionalism, natural law); comparative analysis of constitutional systems that permit moral reasoning alongside procedural constraint.',
    'If procedure and substance are inseparable, the positivist reading''s claim to neutrality is false and the constraint is pure extraction (locking in outcomes of ruling coalitions). If separable, the coordination benefit is real but may be achievable with less extraction through hybrid frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_vs_substance_separability, conceptual, 'Whether proceduralism without moral substance is coherent or necessarily disguises outcome-driven reasoning.').

omega_variable(
    founding_problem_expiration,
    'Is the founding problem—judicial discretion threatening rule of law—still live, or has it been superseded by a different problem: how to update the Constitution when the amendment process is broken?',
    'Historical analysis of when the amendment process became effectively gridlocked (arguably: 1975 onward); comparative study of how other democracies solve discretion + obsolescence simultaneously; analysis of whether the reading''s operation today primarily constrains discretion or primarily locks in past outcomes.',
    'If the problem is live, the reading delivers real coordination benefit. If expired, the reading is pure extraction—mandatrophy. If transformed, the reading solves the wrong problem and a hybrid framework (procedure + limited moral reasoning) would better serve both coordination and substantive justice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_expiration, empirical, 'Whether the positivist reading''s founding problem persists or has become obsolete.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of moral reasoning structural (constitutional law genuinely forbids it) or internalized (judges believe moral reasoning is forbidden and self-censor, even though the Constitution does not explicitly prohibit it)?',
    'Post-suppression trajectory: if judges who exit the judiciary (retire, move to academia) suddenly permit moral reasoning in their reasoning, the suppression was internalized rather than structural. Comparative study of judicial cultures in democracies where moral reasoning is permitted to see if outcomes differ.',
    'If structural, the constraint is harder to change (requires doctrinal shift). If internalized, the constraint''s persistence depends on continued training and socialization; exit options improve if suppression is recognized as internalized rather than inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of moral reasoning is structural or internalized in judicial culture.').

omega_variable(
    moral_reasoning_kernel_ambiguity,
    'Is the positivist reading incompatible with living constitutionalism as a matter of logical necessity (forecloses), or do both readings represent live policy choices that can coexist in different institutional spaces?',
    'Jurisprudential analysis: if originalism, positivism, and living constitutionalism share a core commitment (constraining judicial discretion) and differ only on the anchoring mechanism (history, procedure, or evolving values), coexistence is possible. If positivism''s core premise (procedure exhausts legitimacy) contradicts living constitutionalism''s core premise (values evolve), foreclosure occurs.',
    'If coexistence is possible, the contest between readings is a live policy dispute, not a logical contradiction. If foreclosure occurs, the tripartite contest may resolve into a binary (positivism+originalism vs. living constitutionalism). The classification (coexists_with vs. forecloses) in cs_structure.reading_relations depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_reasoning_kernel_ambiguity, conceptual, 'Whether positivism and living constitutionalism can coexist or logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(us_c_be_t50, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(us_c_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(us_c_su_t50, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__positivist_reading, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(us_c_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The positivist reading is one of three structurally distinct readings of the kernel 'us_constitution_meaning'. All three readings share the same textual referent (the U.S. Constitution) but instantiate different constraints because they define constitutional validity differently. Positivism = procedure + institutional authority; Originalism = historical public meaning; Living Constitutionalism = evolved principles. The three readings have different ε values, different beneficiary/victim structures, and different threat surfaces. They are NOT three perspectives on the same constraint; they are three separate constraints sharing a kernel. Each reading must be authored with its own ε, its own stakeholder structure, and its own six-questions interview. This story authors the positivist reading only. The originalist and living-constitutionalist readings are separate JSON files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
