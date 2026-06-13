% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial-supremacy reading of the
 *   contested constitutional-text kernel. The reading asserts that
 *   constitutional text grants courts final interpretive authority: once
 *   courts invalidate legislation as unconstitutional, that determination is
 *   conclusive and cannot be reversed by legislative action (short of
 *   constitutional amendment). This reading competes with
 *   legislative-sovereignty and popular-sovereignty readings of the same
 *   kernel. Each reading produces a structurally different constraint with
 *   different beneficiaries, victims, and extraction profiles. This story
 *   models judicial supremacy as a tangled rope: it coordinates
 *   constitutional interpretation (real coordination function) while
 *   asymmetrically extracting democratic responsiveness from legislatures and
 *   majoritarian coalitions (real extraction). The constraint's persistence
 *   depends on active enforcement — courts must continuously invalidate
 *   legislation and maintain immunity from legislative override.
 *
 * KEY AGENTS:
 *   - Courts and judiciary: institutional gatekeeper, sets the interpretive rules, collects no direct rents but holds monopoly power
 *   - Rights-claimants against majoritarian overreach: beneficiaries who rely on courts to protect them from majoritarian legislation
 *   - Legislatures and elected representatives: payers who face judicial veto without override power
 *   - Majoritarian coalitions: payers whose electoral will is constrained by judicial determinations
 *   - Competing interpretive traditions (legislative sovereignty, popular sovereignty): excluded voices advocating alternative readings
 *   - Constitutional amendment actors: structural override mechanism, but prohibitively costly to activate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '07d68d47-cf38-450d-a613-deea62b51d32').
narrative_ontology:cs_kernel_codification('07d68d47-cf38-450d-a613-deea62b51d32', fixed_text).
narrative_ontology:cs_authority_grounding('07d68d47-cf38-450d-a613-deea62b51d32', extraction).
narrative_ontology:cs_interpretation_layer_present('07d68d47-cf38-450d-a613-deea62b51d32').
narrative_ontology:cs_reading_relation('07d68d47-cf38-450d-a613-deea62b51d32', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('07d68d47-cf38-450d-a613-deea62b51d32', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('07d68d47-cf38-450d-a613-deea62b51d32', foundational, judicial_finality_in_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('07d68d47-cf38-450d-a613-deea62b51d32', judicial_finality_in_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('07d68d47-cf38-450d-a613-deea62b51d32', foundational, legislative_override_impossibility).
narrative_ontology:cs_axiom_status(legislative_override_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('07d68d47-cf38-450d-a613-deea62b51d32', legislative_override_impossibility, deontological).
narrative_ontology:cs_reference_frame('07d68d47-cf38-450d-a613-deea62b51d32', constitutional_text_grants_courts_final_interpretive_authority).
narrative_ontology:cs_drift_state('07d68d47-cf38-450d-a613-deea62b51d32', contemporary_institutional_evolution, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('07d68d47-cf38-450d-a613-deea62b51d32', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_legislative_responsiveness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislature_and_elected_representatives).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, majoritarian_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds formal authority to interpret the constitution and invalidate legislation. Sets rules governing constitutional meaning through judicial review and precedent. Justifies this role as necessary to protect constitutional limits and individual rights against majoritarian erosion. Maintains the constraint through continuous enforcement — accepting constitutional challenges, reviewing legislation, and invalidating laws found unconstitutional. Can theoretically exit only through reversal of precedent (analytical, within their own power).
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and minority groups seeking judicial protection against majoritarian legislation. Benefit from having a final arbiter outside electoral cycles that can strike down laws harming their rights. Their exit option is political (lobbying for legislative change or constitutional amendment), which requires sustained coalition-building across many states and is structurally expensive relative to a single favorable court decision.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants_against_majoritarian_overreach, beneficiary,
    moderate, biographical, constrained, national).

% Enacts legislation that courts can invalidate without legislative override. Faces the constraint that judicial invalidation is final — they cannot restore a struck-down law through simple majority vote. Exit requires constitutional amendment (supermajority in both chambers plus state ratification), which is prohibitively costly. Argues the constraint removes democratic responsiveness to constituent preferences about constitutional scope.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislature_and_elected_representatives, payer,
    institutional, generational, constrained, national).

% Electoral coalitions that support legislation struck down as unconstitutional. Experience the constraint as a veto on their political will — they elect representatives but cannot guarantee the laws pass judicial review. Exit is constitutional amendment (supermajority requirement), which requires sustained multi-generational commitment and is structurally prevented from being responsive to electoral swings.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, majoritarian_coalitions, payer,
    organized, biographical, constrained, national).

% Advocates of legislative sovereignty and popular sovereignty readings are formally excluded from the interpretive process once courts have ruled. They would argue that the constitutional text permits alternative readings — legislative override through notwithstanding clauses, or constituent amendment through simpler democratic procedures — but the judicial supremacy constraint forecloses these alternatives in jurisdictions where courts maintain supremacy. Cannot influence constitutional meaning once courts speak (except through constitutional amendment).
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, competing_interpretive_traditions, excluded,
    institutional, generational, trapped, national).

% The supermajority coalitions required to amend the constitution. Represent the theoretical override mechanism available to all seats — through amendment, any provision including judicial supremacy could be changed. Sit outside the constraint's normal operation (they rarely assemble), but represent the ultimate structural check on judicial power. Amendment costs are prohibitively high, making this exit path theoretical rather than practical.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_amendment_actors, observer,
    powerful, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, courts_and_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable, authoritative interpretation of constitutional text — prevents courts from contradicting each other, prevents legislative reinterpretation of settled law, and creates a predictable rule of law that citizens and officials can rely on. Solves the coordination problem of what the constitution means when different institutions disagree.
% TRANSFER_FUNCTION: Transfers interpretive power from democratic legislatures (and theoretically the people through electoral will) to courts, who claim final authority. The constraint extracts from majoritarian coalitions the ability to override judicial determinations of unconstitutionality, and from legislatures the power to re-legislate around court decisions on constitutional grounds.
% ABSENT_VOICES: Proponents of legislative sovereignty and popular sovereignty readings are structurally excluded — they would argue the constitutional text permits courts to be overridden or that amendment procedures should be more accessible. Ordinary citizens whose policy preferences are nullified by courts (not rights-claimants seeking judicial protection, but majorities seeking to enact legislation) have no formal voice in the interpretive process.
% DISAPPEARANCE_RATIONALE: If judicial supremacy disappeared and the constraint lifted, the constitutional system would reorganize: legislatures would claim power to overrule courts, amendment procedures would shift, rights protections would depend on electoral coalitions rather than judicial guarantees. The entire structure of constitutional government would reorient around a different authority hierarchy.
% FOUNDING_PROBLEM: Early constitutional systems faced interpretive chaos: different institutions claimed authority to interpret the constitution, courts and legislatures contradicted each other, constitutional meaning shifted with political winds. The problem was regulatory uncertainty — no stable way to know what the constitution requires.
% FOUNDING_PROBLEM_CORROBORATION: Courts and rights advocates affirm the founding problem is still live, citing ongoing threats to constitutional protections. Legislatures and democratic theorists contest that the founding problem justifies permanent judicial supremacy, arguing that stability can be achieved through other means (legislative negotiation, clearer constitutional text, more accessible amendment procedures). Legal scholars and constitutional theorists from outside the beneficiary set provide evidence both ways — the problem is genuinely contested across the discipline.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end because the constraint transfers interpretive power permanently from legislatures to courts without corresponding democratic accountability. The trajectory rises from 0.45 to 0.68 as courts accumulate case law, precedent, and institutional confidence — extraction increases as judicial power grows more consolidated. Suppression is 0.71 because the constraint actively forecloses legislative override (the core enforcement dynamic) and makes constitutional amendment the only formal alternative (prohibitively expensive, structurally suppressed). Theater ratio grows from 0.20 to 0.42 because over time, courts increasingly justify their decisions in terms of fidelity to constitutional text, even as their decisions increasingly reflect judge-made doctrine and precedent that diverge from the original constraints on interpretation. The measured theater increase reflects the growing performative gap between the supremacy framing (courts discovering constitutional meaning) and the institutional reality (courts making constitutional meaning). Measurements are aligned on one shared time grid at every point for every metric, satisfying the alignment rule (OQ-83 rider). The stabilization at t=40 reflects a mature institutional equilibrium where the supremacy reading has become normalized and resistance has hardened into established pathways (amendment, legislative negotiation within judicially-set bounds) rather than direct contestation.
 *
 * PERSPECTIVAL GAP:
 *   Courts experience the constraint as coordination and institution-building (necessary protection of constitutional limits); legislatures experience it as extraction and subordination (permanent veto without override). This divergence arises from their structural positions: courts hold the power to interpret and courts set the rules, while legislatures face the cost of vetoes they cannot overturn. The engine computes this divergence from power atoms, beneficiary/victim declarations, and exit options — the authored claim (tangled rope) reflects both readings simultaneously, which is the definition of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts are structural beneficiaries: they hold interpretive authority, set the rules of constitutional meaning, face no legislative override, and operate from a seat of institutional power with analytical exit (they can in principle always reverse their own decisions through new precedent). Directionality for courts should be near 0.0 (full beneficiary), computing negative effective extraction. Rights-claimants are asymmetric beneficiaries: they benefit from judicial protection but bear diffuse indirect costs (reduced responsiveness to their preferences on non-rights issues). Legislatures and majoritarian coalitions are clear targets: they pay the cost of judicial veto, have constrained exit (they can amend, but the cost is supermajority + state ratification), and operate from an institutional seat that is formally subordinate in constitutional interpretation. Directionality for legislatures should be near 1.0 (full target), computing high effective extraction. The asymmetry between court and legislature is the core structural fact that drives the tangled-rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Judicial supremacy faces a classic mandatrophy problem: its founding mandate was to prevent interpretive chaos and protect constitutional limits against majoritarian drift. Over time (modeled in the measurement series), the constraint's coordination function (stable, predictable constitutional meaning) remains genuine but an increasing share of enforcement energy goes to defending judicial power itself (theater ratio rises) rather than preventing interpretive chaos (which is now taken for granted). At t=0, extractiveness is 0.45 because the founding mandate is live and recent — chaos prevention is still felt as a real problem. At t=50, extractiveness is 0.68 because the mandate has succeeded (constitutional meaning is stable, chaos is not a live threat), but the constraint persists and extracts because courts have institutional interest in maintaining interpretive supremacy. The theater ratio rising from 0.20 to 0.42 models this drift: increasing share of court activity is narrative maintenance (justifying supremacy through originalism, living constitutionalism, or other interpretive theory) rather than chaos prevention. A divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges signals mandatrophy: removal would rearrange the system (suggesting the constraint is not natural), but the founding problem is contested (suggesting its urgency has faded or shifted). The constraint should trigger a mandatrophy review because the founding problem (interpretive chaos) is now historical, not contemporary, but the institutional extraction (courts controlling meaning) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_judicial_oligarchy,
    'At what point does judicial monopoly on constitutional interpretation shift from coordination (preventing chaos) to institutional oligarchy (courts protecting their own power)?',
    'Empirical measurement of the gap between court decisions that reverse precedent (indicating courts are authoritative) versus court decisions that reaffirm precedent (indicating precedent is path-dependent and courts defer to institutional inertia). If reversal rates drop toward zero over time, courts are functioning as precedent-bounded ratchets, not as genuine arbiters of constitutional meaning.',
    'High reversal rates support the coordination reading (courts genuinely arbitrate); low reversal rates support the oligarchy reading (courts protect precedent and their own power). The mandatrophy trajectory (extraction rising as founding problem recedes) is consistent with oligarchy emergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_judicial_oligarchy, empirical, 'Whether judicial supremacy maintains its coordination function or degrades into institutional self-protection.').

omega_variable(
    alternative_coordination_mechanisms,
    'Could constitutional stability be achieved through mechanisms other than judicial supremacy — for example, legislative negotiation on constitutional meaning, clearer constitutional text, or more accessible amendment procedures?',
    'Comparative institutional analysis from jurisdictions with different constitutional authority arrangements (e.g., Canada with notwithstanding clauses, UK with parliamentary sovereignty, Switzerland with direct democratic amendment). Do they achieve comparable stability with different institutional arrangements?',
    'If alternative arrangements achieve comparable stability, judicial supremacy is not a necessary coordination solution and is better classified as pure extraction. If alternatives achieve worse stability, the coordination function is genuinely necessary and the constraint is correctly classified as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Whether judicial supremacy is the only viable solution to constitutional interpretation coordination, or whether alternatives exist.').

omega_variable(
    reading_foreclosure_logic,
    'Does the judicial-supremacy reading logically foreclose the legislative-sovereignty and popular-sovereignty readings, or do all three readings remain coherent within different commitment frameworks?',
    'Conceptual analysis: the judicial-supremacy reading asserts courts have FINAL authority. The legislative-sovereignty reading asserts legislatures have final authority. These are logical contradictions within a single framework — only one can be true. The popular-sovereignty reading asserts the people retain ultimate authority through amendment or convention, which is compatible with either courts or legislatures having day-to-day supremacy but subordinate to the people''s power to amend. The question is whether this is foreclosure or coexistence.',
    'If judicial supremacy logically forecloses legislative sovereignty (because final authority cannot be in both places), the reading_relations should be ''forecloses'' rather than ''coexists_with''. If all three readings represent different legitimate readings of ambiguous constitutional text (rather than logical contradictions), they coexist. The answer determines whether this is an ongoing institutional contest or a resolved hierarchy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_logic, conceptual, 'The logical relationship between the three competing readings of constitutional authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural — external barriers that prevent legislatures from overriding courts (amendment supermajority, political costs of amendment movements) — or internalized — legislatures have accepted judicial supremacy as legitimate and no longer attempt to override?',
    'Historical analysis of legislative override attempts: if legislatures frequently attempt to override courts and are blocked by structural barriers, suppression is structural. If legislatures rarely attempt override (even when they have the political power), suppression is partially internalized. The shift from high attempted-override rates to low attempted-override rates over the interval would indicate internalization (the constraint becomes self-reinforcing as legislatures normalize subordination).',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests — the constraint has changed how legislatures think, not just what they can do. This would support reclassification toward snare (if internalization is high) or confirm tangled rope (if suppression remains partially structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The structural vs. internalized composition of the constraint''s suppression mechanism.').

omega_variable(
    kernel_reading_vs_institutional_lock_in,
    'Is the judicial-supremacy reading a defensible interpretation of the constitutional text, or has it become an institutional lock-in where the reading is maintained because courts have power to maintain it, rather than because the text compels it?',
    'Close textual analysis of the constitutional document by scholars outside the judiciary (independent constitutional theorists, legal historians, comparative constitutionalists). If the text is genuinely ambiguous and admits multiple readings, the constraint is a reading. If the text has been reinterpreted by courts over time to support their power, and the original text was ambiguous or unsettled on this point, the constraint is institutional lock-in disguised as interpretation.',
    'If the reading is genuinely textually grounded, it should be stable across time and jurisdictions with similar constitutional text. If it is institutional lock-in, it should vary with institutional power dynamics and be difficult to defend through pure textual argument. High variance across jurisdictions would indicate lock-in rather than reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_institutional_lock_in, conceptual, 'Whether judicial supremacy is a reading of the constitutional text or an institutional artifact that has captured the text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__judicial_supremacy_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__judicial_supremacy_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__judicial_supremacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__judicial_supremacy_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(cons_tr_t32, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(cons_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text__judicial_supremacy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text__judicial_supremacy_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text__judicial_supremacy_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t32, constitutional_text__judicial_supremacy_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(cons_be_t32, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(cons_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text__judicial_supremacy_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text__judicial_supremacy_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text__judicial_supremacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t32, constitutional_text__judicial_supremacy_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cons_su_t32, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(cons_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'constitutional_text'. The sibling readings (legislative_sovereignty_reading, popular_sovereignty_reading) are separate constraint stories with different ε values, beneficiary structures, and type classifications. All three stories are linked via network.affects_constraints because they instantiate competing interpretations of the same constitutional kernel. The decomposition follows ε-invariance (OQ-64): each reading produces a structurally different constraint — judicial supremacy creates extraction from legislatures and majoritarian coalitions (ε ≈ 0.68), legislative sovereignty would create different extraction (from courts and rights-claimants, ε lower), popular sovereignty would create yet different extraction (diffuse across all institutional seats). Single-constraint treatment of the kernel would fabricate averaging across readings, which would destroy the measurement function. Separate stories per reading preserve the structural distinctness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
