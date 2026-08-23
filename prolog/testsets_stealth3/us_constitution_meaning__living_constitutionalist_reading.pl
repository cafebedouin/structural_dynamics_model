% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living-Constitutionalist Reading of US Constitutional Meaning
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   The constraint under classification is the living-constitutionalist
 *   practice of American constitutional interpretation: constitutional
 *   principles are treated as enduring, but their application legitimately
 *   evolves with social attitudes and circumstances, with the federal
 *   judiciary as the authorized site of adaptation. This story instantiates
 *   ONE reading of the us_constitution_meaning kernel; the originalist and
 *   positivist readings are separate constraint stories with their own
 *   epsilon values, beneficiary structures, and classifications — per the
 *   epsilon-invariance principle, nothing is averaged across readings here.
 *   The referent of epsilon is this reading's standing arrangement (the
 *   adaptive-interpretation practice as it actually operates), assessed by
 *   the reading's own lights: the reading endorses the practice, and its
 *   honest accounting still records a real counter-majoritarian transfer of
 *   decision authority.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and beneficiary (institutional/identity_locked) — administers the interpretive practice and collects the decision authority it concentrates
 *   - rights_claimant_movements: primary beneficiary (organized/constrained) — receives enforceable protections through evolving application after political channels close
 *   - majoritarian_governance_branches: primary target (institutional/constrained) — bears the counter-majoritarian transfer; statutes struck, electoral deliverables lost without recourse
 *   - future_generations_bound_by_adaptations: excluded target (powerless/trapped) — inherits interpretive settlements without consent and cannot exit
 *   - legal_academy: analytical observer (analytical/analytical) — sees the full structure; source of both the counter-majoritarian objection and the elite-consensus critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.45).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living-Constitutionalist Reading of US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, 'a8012bdd-5b31-4e2e-8cf0-311e64979402').
narrative_ontology:cs_kernel_codification('a8012bdd-5b31-4e2e-8cf0-311e64979402', fixed_text).
narrative_ontology:cs_authority_grounding('a8012bdd-5b31-4e2e-8cf0-311e64979402', lineage).
narrative_ontology:cs_interpretation_layer_present('a8012bdd-5b31-4e2e-8cf0-311e64979402').
narrative_ontology:cs_reading_relation('a8012bdd-5b31-4e2e-8cf0-311e64979402', us_constitution_meaning__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a8012bdd-5b31-4e2e-8cf0-311e64979402', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('a8012bdd-5b31-4e2e-8cf0-311e64979402', foundational, principles_endure_applications_evolve).
narrative_ontology:cs_axiom_status(principles_endure_applications_evolve, holdable).
narrative_ontology:cs_axiom_grounding('a8012bdd-5b31-4e2e-8cf0-311e64979402', principles_endure_applications_evolve, conventional).
narrative_ontology:cs_axiom('a8012bdd-5b31-4e2e-8cf0-311e64979402', secondary, contemporary_moral_consensus_is_relevant).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_is_relevant, holdable).
narrative_ontology:cs_axiom_grounding('a8012bdd-5b31-4e2e-8cf0-311e64979402', contemporary_moral_consensus_is_relevant, instrumental).
narrative_ontology:cs_reference_frame('a8012bdd-5b31-4e2e-8cf0-311e64979402', enduring_principles_adaptive_application).
narrative_ontology:cs_drift_state('a8012bdd-5b31-4e2e-8cf0-311e64979402', contemporary_originalist_ascendancy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a8012bdd-5b31-4e2e-8cf0-311e64979402', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimant_movements).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, majoritarian_governance_branches).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, future_generations_bound_by_adaptations).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, principle_application_distinction).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, contemporary_moral_consensus_relevance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured judges who decide what the Constitution's principles require in each generation's circumstances. Their interpretations bind Congress, the president, the states, and every litigant; nothing short of a two-thirds-plus-three-quarters amendment supermajority or the judges' own reversal displaces a settled reading. Members are appointed into the practice and their professional identity is built on exercising interpretive discretion — the office is the interpretation. They receive the decision-making authority the practice concentrates, along with its prestige and its backlash.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Organized movements — racial equality, gender equality, disability rights, sexual-minority rights — that turn to the courts after electoral and legislative channels close. Evolving application converts their claims into enforceable protections the political branches had refused: school integration, workplace protections, marriage recognition. Their access runs through persuading judges, not winning elections; if the interpretive practice narrows, their channel narrows with it.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimant_movements, beneficiary,
    organized, biographical, constrained, national).

% Elected Congresses, presidents, and state governments that enact their coalitions' programs and then see courts revise or strike them under updated readings of enduring principles. Ordinary legislation cannot overturn a settled interpretation, and the amendment threshold is rarely reachable, so each overridden statute is an electoral deliverable lost without recourse. Their remedies are slow: waiting for retirements, shaping appointments, or fighting case by case.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, majoritarian_governance_branches, payer,
    institutional, immediate, constrained, national).

% People not yet born who will inherit today's interpretive settlements — expanded rights, redrawn federal-state balances, precedents hardened into doctrine — without consenting to any of them. Every adaptation is issued partly in their name, on the claim that the Charter belongs to each generation alike; they cannot object, vote, or exit short of emigration.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, future_generations_bound_by_adaptations, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, future_generations_bound_by_adaptations, payer).

% Constitutional scholars, legal historians, and comparativists who study how the interpretive practice actually operates, test its justifications against the archival record, and supply the vocabulary both its defenders and its critics use. They collect nothing from the practice and bear none of its costs; the counter-majoritarian objection and the elite-consensus critique were both systematized here.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single supreme written law authoritative across centuries while letting its application track changed circumstances: one constitutional framework absorbs social change peacefully instead of forcing a choice between rigid eighteenth-century governance and perpetual formal amendment or rupture.
% TRANSFER_FUNCTION: Moves ultimate decision authority over contested social questions from the elected branches and voting publics to the federal judiciary, and moves legal recognition and protection to rights claimants whose claims the political process had declined to honor.
% ABSENT_VOICES: Future generations bound by today's settlements cannot object; citizens of states whose preferred policies are preempted by national judicial standards have voice only through the very institutions being overridden; originalist and positivist critics argue loudly in public but stand outside the interpretive operation itself — their objections do not bind the practice they contest.
% DISAPPEARANCE_RATIONALE: If the practice vanished overnight, several hundred settled doctrinal outcomes — incorporation, privacy, equal-protection application, federal preemption balances — would reopen simultaneously; rights claimants would lose their principal channel and shift to politics or protest; the legal system would reorganize around either fixed historical meaning or bare procedural validity; and the appointment conflicts that dominate judicial politics would change character entirely.
% FOUNDING_PROBLEM: How a republic keeps an eighteenth-century text as binding supreme law while the circumstances it must govern — corporations, mass media, civil equality, a national economy — are ones the framers did not and could not specify; the founders themselves wrote broad principles and expected interpretation to carry the load.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: ratification-era writings (Hamilton and Madison on interpretation and change) attest the design intent; comparative constitutional scholarship documents the same endurance-adaptation tension in every written constitution; and originalist scholars — adversaries of this reading — concede the tension is real while disputing the solution. No party claims the problem is solved.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope, stated independently of the metrics: the practice has a genuine coordination function (one enduring framework absorbing social change without perpetual amendment crises or rupture) AND an asymmetric transfer (ultimate decision authority moves from elected institutions to life-tenured judges; losers in rights conflicts pay without exit), held together by active enforcement (binding judicial review). Metrics are authored descriptively: extractiveness 0.45 — a substantial but bounded transfer, purchased by real coordination value and not primarily rent-seeking; suppression 0.42 — rulings are coercively binding once issued, but alternatives (Article V amendment, appointment politics, doctrinal reversal, the sibling readings themselves) remain visible and partly usable, so suppression sits well below snare levels; theater_ratio 0.28 — 'enduring principles' rhetoric sometimes covers policy preference, but the interpretive function remains mostly real; accessibility_collapse 0.30 — competing interpretive theories survive openly (they are the sibling constraints), so alternatives do not collapse; resistance 0.62 — a sustained originalist counter-movement, recurring court-curbing proposals, and permanent appointment warfare meet the practice continuously. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. The temporal series share one grid (t = 0,15,30,45,60,75,90) with every tracked metric authored at every point; the mid-interval extractiveness peak corresponds to the high-adaptation decades, followed by partial consolidation and rising contestation. Boltzmann coordination_type is enforcement_mechanism: the practice is a governance structure allocating interpretive authority, maintained by dedicated enforcement infrastructure (judicial review).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal_judiciary seat the arrangement is the practice it embodies: adaptation is not something done TO judges but what judging IS, and its identity_locked exit (life tenure fused with interpretive discretion — the office is the interpretation) makes the seat experience the constraint as self-constituting rather than imposed. From the majoritarian_governance_branches seat the same structure operates as a standing veto exercised by unelected officials over electoral deliverables, with exit limited to decade-scale appointment strategies. Rights-claimant seats experience it as subsidy: a channel that opens exactly where political channels close. If the judiciary's identity frame broke — if judges came to see interpretive discretion as delegable or illegitimate — the practice would migrate toward the originalist instantiation and this constraint's beneficiary structure would invert.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. federal_judiciary (beneficiary plus agenda_setter, identity_locked) sits near the beneficiary end — it collects the concentrated decision authority — though not at zero, since it bears backlash and legitimacy erosion. rights_claimant_movements (beneficiary, constrained exit: courts are the channel precisely because political channels closed) sit low. majoritarian_governance_branches (payer, institutional power but constrained exit: ordinary legislation cannot overturn a settled reading and the amendment threshold is rarely reachable) sit high — powerful agents with poor exits are classic high-directionality targets. future_generations_bound_by_adaptations (payer by inheritance, trapped) sit nearest the full-target end. No directionality overrides are authored: the beneficiary/victim declarations plus exit differentiation already separate the seats, and the two institutional actors (judiciary versus elected branches) differ in declared structural position, not merely in power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an old text governing unforeseen circumstances — is live by construction: every generation reproduces it, so the arrangement cannot have outlived its mandate. Mandatrophy is therefore not resolved, and the mismatch consumer should find status=live paired with verdict=world_rearranges: arrangements demonstrably depend on the practice, so no zombie flag fires. The theater_ratio (0.28, rising slowly) is the number to watch: if principle-rhetoric decoupled further from doctrinal function while the transfer persisted, the practice would drift toward performance maintenance — but at authored values the interpretive function remains load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the living_constitutionalist_reading of the us_constitution_meaning kernel — how much of the classification is indexical to this reading rather than to ''the Constitution''s meaning'' as such?',
    'Compare the linked sibling stories (originalist_reading, positivist_reading): shared topic, different epsilon, different beneficiary/victim structures, different types. Divergence across siblings is the signal that the colloquial label ''constitutional meaning'' conflates structurally distinct constraints.',
    'If read as a property of the kernel rather than of this reading, the corpus would average incompatible arrangements; the classification here is valid only per-reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    elite_vs_mass_consensus_divergence,
    'Does judicial application actually track contemporary moral consensus, or an elite legal-class consensus that diverges from mass opinion on the very questions courts decide?',
    'Time-series alignment of landmark rulings against contemporaneous mass polling on the same questions; diffusion studies comparing doctrinal change with attitude change across demographic strata.',
    'If elite-divergent, the consensus justification weakens, the beneficiary structure narrows to litigation-capable groups, and effective extraction on the majoritarian seat rises above the authored estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_vs_mass_consensus_divergence, empirical, 'Whose consensus does the adaptation track?').

omega_variable(
    counter_majoritarian_cost_allocation,
    'Where does the counter-majoritarian cost actually land — as a diffuse democratic-process discount spread across all voters, or concentrated on the specific coalitions whose statutes are struck?',
    'Trace the populations affected by struck statutes and their political weight across the interval; compare the welfare effects of overridden statutes against the protections substituted for them.',
    'Concentrated allocation supports treating the majoritarian branches as a true payer seat; diffuse allocation pushes the arrangement toward a coordination-with-overhead profile and lowers the target-seat directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_cost_allocation, empirical, 'Distribution of the counter-majoritarian burden across the governed.').

omega_variable(
    adaptation_overreach_boundary,
    'Is there a principled boundary separating legitimate adaptation of application from judicial overreach, or is the boundary in practice whatever a current court majority accepts?',
    'Doctrinal analysis of the limiting criteria courts themselves invoke (textual anchoring, precedent discipline, neutrality principles) and whether those criteria actually constrain decisions they would in principle forbid.',
    'If no operative boundary exists, the coordination function degrades toward open-ended authority transfer and the extraction estimate rises; a working boundary supports the hybrid coordination-plus-transfer reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_overreach_boundary, conceptual, 'Whether adaptation operates under a limiting principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_living_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(us_const_living_tr_t0, observed).
narrative_ontology:measurement(us_const_living_tr_t15, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(us_const_living_tr_t15, observed).
narrative_ontology:measurement(us_const_living_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(us_const_living_tr_t30, observed).
narrative_ontology:measurement(us_const_living_tr_t45, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(us_const_living_tr_t45, observed).
narrative_ontology:measurement(us_const_living_tr_t60, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(us_const_living_tr_t60, observed).
narrative_ontology:measurement(us_const_living_tr_t75, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 75, 0.27).
narrative_ontology:measurement_basis(us_const_living_tr_t75, observed).
narrative_ontology:measurement(us_const_living_tr_t90, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(us_const_living_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(us_const_living_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(us_const_living_be_t0, observed).
narrative_ontology:measurement(us_const_living_be_t15, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(us_const_living_be_t15, observed).
narrative_ontology:measurement(us_const_living_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(us_const_living_be_t30, observed).
narrative_ontology:measurement(us_const_living_be_t45, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement_basis(us_const_living_be_t45, observed).
narrative_ontology:measurement(us_const_living_be_t60, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement_basis(us_const_living_be_t60, observed).
narrative_ontology:measurement(us_const_living_be_t75, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 75, 0.47).
narrative_ontology:measurement_basis(us_const_living_be_t75, observed).
narrative_ontology:measurement(us_const_living_be_t90, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(us_const_living_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_const_living_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(us_const_living_su_t0, observed).
narrative_ontology:measurement(us_const_living_su_t15, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement_basis(us_const_living_su_t15, observed).
narrative_ontology:measurement(us_const_living_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(us_const_living_su_t30, observed).
narrative_ontology:measurement(us_const_living_su_t45, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 45, 0.43).
narrative_ontology:measurement_basis(us_const_living_su_t45, observed).
narrative_ontology:measurement(us_const_living_su_t60, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement_basis(us_const_living_su_t60, observed).
narrative_ontology:measurement(us_const_living_su_t75, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(us_const_living_su_t75, observed).
narrative_ontology:measurement(us_const_living_su_t90, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(us_const_living_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Constitution means' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the us_constitution_meaning kernel. Each has its own epsilon (this reading: moderate, ~0.45; the originalist reading suppresses adaptation and burdens rights claimants differently; the positivist reading relocates extraction to procedure-wielding institutions), its own beneficiary/victim structure, and its own classification. Stories are linked pairwise through affects_constraints. The originalist reading currently exerts upstream pressure on this one through appointment composition; this story records that pressure as drift_state.repudiation_pressure rather than as a change to its own epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
