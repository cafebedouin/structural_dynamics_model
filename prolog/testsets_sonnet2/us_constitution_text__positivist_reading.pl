% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Constitutional Validity as Formal Pedigree (Positivist Reading)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint isolates the positivist reading of the contested
 *   constitutional-validity kernel: a norm is constitutional law because it
 *   was enacted through the pedigreed procedure — proper ratification, valid
 *   amendment under Article V, statutes properly derived from enacted text —
 *   regardless of its moral content or its framers' historical intent. This
 *   is a distinct structural claim from the originalist reading (which fixes
 *   meaning at ratification and asks judges to recover original public
 *   understanding) and from the living-constitutionalist reading (which asks
 *   judges to adapt principles to contemporary circumstances). All three
 *   share the same textual kernel — the Constitution as document — but
 *   instantiate different validity tests with different beneficiary/victim
 *   structures and different ε. This story authors ONLY the positivist
 *   reading's ε and structure; the siblings are separate constraint files
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - judicial_institution: Primary agenda-setter (institutional/analytical) — administers the pedigree test
 *   - legislative_drafters: Primary beneficiary (powerful/mobile) — gains a controllable lever for binding law via procedure alone
 *   - substantive_justice_claimants: Primary target (powerless/trapped) — moral claims without formal enactment gain no standing
 *   - constitutional_law_scholars: Analytical observer — traces whether the test explains or merely rationalizes outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.52).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.61).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Constitutional Validity as Formal Pedigree (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, 'd520b60e-44c6-41a6-9817-79e9ba2997d5').
narrative_ontology:cs_kernel_codification('d520b60e-44c6-41a6-9817-79e9ba2997d5', fixed_text).
narrative_ontology:cs_authority_grounding('d520b60e-44c6-41a6-9817-79e9ba2997d5', lineage).
narrative_ontology:cs_interpretation_layer_present('d520b60e-44c6-41a6-9817-79e9ba2997d5').
narrative_ontology:cs_reading_relation('d520b60e-44c6-41a6-9817-79e9ba2997d5', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d520b60e-44c6-41a6-9817-79e9ba2997d5', us_constitution_text__living_constitutionalist_reading, influences).
narrative_ontology:cs_axiom('d520b60e-44c6-41a6-9817-79e9ba2997d5', foundational, validity_from_procedure_alone).
narrative_ontology:cs_axiom_status(validity_from_procedure_alone, holdable).
narrative_ontology:cs_axiom_grounding('d520b60e-44c6-41a6-9817-79e9ba2997d5', validity_from_procedure_alone, conventional).
narrative_ontology:cs_axiom('d520b60e-44c6-41a6-9817-79e9ba2997d5', secondary, moral_content_irrelevant_to_legal_validity).
narrative_ontology:cs_axiom_status(moral_content_irrelevant_to_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('d520b60e-44c6-41a6-9817-79e9ba2997d5', moral_content_irrelevant_to_legal_validity, conventional).
narrative_ontology:cs_reference_frame('d520b60e-44c6-41a6-9817-79e9ba2997d5', hartian_rule_of_recognition).
narrative_ontology:cs_drift_state('d520b60e-44c6-41a6-9817-79e9ba2997d5', contemporary_textualist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d520b60e-44c6-41a6-9817-79e9ba2997d5', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_institution).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legislative_drafters).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, settled_property_and_contract_holders).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, unenacted_moral_minorities).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, litigants_relying_on_unwritten_principles).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies a source-validity test: a norm counts as constitutional law because it was enacted through the pedigreed procedure (Article V amendment, ratified text, valid statute under that text), not because of its moral content. This lets courts decide hard cases by tracing pedigree rather than adjudicating contested moral claims directly, which shields the institution from charges of imposing its own values and stabilizes its authority across changes in judicial personnel.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, judicial_institution, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, judicial_institution, beneficiary).

% Know that whatever they enact through the correct procedure will be treated as valid law regardless of its substantive fairness, so long as it clears the formal gates. This gives them a predictable, controllable lever: they do not need moral consensus, only procedural compliance, to bind the entire polity.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legislative_drafters, beneficiary,
    powerful, generational, mobile, national).

% Rely on the predictability that formally-enacted rules will be enforced regardless of shifting moral fashions. Their existing entitlements are protected precisely because validity does not turn on contestable substantive judgments about fairness or history.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, settled_property_and_contract_holders, beneficiary,
    organized, generational, constrained, national).

% Bring claims resting on moral or equitable grounds — a right they believe self-evidently just, or a historical wrong they believe demands remedy — but find courts unwilling to recognize the claim as constitutional law unless it can be traced to a formally enacted source. Their only path to redress is the amendment process or ordinary legislation, both of which require winning supermajorities or legislative majorities they may not have.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Hold positions that never achieved formal enactment — either because they were defeated in the ratification process or never brought to a vote. Under this reading, their moral claims have zero standing as constitutional law no matter how compelling, until and unless the formal process is completed in their favor.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, unenacted_moral_minorities, payer,
    powerless, biographical, trapped, national).

% Argue for rights grounded in natural law, tradition, or evolving societal consensus rather than enacted text. Under the positivist test these arguments are treated as policy advocacy directed at the amendment process, not as legal claims a court can vindicate directly — their exit is to relitigate as ordinary politics rather than constitutional right.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, litigants_relying_on_unwritten_principles, payer,
    moderate, immediate, constrained, national).

% Study whether source-based validity actually explains judicial decisions or merely rationalizes outcomes reached on other grounds, and track how the doctrine interacts with the sibling originalist and living-constitutionalist readings within the same textual kernel.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, procedure-based test for what counts as valid constitutional law, so that officials, courts, and citizens can identify binding law without first resolving contested moral or historical questions case by case.
% TRANSFER_FUNCTION: Moves interpretive authority away from claimants of unenacted moral or historical rights and toward whoever last cleared the formal enactment gate (ratifiers, legislatures, amendment supermajorities), and toward the judicial institution that administers the pedigree test.
% ABSENT_VOICES: Groups whose claims were defeated or never brought to a vote in the formal process have no standing under this test to have their claim recognized as constitutional law; they are structurally routed to future political contests rather than present adjudication.
% DISAPPEARANCE_RATIONALE: If courts abandoned source-validity as the test for constitutional law, judges would need some other basis (moral content, original meaning, evolving consensus) to decide hard cases — which is exactly what the sibling readings supply. Settled expectations built on 'the rule is valid because it was properly enacted' would become contestable on substantive grounds, destabilizing predictability that legislatures and property holders currently rely on.
% FOUNDING_PROBLEM: Legal systems need a way to distinguish binding law from mere moral or political argument without requiring every judge to personally adjudicate contested first-order moral questions in every case — otherwise judicial decisions collapse into unconstrained value judgments dressed as law.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivist scholars (Hart, and successors applying rule-of-recognition theory) attest the problem persists as a genuine jurisprudential need, independent of the judicial institution's own interest in appearing constrained; critics from natural-law and critical legal studies traditions — outside the beneficiary set — corroborate that the problem is real but argue the positivist solution masks substantive choices as procedural ones rather than eliminating them.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects that the positivist test systematically forecloses substantive-justice claims that cannot point to a formally enacted source, transferring interpretive authority to whoever controls the formal enactment process (legislatures, ratifying supermajorities) and to the judicial institution administering the test. Suppression (0.61) is higher than extraction because the test is actively enforced as a threshold matter — courts routinely dismiss substantive claims as non-justiciable or as 'political question' precisely because they lack formal pedigree, which is a coercive gate on argument, not merely a distributional effect. Theater ratio is moderate-low (0.28): the pedigree analysis is a real interpretive practice, not empty performance, though its rising trajectory reflects increasing formalization of pedigree-tracing doctrine (textualism, historical-practice tests) that partly serves to legitimate outcomes reached on other grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial institution's seat, source-validity is a genuine coordination solution: it lets courts decide cases without adjudicating first-order moral disputes, which is a real institutional good. From a substantive justice claimant's seat, the identical test operates as a formal barrier that launders a substantive value choice (deference to whatever was formally enacted, including past injustice) as a neutral procedural rule. The engine should compute divergent seat classifications from this same structural data — the tangled-rope claim asserts both the coordination function and the asymmetric extraction are real simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial institution and legislative drafters sit near the beneficiary end: the former administers and is legitimated by the test, the latter gains a reliable lever requiring only procedural compliance. Settled property and contract holders benefit from the predictability the test protects. Substantive justice claimants, unenacted moral minorities, and litigants relying on unwritten principles sit near the target end: their claims are structurally unrecognizable as constitutional law however compelling on the merits, and their only recourse — the amendment process or ordinary legislation — requires exactly the supermajority or majority power they typically lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist test was built to solve a genuine problem (unconstrained judicial value-imposition), and that problem remains live — so this is not simple mandatrophy where the founding function has vanished. But the test's operation is not neutral: it necessarily favors whoever already achieved formal enactment, including enactments reflecting past exclusion (e.g., a constitutional text ratified without full political participation of the groups it governs). Classifying this as tangled_rope rather than pure rope or pure snare avoids two errors: treating the coordination function as fake (it is real — courts need SOME test), and treating the extraction as accidental (it is a structural, predictable byproduct of tying validity exclusively to procedure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_of_kernel,
    'Is source-validity (positivist reading) a defensible independent account of constitutional law, or does it covertly smuggle in substantive value choices under the guise of pure procedure — collapsing toward the living-constitutionalist reading it claims to reject?',
    'Track hard cases where the pedigree test itself is contested (e.g., disputes over what counts as a valid Article V ratification, or whether a superseded practice retains formal validity) and observe whether courts resolve the pedigree dispute using purely formal criteria or smuggle in substantive judgments about which outcome is more just.',
    'If pedigree disputes are resolved by covert substantive reasoning, positivism''s claimed procedural purity collapses and its ε should be revised toward a value closer to the living-constitutionalist reading''s structure; if pedigree disputes are resolved by genuinely formal criteria (vote counts, certified ratification records), the reading holds as structurally distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_of_kernel, conceptual, 'Whether positivist validity is genuinely procedure-only or a disguised substantive test.').

omega_variable(
    sibling_reading_divergence_location,
    'Where exactly do the three kernel readings (positivist, originalist, living-constitutionalist) locate their disagreement — is it about WHAT counts as a valid source (positivist vs. originalist: procedure vs. original meaning) or about WHETHER meaning is fixed at all (originalist/positivist vs. living-constitutionalist)?',
    'Map specific doctrinal disputes (e.g., Eighth Amendment ''evolving standards of decency'' cases) against each reading''s predicted outcome to see which axis of disagreement is doing the actual work in contested cases.',
    'Clarifies which sibling relation (forecloses vs. coexists_with vs. influences) best describes each pairwise relationship, and whether the positivist reading is closer in practice to originalism (both fix validity to a past formal act) than the schema''s independent treatment suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_divergence_location, conceptual, 'Locating the actual axis of disagreement among the three kernel readings.').

omega_variable(
    enactment_defect_ambiguity,
    'Does the positivist reading''s ε account for cases where the formal enactment process itself was procedurally compromised (e.g., ratification under exclusionary suffrage rules) — does formal validity launder substantive injustice baked into the original procedure?',
    'Historical analysis of which groups could participate in the formal enactment and amendment processes at each ratification point, cross-referenced with whose substantive claims are foreclosed today by appeal to that same pedigree.',
    'If the formal process was itself substantively exclusionary, treating its outputs as automatically valid regardless of content extends that original exclusion forward — raising the effective extraction beyond what the current metric captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enactment_defect_ambiguity, empirical, 'Whether formal validity inherits and launders defects from historically exclusionary enactment processes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__positivist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__positivist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__positivist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__positivist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__positivist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__positivist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__positivist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__positivist_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__positivist_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__positivist_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__positivist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__positivist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__positivist_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__positivist_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__positivist_reading, suppression_requirement, 50, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints sharing the us_constitution_text kernel. positivist_reading ties validity exclusively to formal enactment procedure; originalist_reading ties validity to original public meaning at ratification; living_constitutionalist_reading ties validity to evolving societal application of enacted principles. Each carries its own ε, its own beneficiary/victim structure, and its own claimed_type — they are not the same constraint measured three ways; they are three distinct constraints sharing a textual kernel, linked here per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
