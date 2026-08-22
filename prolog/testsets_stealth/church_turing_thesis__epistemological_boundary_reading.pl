% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis — Epistemological Boundary Reading (Proof-Theoretic Admission Standard)
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   The Church-Turing thesis, read epistemologically, functions as the
 *   admission standard for computability knowledge: a function counts as
 *   provably computable exactly when a Turing-relative procedure can be
 *   exhibited or mechanically derived, and physical realizability neither
 *   extends nor threatens that domain. This story instantiates one reading of
 *   the church_turing_thesis kernel; the definitional reading (true by
 *   stipulation, near-zero epsilon, no victims) and the physical reading (a
 *   falsifiable empirical claim about nature) are separate stories linked
 *   through the network. The claim/metric gap is deliberate: the arrangement
 *   is CLAIMED here as tangled_rope — a genuine coordination standard
 *   carrying asymmetric exclusion — while the metrics describe that operation
 *   directly; the engine computes per-seat classifications from the
 *   structural data. Epsilon's referent is the standing gatekeeping
 *   arrangement as it operates, assessed by this reading's own lights — not
 *   the boundary-open alternative its critics would install.
 *
 * KEY AGENTS:
 *   - proof_theoretic_gatekeepers: agenda-setting seat (institutional/arbitrage) — journal editors, program committees, textbook authors who administer the proof standard
 *   - computability_research_community: beneficiary seat (organized/mobile) — collects a well-defined problem space and citation economy
 *   - formal_verification_practitioners: beneficiary seat (organized/constrained) — toolchain builders whose semantics depend on the standard
 *   - hypercomputation_research_programs: payer seat (moderate/identity_locked) — bears the exclusion; professional identity fused with challenging the boundary
 *   - non_constructive_computability_claimants: payer seat (powerful/mobile) — nominally targeted, minimally burdened; claims survive constructive repair
 *   - physics_of_computation_community: excluded seat (organized/mobile) — bracketed by the reading's 'regardless of physical possibility' clause
 *   - philosophy_of_computation_analysts: observer seat (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.4).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis — Epistemological Boundary Reading (Proof-Theoretic Admission Standard)").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/foundations_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '7f52fa53-fb26-4f28-a40a-770f41bcc873').
narrative_ontology:cs_kernel_codification('7f52fa53-fb26-4f28-a40a-770f41bcc873', distributed).
narrative_ontology:cs_authority_grounding('7f52fa53-fb26-4f28-a40a-770f41bcc873', expertise).
narrative_ontology:cs_interpretation_layer_present('7f52fa53-fb26-4f28-a40a-770f41bcc873').
narrative_ontology:cs_reading_relation('7f52fa53-fb26-4f28-a40a-770f41bcc873', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f52fa53-fb26-4f28-a40a-770f41bcc873', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('7f52fa53-fb26-4f28-a40a-770f41bcc873', foundational, formal_knowability_requires_mechanical_certification).
narrative_ontology:cs_axiom_status(formal_knowability_requires_mechanical_certification, holdable).
narrative_ontology:cs_axiom_grounding('7f52fa53-fb26-4f28-a40a-770f41bcc873', formal_knowability_requires_mechanical_certification, deontological).
narrative_ontology:cs_axiom('7f52fa53-fb26-4f28-a40a-770f41bcc873', foundational, physical_possibility_irrelevant_to_formal_knowledge).
narrative_ontology:cs_axiom_status(physical_possibility_irrelevant_to_formal_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('7f52fa53-fb26-4f28-a40a-770f41bcc873', physical_possibility_irrelevant_to_formal_knowledge, deontological).
narrative_ontology:cs_reference_frame('7f52fa53-fb26-4f28-a40a-770f41bcc873', proof_theoretic_knowability_boundary).
narrative_ontology:cs_drift_state('7f52fa53-fb26-4f28-a40a-770f41bcc873', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7f52fa53-fb26-4f28-a40a-770f41bcc873', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_gatekeepers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_research_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, formal_verification_practitioners).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_research_programs).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_equivalence_of_effective_calculability_formalisms).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, entscheidungsproblem_undecidability).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, mechanical_checkability_of_formal_proof).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit the journals, run the program committees, and write the textbooks through which computability claims pass. Admit a computability proof when it exhibits or mechanically derives a Turing-relative procedure; return everything else with a request for constructive content. Their own careers, courses, and canons are built inside the standard they administer, and the standard travels with them — they can practice it at any institution.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_gatekeepers, beneficiary).

% Work daily inside the standard: reductions, degree structures, undecidability results. The shared criterion gives them comparable results, a common citation economy, and a stable supply of open problems. Leaving would mean rebuilding their accumulated results inside some other framework for little gain.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_research_community, beneficiary,
    organized, biographical, mobile, global).

% Build proof assistants and verified software stacks whose semantics assume the standard's notion of mechanical procedure. Their toolchains, libraries, and training encode it deeply; switching frameworks would orphan decades of artifacts and certifications.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, formal_verification_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Propose models — analog acceleration, relativistic spacetime computation, infinite-time registers — aimed at functions beyond Turing reach, and seek formal recognition for them. Proposals come back from mainstream venues classified as philosophy or as error, and funding follows the verdict. Their professional identities formed around the challenge, so relocating to adjacent orthodox work would mean abandoning the careers they built.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_research_programs, payer,
    moderate, biographical, identity_locked, global).

% Classically-trained mathematicians who occasionally establish that an algorithm exists without exhibiting one. In practice their claims are repaired — a witness is extracted or the claim is restated in Turing-relative terms — and they proceed with reputation intact. The standard costs them a revision step, not a career.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants, payer,
    powerful, biographical, mobile, global).

% Study what physical processes compute and argue the universe may permit more than Turing machines allow. The reading they face declares physical possibility irrelevant to formal knowability, so their results land in physics and philosophy venues rather than the proof-theoretic ones that adjudicate computability. They retain functioning careers in adjacent fields.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, physics_of_computation_community, excluded,
    organized, biographical, mobile, global).

% Analyze what the thesis claims, which formulations are equivalent, and what its status is — definitional, epistemic, or physical. They take no side in admission decisions and bear none of their costs; their output is assessment.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophy_of_computation_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_gatekeepers).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one shared criterion for what counts as a proof that a function is computable, so that a worldwide research community can check each other's claims, compare rival formalisms, and stack results — reductions, undecidability theorems, verified software — on a common foundation.
% TRANSFER_FUNCTION: Moves epistemic legitimacy and the resources that follow it — publication slots, grants, jobs, curriculum time — toward work certified in Turing-relative terms and away from programs seeking recognition for non-Turing or non-constructive computability claims.
% ABSENT_VOICES: Hypercomputation and physical-computation researchers would object that the standard settles by fiat what they think should be settled by experiment or by broader proof notions; they are present in philosophy and physics venues but absent from the editorial and program-committee rooms where admission is decided. Constructivist traditions would object that the Turing anchor is one choice among several; they are present historically, marginal currently.
% DISAPPEARANCE_RATIONALE: Without the shared criterion, computability claims would fragment across rival formalisms as in the 1920s-30s, undecidability and correctness results would need per-framework restatement, and the verification toolchain industry would lose its semantic anchor; boundary-challenging programs would regain admission overnight while mainstream results lost their common currency.
% FOUNDING_PROBLEM: Before 1936, 'effective calculability' had no precise meaning; Hilbert's Entscheidungsproblem demanded an exact notion of mechanical procedure, and competing formalisms (lambda calculus, general recursion, Post systems) needed reconciliation before anything could be proved about mechanical computation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematical logic corroborate the founding problem and its resolution: the 1936-37 equivalence proofs settled the definitional crisis and answered the Entscheidungsproblem negatively. Philosophers of computation writing outside the benefiting community corroborate that the arrangement now governs questions — physical computation, hypercomputation, machine-learning claims — the founders never posed, which is why the status is contested rather than plainly live or dead.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.35: the arrangement's day-to-day operation is overwhelmingly coordinative — millions of proofs admitted smoothly — with extraction concentrated on a small boundary population, hence low-to-moderate rather than high. Suppression 0.40: coercion is soft and institutional (referees, panels, curricula) rather than legal or physical, and alternatives persist in adjacent fields, but for the identity-locked minority the lack of alternatives is nearly total. Theater 0.20: the standard performs real work in every admitted proof; the theatrical share is ritual citation of 'the Church-Turing thesis' in venues that never specify a formalism. Accessibility_collapse 0.55: inside proof theory and verification, alternatives to the standard effectively vanish once it is understood; outside them (physics, philosophy of computation) workable alternatives persist. Resistance 0.50: sustained minority contestation — hypercomputation proposals, physical-computation arguments, constructivist qualms — without mass defection. Measurements share one grid (years since Kleene's 1952 codification, t=0..72): extractiveness accumulates gently with each boundary controversy; theater rises as invocation ritualizes; suppression traces a shallow U — active adjudication of rival formalisms early, consolidation mid-period, renewed but still modest enforcement during the hypercomputation and physical-computation debates. Suppression_requirement is tracked because the story's enforcement history genuinely changes shape across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeper seat the arrangement is a hard-won instrument of rigor that any competent newcomer can satisfy; from the hypercomputation seat it is a closed guild whose admission criterion assumes its conclusion; from the non-constructive claimant's seat it is a minor stylistic toll. Same structure, three experiences — the engine computes this divergence from power, exit, and declared position rather than from anyone's testimony.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (gatekeepers, computability community, verification practitioners) derive low directionality — the standard subsidizes them. Declared victims derive high directionality, modulated by exit: hypercomputation researchers are identity-locked, so they sit near the full-target end; non-constructive claimants are powerful and mobile, and their claims survive constructive repair, so the victim-based derivation overshoots for them — the override sets the powerful atom to 0.55, near-symmetric with a slight target lean. The physics-of-computation community is excluded rather than declared: bracketed by the reading's own 'regardless of physical possibility' clause, they experience moderate targeting without appearing in the victim arrays. Scope is global — the standard governs a worldwide practice — which the engine folds into effective-extraction scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining mechanical procedure; settling the Entscheidungsproblem) was solved within a decade of the arrangement's creation, yet the arrangement persists and now does successor work: anchoring verification infrastructure, pedagogy, and boundary adjudication. Reading status=contested against verdict=world_rearranges yields no zombie flag — the rearrangement dependence is real (verification, undecidability results, curricula all hang on the standard) — but the theater series' slow rise marks where mandate-drift would show first: ritual invocations of the thesis in fields that never operationalize it. The classification prevents two opposite mislabels: reading the gatekeeping as pure extraction ignores the genuine coordination function every working computability theorist and verification engineer draws on daily; reading it as pure coordination erases the programs that pay for the boundary — hypercomputation researchers bear real, identity-fused costs, and the excluded physical-computation community is bracketed by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the church_turing_thesis kernel governs a given community''s practice — this epistemological boundary reading, the mathematical definition reading, or the physical claim reading?',
    'Corpus analysis of how practitioners deploy the thesis: as stipulated definition (definitional reading), as proof-admission standard (this reading), or as falsifiable physics (physical reading).',
    'Under the definitional reading epsilon approaches zero (true by convention, no victim set); under the physical reading victims shift to physical-computation researchers and contestation rises; this story''s epsilon 0.35 and victim set hold only for the epistemological reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'One kernel, three structurally distinct constraints; classification is reading-indexed.').

omega_variable(
    hypercomputation_counterexample_possibility,
    'Can any physical or formal system yield a certifiable counterexample — a provably non-Turing-computable function computed by an implementable process?',
    'Either a verified implementation with a formal correctness proof exceeding Turing power, or accumulated formal results showing that certification itself is Turing-bounded.',
    'A certified counterexample would force this reading''s boundary clause to fail openly, spiking extraction on the defending establishment; continued failure consolidates the reading and slowly converts residual victims into self-selected outsiders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_counterexample_possibility, empirical, 'Whether the boundary can ever be breached by a certifiable case.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of boundary-challenging programs structural (venue, funding, curricular gatekeeping) or internalized (trained researchers treat super-Turing claims as category errors without needing enforcement)?',
    'Post-exit trajectory: track researchers who leave boundary-challenging programs for mainstream work — if the boundary assumptions persist unenforced, the internalized share is large.',
    'If mostly internalized, effective suppression exceeds the structural measure and persists even if gatekeeping relaxes; if mostly structural, relaxing venue gatekeeping would rapidly revive boundary programs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the enforcement of the proof standard.').

omega_variable(
    discovered_vs_administered_boundary,
    'Is the knowability boundary a discovered feature of formal proof (any proof is mechanically checkable, hence Turing-simulable) or an administered convention (a community choice to admit only Turing-relative certificates)?',
    'Proof-theoretic analysis: if the checkability-of-proof argument closes the boundary a priori, the core is discovered and the administration merely implements it; if gaps exist between checkability and Turing simulation under broadened proof notions, the boundary is administratively maintained.',
    'A discovered core would push the constraint''s core toward natural-law status with the enforcement layer as ordinary coordination; a purely administered boundary would make the whole arrangement revisable by community decision, raising the stakes of the gatekeeping seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_vs_administered_boundary, conceptual, 'Whether the boundary is a theorem-in-disguise or a maintained convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_epistemic_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t0, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t12, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t12, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t24, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t24, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t36, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 36, 0.13).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t36, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t48, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t48, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t60, observed).
narrative_ontology:measurement(ctt_epistemic_tr_t72, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 72, 0.2).
narrative_ontology:measurement_basis(ctt_epistemic_tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(ctt_epistemic_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(ctt_epistemic_be_t0, observed).
narrative_ontology:measurement(ctt_epistemic_be_t12, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement_basis(ctt_epistemic_be_t12, observed).
narrative_ontology:measurement(ctt_epistemic_be_t24, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement_basis(ctt_epistemic_be_t24, observed).
narrative_ontology:measurement(ctt_epistemic_be_t36, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 36, 0.29).
narrative_ontology:measurement_basis(ctt_epistemic_be_t36, observed).
narrative_ontology:measurement(ctt_epistemic_be_t48, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 48, 0.31).
narrative_ontology:measurement_basis(ctt_epistemic_be_t48, observed).
narrative_ontology:measurement(ctt_epistemic_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement_basis(ctt_epistemic_be_t60, observed).
narrative_ontology:measurement(ctt_epistemic_be_t72, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 72, 0.35).
narrative_ontology:measurement_basis(ctt_epistemic_be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(ctt_epistemic_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(ctt_epistemic_su_t0, observed).
narrative_ontology:measurement(ctt_epistemic_su_t12, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(ctt_epistemic_su_t12, observed).
narrative_ontology:measurement(ctt_epistemic_su_t24, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement_basis(ctt_epistemic_su_t24, observed).
narrative_ontology:measurement(ctt_epistemic_su_t36, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 36, 0.3).
narrative_ontology:measurement_basis(ctt_epistemic_su_t36, observed).
narrative_ontology:measurement(ctt_epistemic_su_t48, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 48, 0.33).
narrative_ontology:measurement_basis(ctt_epistemic_su_t48, observed).
narrative_ontology:measurement(ctt_epistemic_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.37).
narrative_ontology:measurement_basis(ctt_epistemic_su_t60, observed).
narrative_ontology:measurement(ctt_epistemic_su_t72, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 72, 0.4).
narrative_ontology:measurement_basis(ctt_epistemic_su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% Constraint family from epsilon-invariance decomposition: the colloquial label 'Church-Turing thesis' covers three structurally distinct claims with different epsilon, victim sets, and failure modes. The definitional reading is upstream (its stipulation is cited as grounds by both downstream readings); this epistemological reading and the physical reading are downstream siblings — the epistemological reading brackets the physical question ('regardless of physical possibility') rather than answering it. Each family member links the others via affects_constraints; no single story may average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
