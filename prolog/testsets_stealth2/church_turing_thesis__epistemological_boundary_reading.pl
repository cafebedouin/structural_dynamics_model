% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Church-Turing Thesis — Epistemological Boundary Reading
 *   domain: philosophy_of_mathematics/computation_foundations
 *
 * SUMMARY:
 *   The epistemological boundary reading treats the Church-Turing thesis as a
 *   rule of evidence: within mathematics and theoretical computer science, a
 *   function counts as knowably computable exactly when an explicit effective
 *   procedure — a Turing machine or recognized equivalent — can be exhibited
 *   for it. Claims of computability established by other means (indirect
 *   classical argument, appeal to physical processes) fall outside the
 *   boundary of formal knowability, whatever their physical fate. The
 *   standard is administered in ordinary refereeing: submissions asserting
 *   computability without a procedure are returned or rejected. This story
 *   instantiates ONE reading of the contested kernel church_turing_thesis;
 *   the mathematical-definition and physical-claim readings are separate
 *   stories with their own epsilon values and victim sets (see
 *   network.dual_formulation_note). The claimed type and the metrics below
 *   are authored independently: the claim records the structure judged true
 *   of this reading; the metrics record its observed operation.
 *
 * KEY AGENTS:
 *   - logic_and_cs_journal_editors: agenda setter (institutional/mobile) — administer the evidential standard through review; rotate personally, persist institutionally
 *   - computability_theorists: primary beneficiary (organized/identity_locked) — field delimited and legitimated by the procedural standard
 *   - constructive_mathematicians: secondary beneficiary (organized/identity_locked) — witness-bearing proof practice vindicated as the admissible route
 *   - formal_verification_community: tertiary beneficiary (organized/constrained) — mechanizes the standard; invested toolchains depend on its stability
 *   - non_constructive_computability_claimants: partial target (powerful/constrained) — bear extra labor on the narrow slice of practice touching computability claims
 *   - hypercomputation_researchers: concentrated target (moderate/identity_locked) — research program placed outside formal knowability regardless of physical realizability
 *   - analog_computing_engineers: excluded party (moderate/trapped) — build computing systems with no seat in the venues that classify them
 *   - philosophers_of_computation: analytical observer (analytical/analytical) — sees the full three-reading structure; administers and collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.4).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis — Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/computation_foundations").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, 'd0a63c5b-2c4f-4291-864d-5df371251b08').
narrative_ontology:cs_kernel_codification('d0a63c5b-2c4f-4291-864d-5df371251b08', fixed_text).
narrative_ontology:cs_authority_grounding('d0a63c5b-2c4f-4291-864d-5df371251b08', practice).
narrative_ontology:cs_interpretation_layer_present('d0a63c5b-2c4f-4291-864d-5df371251b08').
narrative_ontology:cs_reading_relation('d0a63c5b-2c4f-4291-864d-5df371251b08', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0a63c5b-2c4f-4291-864d-5df371251b08', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('d0a63c5b-2c4f-4291-864d-5df371251b08', foundational, procedure_exhibition_licences_computability_knowledge).
narrative_ontology:cs_axiom_status(procedure_exhibition_licences_computability_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('d0a63c5b-2c4f-4291-864d-5df371251b08', procedure_exhibition_licences_computability_knowledge, conventional).
narrative_ontology:cs_axiom('d0a63c5b-2c4f-4291-864d-5df371251b08', secondary, physical_realizability_bracketed_from_knowability).
narrative_ontology:cs_axiom_status(physical_realizability_bracketed_from_knowability, holdable).
narrative_ontology:cs_axiom_grounding('d0a63c5b-2c4f-4291-864d-5df371251b08', physical_realizability_bracketed_from_knowability, instrumental).
narrative_ontology:cs_reference_frame('d0a63c5b-2c4f-4291-864d-5df371251b08', epistemic_boundary_of_formal_knowability).
narrative_ontology:cs_drift_state('d0a63c5b-2c4f-4291-864d-5df371251b08', contemporary_hypercomputation_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0a63c5b-2c4f-4291-864d-5df371251b08', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, formal_verification_community).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, logic_and_cs_journal_editors).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_machine_canon_of_effective_procedure).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, witness_bearing_proof_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit and referee the journals and conferences where computability results appear. A submission claiming that some function is computable is accepted only when the authors exhibit an explicit effective procedure — a Turing machine, a recursive definition, a program — or invoke a formalism the community already recognizes as equivalent. Papers asserting computability by indirect or non-mechanical argument are returned for construction or rejected. Editorial boards rotate person by person, but the reviewing standard persists across venues and across generations of editors, and serving it accrues reputational standing to those who administer it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, logic_and_cs_journal_editors, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, logic_and_cs_journal_editors, beneficiary).

% Research the structure of Turing-computable and non-computable functions: degrees of unsolvability, reducibility hierarchies, oracle constructions. The field's subject matter is fixed by the agreed procedural standard — every object studied is defined relative to the machine model. Training, terminology, and open-problem lists all presuppose it; a researcher who abandoned the framework would leave the field's questions behind entirely.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_theorists, beneficiary,
    organized, generational, identity_locked, global).

% Develop mathematics in which existence claims carry algorithms or explicit witnesses. The procedural standard for computability vindicates their proof discipline: a constructive proof of computability is automatically admissible, while classical non-constructive argument gains nothing. Their schools, journals, and conference circuits are organized around witness-bearing proof, and their professional identity is bound to the view that assertion without construction is incomplete.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, beneficiary,
    organized, generational, identity_locked, global).

% Build proof assistants and mechanized libraries — Coq, Lean, Agda — in which computability claims are checked by machine. Their tools implement the procedural standard: a function counts as computable inside the system when a terminating program or realized specification is supplied. Toolchains, curated libraries, and funded projects depend on the standard remaining stable; adopting a rival demarcation would strand existing formalizations.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, formal_verification_community, beneficiary,
    organized, biographical, constrained, global).

% Classically trained mathematicians who occasionally establish that a function or set is computable, or that an algorithm exists, using indirect arguments — compactness, choice, counting — without producing a procedure. When they bring such results to logic and computer science venues they are asked to exhibit a method or withdraw the claim. Most can restate the result constructively at the cost of extra work, and much of their output never touches computability at all; the standard imposes extra labor on a narrow slice of their practice.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants, payer,
    powerful, biographical, constrained, global).

% Investigate proposed routes to computation beyond the Turing limit: idealized analog devices with unlimited precision, accelerated or Zeno-style machines, relativistic spacetime configurations permitting supertasks, and interpretations of physical theory suggesting non-effective processes. Mainstream logic venues treat their outputs as lying outside formally knowable computation regardless of physical realizability, so publication, citation, and funding concentrate in specialist workshops and sympathetic journals. Their careers and research programs are built on the trans-Turing question; abandoning it would dissolve the program's identity.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Design and operate physical computing systems — analog simulators, neuromorphic hardware, optical processors — whose behavior is engineered and measured without reference to proof-theoretic standards. They do not participate in the logic-community conversation that fixes what counts as formally knowable computation, and nothing in their workflow requires them to. The verdict on whether their artifacts fall inside that category is decided in venues where they hold no seat.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, analog_computing_engineers, excluded,
    moderate, immediate, trapped, regional).

% Analyze what the thesis claims, what it could not claim, and how its readings diverge; trace the line from Hilbert's program through the 1936 Church and Turing papers to present disputes over physical computation. They publish critiques and reconstructions but administer nothing and collect nothing from the standard's operation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_computation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, computability_theorists).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a shared, checkable evidential standard for computability claims across mathematics and computer science: to establish that a function is computable, one exhibits an explicit effective procedure in a recognized formalism. This lets results be stated, compared, reused, and mechanically checked across groups that would otherwise rely on incompatible intuitions of what is calculable in principle.
% TRANSFER_FUNCTION: Moves epistemic standing — publication, citation, legitimacy as established knowledge — toward claims cashed out in Turing-equivalent procedural terms, and away from claims supported only by indirect or non-mechanical argument; indirectly moves research attention and funding away from trans-Turing programs.
% ABSENT_VOICES: Analog and novel-hardware computing engineers, whose systems compute in an engineering sense but who hold no seat in the logic venues where the standard is administered; also physicists proposing spacetime or cosmological super-Turing processes, who encounter the boundary as an externally imposed verdict on their work. Both would object that 'formally knowable' quietly narrows 'computable' to 'provable by our methods.'
% DISAPPEARANCE_RATIONALE: Without the shared procedural standard, computability claims would fragment across rival evidential criteria; formal verification libraries, complexity-theoretic baselines, and the cumulativeness of computability theory would lose their common ground, and claims currently excluded would re-enter the knowable category case by case. Curricula, proof assistants, and venue norms built on the standard would require wholesale renegotiation.
% FOUNDING_PROBLEM: In the 1920s and 1930s, Hilbert's program and the Entscheidungsproblem required a precise, mechanical characterization of effective calculability so that questions about computability could be settled by proof rather than informal intuition, and so that the newly invented formalisms — Turing machines, lambda calculus, general recursiveness — could be recognized as capturing one and the same notion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: historians and philosophers of mathematics attesting the Entscheidungsproblem's role and the 1936 Church and Turing papers confirm the original demarcation problem and its urgency; hypercomputation researchers, though they dispute the standard's current answers, concede the historical problem was real. No serious participant denies the founding problem existed; the live dispute is over whether the boundary answer remains correct at the edges.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.40 at interval end) rather than high because what is taken is epistemic standing, not resources, and the burden falls on a bounded slice of practice: most mathematics never asserts computability, and classical mathematicians can usually restate claims constructively at real but survivable cost. Suppression (0.42) is a raw structural property, unscaled by power or scope — it reflects venue gatekeeping, funding concentration, and canonization, soft coercion with career consequences rather than physical force; only extractiveness is scaled by directionality and scope in the engine's computation. Theater is low (0.20): the standard does continuous real work — procedures are exhibited, proofs checked, formalizations run — with a growing but minor ritual component (citation of the thesis in introductions as if it were a theorem). Accessibility_collapse (0.50) sits mid-range: alternatives do not vanish — non-constructive mathematics and hypercomputation research continue — but they collapse inside mainstream logic venues, where a computability claim without a procedure is unpublishable. Resistance (0.55) is substantial and persistent: decades of hypercomputation advocacy, pluralism about calculability, and philosophy-of-computation critique. The temporal series run on one shared grid (t = 0,15,30,45,60,75,90 years since 1936) so every tracked metric is authored at every examined point; enforcement capacity (suppression_requirement) rises as the standard moved from voluntary adoption to journal institutionalization to mechanized checking, which is why that series is tracked alongside the other two.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From inside the proof-theoretic framework — the editors' and beneficiaries' position — the boundary simply is rigor: a shared checkable standard that made results cumulative and machine-checkable. From the payer seats the same structure operates as methodological exile: whole classes of claim are ruled out of the knowable. Identity lock differentiates same-power actors: computability_theorists and constructive_mathematicians (organized, generational) are fused with the framework at the level of field constitution — their questions are defined relative to the machine model, so exit means leaving the field's problems — while the comparably-situated non_constructive_computability_claimants retain constrained exit because the standard touches only a slice of their practice. Hypercomputation_researchers are locked by professional identity: the program's constitutive question is the trans-Turing one, so abandoning the frame dissolves the program. If the identity frames broke — computability theorists adopting plural evidential standards, hypercomputation researchers reframing their work as physics or mathematics of idealizations — the computed types would shift accordingly: the structure would read as nearer pure coordination from the former seat, and the latter would exit the victim set entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (computability_theorists, constructive_mathematicians, formal_verification_community) sit near the subsidized end: the standard ratifies exactly their proof practice and imposes nothing on it. Declared victims derive high directionality: hypercomputation_researchers are concentrated, identity-locked targets near the full-target end; non_constructive_computability_claimants are declared victims but only partial ones. Three overrides correct places the derivation chain cannot reach or would overreach: (1) powerful / 0.65 for the non-constructive claimants — the victim declaration alone would derive near-full targeting, but their actual exposure is a narrow slice of practice with a cheap constructive workaround; (2) institutional / 0.2 for the editors — the agenda-setter seat has no beneficiary/victim declaration, so the canonical fallback would misplace them, yet they demonstrably accrue reputational capital from administering the standard; (3) moderate / 0.7 for the analog_computing_engineers — the excluded seat bears the boundary's cost (their artifacts rendered formally unknowable) without appearing in the victim arrays, so structural derivation has nothing to read. Global spatial scope applies the engine's modest verification-difficulty amplification to extractiveness; suppression passes through unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — demarcating effective calculability so computability could be settled by proof — is still live: it is exercised in every refereeing decision and every formalization effort, not commemorated. The R5 mismatch consumer therefore reads status=live x verdict=world_rearranges: no capture/zombie flag. The tangled-rope classification prevents two symmetric mislabels. Reading the boundary as pure rope — 'just a shared standard' — erases the named payers and the active enforcement that maintains their exclusion; reading it as pure snare — 'gatekeeping that suppresses dissident computation research' — erases the genuine, heavily-used coordination function that makes results comparable, cumulative, and mechanically checkable, a function no serious participant proposes abolishing outright. The hybrid holds both: real coordination, real extraction, enforcement required to keep the asymmetry in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the church_turing_thesis kernel is being assessed — the epistemological boundary (this story), the mathematical definition, or the physical claim?',
    'Cross-reading comparison across the three linked stories: classify each reading on its own structural data and compare victim sets, epsilon, and type; divergences locate the indexicality.',
    'The definition reading carries near-zero epsilon and no victims (true by convention); the physical reading''s epsilon tracks empirical physics and its victims are physical-computation researchers; collapsing the readings into one colloquial label would average incompatible structures and produce a spurious middle classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    boundary_edge_stability,
    'Will the epistemic boundary hold at its edges — against proposed super-Turing physical processes and against non-constructive establishment of computability?',
    'Case-by-case adjudication of concrete proposals (precision limits of idealized analog devices, relativistic supertask configurations, non-constructive computability arguments) in mainstream logic venues.',
    'An accepted, reproducible super-Turing process would force explicit rescoping of the boundary and intensify enforcement, raising epsilon; continued rejection confirms the current moderate-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_edge_stability, empirical, 'Stability of the knowability boundary at the hypercomputation and non-constructivity edges.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of trans-Turing and non-constructive computability claims structural (venue gatekeeping, funding allocation) or internalized (researchers pre-filter their own questions as illegitimate)?',
    'Post-exit trajectory: track researchers who leave trans-Turing programs for mainstream areas — if boundary-respecting self-censorship persists after gatekeeping pressure is removed, part of the suppression is internalized.',
    'Internalized suppression raises effective suppression above the structural measure and makes the boundary self-reproducing even if formal enforcement relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized character of boundary enforcement.').

omega_variable(
    mechanized_enforcement_constitution,
    'As proof assistants mechanize the standard, does the boundary become constitutive — identical to what Coq, Lean, and Agda accept — or does the community retain independent authority over the demarcation?',
    'Compare cases where mechanized checking and community judgment diverge (accepted-but-unformalized arguments, formalized-but-contested encodings) and observe which side prevails over time.',
    'If mechanization becomes constitutive, enforcement locks into toolchains and future drift becomes toolchain drift; if community judgment retains authority, the boundary remains revisable by practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanized_enforcement_constitution, conceptual, 'Whether mechanized proof checking replaces or merely implements the community''s demarcation authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(chur_tr_t0, observed).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(chur_tr_t15, observed).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(chur_tr_t30, observed).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.13).
narrative_ontology:measurement_basis(chur_tr_t45, observed).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(chur_tr_t60, observed).
narrative_ontology:measurement(chur_tr_t75, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement_basis(chur_tr_t75, observed).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 90, 0.2).
narrative_ontology:measurement_basis(chur_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(chur_be_t0, observed).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(chur_be_t15, observed).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(chur_be_t30, observed).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement_basis(chur_be_t45, observed).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(chur_be_t60, observed).
narrative_ontology:measurement(chur_be_t75, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement_basis(chur_be_t75, observed).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 90, 0.4).
narrative_ontology:measurement_basis(chur_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(chur_su_t0, observed).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement_basis(chur_su_t15, observed).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(chur_su_t30, observed).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 45, 0.33).
narrative_ontology:measurement_basis(chur_su_t45, observed).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.37).
narrative_ontology:measurement_basis(chur_su_t60, observed).
narrative_ontology:measurement(chur_su_t75, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(chur_su_t75, observed).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(chur_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Church-Turing thesis' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a linked family. The mathematical_definition_reading is a stipulative convention — near-zero epsilon, no victims, true by definition — and is upstream: its stipulated equivalence of formalisms is what licenses drawing any boundary at the Turing limit. This epistemological_boundary_reading is the methodological gate built on that equivalence: moderate epsilon, victims among non-constructive claimants and hypercomputation researchers, enforced through refereeing. The physical_claim_reading is the empirical claim about physical processes — epsilon contingent on physics, victims among physical-computation researchers — and stands downstream of this reading's structural pressure: by classifying trans-Turing physical speculation as formally unknowable regardless of physical possibility, the boundary reading changes the legitimacy conditions and resource availability of the physical-claim program without foreclosing it. Each story carries its own epsilon, stakeholders, and classification; the links here propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, powerful, 0.65).
constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, institutional, 0.2).
constraint_indexing:directionality_override(church_turing_thesis__epistemological_boundary_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
