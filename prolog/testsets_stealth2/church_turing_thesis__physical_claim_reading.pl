% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__physical_claim_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Physical Church-Turing Thesis as Research-Governance Boundary (Physical Claim Reading)
 *   domain: philosophy of mathematics / philosophy of computation / foundations of computer science
 *
 * SUMMARY:
 *   This story instantiates the physical_claim_reading of the
 *   church_turing_thesis kernel: the thesis as an empirical claim about the
 *   universe — no physical process computes beyond Turing-machine
 *   computability — operating as a research-governance boundary in the
 *   computation sciences. The standing arrangement under contest (the ε
 *   referent) is the gatekeeping regime built on the claim: peer review,
 *   funding panels, and textbook tradition treat Turing-boundedness as
 *   settled enough to screen research programs, so proposals presupposing
 *   physical hypercomputation are rejected as category errors rather than
 *   adjudicated on evidence. Assessed by this reading's own lights, the claim
 *   is empirical — well supported, never refuted, but contestable — and the
 *   arrangement it grounds has real parties: a mainstream whose paradigm it
 *   insulates, gatekeepers who administer it, and a small minority of
 *   hyper-Turing claimants who bear its costs. The sibling readings
 *   (mathematical definition, epistemological boundary) are separate
 *   constraints with their own ε values and are linked via
 *   network.affects_constraints; this story's ε is indexed to this reading
 *   only. The claim/metric gap is deliberate and independent: claimed_type is
 *   what I take to be structurally true of the arrangement, the metrics
 *   describe its actual operation, and the engine computes per-seat
 *   classifications — divergence between them is the datum.
 *
 * KEY AGENTS:
 *   - computation_research_gatekeepers: agenda-setter (institutional/arbitrage) — administers the boundary through review, funding, and textbook authority; also collects adjudicative incumbency
 *   - theoretical_cs_establishment: primary beneficiary (institutional/arbitrage) — paradigm insulated, the arrangement's gains accrue here
 *   - quantum_computing_mainstream: secondary beneficiary (organized/mobile) — foundational-safe so long as its claims stay Turing-consistent
 *   - hypercomputation_researchers: primary target (moderate/identity_locked) — careers fused to the contested claim
 *   - analog_computation_researchers: secondary target (moderate/constrained) — strongest claims screened out before adjudication
 *   - quantum_gravity_computation_proposers: excluded voice (moderate/constrained) — dismissed before development, no standing venue
 *   - philosophy_of_computation_community: analytical observer — sees the full structure, collects and pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.55).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.6).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis as Research-Governance Boundary (Physical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy of mathematics / philosophy of computation / foundations of computer science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '8efc086d-d5d4-4c25-99b1-10bfccf91ba2').
narrative_ontology:cs_kernel_codification('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', formalized).
narrative_ontology:cs_authority_grounding('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', expertise).
narrative_ontology:cs_interpretation_layer_present('8efc086d-d5d4-4c25-99b1-10bfccf91ba2').
narrative_ontology:cs_reading_relation('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', foundational, physical_computation_turing_bounded).
narrative_ontology:cs_axiom_status(physical_computation_turing_bounded, holdable).
narrative_ontology:cs_axiom_grounding('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', physical_computation_turing_bounded, empirically_contingent).
narrative_ontology:cs_axiom('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', foundational, ctt_is_empirically_testable_claim).
narrative_ontology:cs_axiom_status(ctt_is_empirically_testable_claim, holdable).
narrative_ontology:cs_axiom_grounding('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', ctt_is_empirically_testable_claim, conventional).
narrative_ontology:cs_reference_frame('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', physical_computation_empirical_law).
narrative_ontology:cs_drift_state('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', contemporary_post_hypercomputation_debate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8efc086d-d5d4-4c25-99b1-10bfccf91ba2', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, theoretical_cs_establishment).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_computing_mainstream).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, analog_computation_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, computation_research_gatekeepers).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, turing_model_physical_sufficiency).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, no_hypercomputation_in_physics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journal editors, program-committee members, grant-panel reviewers, and textbook authors who adjudicate which computation-research proposals are taken seriously. They apply the physical Church-Turing claim as a screening assumption: proposals that presuppose physical processes computing beyond Turing are rejected or defunded as category errors rather than engaged on the evidence. Their adjudicative authority and professional standing are bound up with the paradigm they administer; their skills transfer freely to other editorial and review roles, so leaving the gatekeeping seat carries little personal cost.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computation_research_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, computation_research_gatekeepers, beneficiary).

% The mainstream theoretical-computer-science and complexity-theory community. The physical Church-Turing claim certifies that their formal models describe all physically possible computation, so complexity classes, intractability results, and cryptographic assumptions hold of the world unconditionally. Research legitimacy, funding, and talent concentrate inside Turing-consistent programs, and the community runs no risk of a physical discovery invalidating its foundations. Individual members could move fields, but the community's collective position depends on the boundary holding.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, theoretical_cs_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% The quantum-computing research field with its industrial and governmental sponsors. The physical Church-Turing claim is what makes quantum advantage foundational-safe: quantum computers compute Turing-computable functions faster, so the field's most spectacular claims reinforce rather than threaten the classical foundations of computer science, and this certification secures funding and publication. Only a demonstration that quantum processes exceed Turing computability would flip this seat from beneficiary to target.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_computing_mainstream, beneficiary,
    organized, biographical, mobile, global).

% Researchers who argue that some physical processes — Malament-Hogarth spacetimes, closed timelike curves, supertasks, infinite-precision dynamics — could compute functions beyond Turing reach. They publish in philosophy-of-physics and logic venues, but grant panels and mainstream CS venues treat their presupposition as a settled impossibility. Careers built on the program make leaving costly in a specific way: exit means abandoning the claim their professional identity is constituted by, not merely changing topic. Their proposals face a burden of proof the mainstream never faces.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Researchers pursuing computation by continuous physical dynamics — Pour-El/Richards-type results, Blum-Shub-Smale machines, infinite-precision claims — where a physical system's trajectory is taken to decide a non-Turing-computable predicate. The physical Church-Turing claim is deployed against them as a screening assumption: noise and finite precision are held to collapse their models into Turing bounds, so their strongest claims are dismissed before full adjudication. Their mathematical and physical skills transfer to numerical analysis and control theory, so exit is possible, but only at the cost of abandoning the research program itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, analog_computation_researchers, payer,
    moderate, biographical, constrained, global).

% Physicists proposing that spacetime structure or quantum gravity might permit non-Turing computation — black-hole evaporation deciding non-computable predicates, Planck-scale structures with unbounded capacity. Their proposals are typically dismissed at grant-review or seminar stage by invocation of the physical Church-Turing claim before the physics is worked out, and they lack a standing venue where the claim's application to their domain would be adjudicated on the evidence. Their objections surface mainly as rejected proposals.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_gravity_computation_proposers, excluded,
    moderate, generational, constrained, global).

% Philosophers of computation and science who analyze what the Church-Turing thesis claims, which of its readings is testable, and where conflations between the readings do argumentative work. They document the dispute between the readings and the burden-of-proof asymmetries in gatekeeping practice, but they collect no rents from the arrangement and bear none of its costs; their seat sees the full structure.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_of_computation_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single shared boundary assumption for the computation sciences: that Turing-computability bounds what any physical process can compute. Hardware design, complexity analysis, cryptographic assumptions, and physics-of-computation research all proceed without hedging against hyper-computational physics, and proposals are screened against one common standard of physical realizability.
% TRANSFER_FUNCTION: Moves research legitimacy, grant funding, publication access, and career security within the computation sciences from programs that contest the Turing boundary (hypercomputation, analog-exceeds-Turing, exotic-spacetime computation) to programs operating inside it; adjudicative authority over the boundary accrues to the gatekeeping seats.
% ABSENT_VOICES: Quantum-gravity computation proposers are dismissed before their physics is developed and hold no standing seat where the claim's application to their domain is adjudicated; hypercomputation researchers hold venues in philosophy but not in the funding and mainstream-CS rooms where the gate actually operates. Future researchers whose programs the current settlement forecloses are absent by construction.
% DISAPPEARANCE_RATIONALE: If the physical Church-Turing claim ceased to operate as a research gate overnight, hypercomputation and exotic-spacetime proposals would receive full evidentiary hearings, funding agencies would need new criteria for physical-realizability screening, the CS-physics interface would restructure around open computational limits, and the mainstream's unconditional-foundation guarantee would lapse — the Turing model would survive as mathematics but lose its physical monopoly. The named parties' arrangements visibly depend on the constraint.
% FOUNDING_PROBLEM: Whether the Turing model of computation is the complete account of physically possible computation: the physical reading was consolidated (Deutsch 1985 and the physics-of-computation program) to give computer science a physically unconditional foundation and to settle whether physics must be searched for processes exceeding Turing reach.
% FOUNDING_PROBLEM_CORROBORATION: The question's live status is attested from outside the benefiting parties: hypercomputation and analog-computation researchers stake their programs on it, the philosophy-of-computation literature (Copeland's critique of thesis-conflation, Piccinini's physical-computation analyses) documents it as open, and quantum-gravity computation proposals presuppose it. No party outside the beneficiary set attests that the question is settled; the mainstream's settledness claim is precisely what the outside seats contest.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55): the gate moves legitimacy, funding, and career security from a small minority of hyper-Turing claimants to the incumbent paradigm, but the transferred goods are relative advantages (incumbency protection, foreclosed competition) rather than seized resources, and the field bearing the costs is small. Suppression (0.60) is real but non-state: it operates through review norms, funding screens, and a burden-of-proof asymmetry under which hyper-Turing proposals must overcome a settled-impossibility presumption the mainstream never faces. Theater (0.30) is moderate-low: the boundary-maintenance function is real, but a visible share of maintenance activity is performative — textbooks and surveys presenting the physical claim as proven (borrowing the mathematical result's certainty for an unproven empirical claim), review rejections citing the thesis without engaging the physics. Accessibility collapse is moderate (0.50): once the claim is accepted, hyper-Turing alternatives appear largely closed, but its empirical status keeps them partially open in principle — exotic spacetime structure and analog dynamics remain live channels. Resistance (0.55) is substantial for so small a field: an organized literature (Copeland's conflation critique, Pour-El/Richards-type results, Malament-Hogarth analysis, Piccinini's physical-computation work) actively contests the gate's scope and evidential basis. The measurement series share one grid (T=0 ≈ 1985, Deutsch's physical Church-Turing principle and the institutionalization of physics-of-computation; T=40 ≈ 2025): extraction and enforcement rose as the gate hardened through the hypercomputation debates of the 1990s–2000s, then plateaued with slight decay as the contest gained respectable venues. Identity-lock dynamics: the primary target's exit cost is professional-identity fusion — careers constituted by the contested claim — so gate pressure on them produces persistence at the margin rather than exit or revolt; a gate facing mobile targets would see either exit or open conflict, not marginalization.
 *
 * PERSPECTIVAL GAP:
 *   From the establishment and gatekeeper seats the arrangement is a settled foundation and quality control: the Turing model's guarantees hold of the world, and screening out hyper-Turing proposals protects rigor and resources. From the payer seats the same structure is a burden-of-proof asymmetry that forecloses a research frontier without adjudication. The excluded quantum-gravity seat experiences it as pre-emption — dismissal before the physics is developed. Same-level dynamics sharpen the divergence: establishment researchers and hypercomputation researchers hold comparable credentials and nominal academic standing, but the constraint differentiates them — proximity to the paradigm's center confers adjudicative power and arbitrage exit, while margin position leaves the targets with constrained or identity-locked exit. The power asymmetry between nominally equal academics is produced by the constraint, not prior to it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment and the quantum mainstream are declared beneficiaries (d near the beneficiary end): the boundary insulates their paradigms and certifies their claims as foundational-safe. The gatekeepers administer the boundary and collect adjudicative authority — structurally near the beneficiary end as well, which the institutional directionality override makes explicit (a canonical institutional fallback calibrated for neutral regulatory seats would misplace these seats toward symmetric). Hypercomputation and analog-computation researchers are the declared victims (d near the target end), with identity-locked and constrained exit respectively pushing them toward full-target. The quantum seat's position is contingent: mainstream quantum advantage computes Turing-computable functions faster and benefits from the certification; a demonstrated hyper-Turing quantum process would flip that seat from beneficiary to target (see omega victim_set_boundary). Quantum-gravity proposers are excluded rather than coordinated — their pre-emption is part of what the gate's enforcement maintains. The philosophy-of-computation seat is analytical and symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabelings. First, from the mainstream seat the gate presents as pure coordination — a settled law steering effort away from the impossible — which would erase the victims; the tangled_rope structure keeps the coordination function and the asymmetric costs simultaneously visible, so the truth-value question (omega empirical_truth_of_physical_claim) governs the balance instead of being presupposed. Second, if the founding problem were dead the arrangement would be a zombie — a gate maintained over a settled question; but the founding problem (does physics permit non-Turing computation?) is live and corroborated from outside the beneficiary set, so the arrangement does live, contested work. The R5 mismatch check (live status × world_rearranges verdict) raises no capture flag: the gate's persistence tracks an open empirical question, not an outlived mandate. Should the question be resolved in either direction, the classification should be re-run: vindication of the physical claim pushes the arrangement toward rope (and the bare regularity toward mountain), refutation pushes it toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the church_turing_thesis kernel — the physical_claim_reading. What would the sibling readings change structurally if adopted in place of this one?',
    'Compare against the sibling stories church_turing_thesis__mathematical_definition_reading and church_turing_thesis__epistemological_boundary_reading: the definitional reading has no victims (a convention takes nothing from anyone, ε near zero); the epistemological reading''s target set is limited to those claiming provable-computability gaps, and its enforcement is logical rather than institutional.',
    'The disagreement is located in the thesis''s modal status — convention, empirical claim, or epistemic limit — which determines testability, victim set, and enforcement mode. Adopting a sibling reading dissolves this story''s victim structure entirely; the three stories must not be merged into one constraint with a variable ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    empirical_truth_of_physical_claim,
    'Is the physical claim actually true — does any physically realizable process compute beyond Turing-machine computability?',
    'A demonstrated physical hypercomputation (a physically realizable Malament-Hogarth trajectory, an analog system deciding a non-computable predicate at finite precision, an accepted quantum-gravity computation result), or a physics-level argument closing every proposed channel.',
    'If the claim is true, the gate''s coordination function is genuine and much of the measured extraction is the price of not wasting effort on the impossible (pushing the arrangement toward rope); if false, the gate suppresses a real frontier and the arrangement is closer to snare. The reading''s own ε depends on this unresolved empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_truth_of_physical_claim, empirical, 'Whether the constraint''s factual core holds, which conditions the extraction/coordination balance.').

omega_variable(
    natural_law_vs_constructed_gate,
    'Is the constraint a natural law of physics (which would persist regardless of defenders and have no beneficiaries) or a constructed research-governance arrangement with identifiable beneficiaries?',
    'Separate the factual regularity (if any) from the gatekeeping arrangement built on it: the regularity would be a mountain; the gate — burden-of-proof asymmetries, funding screening, publication norms — is maintained by identifiable actors who collect incumbency benefits. Examine whether the gate''s strictness tracks the evidence or the paradigm''s protection needs.',
    'If the operative constraint is the natural regularity alone, the beneficiary declarations should be withdrawn and the story re-authored as a mountain; if the operative constraint is the gate, the beneficiary declarations stand and false-summit evaluation applies to any natural-law presentation of the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_gate, conceptual, 'Whether the constraint under classification is the physical regularity or the social gate built upon it.').

omega_variable(
    victim_set_boundary,
    'Which research programs actually bear the constraint''s costs — specifically, do mainstream quantum-supremacy programs fall inside the victim set?',
    'Adjudicate each program''s claim: standard quantum advantage computes Turing-computable functions faster and is not a target; only programs claiming physical non-Turing computability (hypercomputation, analog-exceeds-Turing, exotic-spacetime computation) bear the gate. A demonstrated quantum process exceeding Turing would move the entire quantum-computing seat from beneficiary to target.',
    'The victim set is currently small and moderate-power; if quantum computation were shown to exceed Turing, the victim set would expand to include the field''s institutional core, the measured extraction would rise sharply, and the arrangement''s type would shift toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Boundary of the victim set: hyper-Turing claimants only, with the quantum seat''s position contingent on its claims.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression structural (funding structures, review norms, publication barriers) or internalized (researchers pre-emptively self-censor hyper-Turing proposals as futile)?',
    'Post-liberalization trajectory: if gate strictness were relaxed (e.g., a funding program explicitly soliciting physical-CTT tests) and hyper-Turing proposals still failed to appear at expected rates, the residual gap would measure internalized suppression; proposal-pipeline data before and after gate relaxations would resolve it.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the gate with them across venues, and relaxing enforcement would not immediately restore the suppressed programs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in the research gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_physical_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ctt_physical_tr_t8, church_turing_thesis__physical_claim_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ctt_physical_tr_t16, church_turing_thesis__physical_claim_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ctt_physical_tr_t24, church_turing_thesis__physical_claim_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(ctt_physical_tr_t32, church_turing_thesis__physical_claim_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(ctt_physical_tr_t40, church_turing_thesis__physical_claim_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(ctt_physical_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ctt_physical_be_t8, church_turing_thesis__physical_claim_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(ctt_physical_be_t16, church_turing_thesis__physical_claim_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ctt_physical_be_t24, church_turing_thesis__physical_claim_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(ctt_physical_be_t32, church_turing_thesis__physical_claim_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(ctt_physical_be_t40, church_turing_thesis__physical_claim_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ctt_physical_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ctt_physical_su_t8, church_turing_thesis__physical_claim_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(ctt_physical_su_t16, church_turing_thesis__physical_claim_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(ctt_physical_su_t24, church_turing_thesis__physical_claim_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(ctt_physical_su_t32, church_turing_thesis__physical_claim_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(ctt_physical_su_t40, church_turing_thesis__physical_claim_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, identity_coordination).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Church-Turing thesis' covers three structurally distinct claims (ε-invariance decomposition): a mathematical definition (untestable, no victims), an epistemological boundary about provability (logically enforced), and a physical claim about the universe (institutionally enforced as a research gate). This story is the third. Their ε values differ widely; in particular, the definitional reading's mathematical certainty is routinely cited as if it established the physical claim — the conflation is part of this arrangement's theater and its enforcement. The stories form one constraint family linked via affects_constraints; the upstream (definitional) certitude feeds the downstream (physical) gate's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
