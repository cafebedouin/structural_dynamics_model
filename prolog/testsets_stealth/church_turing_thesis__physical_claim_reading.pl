% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Physical Church-Turing Thesis as Enforced Research Boundary
 *   domain: philosophy of mathematics / foundations of computation
 *
 * SUMMARY:
 *   The physical Church-Turing thesis — no physical process computes beyond
 *   Turing-machine computability — began as a plausible extrapolation from
 *   Turing's definitional analysis and Gandy's bounds on discrete devices,
 *   and has been institutionalized as settled doctrine: textbooks state it
 *   without qualification, referees reject beyond-Turing proposals on its
 *   authority, and funding taxonomies file the area as speculative. The
 *   standing arrangement under contest (and the fixed epsilon referent for
 *   this story, assessed by the physical reading's own lights, which hold the
 *   claim open and testable) is this institutionalized doctrine-treatment,
 *   not the underlying physics. Interval maps approximately 1980-2025
 *   (Gandy's consolidation to the present). KEY AGENTS (by structural
 *   relationship): - computational_complexity_community: Primary beneficiary
 *   (organized/identity_locked) — framework vindicated, careers constituted
 *   by the frame - cryptographic_security_establishment: Secondary
 *   beneficiary (powerful/constrained) — security proofs presuppose
 *   Turing-bounded adversaries - cs_funding_agencies: Agenda setter
 *   (institutional/constrained) — allocates resources, files beyond-Turing
 *   work as speculative - hypercomputation_researchers: Primary target
 *   (powerless/identity_locked) — bears marginalization and funding exclusion
 *   - unconventional_computing_labs: Dual-positioned payer/beneficiary
 *   (moderate/constrained) — bears gatekeeping, collects umbrella legitimacy
 *   - quantum_gravity_theorists: Excluded voice (organized/mobile) — would
 *   contest the claim's settled status, outside the room -
 *   philosophy_of_computation_scholars: Analytical observer
 *   (moderate/analytical) — sees the full structure including the
 *   version-conflation Constraint-family note (epsilon-invariance
 *   decomposition): the colloquial label Church-Turing thesis covers three
 *   structurally distinct claims. The mathematical_definition_reading (true
 *   by convention, no parties harmed, epsilon near zero) and the
 *   epistemological_boundary_reading (provability boundary, few parties,
 *   low-moderate epsilon) are separate stories; THIS story owns the
 *   physical_claim_reading, whose epsilon is moderate precisely because the
 *   claim is empirically contestable while the arrangement treats it as
 *   closed. The definitional reading's accumulated prestige is the upstream
 *   resource the physical arrangement borrows — the conflation is the
 *   extraction channel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.52).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Physical Church-Turing Thesis as Enforced Research Boundary").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy of mathematics / foundations of computation").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'bcc9b4de-090f-49d3-bdd0-cc93f87104f3').
narrative_ontology:cs_kernel_codification('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', fixed_text).
narrative_ontology:cs_authority_grounding('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', expertise).
narrative_ontology:cs_interpretation_layer_present('bcc9b4de-090f-49d3-bdd0-cc93f87104f3').
narrative_ontology:cs_reading_relation('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', foundational, physical_processes_turing_bounded).
narrative_ontology:cs_axiom_status(physical_processes_turing_bounded, holdable).
narrative_ontology:cs_axiom_grounding('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', physical_processes_turing_bounded, empirically_contingent).
narrative_ontology:cs_axiom('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', secondary, beyond_turing_inquiry_is_legitimate_science).
narrative_ontology:cs_axiom_status(beyond_turing_inquiry_is_legitimate_science, holdable).
narrative_ontology:cs_axiom_grounding('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', beyond_turing_inquiry_is_legitimate_science, instrumental).
narrative_ontology:cs_reference_frame('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', empirical_generalization_over_physical_processes).
narrative_ontology:cs_drift_state('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', contemporary_quantum_computing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bcc9b4de-090f-49d3-bdd0-cc93f87104f3', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, computational_complexity_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, cryptographic_security_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, unconventional_computing_labs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, unconventional_computing_labs).
narrative_ontology:constraint_vindicates(church_turing_thesis__physical_claim_reading, gandy_discrete_mechanical_device_bounds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the field's core problems, curricula, and hiring criteria around the Turing-bounded model of computation; daily technical work presupposes that computable means Turing-computable. Reputation, publication channels, and the shared problem landscape are all routed through venues that share this presupposition. Leaving the frame would mean abandoning the accumulated toolkit that constitutes professional competence.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computational_complexity_community, beneficiary,
    organized, generational, identity_locked, global).

% States and proves security of cryptosystems against adversaries modeled as arbitrary Turing-computable processes; the tractability half of the model is what separates existence-in-principle from feasible attack. Deployed standards, certification regimes, and decades of proof libraries are built on this modeling. Adopting a framework that admitted non-Turing adversaries would require rebuilding the proof infrastructure from the ground up.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, cryptographic_security_establishment, beneficiary,
    powerful, biographical, constrained, global).

% Runs competitive grant programs for computing research. Proposal taxonomies classify beyond-Turing-computation work as speculative, steering funds toward mainstream theory, systems, and AI lines. Accountable to political principals for measurable output, the agencies rarely fund programs whose success would appear to require new physics.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, cs_funding_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% A small, geographically dispersed group studying models such as Malament-Hogarth spacetimes, infinite-time register machines, and accelerated Turing machines. Papers land in interdisciplinary or philosophy-of-physics venues; grant rejections are routine; invitations to mainstream theory conferences are rare. Decades of specialized expertise make departure costly, and the group is too small and scattered to mount coordinated pressure.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    powerless, biographical, identity_locked, global).

% Works on molecular, DNA, chemical, and annealing substrates. To stay fundable these groups frame results as novel implementations within the standard computability frame; occasional flirtation with beyond-Turing claims draws reviewer hostility, while staying inside the frame yields umbrella legitimacy and access to dedicated unconventional-computing funding lines.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, unconventional_computing_labs, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, unconventional_computing_labs, beneficiary).

% Some argue that quantum gravity may evade the classical bounds relied on in computability discussions and propose thought experiments probing physical computability at Planck or cosmological scales. They publish in physics venues, hold positions independent of computer science politics, and are essentially absent from theory conference program committees and funding panels — the rooms where the computability frame is maintained.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_gravity_theorists, excluded,
    organized, generational, mobile, global).

% Analyzes what the thesis asserts, documents the historical conflation of its definitional, epistemological, and physical versions, and assesses testability. Holds no stake in research funding flows and sits outside the technical review loop; its analyses are the main external check on how the arrangement describes itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, philosophy_of_computation_scholars, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared formal model of effective computation, enabling machine-independent problem classification, transferable algorithms and results across subfields, verifiable security reductions, and a common standard for what counts as a constructive procedure — solved once, centrally, instead of per-researcher ad hoc notions of mechanical method.
% TRANSFER_FUNCTION: Moves credibility, funding, and career security away from research programs that posit or seek physical processes exceeding Turing computability and toward programs operating strictly within the Turing frame. Nothing material is moved; the transfer is epistemic-resource allocation mediated by review norms and funding taxonomies.
% ABSENT_VOICES: Hypercomputation proponents and quantum-gravity theorists who regard the physical version as an untested conjecture are largely absent from theory program committees and funding panel rosters; philosophy-of-computation critics who document the version-conflation sit outside the technical conversation entirely. The unanimity of the frame is partly an artifact of who was never in the room.
% DISAPPEARANCE_RATIONALE: If the settled status of the physical claim vanished overnight, funding calls would open to beyond-Turing proposals, referees could no longer reject on Church-Turing grounds alone, curricula would mark the physical version as an open empirical question, and a wave of speculative programs would launch while security proofs would need explicit caveats about the adversary model. The mathematical content (Turing machines exist and define a class) persists; the arrangement around the physical claim does not.
% FOUNDING_PROBLEM: Early twentieth-century mathematics lacked any rigorous, intersubjective criterion of mechanical calculability, blocking resolution of Hilbert's Entscheidungsproblem. Turing's 1936 analysis supplied the definitional solution; the physical reading was layered on later, consolidated by Gandy's 1980 bounds on discrete mechanical devices.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of computation outside the benefiting parties (documenting Turing's own statements and the later conflation of versions) attest that the original founding problem was definitional and was solved, distinct from the physical claim; published analyses of Gandy's theorem corroborate that its bounds cover only discrete deterministic devices, leaving the physical generalization unproven. Mainstream theorists, by contrast, attest the physical problem is effectively settled by accumulating absence of counterexamples. Both attestations come from outside the direct beneficiary set; the dispute between them is itself the signal.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.52): the arrangement diverts real resources — careers, grants, venue access — from a small research population, and collects credibility rents by presenting an open empirical question as closed, but the diverted volume is small relative to the field and the coordination delivered is enormous. Suppression (0.58) is higher than extraction because persistence depends on actively maintaining the settled status against periodic challenge: review norms, funding classifications, and canonization do the coercive work; note suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater ratio (0.22) is low: the coordination function is exercised intensively and genuinely every day; the theatrical component is the growing share of thesis-invocations that are ritual citations rather than engagements with the physics. Accessibility_collapse (0.35) is far below mountain range: alternatives remain visible and publishable in peripheral venues, so the constraint closes mainstream channels without eliminating exits. Resistance (0.45) reflects a continuous critical literature, recurring quantum-supremacy-adjacent friction, and sustained philosophical critique. The three temporal series run on one shared six-point grid (0, 9, 18, 27, 36, 45) so every metric is authored at every examined time point; all three drift monotonically — there is no oscillation to model, so no cyclical battery is invoked. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity hardening (reflexive rejection norms and funding-taxonomy maturation through the crypto boom and post-supremacy era), which is exactly the enforcement-infrastructure dynamic the scalar base_properties.suppression cannot carry alone.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the engine derives that divergence from the structural data rather than from this claim. From the complexity community's seat the thesis is a self-evident truism its members have never seen violated — the arrangement looks like accurate bookkeeping. From the cryptographic seat it is the load-bearing wall of every security proof. From the hypercomputation researcher's seat the same structure operates as gatekeeping that converts an open physics question into a career hazard. From the funding agency's seat it is prudent portfolio hygiene. From the excluded physicist's seat it is an untested generalization defended by people who never had to defend it. Same nominal academic standing, radically different experienced constraint — differentiated by exit options and role, not by global power.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiary groups derive low directionality (subsidized seats): the complexity community is identity-locked into the frame it benefits from, pushing it toward the full-beneficiary end despite having no arbitrage exit; the crypto establishment is powerful and constrained, near-beneficiary with mild exposure. The two declared victim groups derive high directionality: hypercomputation researchers are powerless and identity-locked, sitting nearest the full-target end; unconventional computing labs are moderated by their secondary beneficiary position — they bear gatekeeping costs but collect umbrella legitimacy, pulling their derived d down from the pure-target end. Funding agencies sit mid-range as administrators who neither collect the rents nor bear the costs. No directionality_overrides are authored: the derivation chain produces the right relationships from the declared roles and exits, and the available override mechanism keys on power_atom alone — an override for the moderate-power labs would misfire onto the moderate-power philosophy observers, whose relationship is entirely different. The labs' dual position is carried by secondary_role instead, which is the structurally honest channel for it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — formalizing effective calculability — was solved in 1936, and the definitional mandate is dead; the physical overlay inherited the solved problem's prestige and persists under a newer, implicit mandate (bounding rational research expectations) that was never consciously adopted. This is the classic mandatrophy shape: the arrangement outlived its declared function and continues on inertia plus active maintenance. The tangled_rope classification prevents two opposite mislabels: calling the arrangement a pure snare would erase the genuine, heavily-used coordination function (a shared computability standard underwriting verification and security); calling it a rope would erase the asymmetric extraction channel (the conflation through which definitional prestige subsidizes an unproven physical claim at the expense of a small research population). On the R5 mismatch consumer: founding_problem_status is authored contested rather than dead, because the parties genuinely dispute whether a live problem remains — mainstream attests the physical question is settled-by-absence, critics attest it was never opened. Had status been authored dead alongside verdict world_rearranges, the capture/zombie flag would fire; the honest reading is that the zombie question is itself the live dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_enforced_doctrine,
    'Is the measured extraction a property of the thesis itself (if it is a true law of nature, the targets are misallocating against reality and the arrangement merely tracks physics) or of the social arrangement enforcing it ahead of the evidence?',
    'Either a confirmed physical counterexample (instantiated Malament-Hogarth-type process or equivalent) or a rigorous extension of Gandy-style bounds to broad classes of physical systems, including quantum and relativistic regimes.',
    'If the thesis is a genuine law, the arrangement trends toward mountain-tracking and the declared victims dissolve into misallocated effort; if it is doctrine outrunning evidence, the tangled-rope extraction stands and hardens toward snare as enforcement deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_enforced_doctrine, empirical, 'Whether the constraint is natural law or constructed arrangement is the master uncertainty for this story.').

omega_variable(
    sibling_reading_structural_delta,
    'How would instantiating a sibling reading change this constraint''s structure?',
    'Author the sibling stories: under the mathematical_definition_reading the victim set vanishes (no one is harmed by a convention), epsilon drops toward zero, and the type becomes rope-or-conventional-mountain; under the epistemological_boundary_reading the parties shrink to proof theorists and the physical-evidence question becomes irrelevant.',
    'Confirms that the moderate epsilon and the victim set are properties of THIS reading only, not of the kernel label; cross-reading comparison is valid only across the decomposed family, never within one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame routing: records that this story is one reading of the kernel and what the siblings would change.').

omega_variable(
    hypercomputation_physical_instantiability,
    'Can any proposed beyond-Turing physical process (Malament-Hogarth spacetimes, supertask machines, quantum-gravity evasions) be physically instantiated rather than merely consistent with some idealized theory?',
    'Progress in quantum gravity and cosmological observation bearing on whether the required spacetime structures or divergent regimes are realizable; convergence of idealized models with or against physical realizability.',
    'A confirmed instantiation collapses the arrangement''s warrant and exposes the enforcement as suppression of viable research (snare-direction); robust impossibility results covering realistic physics push the arrangement toward legitimate mountain-tracking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypercomputation_physical_instantiability, empirical, 'The empirical hinge on which the physical reading itself turns.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the marginalization of beyond-Turing research structural (funding taxonomies, venue norms, review reflexes) or internalized (self-censorship, anticipation of crank-stigma, identity fusion with the mainstream frame)?',
    'Post-exit trajectory: track researchers who left the specialty — if stigma-shaped self-limitation persists after moving to neutral fields, part of the suppression is internalized; compare proposal-submission rates against acceptance rates to separate gatekeeping from self-selection.',
    'If substantially internalized, effective suppression exceeds the structural measure and would survive removal of the enforcement machinery; if structural, dismantling the taxonomy and review norms would release the suppressed population quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism ambiguity in the suppression scalar: structural barriers versus carried cognitive patterns.').

omega_variable(
    extended_ct_conflation_contamination,
    'Is this story''s epsilon contaminated by conflation with the extended Church-Turing thesis (poly-time equivalence of reasonable models), which faces genuinely different pressure from quantum computing?',
    'Audit the enforcement episodes counted in the suppression series: exclude any episode whose stated ground was polynomial-efficiency rather than Turing-boundedness; re-score if a material fraction of the enforcement record is actually extended-thesis enforcement.',
    'If contamination is material, the physical-claim story is over-measuring extraction that belongs to a separate, decomposed constraint (the extended thesis under quantum pressure); the family gains another member and this story''s epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extended_ct_conflation_contamination, conceptual, 'Decomposition-discipline guard: keeping the physical claim separable from the efficiency claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctphys_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ctphys_tr_t0, observed).
narrative_ontology:measurement(ctphys_tr_t9, church_turing_thesis__physical_claim_reading, theater_ratio, 9, 0.12).
narrative_ontology:measurement_basis(ctphys_tr_t9, observed).
narrative_ontology:measurement(ctphys_tr_t18, church_turing_thesis__physical_claim_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(ctphys_tr_t18, observed).
narrative_ontology:measurement(ctphys_tr_t27, church_turing_thesis__physical_claim_reading, theater_ratio, 27, 0.17).
narrative_ontology:measurement_basis(ctphys_tr_t27, observed).
narrative_ontology:measurement(ctphys_tr_t36, church_turing_thesis__physical_claim_reading, theater_ratio, 36, 0.2).
narrative_ontology:measurement_basis(ctphys_tr_t36, observed).
narrative_ontology:measurement(ctphys_tr_t45, church_turing_thesis__physical_claim_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement_basis(ctphys_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(ctphys_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ctphys_be_t0, observed).
narrative_ontology:measurement(ctphys_be_t9, church_turing_thesis__physical_claim_reading, base_extractiveness, 9, 0.41).
narrative_ontology:measurement_basis(ctphys_be_t9, observed).
narrative_ontology:measurement(ctphys_be_t18, church_turing_thesis__physical_claim_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement_basis(ctphys_be_t18, observed).
narrative_ontology:measurement(ctphys_be_t27, church_turing_thesis__physical_claim_reading, base_extractiveness, 27, 0.47).
narrative_ontology:measurement_basis(ctphys_be_t27, observed).
narrative_ontology:measurement(ctphys_be_t36, church_turing_thesis__physical_claim_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement_basis(ctphys_be_t36, observed).
narrative_ontology:measurement(ctphys_be_t45, church_turing_thesis__physical_claim_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement_basis(ctphys_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(ctphys_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ctphys_su_t0, observed).
narrative_ontology:measurement(ctphys_su_t9, church_turing_thesis__physical_claim_reading, suppression_requirement, 9, 0.43).
narrative_ontology:measurement_basis(ctphys_su_t9, observed).
narrative_ontology:measurement(ctphys_su_t18, church_turing_thesis__physical_claim_reading, suppression_requirement, 18, 0.46).
narrative_ontology:measurement_basis(ctphys_su_t18, observed).
narrative_ontology:measurement(ctphys_su_t27, church_turing_thesis__physical_claim_reading, suppression_requirement, 27, 0.5).
narrative_ontology:measurement_basis(ctphys_su_t27, observed).
narrative_ontology:measurement(ctphys_su_t36, church_turing_thesis__physical_claim_reading, suppression_requirement, 36, 0.54).
narrative_ontology:measurement_basis(ctphys_su_t36, observed).
narrative_ontology:measurement(ctphys_su_t45, church_turing_thesis__physical_claim_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement_basis(ctphys_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label Church-Turing thesis decomposes into three structurally distinct constraints per the epsilon-invariance principle. The mathematical_definition_reading is upstream (established, conventional, epsilon near zero, no parties); the epistemological_boundary_reading is midstream (provability boundary, few parties); this physical_claim_reading is downstream (contested, empirically open, moderate epsilon, full party structure). The upstream definitional success is the prestige resource the downstream physical arrangement borrows — the conflation of versions is the extraction channel, which is why the family edges run definitional -> epistemological -> physical. Each story carries its own epsilon, beneficiaries, victims, and claimed type; sibling stories should reciprocate the links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
