% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Non-Delegation Boundary (Separation of Powers - Formalist Reading)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This story instantiates the FORMALIST READING of the
 *   separation_of_powers_text kernel as a clean, epsilon-invariant
 *   constraint: separation of powers as strict, impermeable branch boundaries
 *   under which Congress may not delegate legislative authority to
 *   administrative agencies. Where enforced, the boundary voids broad
 *   statutory delegations; administrative agencies lose the authority their
 *   programs run on; regulatory capacity contracts to what Congress can
 *   specify in statutory text and what courts will uphold. The delegative
 *   arrangement the formalist critique attacks is NOT this story's referent -
 *   epsilon is authored for the strict-boundary arrangement itself, the
 *   standing arrangement this story is about (see omega
 *   epsilon_referent_reading_index). Sibling readings (functionalist_reading,
 *   unitary_executive_reading) are separate constraints linked through
 *   network.affects_constraints; their structural data live in their own
 *   files. Claim/metric independence: the claimed type (tangled_rope - a
 *   genuine anti-concentration coordination function plus asymmetric
 *   extraction requiring active judicial enforcement) is stated independently
 *   of the metrics, which describe substantially extractive, heavily
 *   suppressive operation with a dormancy-shaped temporal arc.
 *
 * KEY AGENTS:
 *   - - formalist_judiciary: agenda-setter and beneficiary (institutional / identity_locked) - administers the boundary, collects interpretive authority with each enforcement, fused with the originalist method
 *   - - administrative_agencies: primary target (institutional / trapped) - delegations voided; programs, budgets, and personnel stand or fall with delegation validity
 *   - - deregulatory_industries: primary material beneficiary (powerful / arbitrage) - compliance relief converts destroyed regulatory capacity into margin
 *   - - approval_dependent_industries: secondary target (powerful / constrained) - market access runs through the agency gatekeeping the boundary destabilizes
 *   - - regulatory_program_dependents: diffuse target (powerless / trapped) - bear program lapse; no collective seat in the constitutional litigation
 *   - - congressional_institutions: dual payer/beneficiary (institutional / constrained) - gains a standing weapon against agency power, loses the drafting flexibility its workload runs on
 *   - - career_agency_experts: excluded voice - technical judgment devalued by the boundary, no seat in the constitutional conversation
 *   - - administrative_law_academy: analytical observer - maps the doctrine's history and effects from a seat that collects nothing and pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Non-Delegation Boundary (Separation of Powers - Formalist Reading)").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '63d0cf0d-e1e2-4227-a9f5-2af40c860427').
narrative_ontology:cs_kernel_codification('63d0cf0d-e1e2-4227-a9f5-2af40c860427', fixed_text).
narrative_ontology:cs_authority_grounding('63d0cf0d-e1e2-4227-a9f5-2af40c860427', lineage).
narrative_ontology:cs_interpretation_layer_present('63d0cf0d-e1e2-4227-a9f5-2af40c860427').
narrative_ontology:cs_reading_relation('63d0cf0d-e1e2-4227-a9f5-2af40c860427', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('63d0cf0d-e1e2-4227-a9f5-2af40c860427', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('63d0cf0d-e1e2-4227-a9f5-2af40c860427', foundational, legislative_power_nondelegable).
narrative_ontology:cs_axiom_status(legislative_power_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('63d0cf0d-e1e2-4227-a9f5-2af40c860427', legislative_power_nondelegable, conventional).
narrative_ontology:cs_axiom('63d0cf0d-e1e2-4227-a9f5-2af40c860427', secondary, intelligible_principle_test_invalid).
narrative_ontology:cs_axiom_status(intelligible_principle_test_invalid, holdable).
narrative_ontology:cs_axiom_grounding('63d0cf0d-e1e2-4227-a9f5-2af40c860427', intelligible_principle_test_invalid, instrumental).
narrative_ontology:cs_reference_frame('63d0cf0d-e1e2-4227-a9f5-2af40c860427', strict_impermeable_separation_framework).
narrative_ontology:cs_drift_state('63d0cf0d-e1e2-4227-a9f5-2af40c860427', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('63d0cf0d-e1e2-4227-a9f5-2af40c860427', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, deregulatory_industries).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_program_dependents).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, approval_dependent_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congressional_institutions).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, congressional_institutions).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, nondelegation_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, formalist_separation_originalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices committed to the formalist method read Article I's vesting of legislative power as forbidding its transfer to agencies. They decide when a statute crosses the line and strike the delegations that fail the test they apply. Their authority grows with each enforcement - the line exists only where they draw it - and their method, original meaning read through structural inference, is the only lens through which the text reaches modern government. Leaving the method would mean abandoning the jurisprudential identity their judicial practice is built on.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, formalist_judiciary, beneficiary).

% Federal agencies run environmental, workplace, financial, and health programs under statutory mandates Congress wrote in broad terms. Under this reading those broad mandates are void: each delegation a court strikes dissolves part of the agency's governing authority, and with it the agency's reason for existing. Exit is unavailable - an agency cannot relocate its mandate or re-charter itself. Its personnel, budgets, and programs stand or fall with the validity of the delegations.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, generational, trapped, national).

% Firms and trade associations whose costs fall when agency rules are struck down. They fund the litigation that raises delegation challenges and collect the compliance relief when challenges succeed. Their footprint spans jurisdictions: when federal rules fall they shift production, forum, and lobbying among states and across regimes, and their exposure to any single framework is limited.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, deregulatory_industries, beneficiary,
    powerful, biographical, arbitrage, national).

% Pharmaceutical, medical-device, and financial firms whose business models run through agency gatekeeping - approvals, charters, clearances. When the delegations behind those regimes are struck or destabilized, their paths to market lose their legal foundation. They cannot exit into a deregulated world, because their products cannot lawfully reach market without the agency determinations this reading puts in doubt.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, approval_dependent_industries, payer,
    powerful, biographical, constrained, national).

% Workers, consumers, patients, and communities whose protections - clean air, safe drugs, honest markets - are administered through the agencies. They bear program lapse directly and have no collective seat in the constitutional litigation that decides their protections' fate. The harms the programs address follow them wherever they live and work; there is no jurisdiction to exit to.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulatory_program_dependents, payer,
    powerless, generational, trapped, national).

% Congress wrote the broad statutes and holds formal power to legislate in detail. Under this reading it must either draft at a specificity the modern workload has never permitted - a capacity it has demonstrably lost - or watch its programs fall in court. Individual members gain a standing rhetorical weapon against agency overreach; the institution as a whole loses the governing flexibility its output depends on. Electoral time horizons reward the rhetoric; the drafting burden lands on the same body.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congressional_institutions, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__formalist_reading, congressional_institutions, beneficiary).

% Career scientists, economists, and administrators whose technical judgment supplies the content of agency programs. The constitutional conversation about their authority's validity is conducted entirely among courts, Congress, and industry litigants; the people whose judgment the delegations channel hold no seat in it. They would object that governance by fully specified statute cannot absorb the technical decisions their programs make daily.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, career_agency_experts, excluded,
    moderate, biographical, trapped, national).

% Scholars of constitutional and administrative law who map the doctrine's history, model its effects on regulatory capacity, and testify in the delegation litigation. The literature holds both readings simultaneously and can state the full structure - what enforcement costs, what dormancy preserved, where the revival is heading - from a seat that collects nothing and pays nothing.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, deregulatory_industries).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a fixed line between making law and executing it: Congress must itself resolve policy questions in statutory text, keeping legislative choices tied to electoral accountability and preventing any institution from both writing and enforcing the rules it writes.
% TRANSFER_FUNCTION: Moves lawmaking discretion from administrative agencies back to Congress (which must specify policy in detail) and adjudicatory power over the boundary to the courts; moves the protections agencies administered away from program dependents; moves compliance relief to regulated firms when delegations fall.
% ABSENT_VOICES: Career agency experts have no seat in the constitutional conversation that decides their authority's validity; regulatory program dependents appear only as intervenors when litigation reaches them; the functionalist reading's holders appear only as dissenters within the formalist framework's terms. Unanimity in formalist opinions arises from a conversation these seats were never admitted to.
% DISAPPEARANCE_RATIONALE: If the strict boundary vanished overnight, broad delegations would stand, regulatory programs would operate on intelligible-principle flexibility, and the compliance and approval structures built around agency discretion would persist - the delegative arrangement the formalist reading contests would simply become the uncontested arrangement. Agency authority, program administration, and industry compliance planning all depend on which boundary governs.
% FOUNDING_PROBLEM: Concentrated power: the framing generation's concern that combining legislative and executive authority in one institution produces arbitrary rule, carried into the modern form that Congress cannot launder hard policy choices through unelected experts to escape accountability for them.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: functionalist constitutional scholars and congressional-capacity studies attest the anti-concentration concern is genuine at the founding and in the abstract while disputing its present force, arguing the live modern problem runs the opposite direction (an institution that cannot legislate in detail); political-historical accounts of the 1937 settlement corroborate that the doctrine's abandonment was a deliberate institutional choice rather than a discovery that the founding concern was mistaken. The deregulatory industries that gain from enforcement do not attest the founding problem; they attest the compliance relief.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the boundary, where enforced, destroys standing regulatory capacity: delegations fall, programs lapse, and the destroyed protection converts into compliance relief for deregulatory industries and interpretive authority for the enforcing courts. Suppression is higher still (0.78) because the claim's own terms are 'strict' and 'impermeable' - persistence requires foreclosing the functionalist alternative (intelligible-principle flexibility, agency discretion), and the enforcement machinery (delegation-striking review) is the active suppression of that alternative. Suppression is a raw structural property here - judicial enforcement and doctrinal exclusivity - and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater_ratio is moderate (0.30) at interval end but carries a dormancy hump: from 1937 through the 1990s the doctrine was invoked rhetorically (concurrences, law reviews, dissents) while deciding almost nothing - performance without operative effect - peaking near 0.60 before declining as operative force returns. Accessibility_collapse is moderate-high (0.60): once the boundary binds, flexible alternatives collapse, though Congress retains the theoretical exit of legislating in detail and states retain regulatory space. Resistance is high (0.70): the entrenched administrative state, congressional program sponsors, and the functionalist judiciary held the doctrine in dormancy for roughly ninety years - the current revival is the constraint's second attempt to hold. Receipt and fix-cost: the material gains demonstrably accrue to deregulatory industries (gain_flow), with the judiciary's interpretive-authority gain secondary; fixing - abandoning the reading - is prohibitive for the seat that could fix it (the formalist majority), whose identity fusion with the method makes the one-opinion institutional exit unavailable to its holders. The measurement series run on one shared time grid (8 points; 1935-2026 mapped to 0-91) so every tracked metric is authored at every examined time point; the arc is U-shaped (operative, collapsed, dormant-theatrical, reviving), not oscillatory, and the end-state values are measured in the revival phase.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (formalist_judiciary), the boundary is accountability-protective coordination the court administers - rope-shaped from that seat. From the trapped target seats (administrative_agencies, regulatory_program_dependents), the same structure operates as enforced destruction of governing and protective capacity - snare-shaped. Deregulatory industries experience it as liberation: compliance costs fall with each struck delegation. Approval-dependent industries experience it as existential instability: their market-access pathways lose legal foundation. Congress experiences both faces at once - institutional weapon and drafting impossibility. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Deregulatory industries and the formalist judiciary sit near the beneficiary end (low d): the first collect compliance relief with arbitrage-grade exit across regimes, the second collect interpretive authority and administer the boundary they benefit from. Administrative agencies and regulatory program dependents sit near the target end (high d): trapped - an agency cannot re-charter its mandate, a program dependent cannot exit the harm the program addressed. Approval-dependent industries bear high d despite powerful standing because their exit is constrained: they cannot lawfully reach market without the agency determinations the boundary destabilizes. Congress sits mid-scale: it gains a rhetorical and institutional weapon against agency power while losing the drafting flexibility its output depends on. Career agency experts hold no directional position in the derivation - they are the excluded voice, commentary-grade only, and do not drive classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Reading the arrangement as pure coordination would hide that its enforcement transfers regulatory capacity to identifiable gainers while trapped parties bear the loss; reading it as pure extraction would erase the genuinely live anti-concentration problem the formalist tradition articulates and that corroborating scholarship outside the beneficiary set attests. The R5 mismatch check: founding_problem_status is contested (not dead) and disappearance_verdict is world_rearranges, so no zombie flag fires - the arrangement is not maintained after its problem died; the parties dispute whether the problem is live. The open mandatrophy question is narrower: whether the strict-boundary mandate has outlived the governance conditions (a Congress capable of legislating in detail) under which it could function as coordination rather than as capacity destruction. That question is carried by omega operative_force_trajectory and by the dormancy hump in the theater series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the formalist reading of the separation_of_powers_text kernel - how would instantiating the functionalist reading instead change the structural data?',
    'Author the functionalist reading as its own constraint story: administrative agencies leave the target set, regulatory capacity is preserved under the intelligible-principle test, suppression of the delegative arrangement drops, and the beneficiary set thins to whatever asymmetric residue the flexible framework carries.',
    'The victim set, epsilon, and type all move with the reading choice; the corpus measures the divergence between readings rather than resolving it inside one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one kernel, three readings; this story is the formalist instantiation.').

omega_variable(
    epsilon_referent_reading_index,
    'Is epsilon authored for the formalist strict-boundary arrangement itself (this story''s referent) or for the delegative administrative arrangement as the formalist critique assesses it?',
    'Corpus rule: the referent is the standing arrangement the story is about - the formalist boundary arrangement. The delegative status quo under formalist assessment is a different measurement and belongs in a separate story or in the formalist critique''s own file.',
    'Flipping the referent inverts the target set (agencies become the extraction vehicle rather than the party whose authority falls) and would push epsilon toward the formalist''s own assessment of the administrative state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_reading_index, conceptual, 'Epsilon referent fixed to the formalist arrangement itself, per the kernel-reading rule.').

omega_variable(
    operative_force_trajectory,
    'Will the formalist reading complete its revival and become the operative boundary, or does it remain a dormant position invoked rhetorically (as it was from 1937 through the 1990s)?',
    'Supreme Court holdings striking statutes on non-delegation grounds (not merely avoiding them via major-questions or clear-statement routes), and the fate of pending delegation challenges in the lower courts.',
    'If dormancy persists, measured extraction stays near the mid-series low, theater_ratio climbs past 0.5 (piton-side drift), and the arrangement computes as theatrical maintenance; if revival completes, the authored end-state values hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_force_trajectory, empirical, 'Whether the constraint''s revival completes or the doctrine returns to dormancy.').

omega_variable(
    formalist_identity_lock,
    'Does the formalist judiciary''s fusion with the originalist method make the reading self-sustaining regardless of consequences - would the reading survive evidence that strict enforcement collapses approval regimes its own coalition''s constituencies rely on?',
    'Track whether formalist judges narrow the doctrine when its application threatens drug approvals, financial charters, and other gatekeeping regimes their constituencies depend on; selective enforcement would show the identity frame bending to material interest.',
    'If identity holds, exit options for the agenda-setting seat stay identity_locked and the reading persists through consequence; if the frame breaks under material pressure, the reading''s persistence becomes ordinary coalition maintenance and the seat moves toward constrained or mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_identity_lock, empirical, 'Identity-lock dynamics of the formalist judiciary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 91).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sepa_tr_t13, separation_of_powers_text__formalist_reading, theater_ratio, 13, 0.55).
narrative_ontology:measurement(sepa_tr_t26, separation_of_powers_text__formalist_reading, theater_ratio, 26, 0.6).
narrative_ontology:measurement(sepa_tr_t39, separation_of_powers_text__formalist_reading, theater_ratio, 39, 0.58).
narrative_ontology:measurement(sepa_tr_t52, separation_of_powers_text__formalist_reading, theater_ratio, 52, 0.5).
narrative_ontology:measurement(sepa_tr_t65, separation_of_powers_text__formalist_reading, theater_ratio, 65, 0.42).
narrative_ontology:measurement(sepa_tr_t78, separation_of_powers_text__formalist_reading, theater_ratio, 78, 0.35).
narrative_ontology:measurement(sepa_tr_t91, separation_of_powers_text__formalist_reading, theater_ratio, 91, 0.3).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sepa_be_t13, separation_of_powers_text__formalist_reading, base_extractiveness, 13, 0.25).
narrative_ontology:measurement(sepa_be_t26, separation_of_powers_text__formalist_reading, base_extractiveness, 26, 0.2).
narrative_ontology:measurement(sepa_be_t39, separation_of_powers_text__formalist_reading, base_extractiveness, 39, 0.22).
narrative_ontology:measurement(sepa_be_t52, separation_of_powers_text__formalist_reading, base_extractiveness, 52, 0.28).
narrative_ontology:measurement(sepa_be_t65, separation_of_powers_text__formalist_reading, base_extractiveness, 65, 0.35).
narrative_ontology:measurement(sepa_be_t78, separation_of_powers_text__formalist_reading, base_extractiveness, 78, 0.45).
narrative_ontology:measurement(sepa_be_t91, separation_of_powers_text__formalist_reading, base_extractiveness, 91, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sepa_su_t13, separation_of_powers_text__formalist_reading, suppression_requirement, 13, 0.3).
narrative_ontology:measurement(sepa_su_t26, separation_of_powers_text__formalist_reading, suppression_requirement, 26, 0.22).
narrative_ontology:measurement(sepa_su_t39, separation_of_powers_text__formalist_reading, suppression_requirement, 39, 0.2).
narrative_ontology:measurement(sepa_su_t52, separation_of_powers_text__formalist_reading, suppression_requirement, 52, 0.28).
narrative_ontology:measurement(sepa_su_t65, separation_of_powers_text__formalist_reading, suppression_requirement, 65, 0.35).
narrative_ontology:measurement(sepa_su_t78, separation_of_powers_text__formalist_reading, suppression_requirement, 78, 0.5).
narrative_ontology:measurement(sepa_su_t91, separation_of_powers_text__formalist_reading, suppression_requirement, 91, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: separation_of_powers_text decomposes into three reading-stories per the epsilon-invariance principle. This file is the formalist instantiation (strict non-delegation; agencies in the target set; high suppression of the flexible alternative). The functionalist instantiation (permissive delegation under intelligible principles) and the unitary-executive instantiation carry their own epsilon and stakeholder data. The delegative arrangement's epsilon as the formalist critique assesses it is a property of the formalist critique, not of this constraint's operation, and belongs in its own measurement. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
