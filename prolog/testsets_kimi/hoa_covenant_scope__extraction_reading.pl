% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Extraction Reading: Revenue Generation and Board Power Consolidation
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint is the extraction_reading of the contested kernel
 *   hoa_covenant_scope. Where the coordination_reading sees genuine
 *   shared-infrastructure governance and the behavioral_control_reading sees
 *   aesthetic uniformity and property value maximization, this reading claims
 *   the covenant text is wielded primarily as a revenue generation mechanism
 *   for service vendors and a board power consolidation tool. It instantiates
 *   a distinct structural profile with high base extractiveness (0.64),
 *   active enforcement (0.72 suppression), and a theater ratio (0.45)
 *   indicating substantial performative maintenance of fiduciary narratives.
 *
 * KEY AGENTS:
 *   - hoa_board_members (agenda_setter/organized/mobile): directs enforcement priorities, approves vendor contracts, captures neighborhood political authority
 *   - property_management_firms (beneficiary/organized/mobile): scales revenue with enforcement volume, processes violations and collections
 *   - hoa_legal_counsel (beneficiary/organized/mobile): generates billable hours from lien and foreclosure actions, captures attorney fee awards
 *   - financially_vulnerable_homeowners (payer/powerless/constrained): bear escalating fines, liens, and foreclosure risk with limited exit or contest capacity
 *   - renters_via_pass_through (payer/powerless/constrained): absorb assessment and fine pass-throughs without representation in governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.64).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.72).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Extraction Reading: Revenue Generation and Board Power Consolidation").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'f444fcac-f458-457b-877c-c94c1a28e1d8').
narrative_ontology:cs_kernel_codification('f444fcac-f458-457b-877c-c94c1a28e1d8', fixed_text).
narrative_ontology:cs_authority_grounding('f444fcac-f458-457b-877c-c94c1a28e1d8', extraction).
narrative_ontology:cs_interpretation_layer_present('f444fcac-f458-457b-877c-c94c1a28e1d8').
narrative_ontology:cs_reading_relation('f444fcac-f458-457b-877c-c94c1a28e1d8', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f444fcac-f458-457b-877c-c94c1a28e1d8', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('f444fcac-f458-457b-877c-c94c1a28e1d8', foundational, covenant_as_revenue_instrument).
narrative_ontology:cs_axiom_status(covenant_as_revenue_instrument, holdable).
narrative_ontology:cs_axiom_grounding('f444fcac-f458-457b-877c-c94c1a28e1d8', covenant_as_revenue_instrument, empirically_contingent).
narrative_ontology:cs_axiom('f444fcac-f458-457b-877c-c94c1a28e1d8', secondary, selective_enforcement_for_rent_extraction).
narrative_ontology:cs_axiom_status(selective_enforcement_for_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f444fcac-f458-457b-877c-c94c1a28e1d8', selective_enforcement_for_rent_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('f444fcac-f458-457b-877c-c94c1a28e1d8', private_planning_contract).
narrative_ontology:cs_drift_state('f444fcac-f458-457b-877c-c94c1a28e1d8', contemporary_hoa_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f444fcac-f458-457b-877c-c94c1a28e1d8', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits on the HOA board, votes on rule changes and fine schedules, directs the management firm to prioritize certain violations, and approves legal counsel contracts. Captures consolidated neighborhood political authority and informal vendor relationships.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    organized, biographical, mobile, local).

% Contracts with the HOA to manage day-to-day operations, process violations, and collect fines. Revenue is often tied to enforcement volume through percentage-based fee structures or flat monthly rates that scale with complaint volume.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, mobile, regional).

% Represents the HOA in collections and foreclosure actions, drafts demand letters, and records liens. Generates billable hours from enforcement proceedings and is frequently awarded attorney fees against homeowners in default.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_legal_counsel, beneficiary,
    organized, biographical, mobile, regional).

% Own property within the covenant-bound community, subject to fines for aesthetic and behavioral violations. Face escalating late fees, lien recordings, and foreclosure risk when unable to pay assessments or penalties. Limited liquidity to hire competing legal counsel or sell under clouded title.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, constrained, local).

% Lease units within the HOA-governed community. Pay rent that reflects HOA assessments and pass-through fines. Subject to all covenant rules but hold no voting rights in board elections or rule changes. Exit requires lease-breaking or landlord non-renewal.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains common infrastructure and architectural standards in a private planned community, providing a governance structure for shared spaces and exterior maintenance obligations that would otherwise suffer free-rider problems.
% TRANSFER_FUNCTION: Moves money from homeowners and renters to HOA service vendors (management firms, legal counsel) and concentrates discretionary authority in board members, via fines, assessments, lien-based collections, and attorney fee awards.
% ABSENT_VOICES: Renters who bear pass-through costs without voting rights; financially distressed homeowners facing foreclosure from fine accumulation; alternative governance models such as municipal takeover or opt-out association are structurally excluded by the covenant's private delegation and recording statutes.
% DISAPPEARANCE_RATIONALE: If the covenant enforcement machinery vanished, board authority would collapse, management and legal revenue streams would dry up, and homeowner property rights would revert to less mediated individual control; the neighborhood governance structure would reorganize around municipal codes or voluntary association without lien-backed coercion.
% FOUNDING_PROBLEM: Provision of shared amenities and maintenance in a private residential subdivision without direct municipal management, and protection of mutual property values through architectural coherence.
% FOUNDING_PROBLEM_CORROBORATION: Academic housing scholars and consumer protection attorneys attest that the original infrastructure-coordination function has been supplanted by revenue extraction; HOA industry associations attest it remains live. No neutral government audit corroborates the live status.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.64) because the covenant operates as a fee and fine generation apparatus decoupled from marginal service cost, with attorney fee extraction and expedited lien processes amplifying transfer. Suppression is higher (0.72) because persistence depends on selective enforcement against vulnerable parties and the legal barrier to covenant exit (liens cloud title, supermajority amendment requirements). Theater ratio (0.45) reflects that roughly half of enforcement activity performs fiduciary duty and property value protection while the substantive function has shifted to revenue. Accessibility collapse (0.68) captures that alternatives to covenant governance are legally foreclosed by recording statutes and deed restrictions. Resistance (0.42) is moderate because homeowner opposition is diffuse and individually costly to organize.
 *
 * PERSPECTIVAL GAP:
 *   The board and vendor seats experience the constraint as legitimate governance they administer; financially vulnerable homeowners experience it as predatory extraction backed by foreclosure power; renters experience it as taxation without representation. The engine computes this divergence from structural data: identical covenant language produces opposite directionalities depending on whether the seat collects fees, bears liens, or pays pass-through costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for hoa_board_members, property_management_firms, and hoa_legal_counsel â they are subsidized by the constraint's transfer function. Victim declarations drive high directionality for financially_vulnerable_homeowners and renters_via_pass_through â they are extraction targets with constrained or trapped exit. The asymmetry is intentional: the same legal instrument that coordinates common space for one seat extracts rent from another.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â shared infrastructure coordination without municipal management â is structurally dead as the operative mandate. The enforcement apparatus has outgrown its coordination function: fine schedules and expedited liens serve vendor revenue and board authority rather than common maintenance. The classification as tangled_rope prevents mislabeling this as pure coordination (rope) by insisting on named victims and active enforcement, and prevents mislabeling it as pure extraction (snare) by acknowledging the residual coordination function in common space governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the extraction function inhere in the covenant structure itself, or is it an emergent pathology of specific board and vendor capture?',
    'Cross-jurisdictional comparison of HOAs with identical covenant language but different enforcement profiles; if extraction correlates with vendor relationships and board composition rather than text, it is emergent capture.',
    'If emergent, the constraint is better classified as snare under capture conditions and rope under clean governance; if inherent, the text itself is a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether extraction is structurally inherent or emergent capture.').

omega_variable(
    selective_enforcement_intent,
    'Is selective enforcement against financially vulnerable homeowners a deliberate board strategy, or a statistical artifact of who can afford compliance?',
    'Statistical analysis of violation issuance and foreclosure rates correlated with homeowner equity and demographics, controlling for violation type.',
    'If deliberate, the constraint is a snare targeting a specific class; if artifactual, it remains a tangled rope with asymmetric effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intent, empirical, 'Deliberate strategy vs statistical artifact of selective enforcement.').

omega_variable(
    renter_extraction_pathway,
    'Do renters actually bear the economic incidence of HOA fine-driven assessment increases, or do landlords absorb them?',
    'Hedonic rent studies in covenant-controlled versus uncontrolled units with identical physical characteristics; lease term analysis.',
    'If absorbed by landlords, renters are not victims of this constraint; if passed through, the victim set is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(renter_extraction_pathway, empirical, 'Economic incidence of fine-driven assessments on renters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_tr_t25, hoa_covenant_scope__extraction_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_be_t25, hoa_covenant_scope__extraction_reading, base_extractiveness, 25, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(hoa_covenant_scope__extraction_reading_su_t25, hoa_covenant_scope__extraction_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. Each reading instantiates a different beneficiary/victim structure and epsilon profile. This extraction reading carries the highest epsilon and names the most concentrated beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
