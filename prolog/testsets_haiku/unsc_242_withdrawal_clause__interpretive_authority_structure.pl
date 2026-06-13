% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   United Nations Security Council Resolution 242 (1967) calls for
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict.' The withdrawal clause's interpretation is contested at
 *   the meta-level: the ICJ claims authority via judicial interpretation of
 *   the treaty text and negotiating history; drafting states (particularly
 *   the United States and Soviet Union) assert that authorial intent—embedded
 *   in the divergence between English indefinite 'some territories' and
 *   French definite 'the territories'—establishes discretionary withdrawal;
 *   the occupying state (Israel) claims that customary international law
 *   practice on territorial disputes supports discretionary withdrawal and
 *   retention of 'secure boundaries.' This constraint is ONE READING: the
 *   meta-dispute over interpretive authority itself, which makes definitive
 *   resolution structurally impossible and perpetuates substantive ambiguity.
 *   The constraint extracts value by freezing the dispute in the interpretive
 *   register: as long as 'who decides' is contested, no substantive ruling
 *   can bind, and the occupying state (with veto capacity and arbitrage exit
 *   via state practice claims) captures the benefit of indefinite deferral.
 *   The measurement series show increasing extractiveness as successive ICJ
 *   opinions are published but not enforced, theater rising as enforcement
 *   machinery becomes more performative, and suppression requirement climbing
 *   as the ambiguity must be actively defended against alternative
 *   interpretations.
 *
 * KEY AGENTS:
 *   - International Court of Justice: Claims judicial authority; produces interpretive verdicts that lack enforcement power against state veto.
 *   - Drafting states (US, USSR, now successors): Assert authorial intent; defend the text's deliberate ambiguity as grounds for discretionary interpretation.
 *   - Occupying state (Israel): Claims customary practice ground; holds UN veto; benefits structurally from unresolved authority dispute.
 *   - Displaced populations and claimant states: Seek legal closure; locked into dispute by identity and existential stakes; no authority claim capacity.
 *   - Non-permanent UN members: Support full-withdrawal reading; lack veto to enforce it.
 *   - Customary law scholars and practitioners: Invoked instrumentally; lack seat at authoritative table despite being a valid source of international law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.77).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f').
narrative_ontology:cs_kernel_codification('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', fixed_text).
narrative_ontology:cs_authority_grounding('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', extraction).
narrative_ontology:cs_interpretation_layer_present('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f').
narrative_ontology:cs_reading_relation('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', unsc_242_withdrawal_clause__unsc_242_maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', unsc_242_withdrawal_clause__unsc_242_partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', foundational, interpretive_authority_is_fundamentally_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_fundamentally_contested, holdable).
narrative_ontology:cs_axiom_grounding('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', interpretive_authority_is_fundamentally_contested, conventional).
narrative_ontology:cs_axiom('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', foundational, veto_capacity_constitutes_interpretive_veto).
narrative_ontology:cs_axiom_status(veto_capacity_constitutes_interpretive_veto, holdable).
narrative_ontology:cs_axiom_grounding('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', veto_capacity_constitutes_interpretive_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', judicial_interpretive_authority).
narrative_ontology:cs_drift_state('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', contemporary_post_advisory_opinion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76ebc287-fad5-4c0d-8c1b-3c5b68f56c5f', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_state_non_signatories).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, parties_seeking_legal_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, non_permanent_un_members).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint's function is to prevent binding resolution of the substantive withdrawal question. Each authority form—judicial, authorial intent, customary practice—produces a different substantive result, and authority ambiguity is the mechanism that keeps all three readings alive and unresolvable. Suppression is also high (0.77) because active maintenance is required: the occupying state must continuously assert state practice claims against ICJ authority, drafting states must defend the 'deliberate ambiguity' narrative against both the mandatory-withdrawal and discretionary-withdrawal readings, and the ICJ must maintain its interpretive role despite lacking enforcement power. Theater ratio (0.58) reflects the constraint's performative character: resolutions are passed, ICJ opinions are issued, enforcement mechanisms are invoked—all theater that produces no binding outcome. The measurement trajectory shows rising theater and suppression as time passes without resolution, indicating the constraint is increasingly maintained by performance rather than actual coordination function. Extractiveness flattens after year 47 because the occupying state's benefit has been fully captured and stabilized—further interpretation produces no new value, only maintenance costs.
 *
 * PERSPECTIVAL GAP:
 *   From the ICJ's seat, the constraint is a failure of state compliance with judicial authority—it sees itself as having produced correct interpretations that states refuse to follow. From the drafting states' seat, the constraint is a success of textual preservation—ambiguity was built in deliberately to allow parties with incompatible positions to sign the same text. From the occupying state's seat, the constraint is optimal: veto power allows rejection of all adverse interpretations while claiming state practice support for discretionary withdrawal. From the displaced populations' seat, the constraint is a catastrophe: ambiguity means indefinite displacement and no legal recourse. From non-permanent UN members' seat, it is powerlessness: they support resolutions the veto blocks. These divergences are structural, not accidental. The engine computes each seat's classification from power and exit options; the occupying state's institutional power and arbitrage exit produce a snare classification while non-permanent members' moderate power and trapped exit produce victim classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the occupying state with veto capacity: d = 0.85 (full target in the formal reading, but effectively a beneficiary because veto and arbitrage exit flip the arrangement—the formal constraint points at the occupying state, but the occupying state controls exit and interpretation). Override to d = 0.15 to reflect actual beneficiary position. Directionality for displaced populations: d = 0.95 (full target—locked by identity, constrained exit, zero power, bearing indefinite cost of ambiguity). Directionality for non-permanent UN members: d = 0.75 (high target—trapped exit, constrained power, bearing diplomatic cost of blocked resolutions). Directionality for ICJ: d = 0.5 (symmetric—genuine authority in interpretation, but no enforcement power, so the institutional role is real but the effectiveness is asymmetrically constrained). The occupying state override is critical: the formal constraint structure points at the occupying state as the target (the resolution calls for its withdrawal), but power and exit flip the directionality. The override documents that structural inversion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how do we enforce withdrawal when the text is ambiguous and the interpreter is contested) is live and will remain live as long as authority is unresolved. Mandatrophy would require that either (a) the occupying state withdraws without legal compulsion (the founding problem ceases to matter because the outcome is achieved), or (b) a single authority form becomes undisputed (the founding problem is solved, not abandoned). The current state shows neither: extractiveness is stable at 0.81, theater is plateau at 0.58, suppression is plateau at 0.77. The constraint has not resolved; it has stabilized in its extractive form. The theater ratio above 0.5 indicates performative maintenance dominates real coordination. This is a classic piton signature—the original coordination problem (settling territorial disputes) has atrophied, replaced by a meta-dispute (who interprets) that keeps the original unresolved. However, the claimed type is snare, not piton, because the beneficiaries (occupying state, drafting states defending ambiguity) are actively capturing value from the constraint's persistence—they are not merely administering an inert structure. A piton would show no concentrated beneficiary; this constraint has clear beneficiaries with veto power. The distinction is: pitons are maintained by inertia and insufficient motivation to fix; snares are maintained by concentrated beneficiary blocking of alternatives and resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_hierarchy_stability,
    'Is the hierarchy of interpretive authority (judicial > authorial intent > customary practice, or some other ordering) itself a contestable commitment that could collapse if one authority form achieves decisive victory?',
    'Historical counterfactual: if the ICJ had issued a binding ruling enforced by UN enforcement mechanisms, would the occupying state have accepted it, or would customary-practice claims have escalated to military enforcement claims? Or would the drafting states have reasserted authorial intent through new protocols?',
    'If any authority hierarchy proves unstable under enforcement pressure, the constraint''s underlying structure is not snare (meta-dispute keeping readings in play) but a deeper cycle where each authority form claims legitimacy only when veto-able. The snare classification would degrade to piton (performance of authority without real grounding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_hierarchy_stability, conceptual, 'Whether the meta-dispute is truly about authority hierarchy or about veto capacity disguised as epistemology.').

omega_variable(
    beneficiary_capture_mechanism,
    'What is the exact mechanism by which the occupying state''s veto power translates to beneficiary status in an interpretive dispute that nominally concerns legal meaning, not state power?',
    'Trace the causal chain: veto blocks enforcement -> enforcement blockage allows indefinite non-compliance -> indefinite non-compliance yields captured benefit (retention of territory). At what point does state power become interpretive authority? Is the chain veto -> power -> interpretation, or veto -> blockage -> benefit, with ''interpretation'' as post-hoc justification?',
    'If the causal chain is veto -> blockage -> benefit with interpretation as cover, the constraint is more accurately classified as extraction-with-interpretive-rhetoric (a snare with identity-locked framing). If veto genuinely enables a competing but legitimate interpretation of the text, then the snare classification holds but with different framing. This affects how to model whether the constraint could survive non-veto (if veto is the entire mechanism, removal of veto removes the snare; if veto merely amplifies a real interpretive claim, the snare persists without veto).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_capture_mechanism, empirical, 'The structural position of state power in interpretive authority.').

omega_variable(
    displaced_population_voice_route,
    'Do displaced populations have any institutional route to assert a competing interpretive claim, or is their exclusion from the authoritative frame structural and unremediable?',
    'Examine whether any UN body, ICJ chamber, or regional organization has standing to represent displaced populations in interpretation disputes, and whether such representation has ever produced an interpretive verdict that constrained the occupying state.',
    'If displaced populations are permanently excluded from authoritative interpretation, the snare gains an additional dimension: not only does meta-dispute prevent resolution, but the parties prevented from interpreting are precisely those who bear the highest cost. The constraint would be more accurately classified as snare-with-identity-exclusion, where authority is captured not just by the occupying state but by the entire state-centric interpretive apparatus against non-state actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displaced_population_voice_route, empirical, 'Whether displaced populations can access the interpretive frame that determines their status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unsc_tr_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 9, 0.46).
narrative_ontology:measurement(unsc_tr_t19, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 19, 0.51).
narrative_ontology:measurement(unsc_tr_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 28, 0.54).
narrative_ontology:measurement(unsc_tr_t38, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 38, 0.57).
narrative_ontology:measurement(unsc_tr_t47, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 47, 0.58).
narrative_ontology:measurement(unsc_tr_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 57, 0.58).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(unsc_be_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 9, 0.71).
narrative_ontology:measurement(unsc_be_t19, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 19, 0.76).
narrative_ontology:measurement(unsc_be_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 28, 0.79).
narrative_ontology:measurement(unsc_be_t38, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 38, 0.8).
narrative_ontology:measurement(unsc_be_t47, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 47, 0.81).
narrative_ontology:measurement(unsc_be_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 57, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(unsc_su_t9, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(unsc_su_t19, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 19, 0.72).
narrative_ontology:measurement(unsc_su_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 28, 0.74).
narrative_ontology:measurement(unsc_su_t38, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 38, 0.76).
narrative_ontology:measurement(unsc_su_t47, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 47, 0.77).
narrative_ontology:measurement(unsc_su_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 57, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.22).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading constraint family centered on UNSC Resolution 242. The interpretive_authority_structure reading (this one) is the meta-constraint that prevents the other two readings (maximal_withdrawal and partial_withdrawal) from achieving binding status. All three readings share the same kernel (UNSC 242 paragraph 1 text) but instantiate different constraints because they model different structural problems: maximal_withdrawal models the juridical problem (what does the text require), partial_withdrawal models the political-economy problem (what do the drafters' incentives suggest), and interpretive_authority_structure models the meta-institutional problem (who gets to decide, and what happens when that is contested). The three readings are interdependent: neither substantive reading can win as long as interpretive authority is itself contested. This reading's extraction (ε=0.81) comes from its function as a veto-structure that keeps both other readings blocked. See audits/2026-06-11_unsc242_constraint_family/ for the decomposition documentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
