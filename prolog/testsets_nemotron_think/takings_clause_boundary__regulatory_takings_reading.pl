% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Balancing Test)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The regulatory takings reading (Pennsylvania Coal v. Mahon through Penn
 *   Central Transportation Co. v. New York City) holds that the Takings
 *   Clause reaches beyond physical appropriation to regulations that diminish
 *   property value 'too far.' This reading instantiates an ad hoc balancing
 *   test rather than bright-line rules. It expands the victim set to include
 *   property owners suffering severe value diminution without possession, but
 *   creates uncertainty in regulatory space. The constraint coordinates the
 *   regulation/taking boundary while extracting compensation from the public
 *   fisc for property owners — a tangled rope with genuine coordination
 *   function and asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.62).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.48).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Balancing Test)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '9608c23a-da7a-47bd-b799-2eb3799096b5').
narrative_ontology:cs_kernel_codification('9608c23a-da7a-47bd-b799-2eb3799096b5', fixed_text).
narrative_ontology:cs_authority_grounding('9608c23a-da7a-47bd-b799-2eb3799096b5', lineage).
narrative_ontology:cs_interpretation_layer_present('9608c23a-da7a-47bd-b799-2eb3799096b5').
narrative_ontology:cs_reading_relation('9608c23a-da7a-47bd-b799-2eb3799096b5', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('9608c23a-da7a-47bd-b799-2eb3799096b5', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('9608c23a-da7a-47bd-b799-2eb3799096b5', foundational, regulation_can_constitute_taking).
narrative_ontology:cs_axiom_status(regulation_can_constitute_taking, holdable).
narrative_ontology:cs_axiom_grounding('9608c23a-da7a-47bd-b799-2eb3799096b5', regulation_can_constitute_taking, conventional).
narrative_ontology:cs_axiom('9608c23a-da7a-47bd-b799-2eb3799096b5', foundational, ad_hoc_balancing_required).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_required, holdable).
narrative_ontology:cs_axiom_grounding('9608c23a-da7a-47bd-b799-2eb3799096b5', ad_hoc_balancing_required, conventional).
narrative_ontology:cs_reference_frame('9608c23a-da7a-47bd-b799-2eb3799096b5', penn_central_framework).
narrative_ontology:cs_drift_state('9608c23a-da7a-47bd-b799-2eb3799096b5', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9608c23a-da7a-47bd-b799-2eb3799096b5', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, regulatory_state).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, property_rights_include_value_protection).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, government_must_internalize_costs_of_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold property interests that may be diminished by regulation. Can invoke the regulatory takings doctrine to seek compensation when regulation goes 'too far.' Their exit is constrained — they cannot easily move land or escape jurisdiction, but they can litigate. The doctrine gives them a lever against regulatory overreach.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    organized, biographical, constrained, national).

% Enacts regulations for public health, safety, environment, land use. The regulatory takings doctrine constrains this authority by creating compensation liability for value diminution. As agenda_setter, it writes the regulations; as payer, it bears the fiscal risk of takings claims. It can 'arbitrage' by drafting regulations to avoid the 'too far' threshold, but the balancing test creates uncertainty.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, regulatory_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, regulatory_state, payer).

% Ultimately fund compensation awards when takings claims succeed. Have no direct exit from the fiscal burden — they cannot opt out of taxation. The doctrine transfers wealth from the general fisc to specific property owners when regulation is deemed excessive.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, taxpayers, payer,
    powerless, biographical, trapped, national).

% Administer the Penn Central balancing test (economic impact, investment-backed expectations, character of government action). Their decisions shape the doctrine's extractiveness and suppression. They neither collect compensation nor bear its cost directly, but their interpretive choices determine the constraint's operation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, courts, observer,
    institutional, generational, analytical, national).

% Environmental protection advocates, public health proponents, future generations, and communities benefiting from regulation. Would object to expansive takings claims that chill necessary regulation. Structurally excluded from the compensation calculus — the doctrine's balancing test weights property owner expectations more heavily than diffuse public benefits.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, excluded_interests, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a workable boundary between legitimate regulation (no compensation) and regulatory overreach (compensation required), giving both property owners and regulators a framework to order their affairs without constant litigation.
% TRANSFER_FUNCTION: Moves compensation from the public treasury (taxpayers) to property owners when a court finds that regulation has diminished value 'too far' — i.e., when the Penn Central factors weigh in the owner's favor.
% ABSENT_VOICES: Environmental advocates, public health agencies, climate adaptation planners, and future generations who bear the cost of a chilled regulatory state. They are not parties to takings litigation and their interests are not directly represented in the Penn Central calculus.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished overnight, legislatures and agencies would regulate without compensation liability for value diminution. Property rights would be protected only against physical appropriation and total wipeouts. Land use, environmental, and health regulation would expand; property owners would lose their primary constitutional lever against regulatory overreach.
% FOUNDING_PROBLEM: Regulations that destroy property value without formal eminent domain proceedings — the 'too far' problem identified in Pennsylvania Coal v. Mahon (1922): 'while property may be regulated to a certain extent, if regulation goes too far it will be recognized as a taking.'
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Sax, Michelman, Epstein) attest the founding problem was genuine: early 20th century regulation did destroy value without compensation. Property rights scholars attest the problem persists. Progressive scholars and government attorneys attest the doctrine has mutated into a deregulatory tool beyond its founding justification. No single consensus outside the beneficiary set.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that successful takings claims transfer public funds to private owners, and the threat of liability chills regulation. Suppression (0.48) is moderate: the doctrine does not ban regulation but raises its cost, and the balancing test's indeterminacy suppresses regulatory innovation. Theater (0.38) has risen as the Court has added categorical rules (Lucas, Loretto, Nollan/Dolan, Cedar Point) that perform 'takings protection' while the Penn Central test does the real work. Accessibility collapse (0.42) is moderate — alternatives (legislative relief, insurance, market adaptation) exist but are costly. Resistance (0.55) comes from regulatory agencies, progressive scholars, and dissenting justices who view the doctrine as judicial overreach.
 *
 * PERSPECTIVAL GAP:
 *   From the property owner's seat, the doctrine is a rope — genuine coordination protecting their investment-backed expectations. From the taxpayer's seat, it is a snare — extraction for private benefit with no voice. From the regulatory state's seat, it is a tangled rope — it coordinates by channeling disputes into courts, but extracts fiscal resources and constrains policy. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners are structural beneficiaries (d near 0.2) — they hold the claim right and can force compensation. Taxpayers are full targets (d near 0.9) — they pay with no exit. The regulatory state sits near symmetric (d ~0.5) — it both administers the constraint (setting regulations) and bears its cost (compensation liability). Courts are analytical observers (d = 0.5 by definition). Excluded interests are structurally trapped (d ~0.7) — they bear the cost of chilled regulation without standing in the takings calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulatory value destruction without compensation) remains live, but the doctrine has accumulated categorical exceptions that serve as 'pitons' — performative maintenance of a coordination function that has atrophied into a tool for property owners to extract concessions. The mandate has not been resolved; it has been captured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the Takings Clause text (''taken for public use'') structurally support a regulatory takings reading, or does that reading require importing a normative premise not in the text?',
    'Originalist textual analysis vs. living constitutionalist precedent analysis; the Court''s own split in Lucas and subsequent cases reveals the framing contest.',
    'If the text forecloses regulatory takings, this reading is a snare (judicial invention extracting from taxpayers). If the text admits it, the reading is a genuine coordination mechanism with extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the regulatory takings reading is textually grounded or judicially constructed').

omega_variable(
    penn_central_indeterminacy,
    'Is the Penn Central balancing test''s indeterminacy a feature (flexible coordination) or a bug (enables strategic extraction by property owners)?',
    'Empirical study of takings litigation outcomes: do property owners win predictably based on factors, or does unpredictability drive settlements that extract beyond the doctrine''s coordination function?',
    'If a bug, the doctrine''s extraction is higher than its coordination function justifies — pushing toward snare. If a feature, the extraction is the price of flexible coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_indeterminacy, empirical, 'Whether doctrinal indeterminacy serves coordination or extraction').

omega_variable(
    categorical_rules_capture,
    'Do the categorical per se rules (Lucas, Loretto, Nollan/Dolan) serve the coordination function or have they been captured as extraction tools for property owners?',
    'Track whether categorical rules are applied symmetrically (government also benefits from bright lines) or asymmetrically (only property owners invoke them successfully).',
    'If captured, the theater_ratio understates performative maintenance; the constraint is more piton-like than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_rules_capture, empirical, 'Whether categorical takings rules are genuinely coordinating or extractive instruments').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the regulatory chill from takings liability structural (fiscal constraint on legislatures) or internalized (regulators self-censor beyond what the doctrine requires)?',
    'Compare regulatory output in jurisdictions with stronger vs. weaker state takings clauses; survey agency staff on decision calculus.',
    'If internalized, effective suppression exceeds the structural measure — the constraint operates partly through cognitive capture of regulators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in regulatory takings context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1922, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(taki_tr_t1945, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1992, 0.32).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1922, 0.25).
narrative_ontology:measurement(taki_be_t1945, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1922, 0.2).
narrative_ontology:measurement(taki_su_t1945, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1992, 0.42).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__regulatory_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel decomposes into three readings with different ε: physical_appropriation_reading (ε ≈ 0.15, mountain-like), categorical_takings_reading (ε ≈ 0.35, rope/tangled_rope boundary), regulatory_takings_reading (ε ≈ 0.62, tangled_rope). This reading has the highest extractiveness because it makes the most regulations compensable. The categorical reading inherits this reading's balancing test as a residual category, creating downstream influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__regulatory_takings_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
