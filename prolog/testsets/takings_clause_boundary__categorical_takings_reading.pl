% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Rule: Physical Occupations and Total Value Elimination
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The categorical takings rule — permanent physical occupations and total
 *   value eliminations are per se takings; all other regulations evaluated by
 *   Penn Central factors — instantiates ONE reading of the contested takings
 *   clause kernel. This reading attempts to stabilize property owner
 *   expectations by establishing bright-line rules at the extremes (automatic
 *   takings liability when government physically occupies property or
 *   eliminates all economic use) while preserving regulatory flexibility
 *   through contextual balancing (Penn Central multifactor test) in the
 *   middle ground. The constraint exhibits genuine tangled-rope structure: it
 *   coordinates legitimate expectations through bright-line rules while
 *   simultaneously extracting significant costs from regulatory authorities
 *   by foreclosing their defense that a regulation serves an important public
 *   purpose when it crosses the categorical threshold. The rising
 *   theater_ratio over the 30-year interval (0.55 to 0.68) reflects
 *   increasing litigation burden and doctrinal fuzziness as courts struggle
 *   to define 'permanent' and 'total elimination' consistently.
 *
 * KEY AGENTS:
 *   - Property Owners at Categorical Poles: Beneficiaries (institutional/arbitrage) — gain automatic takings entitlements when occupancy or value elimination is permanent/total; benefit from clear rules and reduced litigation cost
 *   - Regulatory Authorities (EPA, state environmental agencies): Victims (powerless/trapped) — cannot exit the categorical rule without loss of policy tools; face automatic takings liability for regulations that serve public purposes but trigger categorical categories
 *   - Property Owners in Middle Ground: Mixed (moderate/constrained) — benefit from Penn Central flexibility but face uncertainty; bear litigation costs and settlement burdens in the uncertain zone
 *   - Real Estate Development Sector: Beneficiary (institutional/arbitrage) — benefits from property-owner wins in takings litigation; has interest in expansive takings doctrine
 *   - Environmental Protection Capacity: Victim (powerless/trapped) — constrained by takings liability exposure; cannot pursue aggressive environmental regulation if it triggers categorical rule
 *   - Adaptive Regulatory Coalition: Organized agents (organized/mobile) — developing workarounds (mitigation banking, TDR, conservation purchase) that bypass the categorical rule; reducing actual suppression through institutional innovation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the categorical rule as a natural law boundary when it is a contingent policy choice between competing approaches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.62).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Rule: Physical Occupations and Total Value Elimination").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'd8755e18-fdb6-415b-9807-5b4f2ab98b41').
narrative_ontology:cs_kernel_codification('d8755e18-fdb6-415b-9807-5b4f2ab98b41', fixed_text).
narrative_ontology:cs_authority_grounding('d8755e18-fdb6-415b-9807-5b4f2ab98b41', lineage).
narrative_ontology:cs_interpretation_layer_present('d8755e18-fdb6-415b-9807-5b4f2ab98b41').
narrative_ontology:cs_reading_relation('d8755e18-fdb6-415b-9807-5b4f2ab98b41', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8755e18-fdb6-415b-9807-5b4f2ab98b41', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('d8755e18-fdb6-415b-9807-5b4f2ab98b41', foundational, bright_line_rules_enable_predictability).
narrative_ontology:cs_axiom_status(bright_line_rules_enable_predictability, holdable).
narrative_ontology:cs_axiom_grounding('d8755e18-fdb6-415b-9807-5b4f2ab98b41', bright_line_rules_enable_predictability, instrumental).
narrative_ontology:cs_axiom('d8755e18-fdb6-415b-9807-5b4f2ab98b41', foundational, regulatory_public_purpose_defense_compromises_property_security).
narrative_ontology:cs_axiom_status(regulatory_public_purpose_defense_compromises_property_security, holdable).
narrative_ontology:cs_axiom_grounding('d8755e18-fdb6-415b-9807-5b4f2ab98b41', regulatory_public_purpose_defense_compromises_property_security, deontological).
narrative_ontology:cs_reference_frame('d8755e18-fdb6-415b-9807-5b4f2ab98b41', taking_eliminates_all_use_or_permanent_occupation).
narrative_ontology:cs_drift_state('d8755e18-fdb6-415b-9807-5b4f2ab98b41', contemporary_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d8755e18-fdb6-415b-9807-5b4f2ab98b41', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_with_extreme_claims).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, real_estate_development_sector).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulatory_authority_flexibility).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, environmental_protection_capacity).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, urban_planning_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED REGULATORY AUTHORITY (SNARE) — Cannot exit the categorical rule's strictures without losing predictability. Regulatory actors (EPA, USDA, state environmental agencies) are trapped: if they regulate in ways that trigger the physical occupation or total value categories, they face automatic takings liability regardless of public benefit. No exit from the categorical rule itself; suppression is extreme because the rule forecloses flexibility-based defenses. The regulatory authority experiences this as pure extraction — they must pay just compensation for regulations that serve legitimate public purposes, or cease regulating.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTY OWNER IN MIDDLE GROUND (TANGLED ROPE) — Faces uncertainty and case-by-case balancing under Penn Central factors. Neither guaranteed a taking nor guaranteed regulatory permission. Coordination function: Penn Central factors enable negotiation and settlement; extraction function: case-by-case litigation is expensive and outcome uncertain. High suppression (must defend against regulatory action, cannot simply relocate property) but genuine coordination possibility exists through negotiated settlements and variances.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTY OWNER AT CATEGORICAL POLE (ROPE) — Benefits from bright-line rules establishing when takings liability is automatic. Owner of property subject to a permanent physical occupation order (e.g., easement for utility access, right of way) or regulation that eliminates all economically viable use has a clear entitlement: takings liability attaches automatically. No extraction experienced — the rule is coordination of legitimate expectations and compensation mechanisms. Arbitrage available through strategic property positioning and takings litigation.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADAPTIVE REGULATORY COALITION (SCAFFOLD) — States and municipalities are developing workarounds to the categorical rule: mitigation banking, conservation easement purchase (voluntary, not regulation), clustering zoning, transferable development rights. These mechanisms coordinate environmental protection while avoiding the categorical rule's triggering conditions. The scaffold has a sunset: as the regulatory ecosystem develops alternatives, the categorical rule's extractive force declines. Extraction is low because the coalition has agency and exit paths that bypass the categorical rule entirely.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PROPERTY RIGHTS IDEOLOGY (PITON) — The categorical rule's legitimacy narrative rests on an ideological commitment to stable property expectations: owners should be able to reliably know when their property rights are being taken. But the rule's actual function has degraded: litigation outcomes under the categorical rule are increasingly unpredictable (disagreement about what constitutes 'permanent' or 'total'); the bright line has become fuzzy through case law; property owners do not reliably know where they stand. The ideology persists through institutional inertia (Supreme Court stare decisis, treatise authority, law school curriculum) despite diminishing functional alignment with actual predictability.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a natural law perspective, property is itself a natural right with inherent boundaries; when government crosses the boundary into 'taking' (physical occupation or total elimination of use), it has violated an inherent limit on state power. The taking/regulation distinction maps onto an immutable boundary between legitimate governance and property confiscation. However, the structural data contradicts mountain classification: the beneficiaries (property owners with extreme claims, development sector) and victims (regulatory capacity, environmental protection) are clearly identifiable, and the categorical rule embodies a contested normative choice (property stability vs. regulatory flexibility), not a law of nature. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(takings_clause_boundary__categorical_takings_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, TR),
    TR >= 0.70.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The categorical rule imposes significant costs on regulatory authorities by eliminating the defense that a regulation serves a compelling public purpose when it crosses the per se threshold. But extraction is not maximal because: (1) regulatory authorities retain Penn Central balancing in the middle ground, (2) states have developed workarounds (mitigation banking, voluntary conservation purchase), (3) the categorical categories are themselves contested and litigated, not absolute. Suppression (0.62): High. Regulatory authorities face severe suppression: they cannot avoid the categorical rule without eliminating entire classes of regulation (permanent easements for infrastructure, environmental protections that eliminate all economic use). Easement requirements for utility access, storm-water management, habitat restoration — all triggers for potential categorical takings liability — are difficult to replace with alternative approaches. The suppression reflects the structural fact that some public purposes are difficult to achieve without encroaching on property owner interests in the categorical zone. Theater ratio (0.68): Moderate-high and rising. The categorical rule claims to provide predictability through bright lines, but actual case outcomes are becoming less predictable as litigation clarifies that 'permanent' and 'total elimination' are fact-intensive inquiries. Courts disagree on whether intermittent government access is permanent; courts split on whether aggregation of holdings affects the 'total elimination' calculation. The performative content is the rule's legitimacy narrative (bright lines promote predictability) relative to its functional outcome (fact-intensive litigation on the boundaries of the categories).
 *
 * PERSPECTIVAL GAP:
 *   The categorical rule generates sharply divergent classifications depending on structural position. Property owners at the categorical poles (beneficiaries with arbitrage options) see a coordinating Rope — the rule clarifies when they are entitled to compensation. Regulatory authorities (powerless/trapped) see a Snare — they are locked into the rule's strictures and cannot defend their regulations on public-purpose grounds. Property owners in the middle ground see Tangled Rope — the rule both coordinates expectations (Penn Central factors apply) and extracts costs (litigation, uncertainty, settlement burdens). The organized coalition developing regulatory workarounds sees a Scaffold — the categorical rule is being eroded by institutional alternatives, creating a sunset pathway. The property-rights ideology sees a Piton — the rule's legitimacy narrative (property stability) is increasingly disconnected from functional outcome (litigation unpredictability). The analytical observer at civilizational scope risks seeing a Mountain (natural property rights boundary) but structural data reveals false summit: identifiable beneficiaries (property owners, development sector) and victims (regulatory capacity, environmental protection) show this is a contested institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The categorical rule's directionality varies dramatically by agent position. Property owners at the categorical poles (institutional/arbitrage) derive low d values from their beneficiary status — they experience the rule as a beneficial coordination mechanism. Regulatory authorities (powerless/trapped) derive high d values from their victim status and exit foreclosure — they cannot avoid the rule's costs without abandoning entire regulatory categories. Property owners in the middle ground (moderate/constrained) occupy intermediate d positions — they benefit from Penn Central's flexibility but bear litigation and settlement costs. The real estate development sector (institutional/arbitrage) experiences beneficiary status through its interest in expansive takings doctrine. Environmental protection capacity (powerless/trapped) experiences maximal d (victim status, structural inability to exit or substitute alternative mechanisms). The directionality overrides are not needed: the structural derivation captures the constraint's asymmetric impact accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing how one interpretation of the takings kernel (categorical rule with bright-line poles plus Penn Central middle ground) distributes extraction and coordination differently than sibling readings would. The categorical reading emphasizes predictability at the extremes and flexibility in the middle. The physical-appropriation reading would emphasize government's affirmative act of taking possession. The regulatory-takings reading would apply Penn Central balancing uniformly across all cases without categorical per se rules. Each reading instantiates different coordination functions and extracts from different victim sets. This reading's mandatrophy is UNRESOLVED in the classical sense: the categorical rule has not proven itself capable of achieving its stated goal (property owner predictability) consistently enough that it has become self-evident or unquestionable. Instead, the rising theater_ratio suggests increasing institutional disconnection between the rule's legitimacy claim and its functional outcome — a signature of unresolved mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanent_occupancy_boundary,
    'What occupancy duration or frequency constitutes ''permanent'' physical occupation triggering automatic takings liability? Is occasional access (quarterly utility inspection) permanent? Is indefinite-but-revocable access (regulatory compliance monitoring) permanent?',
    'Case law doctrinal analysis showing Supreme Court''s treatment of occupancy frequency and duration; historical review of whether categorical takings doctrine has stabilized or fragmented the ''permanent'' boundary.',
    'If boundary is sharp: categorical rule achieves its coordination goal (predictability). If boundary is fuzzy: categorical rule becomes a litigation lottery, and property owners do not gain the promised predictability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanent_occupancy_boundary, conceptual, 'Definition of permanence in physical occupation takings').

omega_variable(
    total_elimination_measurement,
    'Does ''total elimination of economically viable use'' mean zero economic value, zero use by the owner, or zero use consistent with current zoning/regulation? How is ''economically viable'' measured — at the property level, per-acre, or as part of a larger holding?',
    'Comparison of Supreme Court cases (Lucas, Palazzolo, Tahoe-Sierra) to identify conflicting standards; empirical analysis of how lower courts apply the total-value test.',
    'If measurement is property-specific: many small properties fall into the categorical, and regulatory agencies face high takings exposure. If measurement aggregates across holdings: fewer properties trigger the categorical, and agencies retain flexibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(total_elimination_measurement, empirical, 'Definition and measurement of total elimination of economic use').

omega_variable(
    regulatory_purpose_carve_out,
    'Should the categorical rule include an implicit carve-out for regulations serving legitimate public purposes (safety, health, environmental protection) even if they trigger the ''permanent occupation'' or ''total elimination'' categories? Or does the categorical rule operate per se, independent of purpose?',
    'Doctrinal analysis of how courts apply categorical vs. Penn Central reasoning in cases involving safety and health regulations; historical review of whether the categorical rule has ever been subordinated to public-purpose analysis.',
    'If carve-out exists: categorical rule does not foreclose public-health regulations, and suppression is lower. If per se application: public health and safety regulations may trigger automatic takings liability even when serving compelling state interests, and suppression is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_purpose_carve_out, conceptual, 'Whether categorical takings apply per se or admit public-purpose exceptions').

omega_variable(
    reading_kernel_contestation,
    'Is this reading (categorical physical occupation + total value elimination rule) a coherent commitment principle that courts can apply consistently, or does it naturalize a policy choice that other readings (physical appropriation, regulatory takings contextual balancing) would distribute differently?',
    'Comparative analysis of jurisdictions: do states following the categorical rule show greater property-owner compliance, fewer takings lawsuits, or greater regulatory stability than states emphasizing contextual balancing? Do empirical compliance patterns validate or contradict the reading''s predictability claim?',
    'If categorical rule proves effective at stabilizing expectations: reading is coherent. If courts continue to litigate edge cases and categorical rule produces unpredictable outcomes: reading naturalizes a policy choice without achieving its stated coordination goal, and engine will evaluate false summit candidacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contestation, empirical, 'Whether categorical takings reading achieves its stated coordination goal of predictability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takcat_tr_t0, takings_clause_boundary__categorical_takings_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(takcat_tr_t15, takings_clause_boundary__categorical_takings_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement(takcat_tr_t30, takings_clause_boundary__categorical_takings_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(takcat_be_t0, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(takcat_be_t15, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(takcat_be_t30, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(takcat_su_t0, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(takcat_su_t15, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(takcat_su_t30, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_penn_central_factors).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, mitigation_banking_workaround).

% DUAL FORMULATION NOTE:
% The takings clause kernel admits multiple readings. The categorical_takings_reading is ONE constraint story (this file). The physical_appropriation_reading and regulatory_takings_reading are separate constraint stories with different ε values and different victim/beneficiary structures. All three share the same foundational constitutional text but instantiate different interpretations with different structural consequences. Network edges show that this reading affects (influences) the Penn Central factors interpretation and affects (influences) the development of regulatory workarounds like mitigation banking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
