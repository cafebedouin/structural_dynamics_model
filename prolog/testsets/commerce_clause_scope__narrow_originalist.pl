% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope (Narrow Originalist Reading)
 *   domain: constitutional/federalism/economic
 *
 * SUMMARY:
 *   This constraint instantiates the narrow originalist reading of the
 *   Commerce Clause, one of three competing readings of a contested
 *   constitutional kernel. The narrow reading holds that 'commerce among the
 *   states' means trade crossing state lines, 'regulate' means 'make regular'
 *   (facilitate), and federal power is limited to removing state barriers and
 *   ensuring uniform rules for interstate commerce. The reading benefits
 *   state governments and local business by preserving regulatory autonomy
 *   over intrastate activity. It harms national regulatory uniformity
 *   advocates and civil rights enforcement regimes by denying federal reach
 *   via the Commerce Clause into non-commercial or purely local activity. The
 *   constraint CLAIMS rope status (coordination of interstate commerce). The
 *   authored metrics describe low extractiveness (0.28) and suppression
 *   (0.15) — the reading operates with modest enforcement cost because it
 *   commands significant institutional support (five current Supreme Court
 *   justices) and no party actively fights it on the ground; resistance is
 *   doctrinal and political, not structural. Theater is low (0.22) because
 *   the interpretive framework is transparent and not performative —
 *   originalists openly state their method. The constraint is one member of a
 *   constraint family (commerce_clause_scope kernel with three readings). The
 *   other readings (broad_effects_test, intermediate_channels) are separate
 *   constraint stories.
 *
 * KEY AGENTS:
 *   - State governments — benefit from retained autonomy, role: beneficiary
 *   - Federal courts — enforce the reading, role: agenda_setter
 *   - Civil rights enforcement coalitions — excluded from federal commerce power, role: victim
 *   - Interstate commerce participants — benefit from barrier-removal, role: beneficiary/payer
 *   - Congress — constrained by the reading, role: observer/payer
 *   - Environmental protection coalitions — excluded from commerce-clause reach, role: victim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.15).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/federalism/economic").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'ab74a97f-9a3a-4aba-939c-d674ba5053e6').
narrative_ontology:cs_kernel_codification('ab74a97f-9a3a-4aba-939c-d674ba5053e6', formalized).
narrative_ontology:cs_authority_grounding('ab74a97f-9a3a-4aba-939c-d674ba5053e6', lineage).
narrative_ontology:cs_interpretation_layer_present('ab74a97f-9a3a-4aba-939c-d674ba5053e6').
narrative_ontology:cs_reading_relation('ab74a97f-9a3a-4aba-939c-d674ba5053e6', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('ab74a97f-9a3a-4aba-939c-d674ba5053e6', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('ab74a97f-9a3a-4aba-939c-d674ba5053e6', foundational, commerce_means_trade_crossing_borders).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_borders, holdable).
narrative_ontology:cs_axiom_grounding('ab74a97f-9a3a-4aba-939c-d674ba5053e6', commerce_means_trade_crossing_borders, empirically_contingent).
narrative_ontology:cs_axiom('ab74a97f-9a3a-4aba-939c-d674ba5053e6', foundational, regulate_means_facilitate_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('ab74a97f-9a3a-4aba-939c-d674ba5053e6', regulate_means_facilitate_not_restrict, deontological).
narrative_ontology:cs_axiom('ab74a97f-9a3a-4aba-939c-d674ba5053e6', secondary, intrastate_commerce_outside_federal_reach).
narrative_ontology:cs_axiom_status(intrastate_commerce_outside_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('ab74a97f-9a3a-4aba-939c-d674ba5053e6', intrastate_commerce_outside_federal_reach, deontological).
narrative_ontology:cs_reference_frame('ab74a97f-9a3a-4aba-939c-d674ba5053e6', founding_barrier_removal_framework).
narrative_ontology:cs_drift_state('ab74a97f-9a3a-4aba-939c-d674ba5053e6', contemporary_integrated_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab74a97f-9a3a-4aba-939c-d674ba5053e6', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_business_operators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity_advocates).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_regimes).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, environmental_protection_coalitions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the reading's primary function is to PREVENT federal extraction of state regulatory authority, not to extract anything itself. Federal courts extract modest authority to police the line between interstate and intrastate commerce, but that extraction cost is dwarfed by the reading's benefit to states (retained autonomy). Suppression is very low (0.15) because enforcement faces minimal active resistance — federal courts simply apply the reading and strike down statutes exceeding it; states do not fight being told they retain power. Theater is low (0.22) and stable — the interpretive method is stated transparently, not performed. Accessibility collapse is moderate (0.62) because the reading creates a clear doctrinal boundary (interstate vs. intrastate), but that boundary is increasingly contested as modern economies blur the distinction — the boundary itself is becoming less accessible/stable. Resistance is moderate (0.71) because doctrinal and political opposition exists (the broad-effects reading has adherents) even though current Court majority supports the narrow reading. The measurement series shows gentle upward creep in all three metrics (extractiveness 0.18 → 0.28, suppression 0.08 → 0.15, theater 0.08 → 0.22) from year 0 to year 40, reflecting the increased enforcement energy required as the economic integration problem deepens and makes the interstate/intrastate boundary harder to maintain. Measurements are shared on a single time grid.
 *
 * PERSPECTIVAL GAP:
 *   Federal courts and state governments should compute as beneficiaries (courts retain interpretive authority, states retain regulatory autonomy); Congress, civil rights coalitions, and environmental coalitions should compute as targets/victims (Congress is constrained, civil rights and environmental objectives face exclusion). Interstate commerce participants sit near symmetric — they benefit from barrier-removal but pay the cost of navigating fragmented intrastate rules. The engine computes this divergence from power, exit, and beneficiary/victim declarations. From the narrow originalist's seat, the reading is faithful to the Constitution's text and history. From the civil-rights seat, the reading is an obstacle to uniform national protection. Both perspectives arise from the same constraint structure — it is genuinely a different thing depending where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are beneficiaries (d low, near 0.15) — they collect retained autonomy at zero enforcement cost. Civil rights coalitions are victims (d high, near 0.85) — they bear the cost of excluded federal reach. Congress is observer/payer (d moderate, near 0.5–0.6) — constrained but not harmed; Congress always retains the power to legislate through other constitutional hooks or to amend the Constitution. Federal courts are agenda-setters (d low, near 0.2) — they extract modest authority to police the line. No directionality overrides are required; the derivation chain (beneficiary/victim + power + exit) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and contested. The narrow reading frames the founding problem as removing state tariff barriers to create a national market. That problem is solved and stable — the federal courts enforce barrier-removal so effectively that modern state protectionism is rare. However, the reading's scope has become increasingly questioned as new coordination problems (environmental spillovers, civil rights protection, labor standards) have emerged. The civil-rights and environmental victims ask: if the founding problem (state barriers to commerce) is solved, why does the reading continue to prevent federal reach on new problems? The reading's answer is that these new problems are outside the commerce power's enumerated scope. The classification (Rope vs. Snare vs. Piton) hinges on whether the narrow reading is genuinely coordinating interstate commerce (Rope) or merely defending state sovereignty as an end in itself (Piton). The measurement series shows stable but modest theater growth — originalist courts are not performing the reading theatrically; they state the doctrine openly. The extraction is stable, not rising. This suggests the reading is a Rope that coordinates (removes barriers) without pretense. However, the rising suppression and theater ratios hint that as the boundary becomes harder to maintain empirically, enforcement effort increases — the reading may be transitioning toward Piton (inertial persistence) as its founding function becomes obsolete and its operation becomes more performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Commerce Clause''s core function to remove state barriers to interstate trade (''negative commerce''), or to grant Congress affirmative power to regulate all economic activity with national effects (''positive commerce'')?',
    'Originalist historical analysis (Convention records, Founding-era debates, ratification documents) versus consequentialist policy assessment (does national coordination require broader federal reach than barrier-removal alone?). The dispute is conceptual and partly empirical — what problems did the Framers intend to solve, and do modern economic structures present problems the Framers did not anticipate?',
    'This reading asserts the narrow barrier-removal function. If historical evidence strongly favored broad effects, this reading forecloses itself. If historical evidence supports the narrow reading but modern coordination requires broader reach, the reading becomes a true-to-origins but obsolete constraint — a Piton candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The interpretive contest between narrow originalism and broad-effects readings of the Commerce Clause').

omega_variable(
    originalism_vs_modern_coordination,
    'Should constitutional interpretation be bound to the original public meaning of 1787–1789, or should it evolve to address coordination problems the Framers did not anticipate (pollution spillovers, national labor standards, civil rights enforcement)?',
    'Meta-constitutional: resolving this requires a theory of constitutional legitimacy (fidelity to text vs. pragmatic evolution) that is not itself answerable by empirical evidence. Different jurisprudential schools give different answers. This is a preference question, not an empirical one.',
    'If original meaning is normatively privileged, the narrow reading stands. If constitutional evolution is legitimate, modern problems may warrant reading the Commerce Clause more broadly despite its historical meaning. This omega brackets the constraint''s entire legitimacy frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_vs_modern_coordination, preference, 'Whether constitutional interpretation should be fixed by original meaning or evolve with modern circumstances').

omega_variable(
    interstate_vs_intrastate_boundary,
    'Can the boundary between interstate and intrastate commerce be sustainably maintained? Or does modern economic integration mean all economic activity has some interstate effect, collapsing the distinction?',
    'Empirical: examine whether truly localized economic activity (small family business, local services, intrastate production) exists as a meaningfully distinct category in a nationally integrated economy. If virtually all activity has measurable interstate effects, the boundary becomes a arbitrary line, and the narrow reading''s limiting principle breaks down.',
    'A collapsed boundary would force this reading either to accept that nearly all activity falls within federal reach (functionally equivalent to the broad reading), or to adopt an explicit exclusion (e.g., ''non-commercial activity is always intrastate'') that abandons the effects test entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_vs_intrastate_boundary, empirical, 'Whether the interstate/intrastate distinction remains coherent in a nationally integrated economy').

omega_variable(
    suppression_of_competing_readings,
    'Does this reading''s enforcement require active suppression of the broad-effects reading (judicial strike-downs, limiting language), or does it coexist with broad-effects doctrine through reading-selection over time?',
    'Historical analysis of case law and doctrinal evolution. Does the Supreme Court actively reject broad-effects language, or does it simply choose the narrow reading for particular cases? The measurement series shows modest suppression growth (0.08 → 0.15), suggesting enforcement tension rather than doctrinal domination.',
    'If active suppression is required, the reading is a Snare (enforcement-dependent, actively suppresses alternatives). If coexistence is maintained through selective application, it is a Rope (coordination without forcible exclusion). Current suppression levels suggest weak enforcement — the reading may be losing structural hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_competing_readings, empirical, 'Whether this reading''s persistence requires active judicial suppression of competing doctrines or coexists with them').

omega_variable(
    civil_rights_enforcement_tradeoff,
    'What is the normative weight of civil rights enforcement (preventing discrimination in recalcitrant states) relative to federalism autonomy (state authority over local commerce)?',
    'This is a value tradeoff, not an empirical or conceptual question. Historical evidence on the Fourteenth Amendment''s relationship to the Commerce Clause may inform it, but cannot resolve it. Different constitutional traditions (originalist vs. living-constitutionalist) weight these differently.',
    'For originalists committed to the narrow reading, civil rights enforcement must work through explicit constitutional provisions (Fourteenth Amendment) rather than the Commerce Clause. For pragmatists, the narrow reading''s loss of civil-rights leverage is a severe cost that may override originalist fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_rights_enforcement_tradeoff, preference, 'The normative tradeoff between federalism and national civil rights enforcement').

omega_variable(
    originalism_adoption_dynamics,
    'Is the recent adoption of originalism by a majority of the Supreme Court (2020s) the result of superior interpretive method, political realignment, or institutional path-dependence?',
    'Intellectual history and political history: examine whether originalism''s rise correlates with evidence of better interpretive accuracy or with changing political coalitions on the Court. This informs whether the reading''s recent enforcement strength is principled or contingent.',
    'If originalism''s rise is principled, expect sustained enforcement of the narrow reading. If contingent on Court composition, the reading''s dominance may reverse if the Court changes. This omega frames the reading''s stability as contingent on meta-constitutional politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_adoption_dynamics, empirical, 'Whether originalism''s current Court dominance reflects methodological superiority or political contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comm_tr_t8, commerce_clause_scope__narrow_originalist, theater_ratio, 8, 0.11).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_scope__narrow_originalist, theater_ratio, 16, 0.15).
narrative_ontology:measurement(comm_tr_t24, commerce_clause_scope__narrow_originalist, theater_ratio, 24, 0.19).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_scope__narrow_originalist, theater_ratio, 32, 0.21).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__narrow_originalist, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comm_be_t8, commerce_clause_scope__narrow_originalist, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(comm_be_t16, commerce_clause_scope__narrow_originalist, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(comm_be_t24, commerce_clause_scope__narrow_originalist, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(comm_be_t32, commerce_clause_scope__narrow_originalist, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__narrow_originalist, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(comm_su_t8, commerce_clause_scope__narrow_originalist, suppression_requirement, 8, 0.1).
narrative_ontology:measurement(comm_su_t16, commerce_clause_scope__narrow_originalist, suppression_requirement, 16, 0.12).
narrative_ontology:measurement(comm_su_t24, commerce_clause_scope__narrow_originalist, suppression_requirement, 24, 0.14).
narrative_ontology:measurement(comm_su_t32, commerce_clause_scope__narrow_originalist, suppression_requirement, 32, 0.15).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__narrow_originalist, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This story is one reading of the commerce_clause_scope kernel. The three readings (narrow_originalist, broad_effects_test, intermediate_channels) decompose a single contested constitutional concept into three structurally distinct constraints with different ε values, beneficiary/victim sets, and classifications. All three readings are linked via network.affects_constraints to show the family relationship. The narrow reading's low extractiveness and stable metrics contrast with the broad reading's high extractiveness and rising enforcement costs — the ε-invariance principle requires separate constraint stories because the two readings describe different structural problems (state barriers vs. federal reach) and yield different empirical predictions about enforcement and resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
