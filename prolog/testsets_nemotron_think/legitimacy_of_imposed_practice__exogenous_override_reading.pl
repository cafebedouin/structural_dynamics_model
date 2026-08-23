% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Authority Sufficient to Displace Prior Practice
 *   domain: political/historical/cultural_imposition
 *
 * SUMMARY:
 *   This constraint story captures the exogenous override reading of the
 *   legitimacy_of_imposed_practice kernel: the claim that state decree
 *   authority alone is sufficient to displace prior cultural practices, with
 *   compliance following from legal mandate regardless of whether the
 *   affected populations internalize the new practice. The structural delta
 *   specifies two domains — Calendar (pure override: legal abolition of
 *   traditional calendar, rural non-compliance, practical workarounds) and
 *   Dress (partial override: coercive enforcement of new dress codes, but
 *   incomplete displacement of traditional garments). Beneficiaries are the
 *   state modernization agenda and urban administrators who gain
 *   administrative legibility and symbolic capital. Victims are rural
 *   populations bearing adjustment costs without consultation, and
 *   traditional authorities displaced from their normative role. The
 *   constraint is claimed as a snare: the coordination story (modernization,
 *   standardization) is cover for extraction (administrative control,
 *   symbolic dominance) enforced through coercion with suppressed
 *   alternatives.
 *
 * KEY AGENTS:
 *   - state_modernization_agenda: Primary beneficiary/agenda_setter (institutional/arbitrage) — sets the mandate, captures administrative legibility
 *   - urban_administrators: Secondary beneficiary/agenda_setter (organized/arbitrage) — implement decree, gain career advancement through enforcement metrics
 *   - rural_populations: Primary victim/payer (powerless/trapped) — bear adjustment costs, face penalties for non-compliance, no exit from state reach
 *   - traditional_authorities: Victim/excluded (organized/constrained) — displaced from normative authority, co-opted or suppressed
 *   - historical_analysts: Observer (analytical/analytical) — evaluate long-term displacement vs. persistence of informal practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority Sufficient to Displace Prior Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political/historical/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '402f12c7-6851-4944-9187-0a9a32fc52d9').
narrative_ontology:cs_kernel_codification('402f12c7-6851-4944-9187-0a9a32fc52d9', formalized).
narrative_ontology:cs_authority_grounding('402f12c7-6851-4944-9187-0a9a32fc52d9', extraction).
narrative_ontology:cs_interpretation_layer_present('402f12c7-6851-4944-9187-0a9a32fc52d9').
narrative_ontology:cs_reading_relation('402f12c7-6851-4944-9187-0a9a32fc52d9', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('402f12c7-6851-4944-9187-0a9a32fc52d9', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('402f12c7-6851-4944-9187-0a9a32fc52d9', foundational, state_decree_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(state_decree_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('402f12c7-6851-4944-9187-0a9a32fc52d9', state_decree_sufficient_for_displacement, conventional).
narrative_ontology:cs_axiom('402f12c7-6851-4944-9187-0a9a32fc52d9', foundational, internalization_not_required_for_compliance).
narrative_ontology:cs_axiom_status(internalization_not_required_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('402f12c7-6851-4944-9187-0a9a32fc52d9', internalization_not_required_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('402f12c7-6851-4944-9187-0a9a32fc52d9', legal_positivist_state_authority).
narrative_ontology:cs_drift_state('402f12c7-6851-4944-9187-0a9a32fc52d9', contemporary_administrative_capacity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('402f12c7-6851-4944-9187-0a9a32fc52d9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrators).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_authorities).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, legal_positivism_authority).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, modernization_theory_linear_progress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal mandate abolishing traditional calendar and dress codes. Justifies it as modernization, standardization, and progress. Captures administrative legibility (uniform timekeeping, identifiable citizens) and symbolic capital (state as modernizer). Can shift policy if political cost exceeds benefit — has arbitrage-grade exit from this specific mandate.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, agenda_setter,
    institutional, generational, arbitrage, national).

% Implement the decree through local bureaucracy: issue fines, deny services to non-compliers, report compliance metrics. Gain career advancement from enforcement success. Benefit from simplified administration (uniform codes). Constrained exit: transferring out of enforcement roles is possible but career-costly; they are invested in the system.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrators, beneficiary).

% Bear the costs: learn new calendar for official business while maintaining traditional calendar for agriculture/rituals; purchase new clothing; face fines or service denial for non-compliance. No consultation in the mandate. No meaningful exit — state territorial monopoly means they cannot escape the jurisdiction. Practical workarounds (dual practice, hidden traditional dress) are their only resistance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Displaced from normative authority over timekeeping and dress. Some co-opted as state intermediaries (enforcing decree locally); others suppressed. Their legitimacy derives from tradition, which the decree declares obsolete. Constrained exit: they can collaborate (gaining state patronage but losing traditional credibility) or resist (risking removal), but cannot restore the prior normative order.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_authorities, payer,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_authorities, excluded).

% Evaluate long-term outcomes: does the decree produce genuine behavioral displacement or only formal compliance? Measure informal persistence of traditional practices. Assess whether modernization benefits materialize or whether extraction dominates. Their analysis shapes future policy but they bear no direct costs from the constraint.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes timekeeping and dress across a diverse territory to enable modern administration: tax collection, conscription, census, legal proceedings, and market regulation all require legible, uniform categories.
% TRANSFER_FUNCTION: Moves compliance labor, cultural disruption costs, and symbolic submission from rural populations to the state apparatus. The state gains administrative legibility and modernization legitimacy; rural populations lose autonomous timekeeping, traditional dress, and normative authority of local leaders.
% ABSENT_VOICES: Rural populations had no representation in the legislative bodies that issued the decrees. Traditional authorities were not consulted; their objections were treated as obstruction. Women's specific burdens (garment production, calendar management for domestic rituals) were invisible to the male-dominated administration. These voices are structurally excluded by the constraint's design.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement vanished overnight, rural populations would revert to traditional calendar and dress within months (dual practice already exists). Traditional authorities would reclaim normative role. Urban administrators would lose enforcement metrics and simplified categories. The state would need new administrative tools for managing diversity. The world rearranges because arrangements depend on this constraint.
% FOUNDING_PROBLEM: Early modern states faced administrative illegibility: diverse local calendars, dress codes, and measurement systems made taxation, conscription, and governance inefficient. The founding problem was the state's inability to 'see' and administer its population uniformly.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Scott, Foucault, Weber) outside the state modernization agenda attest that the administrative illegibility problem was real in the 18th-19th centuries. Contemporary state capacity researchers attest that modern statistical/administrative tools (sampling, digital records, localized administration) have substantially solved the founding problem without requiring cultural abolition. The state's own archives show the decree persisted long after administrative necessity faded.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the state extracts compliance labor, cultural disruption costs, and symbolic submission from rural populations without compensation. Suppression (0.82) is very high because the constraint's persistence depends on active coercion: legal penalties, denial of state services for non-compliance, surveillance of rural areas, and co-optation of traditional leaders. Theater ratio (0.45) is moderate: the modernization rhetoric performs a coordination function (standardization, progress narrative) but nearly half the enforcement activity defends the payment exclusivity rather than genuine public good. Accessibility collapse (0.76) is high because once the decree is issued, traditional practices are legally abolished — alternatives exist only informally and at risk. Resistance (0.68) is substantial: rural non-compliance, practical workarounds (dual calendar use, hidden traditional dress), and periodic uprisings document active resistance. The measurement series shows extractiveness rising as enforcement intensifies (years 0-10), then stabilizing; theater rising as modernization rhetoric thickens; suppression peaking early then modulating as informal persistence becomes managed rather than eliminated.
 *
 * PERSPECTIVAL GAP:
 *   From the state/urban administrator seat, the constraint appears as genuine coordination: standardization reduces transaction costs, enables modern administration, creates legible populations. From the rural population seat, the same structure operates as enforced extraction: they pay the costs of disruption, gain no voice, and the 'modernization' benefits accrue elsewhere. From the traditional authority seat, it is displacement: their normative role is abolished, replaced by state-appointed intermediaries. The engine computes these divergent per-seat classifications from the structural data — the authored claim (snare) reflects the rural/traditional seat experience, while the state seat would compute rope or tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernization agenda and urban administrators are structural beneficiaries (d near 0.0): they collect administrative legibility, symbolic capital, and career advancement from the decree. Rural populations are full targets (d near 1.0): they bear all adjustment costs, face penalties, have no meaningful exit (trapped by state territorial monopoly). Traditional authorities are targets with constrained exit (d ~ 0.8): displaced from authority, can sometimes negotiate co-optation but cannot restore prior status. Historical analysts sit at analytical (d = 0.5): they observe the structure without bearing costs or collecting benefits. The derivation chain from beneficiary/victim declarations + power + exit produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative illegibility of diverse local practices) was real but the arrangement has outlived it: modern states now have statistical/administrative tools to manage diversity without abolition. The mandate persists because the extraction (compliance labor, symbolic submission) benefits the state apparatus, and the cost to fix (political risk of recognizing traditional authority) exceeds what administrators bear. This is classic mandatrophy: the constraint's function has atrophied but the structure remains because no party is hurt enough to fix it and the administrator benefits from its persistence. The theater ratio captures the performative maintenance of the modernization narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is one reading (exogenous_override_reading) of the contested kernel legitimacy_of_imposed_practice. What structural elements distinguish it from the sibling readings endogenous_climb_reading and hybrid_scaffolding_reading?',
    'Compare the three readings'' beneficiary/victim structures, suppression mechanisms, and coordination claims. The exogenous reading claims decree alone suffices; endogenous claims internalization required; hybrid claims scaffolded imposition works. Each produces different ε and different victim sets.',
    'If the readings are not structurally distinct, they collapse into one constraint with measurement-dependent ε, violating ε-invariance. Distinct readings must author separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committee structure: which reading of the legitimacy kernel this constraint instantiates').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.82) primarily structural (legal penalties, administrative coercion, resource denial) or internalized (rural populations accepting the new practice as legitimate over time)?',
    'Post-reform trajectory analysis: if suppression metrics persist after legal enforcement relaxes, internalization component is significant. Compare regions with identical laws but different enforcement intensity.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the constraint with them. This would increase χ for rural populations beyond what structural coercion alone predicts, reinforcing snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in state imposition of cultural practices').

omega_variable(
    decree_effectiveness_boundary,
    'Where is the boundary between ''decree displaces practice'' (this reading''s claim) and ''decree creates legal fiction while practice persists informally'' (the observed rural non-compliance)?',
    'Longitudinal compliance data: measure formal adoption vs. actual practice at 5, 10, 20 year intervals. The boundary is where formal compliance diverges from behavioral compliance without converging.',
    'If the boundary is narrow (decree quickly becomes fiction), the constraint''s claimed coordination function collapses and extraction is nearly pure — snare classification strengthens. If wide (decree gradually internalizes), tangled_rope becomes plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decree_effectiveness_boundary, empirical, 'Whether legal mandate alone produces behavioral displacement or only formal compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimacy_imposed_exogenous_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_tr_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_tr_t5, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_tr_t5, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_tr_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_tr_t15, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_tr_t15, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(legitimacy_imposed_exogenous_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_be_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_be_t5, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_be_t5, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_be_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_be_t15, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_be_t15, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(legitimacy_imposed_exogenous_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_su_t0, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_su_t5, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_su_t5, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_su_t10, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_su_t15, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_su_t15, observed).
narrative_ontology:measurement(legitimacy_imposed_exogenous_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(legitimacy_imposed_exogenous_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, state_administrative_legibility_standardization).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, rural_traditional_authority_displacement).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'state imposition of cultural practices' into three structurally distinct readings of the legitimacy kernel. The exogenous reading (this story) claims decree suffices and authors high extraction/suppression. The endogenous reading claims internalization required and would author lower suppression but higher resistance. The hybrid reading claims scaffolded imposition works and would author intermediate metrics with ideological coordination function. All three share the kernel_id and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
