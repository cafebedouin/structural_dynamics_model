% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Takings Clause: Categorical vs. Contextual Boundary (Categorical Reading)
 *   domain: constitutional/property
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Takings Clause
 *   kernel: the categorical reading holds that permanent physical occupations
 *   and regulations eliminating all economically viable use are per se
 *   takings (automatic compensation), while all other regulations are
 *   evaluated via Penn Central contextual balancing (economic impact,
 *   character of government action, interference with investment-backed
 *   expectations). This reading attempts to stabilize expectations at the
 *   extremes while preserving regulatory flexibility in the middle range. The
 *   reading's core claim is that bright-line rules at the poles serve both
 *   property owners (predictability) and governments (reduced litigation).
 *   The constraint's extractiveness has increased over the interval (0.38 →
 *   0.58) as litigation over middle-range cases has proliferated and
 *   regulatory agencies have internalized takings liability costs; theater
 *   has risen (0.12 → 0.28) as the categorical rules' bright-line character
 *   becomes increasingly rhetorical (their actual application to novel facts
 *   remains contested).
 *
 * KEY AGENTS:
 *   - property_owners_at_extremes: Benefit from categorical per se rules; automatic compensation protection at poles.
 *   - regulators_in_middle_range: Pay via litigation risk and compensation pressure; face Penn Central balancing uncertainty.
 *   - supreme_court: Agenda-setter that maintains the categorical/contextual boundary and decides novel cases.
 *   - lower_courts: Implement the constraint; benefit from bright-line rules at extremes, face burden at middle.
 *   - middle_range_property_owners: Excluded from per se protection; must establish takings via contextual balancing.
 *   - regulatory_predictability_constituency: Abstract beneficiary; reduced interpretive burden from bright-line extremes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.41).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Takings Clause: Categorical vs. Contextual Boundary (Categorical Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/property").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '4bb4e96d-c758-459c-b35d-cbf802f39243').
narrative_ontology:cs_kernel_codification('4bb4e96d-c758-459c-b35d-cbf802f39243', fixed_text).
narrative_ontology:cs_authority_grounding('4bb4e96d-c758-459c-b35d-cbf802f39243', lineage).
narrative_ontology:cs_interpretation_layer_present('4bb4e96d-c758-459c-b35d-cbf802f39243').
narrative_ontology:cs_reading_relation('4bb4e96d-c758-459c-b35d-cbf802f39243', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bb4e96d-c758-459c-b35d-cbf802f39243', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('4bb4e96d-c758-459c-b35d-cbf802f39243', foundational, bright_line_categorical_rules_at_extremes_necessary).
narrative_ontology:cs_axiom_status(bright_line_categorical_rules_at_extremes_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4bb4e96d-c758-459c-b35d-cbf802f39243', bright_line_categorical_rules_at_extremes_necessary, instrumental).
narrative_ontology:cs_axiom('4bb4e96d-c758-459c-b35d-cbf802f39243', foundational, penn_central_balancing_sufficient_for_middle_range).
narrative_ontology:cs_axiom_status(penn_central_balancing_sufficient_for_middle_range, holdable).
narrative_ontology:cs_axiom_grounding('4bb4e96d-c758-459c-b35d-cbf802f39243', penn_central_balancing_sufficient_for_middle_range, conventional).
narrative_ontology:cs_reference_frame('4bb4e96d-c758-459c-b35d-cbf802f39243', takings_clause_categorical_bright_line_framework).
narrative_ontology:cs_drift_state('4bb4e96d-c758-459c-b35d-cbf802f39243', contemporary_regulatory_takings_expansion_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('4bb4e96d-c758-459c-b35d-cbf802f39243', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_extremes).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, regulatory_predictability_constituency).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulators_in_middle_range).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, public_welfare_balancing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owners of property subject to permanent physical occupation or regulations eliminating all economically viable use receive categorical per se takings protections — compensation is mandated without requiring them to prove harm via Penn Central balancing. They benefit from predictable bright-line rules that eliminate uncertainty at the extremes and make constitutional protection automatic at those poles.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_at_extremes, beneficiary,
    organized, generational, mobile, national).

% State and local governments enacting regulations that reduce property value significantly but fall short of the categorical extremes face unpredictable takings liability. They must navigate the Penn Central factors (character of government action, economic impact, interference with reasonable investment-backed expectations) which generate contested application and litigation risk. The categorical reading creates bright-line safety at the poles but forces them into the uncertain middle.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulators_in_middle_range, payer,
    institutional, biographical, constrained, national).

% The abstract collective good served by regulation — environmental protection, land use planning, public health, community safety — faces a structural constraint: regulations that approach the categorical threshold but do not cross it generate litigation risk and compensation pressure. The categorical reading thus creates a regulatory cost gradient that may deter welfare-advancing regulations in the middle range.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, public_welfare_balancing, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(takings_clause_boundary__categorical_takings_reading, public_welfare_balancing).

% Courts, legal academics, and administrative actors benefit from the categorical reading's reduced interpretive burden at the extremes: bright-line rules (permanent physical occupation, total value elimination) are easier to apply than the contextual Penn Central balancing, which reduces doctrinal uncertainty and speeds adjudication at the poles.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulatory_predictability_constituency, beneficiary,
    analytical, generational, analytical, national).

% Owners whose property is regulated to eliminate 40–90% of value (short of total elimination) are excluded from the per se takings protection and must establish takings claim through Penn Central balancing. Their investment-backed expectations, the regulation's character, and its economic impact must be weighed contextually. They would argue for broader categorical protection but lack the unified voice of the extreme-case constituencies.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, middle_range_property_owners, excluded,
    moderate, biographical, identity_locked, local).

% The U.S. Supreme Court sets and enforces the categorical/contextual boundary via case law. It decides which situations trigger per se rules and which fall to Penn Central balancing. The Court's doctrine-setting authority makes it the administrative agent maintaining the constraint's structure and scope.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Federal and state trial and appellate courts benefit from categorical rules at the extremes (clearer decision procedure) but face the burden of Penn Central application in middle cases. They collect the authority to apply Supreme Court doctrine but carry the interpretive burden when the doctrine does not resolve a case.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, lower_courts, beneficiary,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, supreme_court).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Takings Clause coordination function: establish a stable framework for distinguishing compensable takings from non-compensable regulations, enabling property owners to form reasonable investment-backed expectations and governments to plan regulatory programs within knowable constitutional bounds.
% TRANSFER_FUNCTION: Moves financial obligation from property owners to governments in extreme cases (permanent physical occupation, total value elimination) via categorical compensation requirements; creates litigation risk and potential compensation liability for governments regulating in the middle range where Penn Central balancing governs.
% ABSENT_VOICES: Regulatory agencies whose statutory mandates require strict environmental or land-use controls, and environmental/public-health constituencies arguing that takings liability chills necessary regulation, are structurally excluded from framing the categorical/contextual boundary. Their objection — that the categorical reading makes welfare-advancing regulations more expensive or risky — is not represented in the Supreme Court's doctrine-setting process.
% DISAPPEARANCE_RATIONALE: If the categorical reading vanished (replaced by pure Penn Central balancing for all cases), the regulatory landscape would shift: governments would face identical litigation risk at all value-reduction levels, potentially increasing takings liability costs; property owners would lose the automatic protection of bright-line rules and would be forced into contextual litigation. Investment-backed expectations would depend entirely on how Penn Central is applied, not on bright-line structural positions.
% FOUNDING_PROBLEM: After the physical-vs-regulatory takings split became settled doctrine (late 20th century), courts faced a coherence problem: how to distinguish takings that do require compensation from regulations that do not, in cases that fell short of obvious physical seizure? The categorical reading was built to stabilize expectations by creating bright-line rules at the extremes (permanent physical occupation, total value elimination) while preserving regulatory flexibility in the middle via Penn Central balancing.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, judges, and property rights advocates attest the coherence problem is ongoing: every term the Supreme Court hears takings cases because Penn Central balancing generates contested outcomes. Government agencies attest they must litigate takings claims because the middle range remains uncertain. Property owners and their advocates attest the categorical reading is necessary to stabilize expectations at the extremes.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint's primary operation is redistributing takings liability risk: property owners at extremes are protected; governments in the middle face elevated litigation and compensation exposure; middle-range property owners face denial of automatic protection. Suppression is moderate (0.41) because the constraint does not eliminate alternatives — Penn Central balancing is always available and can reach takings conclusions even in middle-range cases — but it does suppress the alternative of treating all takings contextually (which would give middle-range owners a clearer path to compensation). Theater is moderate (0.28) because the bright-line categories are genuine bright-line rules (permanent physical occupation, total value elimination are reliably per se), but their application to novel facts generates ongoing litigation, making the categorical structure partly performative. Accessibility collapse is high (0.72) because once the categorical framework is understood, alternative interpretive structures (pure contextual balancing, pure property-right-maximalism) are largely foreclosed by Supreme Court binding authority and stare decisis. Resistance is high (0.68) because property rights advocates actively defend the categorical reading against regulatory takings expansion, and regulatory agencies actively litigate Penn Central boundaries. The measurement series tracks the growth of extractiveness and theater over three decades as takings jurisprudence has matured and the middle-range litigation burden has accumulated.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's agenda-setting perspective, the categorical reading is a rational optimization: it provides bright-line certainty at the extremes (where stakes are highest and predictability matters most) while preserving flexibility in the middle (where context matters and rigid rules would over-regulate). From the regulator's perspective in the middle range, the same reading is an extraction mechanism: it shifted the burden from property owners to governments, who now face litigation and compensation costs. From the middle-range property owner's perspective, it is an exclusion: they lost the automatic protections offered at the extremes. This tri-perspectival divergence is a feature of the constraint, not a bug — different seats experience the same categorical structure as stability, extraction, or exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners at the extremes experience directionality near 0.0 (beneficiaries): they receive automatic compensation protection and predictable rules. Regulators in the middle range experience directionality near 1.0 (full targets): they must navigate Penn Central balancing, face litigation risk, and internalize potential compensation costs. The analytical beneficiaries (regulatory predictability, lower courts) experience near-beneficiary directionality because they benefit from reduced interpretive burden. Middle-range property owners are structurally ambiguous: they are excluded from per se protection (target-like) but can still win takings claims via Penn Central (beneficiary-like). This ambiguity is captured in their exclusion status and the omega variables.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical reading's founding problem was coherence: how to distinguish compensable takings from non-compensable regulations? The categorical reading solved it by creating bright-line rules at extremes and Penn Central balancing elsewhere. The problem remains live (courts still litigate takings cases every term), but the categorical structure's performance has degraded over the interval: the lines at the extremes have proven brittle (regulatory takings doctrine has expanded; property owners have challenged Penn Central balancing; courts have disagreed on what counts as 'permanent' occupation or 'total' value elimination). The constraint persists not because it solved the founding problem definitively but because it reduced immediate doctrinal chaos and shifted the burden to ongoing litigation. This is not mandatrophy (the founding problem is still pressing courts), but it is drift toward mandatrophy territory: the categorical structure is increasingly performing a gate-keeping function (sorting cases into extreme vs. middle) rather than a problem-solving function (predicting outcomes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanent_occupation_boundary_dispute,
    'What counts as ''permanent'' physical occupation for per se takings purposes? Courts have disagreed on temporary occupation, perpetual rights-of-way, and continuous regulatory occupation (e.g., government mandated access for firefighting or utility installation).',
    'Systematic review of Supreme Court cases defining permanence boundaries; empirical study of lower-court divergence in applying the permanence standard; legislative clarification of the term.',
    'If permanence is narrowly defined (only indefinite, government-held physical presence), fewer cases reach per se status and Penn Central balancing expands. If broadly defined (any continuous access or occupancy right), more cases receive automatic compensation and regulator costs rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanent_occupation_boundary_dispute, empirical, 'Boundary ambiguity of ''permanent'' physical occupation in per se takings doctrine').

omega_variable(
    total_value_elimination_measurement_dispute,
    'How is ''total economic value elimination'' measured? Must property be economically valueless in absolute terms, or does any remaining use count? If a regulation eliminates all commercial use but permits passive holding or eventual resale, is value truly eliminated?',
    'Case law review; economic analysis of how courts calculate remaining value; empirical study of regulations leading to near-zero-value property.',
    'Narrow measurement (economic use for a reasonable time horizon) expands per se takings; broad measurement (any residual possibility, however remote) restricts per se to true total wipeouts. The measurement choice materially changes the per se category''s scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(total_value_elimination_measurement_dispute, empirical, 'Measurement ambiguity of ''total economic value elimination'' in per se takings doctrine').

omega_variable(
    categorical_vs_contextual_kernel_reading_contest,
    'Is the categorical/contextual binary the correct framing of the takings coherence problem, or are the sibling readings (physical_appropriation_reading, regulatory_takings_reading) capturing distinct axes that the categorical reading conflates?',
    'Comparative analysis of how the three readings resolve contested cases differently; study of which reading better predicts outcomes; examination of whether the categorical reading''s middle-range Penn Central balancing is genuinely contextual or secretly categorical (i.e., whether it applies consistent standards or case-by-case judgment).',
    'If the binary is correct, the categorical reading is the optimal structure. If the sibling readings capture independent coherence structures, the categorical reading may be missing important distinctions and over-extracting from the middle range.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_contextual_kernel_reading_contest, conceptual, 'Whether the categorical/contextual binary is the correct frame for the takings coherence problem or misses distinct axes from sibling readings').

omega_variable(
    regulatory_externality_internalization_mechanism,
    'Does the categorical reading''s extraction of regulatory costs (compensating extreme cases, litigating middle-range cases) achieve its stated purpose of stabilizing investment-backed expectations, or does it displace the problem by making regulators internalize takings costs and pass them to the public through reduced regulation or higher public goods prices?',
    'Empirical study of regulatory agency behavior pre- and post-takings liability expansion; analysis of how compensation requirements affect environmental, land-use, and housing regulation; survey of whether property owners'' expectations are more stable under the categorical reading than under pure Penn Central or pure appropriation readings.',
    'If the reading stabilizes expectations and improves regulatory predictability, it is a genuine coordination achievement. If it merely displaces costs (regulators reduce regulation rather than paying takings liability, property owners face reduced access to public goods), the reading is purely extractive with no coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_externality_internalization_mechanism, empirical, 'Whether categorical takings doctrine stabilizes investment-backed expectations or displaces regulatory costs to the public').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement(taki_tr_t2016, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2016, 0.24).
narrative_ontology:measurement(taki_tr_t2022, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2022, 0.27).
narrative_ontology:measurement(taki_tr_t2026, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.38).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(taki_be_t2016, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2016, 0.54).
narrative_ontology:measurement(taki_be_t2022, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2022, 0.57).
narrative_ontology:measurement(taki_be_t2026, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2000, 0.31).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement(taki_su_t2016, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(taki_su_t2022, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(taki_su_t2026, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2026, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel has three reading-specific constraints: categorical_takings_reading (this one), physical_appropriation_reading, and regulatory_takings_reading. Each reading instantiates a different constraint because each produces a different ε value (how much the constraint extracts from regulators vs. property owners), different beneficiary/victim structures, and different structural classifications. The categorical reading emphasizes bright-line extremes plus middle-range balancing; the physical_appropriation reading restricts per se takings to direct seizure or occupation; the regulatory_takings reading treats all regulations contextually via Penn Central. Each reading is authored as a separate constraint story with its own omegas addressing the reading-specific uncertainties. The three stories are linked via network.affects_constraints to enable contamination analysis: changes in one reading's jurisprudence create downstream pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
