% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause: Physical Appropriation Reading
 *   domain: constitutional/property_rights
 *
 * SUMMARY:
 *   The Takings Clause of the Fifth Amendment (nor shall private property be
 *   taken for public use without just compensation) can be read narrowly or
 *   broadly. The physical-appropriation reading holds that ONLY direct
 *   physical seizure or permanent physical occupation of land triggers the
 *   compensation requirement. All other regulations—including those that
 *   destroy property value—are reviewed under rational-basis or Penn Central
 *   multi-factor tests and rarely succeed as compensable takings. This
 *   reading is one of three contested interpretations of the same
 *   constitutional text. The narrow reading benefits the regulatory state and
 *   preservation constituencies by shielding regulations from takings
 *   liability; it extracts regulatory costs from property owners and
 *   distributes them as background risks of property ownership. The
 *   constraint is claimed as tangled_rope (genuine coordination of government
 *   regulatory capacity + extraction of property-owner losses) and authored
 *   with metrics consistent with substantial active enforcement (suppression
 *   0.71, extraction 0.68) required to hold back broader takings doctrines
 *   that would compensate regulatory losses.
 *
 * KEY AGENTS:
 *   - government_actors: institutional power, analytical exit — sets the boundary, defines what counts as physical appropriation
 *   - property_owners_subjected_to_regulation: moderate power, constrained exit — bear regulatory costs without compensation
 *   - environmental_preservation_constituencies: organized power, mobile exit — benefit from regulations shielded from takings liability
 *   - public_health_regulators: institutional power, analytical exit — benefit from broad regulatory authority without compensation obligation
 *   - landed_property_interests: powerful, constrained exit — EXCLUDED from the reading's adoption; would argue for broader takings doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.71).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '6e80ef1d-437c-4b8d-947b-6c8ee3896c5a').
narrative_ontology:cs_kernel_codification('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', fixed_text).
narrative_ontology:cs_authority_grounding('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', lineage).
narrative_ontology:cs_interpretation_layer_present('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a').
narrative_ontology:cs_reading_relation('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', foundational, physical_possession_necessary_for_taking).
narrative_ontology:cs_axiom_status(physical_possession_necessary_for_taking, holdable).
narrative_ontology:cs_axiom_grounding('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', physical_possession_necessary_for_taking, deontological).
narrative_ontology:cs_axiom('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', foundational, regulatory_losses_are_background_risk).
narrative_ontology:cs_axiom_status(regulatory_losses_are_background_risk, holdable).
narrative_ontology:cs_axiom_grounding('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', regulatory_losses_are_background_risk, deontological).
narrative_ontology:cs_created_at('6e80ef1d-437c-4b8d-947b-6c8ee3896c5a', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_actors).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, regulatory_state).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_subjected_to_regulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_preservation_constituencies).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_health_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal, state, and local governments enact and enforce regulations (zoning, environmental, public welfare, occupancy restrictions, etc.). Under this reading, they bear NO compensation obligation unless they physically seize or permanently occupy the property—a standard that exempts the vast majority of regulatory action from Takings Clause liability. They set the boundary and adjudicate whether a regulation crosses it.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_actors, agenda_setter,
    institutional, generational, analytical, national).

% Face regulations that drastically diminish property value (wetlands preservation mandates that render land worthless, set-aside requirements, building bans, use restrictions). Under this reading, they receive NO compensation unless the government physically appropriates the land itself. Regulatory losses are treated as background risks of property ownership. Exit options are limited: comply with the regulation, abandon the property, or litigate—but courts using this reading almost always reject their takings claims.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_subjected_to_regulation, payer,
    moderate, biographical, constrained, national).

% Benefit from regulations that protect wetlands, endangered habitat, scenic vistas, water quality, and other environmental assets without requiring government to pay property owners for the restrictions. The narrow reading shields regulations from takings liability, enabling environmental law to function without massive government expenditure on compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_preservation_constituencies, beneficiary,
    organized, generational, mobile, national).

% Can enforce building codes, occupancy limits, zoning restrictions, and health and safety mandates without compensating property owners for lost returns, because these regulations don't involve physical appropriation. The reading removes a major fiscal brake on regulatory state expansion.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_health_regulators, beneficiary,
    institutional, generational, analytical, national).

% Large-scale landowners and developers (agriculture, resource extraction, real estate) would argue for broader takings protection—that 'economic takings' (regulations that slash land value) should trigger compensation. They are excluded from the decision-making process about how the Takings Clause is read, yet bear the heaviest regulatory burden under the narrow reading.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, landed_property_interests, excluded,
    powerful, generational, constrained, national).

% Federal and state courts apply the takings doctrine. Under this reading, their task is mechanically narrow: look for direct physical seizure or permanent occupation. If neither is present, the takings claim fails. The narrow framing reduces judicial discretion and case complexity.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts_interpreting_takings, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, government_actors).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the boundary of government's power to regulate property for public purposes without bearing the fiscal burden of compensation. By limiting takings liability to direct physical appropriation, the reading preserves governmental regulatory capacity in environmental, health, land-use, and public-welfare domains without triggering massive compensation obligations that would chill regulation.
% TRANSFER_FUNCTION: Transfers the cost of regulatory compliance from government (which would have to compensate property owners) to property owners (who bear the loss in property value as a cost of living in a regulated society). Effectively moves regulatory costs into the private capital stack rather than the public budget.
% ABSENT_VOICES: Large landowners, resource extractors, and developers who would argue for broader takings protection (economic takings doctrine) are structurally excluded. Rural and agricultural constituencies bearing the heaviest regulatory burden have less organized voice in constitutional interpretation than environmental coalitions and regulatory agencies. Low-income property owners in over-regulated neighborhoods are diffusely excluded.
% DISAPPEARANCE_RATIONALE: If this reading evaporated and broad economic takings became compensable, government would face massive liability for every value-diminishing regulation—zoning restrictions, environmental mandates, occupancy rules. Regulators would either dramatically cut regulations (world restructures toward deregulation) or face insolvency (compensation costs would crowd out other spending). The constraint's persistence is what enables the modern regulatory state.
% FOUNDING_PROBLEM: Early takings doctrine was muddled: courts struggled to distinguish legitimate regulation (which needn't be compensated) from illegitimate taking (which must be). The narrow physical-appropriation reading offered conceptual clarity: if the government takes direct possession, it pays; if it merely restricts use through law, it doesn't. Physical/legal distinction maps cleanly to compensation boundary.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Epstein, Sunstein, Merrill & Smith) and empirical scholars document that most value-destructive regulations were enacted AFTER the physical-appropriation reading solidified (post-Penn Central 1978), suggesting that the founding concern (distinguishing taking from regulation) was resolved by doctrine, not by need. Constitutional commentary and law review literature from outside the regulatory state beneficiary class (libertarian constitutional scholars, property-rights advocates) document the founding problem as solved but the reading persisting.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): The constraint systematically redirects regulatory costs from government budget to private property holders. A zoning restriction that eliminates 90% of land value is NOT a taking under this reading; the owner bears the loss. The extraction is high because the constraint allows government to capture all coordination benefits of regulation while property owners absorb all costs. Suppression (0.71): Courts must actively enforce the narrow boundary. Property owners continuously litigate broader takings theories; courts must reject them with consistent doctrine. The higher-end suppression reflects the litigation pressure against this reading and the doctrinal work required to sustain it. Theater (0.28): Courts apply ritualistic multi-factor tests (Penn Central) and perform detailed takings analysis, but the outcome is nearly predetermined—the narrow boundary means almost all claims fail. The theater ratio reflects the gap between apparent complexity and functional foreordainment. Accessibility collapse (0.62): Once a property owner understands this reading, their alternatives collapse: comply with regulation, lose the property to non-compliance, or litigate (and almost certainly lose). The partial collapse reflects that property owners retain formal exit (leave the jurisdiction, change the use within regulations, sell at depressed value) but the practical options narrow drastically. Resistance (0.58): Sustained resistance from property-owner coalitions, development interests, and originalist legal scholars who argue the reading under-protects property rights. This reading has opponents but lacks the organized political force to displace it at present. The measurement series is authored on one shared time grid (every metric at every time point) to enable temporal analysis of doctrine entrenchment.
 *
 * PERSPECTIVAL GAP:
 *   The government and regulatory-beneficiary seats compute this as legitimate coordination (property uses must be harmonized with public goods; the narrow reading clarifies the boundary). The property-owner seat computes this as extraction (government regulates to capture benefits while property owners absorb costs). The excluded landed-property-interest seat would compute it as a snare if given voice (no meaningful exit, pure cost-bearing). The engine computes per-seat types from the structural data; the gap is the measurement the system exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Government actors and environmental constituencies hold d near the beneficiary end (they collect coordination benefits and the constraint shields them from liability). Property owners subjected to regulation hold d near the target end (they absorb regulatory costs without compensation; trapped or identity-locked exit depending on land dependency). The excluded landed-property-interest would hold d at the full target end if included—they bear costs, have no voice, and would bear even more burden if the regulation expanded. Directionality derivation: beneficiaries (government, environmental state) push d downward; victims (property owners) push d upward; trapped/constrained exit modulates d toward target; analytical exit (courts) holds d near symmetric (they adjudicate the boundary but do not collect extraction). No override is required; the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits early-stage mandatrophy. The founding problem (conceptual confusion about where regulation ends and taking begins) is DEAD—the physical-appropriation doctrine cleanly answers that question. Yet the constraint persists, no longer because it solves a coordination problem but because it benefits the regulatory state and environmental constituencies. The theater_ratio trend (0.08→0.28) documents rising performativity: courts apply elaborate multi-factor tests while nearly always reaching predetermined conclusions. The suppression_requirement trend (0.42→0.71) documents the active enforcement work required as pressure against this reading builds. A genuine tangled-rope constraint (one solving a real coordination problem + asymmetrically extracting) would show stabilizing metrics once the founding problem is solved; this constraint shows rising theater and rising suppression as the founding problem decays, which is the mandatrophy signature. However, mandatrophy is NOT YET RESOLVED—the constraint has not degraded to pure theater (theater_ratio is still 0.28, not 0.6+) and remains enforced by judicial doctrine. The trajectory points toward piton (mostly performance, sustained by inertia) but has not fully transformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_economic_taking_boundary,
    'Is the distinction between physical appropriation and economic regulation a natural legal boundary, or a constructed doctrine that benefits regulatory power?',
    'Comparative constitutional analysis: do property-rights regimes with broader takings doctrine (e.g., German constitutional law''s proportionality test) experience measurably worse regulatory outcomes or more political dysfunction? Historical analysis: was the physical/economic distinction chosen because it was epistemically sound or because it advantaged regulatory coalitions?',
    'If the boundary is epistemically natural, the reading is defensible as clarification; if constructed, the reading is an false-summit—a reading that benefits identifiable actors while presenting itself as neutral doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physical_vs_economic_taking_boundary, conceptual, 'Whether the physical/economic boundary reflects constitutional structure or serves regulatory interests.').

omega_variable(
    regulatory_cost_externalization,
    'How much of the modern regulatory state''s expansion is enabled by the fact that regulations cost property owners (not government budgets) and thus face no fiscal brake?',
    'Counterfactual: if regulations that destroyed property value required compensation, what fraction of current environmental, zoning, and occupancy restrictions would not have been enacted? Empirical: do jurisdictions with broader takings liability (or compensation requirements for regulatory loss) have systematically smaller regulatory states?',
    'If compensation requirements would substantially chill regulation, the narrow reading is an implicit subsidy to the regulatory state. Extraction increases if this is intentional policy (favoring regulation over property rights) or becomes deeper structural injustice if it is unintended consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_cost_externalization, empirical, 'Whether the narrow takings reading enables regulatory expansion by externalizing costs.').

omega_variable(
    foundational_doctrine_obsolescence,
    'Has the physical-appropriation doctrine outlived its founding problem (clarifying the regulation/taking boundary) and now functions primarily as extraction without justification?',
    'Doctrinal and historical: was the boundary genuinely ambiguous when crystallized (post-Penn Central 1978) and is it still ambiguous now? If the boundary is now settled doctrine, does maintaining it serve any function other than exempting regulations from compensation?',
    'If the founding problem is fully dead and only extraction remains, the constraint is mandatrophy-resolved and a piton. Reclassification from tangled_rope to piton would follow. This omega directly addresses the mandatrophy analysis above.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_doctrine_obsolescence, conceptual, 'Whether the physical-appropriation doctrine still solves a coordination problem or now sustains extraction by inertia.').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Can the physical-appropriation reading and the regulatory-takings reading coexist in the same legal system, or does adoption of one logically foreclose the other?',
    'Logical: do the core premises directly contradict (foreclosure) or merely prioritize different values (coexistence)? Empirical: do courts in different jurisdictions hold both readings simultaneously, or does adoption of one preclude the other?',
    'If they foreclose each other, the sibling relationship is structural contradiction, not mere disagreement. If coexistence is possible, both are live readings held by different parties simultaneously—a contentious but stable state. Classification of the reading_relation (forecloses vs. coexists_with) depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Logical structure of the relation between physical-appropriation and regulatory-takings readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1922, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1922, 0.08).
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(taki_tr_t2026, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1922, 0.45).
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1978, 0.58).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(taki_be_t2026, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1922, 0.42).
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1978, 0.62).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(taki_su_t2026, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, environmental_regulation_liability_exposure).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, zoning_restriction_compensation_boundary).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel decomposes into three structurally distinct constraint stories: physical_appropriation_reading (this file, narrow boundary), categorical_takings_reading (permanent occupation per se takings), and regulatory_takings_reading (economic losses can trigger compensation). Each instantiates a different constraint with different victim sets, beneficiary structures, and ε values. The readings coexist as live doctrinal positions held by different jurisdictions and different judicial coalitions. Network links document the upstream/downstream relationships: physical-appropriation reading influences both siblings by establishing the narrowest boundary and thus the highest fiscal shelter for regulatory states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
