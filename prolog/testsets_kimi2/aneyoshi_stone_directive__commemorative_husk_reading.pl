% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive â Commemorative Husk Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint instantiates the commemorative_husk_reading of the
 *   aneyoshi_stone_directive kernel. The Aneyoshi stone, erected after the
 *   1933 Showa tsunami, warns against building below a marked elevation. The
 *   behavioral_competence_reading claims this directive retained binding
 *   land-use force for 78 years. This reading asserts the opposite: the
 *   directive lost behavioral force during the inter-catastrophe period and
 *   the stone became a memorial artifact. Yet the standing arrangement
 *   continues to suppress economically rational coastal development because
 *   municipal governance and heritage institutions leverage the stone's
 *   symbolic authority to enforce land-use restrictions. The result is a
 *   tangled rope: genuine coordination of disaster memory and heritage
 *   tourism is fused with asymmetric extraction from conventional
 *   development. The constraint's decay into a commemorative husk benefits
 *   tourism-development interests while imposing costs on coastal developers
 *   and identity-locked local residents.
 *
 * KEY AGENTS:
 *   - municipal_governance: Agenda-setter (institutional/constrained) â administers heritage zone and enforces land-use restriction while collecting tourism prestige
 *   - heritage_tourism_operators: Beneficiary (moderate/mobile) â monetize the stone's symbolic capital through memorial economy
 *   - coastal_property_developers: Payer (powerful/constrained) â bear suppressed development rights below the marker
 *   - local_residents: Dual-positioned payer/beneficiary (moderate/identity_locked) â land-use restricted but place-identity fused with stone narrative
 *   - disaster_anthropologists: Observer (analytical) â document the gap between official narrative and behavioral evidence
 *   - resilient_coastal_development_advocates: Excluded (organized) â structurally barred from challenging the stone's land-use authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.72).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, tangled_rope).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Stone Directive â Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(aneyoshi_stone_directive__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '1eb6bbf9-0312-4d77-beee-32d5058a1a5e').
narrative_ontology:cs_kernel_codification('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', fixed_text).
narrative_ontology:cs_authority_grounding('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', lineage).
narrative_ontology:cs_interpretation_layer_present('1eb6bbf9-0312-4d77-beee-32d5058a1a5e').
narrative_ontology:cs_reading_relation('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', aneyoshi_stone_directive__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', foundational, memorial_husk_thesis).
narrative_ontology:cs_axiom_status(memorial_husk_thesis, holdable).
narrative_ontology:cs_axiom_grounding('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', memorial_husk_thesis, empirically_contingent).
narrative_ontology:cs_axiom('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', secondary, inter_catastrophe_behavioral_decay).
narrative_ontology:cs_axiom_status(inter_catastrophe_behavioral_decay, holdable).
narrative_ontology:cs_axiom_grounding('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', inter_catastrophe_behavioral_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', active_tsunami_warning_directive).
narrative_ontology:cs_drift_state('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', inter_catastrophe_endpoint, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1eb6bbf9-0312-4d77-beee-32d5058a1a5e', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, heritage_tourism_operators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, coastal_property_developers).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, local_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, local_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the heritage protection zone surrounding the stone, enforces land-use restrictions below the marker line, and collects tourism-related revenue and political prestige from the memorial's fame. Exit is constrained by electoral and bureaucratic accountability to the memorial narrative.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance, agenda_setter,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, municipal_governance, beneficiary).

% Operate guided tours, souvenir commerce, and memorial programming centered on the stone. They benefit directly from the stone's symbolic capital but do not administer the land-use rule. Can relocate to other heritage sites if demand shifts.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, heritage_tourism_operators, beneficiary,
    moderate, biographical, mobile, regional).

% Seek to build residential and commercial coastal properties in the restricted zone below the stone's marker. Their projects are denied permits based on the heritage-tsunami overlay. Litigation and political lobbying are costly; the cultural prestige of the stone makes open opposition publicly risky.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_property_developers, payer,
    powerful, biographical, constrained, national).

% Inhabit the village above the marker line. Their identity is fused with the stone's survival narrative and the village's reputation as a disaster-resilient community. They cannot easily rebuild or expand below the line, nor can they easily leave without severing familial and place-based ties. Some benefit from tourism employment.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, local_residents, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, local_residents, beneficiary).

% Study the stone's actual social function across the inter-catastrophe period, documenting the gap between official heritage narratives and observed behavioral patterns. They neither pay nor benefit from the constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% Advocate for engineered tsunami-resistant coastal construction that would render the absolute elevation rule obsolete. Excluded from heritage governance committees and zoning hearings where the stone's authority is treated as non-negotiable.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, resilient_coastal_development_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, heritage_tourism_operators).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains spatial memory of tsunami risk and structures a disaster heritage tourism economy; coordinates visitor flow and memorial landscape preservation.
% TRANSFER_FUNCTION: Transfers land-use autonomy and coastal development potential from property developers and residents to heritage tourism operators and municipal governance; transfers moral authority from the original warning function to contemporary memorial management.
% ABSENT_VOICES: Coastal engineering firms proposing tsunami-resistant construction, younger residents seeking economic expansion through coastal development, and pro-growth municipal factions are excluded from heritage governance discourse.
% DISAPPEARANCE_RATIONALE: If the stone and its institutional husk vanished, coastal zoning would revert to conventional development pressure, heritage tourism would collapse, and the village's memorial economy would reorganize around other attractions or depopulate.
% FOUNDING_PROBLEM: Protecting coastal settlements from tsunami recurrence by preserving inter-generational spatial memory of inundation limits.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropologists and oral historians attest the directive was not actively transmitted as binding law during the inter-catastrophe period; municipal governance asserts continuity but corroboration from outside the benefiting parties supports the decay thesis.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the land-use restriction suppresses development without the original coordination function (tsunami warning) being live; the justification is a husk. Suppression (0.72) is high because persistence depends on active heritage zoning and exclusion of resistant developers. Theater ratio (0.72) is high because maintenance of the stone and its surrounding ritual increasingly serves performative memorial functions rather than actual behavioral governance. Accessibility collapse (0.60) is moderate: legal and cultural barriers limit alternatives but engineered coastal solutions are technically available. Resistance (0.45) is moderate because developers resist but local residents are identity-locked and the stone carries immense post-disaster moral prestige. The temporal series show monotonic drift from genuine warning toward extractive husk across the inter-catastrophe interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (municipal governance) experiences the constraint as legitimate heritage stewardship protecting a celebrated survival narrative. The payer seats (developers and identity-locked residents) experience the same structure as arbitrary land-use capture justified by a dead directive. The engine will compute divergent per-seat classifications from this structural asymmetry: the governance seat will show lower effective extraction because it is the beneficiary and administrator, while the developer seat will show near-full-target extraction because it is trapped by the regulatory scope and bears the direct costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Municipal governance and heritage tourism operators are structural beneficiaries: they collect prestige and revenue from the memorial economy and are positioned near the low-d (subsidy) end of the directionality spectrum. Coastal property developers are the primary targets: they face permit denials and cultural prohibition, with constrained exit and high d. Local residents sit near symmetric but drift toward target due to identity-locked exit â their fusion with the village's disaster-survival identity makes departure psychologically costly even when economic rationality would favor selling or developing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â inter-generational tsunami warning â is dead in this reading. The directive lost behavioral force during the inter-catastrophe period; the stone did not actively govern settlement patterns for 78 years. Yet the arrangement persists because it has been repurposed into a heritage-extraction mechanism. Without the R5 genealogy fields, this would risk misclassification as a mountain (timeless traditional wisdom) or rope (genuine coordination). Declaring founding_problem_status: dead and mandatrophy_resolved: true prevents this naturalization and forces classification to recognize the atrophied mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_behavioral_decay_verification,
    'Did the Aneyoshi stone directive actually lose behavioral force during the inter-catastrophe period, or was it maintained through informal social enforcement invisible to external observers?',
    'Archival land-use records, oral history interviews with pre-2011 residents, and ethnographic observation of actual settlement decision-making.',
    'If informal enforcement was continuous, the commemorative husk reading overstates the decay and extractiveness may be lower than authored; if true decay is confirmed, the behavioral competence reading is falsified and the husk reading''s epsilon is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(directive_behavioral_decay_verification, empirical, 'Empirical ambiguity about whether the directive maintained behavioral force or decayed into a husk.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the current land-use suppression driven by formal zoning and heritage law (structural), or by the stone''s internalized cultural weight that persists even where legal barriers are surmountable?',
    'Comparative analysis of permit denial rates against legal criteria versus community opposition testimony; post-exit trajectory of developers who leave the jurisdiction.',
    'If suppression is primarily internalized, effective extraction is higher than the structural measure suggests because the target carries the constraint after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism in a heritage-identity context.').

omega_variable(
    gain_capture_ambiguity,
    'Do the gains from the memorial economy accrue primarily to heritage tourism operators, or do they diffuse across the local economy without a concentrated capturer?',
    'Revenue tracing from tourism operators to municipal budgets and local households; input-output analysis of the memorial economy.',
    'If gains are captured by operators, the constraint is snare-flavored toward developers; if diffuse, the extraction is more akin to a public good framing and the tangled rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gain_capture_ambiguity, empirical, 'Whether memorial economy gains are captured or diffuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_husk_tr_t15, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(aneyoshi_husk_tr_t30, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(aneyoshi_husk_tr_t45, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(aneyoshi_husk_tr_t78, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 78, 0.72).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aneyoshi_husk_be_t15, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(aneyoshi_husk_be_t30, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(aneyoshi_husk_be_t45, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(aneyoshi_husk_be_t78, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 78, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(aneyoshi_husk_su_t15, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(aneyoshi_husk_su_t30, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(aneyoshi_husk_su_t45, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(aneyoshi_husk_su_t78, aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 78, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the commemorative_husk_reading of the aneyoshi_stone_directive kernel; the sibling behavioral_competence_reading instantiates the same kernel under an opposed empirical premise. The two readings share the same referent (the standing arrangement governing Aneyoshi land use) but author different epsilon values because they disagree about whether the coordination function is live.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
