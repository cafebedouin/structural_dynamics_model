% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone: Commemorative Husk Reading
 *   domain: disaster_anthropology/institutional_decay
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone (標高 23.5m / elevation marker, carved circa
 *   1611 in Iwate Prefecture, Japan) carries an inscription directing
 *   settlement above its line: 'Do not build below this stone; a tsunami will
 *   come.' For 78 years (1933–2011), residents rebuilt after tsunamis and
 *   maintained the settlement above the stone's mark. This reading frames the
 *   stone's role as COMMEMORATIVE HUSK — a symbol of ancestral wisdom that
 *   retains no operational force in modern land-use decisions. Residents
 *   rebuilt post-2011 regardless of the stone's directive; survival is
 *   attributed to topography and modern hazard warnings, not to adherence to
 *   a 400-year-old stone constraint. The constraint is claimed as PITON: an
 *   atrophied institutional arrangement sustained by memorial operations and
 *   symbolic capital, no longer constraining behavior but maintained
 *   theatrically as a museum piece and cultural identity marker.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents_2011_onward: Treat the stone as historical artifact; make land-use decisions independently
 *   - memorial_operators: Maintain and narrate the stone for tourism; benefit from its symbolic status
 *   - tourism_sector: Extract economic value from stone as heritage attraction
 *   - historical_narrative_custodians: Validate tradition-based resilience narratives through the stone's prestige
 *   - disaster_response_planners: Excluded from treating stone as operative planning tool
 *   - aneyoshi_residents_pre_2011: Historical agents whose compliance is reframed as cultural norm, not structural constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Tsunami Stone: Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/institutional_decay").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '9a1906a5-f8dd-461d-9b55-12bf04e3121c').
narrative_ontology:cs_kernel_codification('9a1906a5-f8dd-461d-9b55-12bf04e3121c', fixed_text).
narrative_ontology:cs_authority_grounding('9a1906a5-f8dd-461d-9b55-12bf04e3121c', lineage).
narrative_ontology:cs_interpretation_layer_present('9a1906a5-f8dd-461d-9b55-12bf04e3121c').
narrative_ontology:cs_reading_relation('9a1906a5-f8dd-461d-9b55-12bf04e3121c', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('9a1906a5-f8dd-461d-9b55-12bf04e3121c', foundational, stone_function_memorialized_not_operant).
narrative_ontology:cs_axiom_status(stone_function_memorialized_not_operant, holdable).
narrative_ontology:cs_axiom_grounding('9a1906a5-f8dd-461d-9b55-12bf04e3121c', stone_function_memorialized_not_operant, empirically_contingent).
narrative_ontology:cs_axiom('9a1906a5-f8dd-461d-9b55-12bf04e3121c', secondary, modern_hazard_systems_supersede_traditional_directive).
narrative_ontology:cs_axiom_status(modern_hazard_systems_supersede_traditional_directive, holdable).
narrative_ontology:cs_axiom_grounding('9a1906a5-f8dd-461d-9b55-12bf04e3121c', modern_hazard_systems_supersede_traditional_directive, empirically_contingent).
narrative_ontology:cs_reference_frame('9a1906a5-f8dd-461d-9b55-12bf04e3121c', stone_as_living_constraint).
narrative_ontology:cs_drift_state('9a1906a5-f8dd-461d-9b55-12bf04e3121c', contemporary_memorial_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9a1906a5-f8dd-461d-9b55-12bf04e3121c', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_operators).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_sector).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, historical_identity_maintenance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, historical_narrative_custodians).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, institutional_memory_as_performance).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, monument_as_symbolic_residue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the same hamlet where the stone stands. In this reading, they treat the stone as a memorial to ancestor warnings rather than an operational constraint on their own building and land-use decisions. They have rebuilt, farmed, and settled within meters of the stone since 2011 without deferring to its directive. Their actual behavior in rebuilding and resettlement treats the stone as historical artifact, not as a binding rule.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_residents_2011_onward, observer,
    powerless, generational, constrained, local).

% Maintain the stone's site, provide interpretive signage, conduct tours, and narrate its history to visitors. They benefit from the stone's continued symbolic status — it draws tourists, supports museum operations, and validates the historical narrative of ancestral wisdom. They manage the constraint's theatrical performance: the stone is cared for and presented precisely because it is no longer operative.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_operators, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_operators, beneficiary).

% Benefits from the stone as a cultural heritage attraction. Tours, media coverage, and educational visits to the site generate economic activity and place-based marketing. The stone's value is strictly as a memorable artifact and historical curiosity, not as a constraint that shapes behavior or land use.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, tourism_sector, beneficiary,
    organized, biographical, mobile, regional).

% Academics, documentary makers, and cultural heritage institutions that use the stone as evidence of indigenous disaster-response wisdom and long-term ancestral memory. They benefit from the stone's continued symbolic prestige and its role in narratives of tradition-based resilience, regardless of whether the constraint operationally influences modern behavior.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, historical_narrative_custodians, beneficiary,
    moderate, generational, constrained, national).

% Would use the stone as an operational rule for land-use planning and building-restriction enforcement if they treated it as a live constraint. In this reading, they are kept out of the authority structure: the stone is framed as a museum piece and symbol rather than as a planning tool, so planners are excluded from consulting it as a decision rule for coastal development policy.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_response_planners, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_operators).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone historically coordinated a shared understanding of tsunami risk and built an intergenerational commitment to maintain a safe settlement perimeter. In this reading, that coordination function has decayed; the stone now functions as a symbol of ancestral wisdom rather than as an operative rule coordinating modern land-use decisions.
% TRANSFER_FUNCTION: Moves symbolic capital and tourist revenue TO memorial operators and heritage institutions FROM the constraint itself (reframed as artifact). Also transfers narrative authority: the stone validates historical memory claims and place-based identity, which benefits cultural custodians. No behavioral cost is transferred in this reading because the constraint is not operant.
% ABSENT_VOICES: Disaster-response planners and modern coastal-hazard scientists are structurally excluded: the stone is kept in the memorial/museum frame rather than elevated to a planning tool, so expert voices that might advocate for reinstating it as an operational land-use rule are not present in the decision structure.
% DISAPPEARANCE_RATIONALE: If the stone disappeared tomorrow, land-use and settlement patterns in Aneyoshi would not rearrange — in this reading, the stone is already decorative and memorial, not operant. Its removal would diminish tourism and historical narrative capital but would not alter the behavioral constraints on residents.
% FOUNDING_PROBLEM: After the 1611 tsunami, survivors carved a stone directive to mark the safe perimeter and communicate to future generations: 'Do not build below this stone; a tsunami will come.' The founding problem was intergenerational communication of disaster risk across centuries when written records might be lost.
% FOUNDING_PROBLEM_CORROBORATION: Modern residents, tourism operators, and scholarly analyses of post-2011 rebuilding patterns attest that land-use decisions were made independently of the stone's directive. The 2011 survival is attributed in published accounts primarily to topographic luck and the 2004 Indian Ocean tsunami memory-network effect, not to the stone. Academic historians and archaeologists studying oral tradition and monument decay support the reading that the stone's operant function has been superseded by modern building codes and hazard mapping, leaving only the symbolic function intact.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).
:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.12 at present) in this reading because no agent is systematically coerced or deprived by the stone's directive — residents ignore it, planners don't consult it, and the constraint produces no behavioral asymmetry. Suppression is MINIMAL (0.08) because the stone exercises no active coercive force; alternatives are not suppressed because the stone no longer competes with modern hazard governance. Theater is VERY HIGH (0.78) — the constraint's entire present function is symbolic and performative: interpretive panels, tours, media narratives, and academic study keep the stone alive as a monument while its original land-use directive has been superseded. The measurement series descends from the stone's founding (projected high extractiveness, low theater) through the post-1933 era of normalization and forgetting (rising theater, declining extractiveness) to the present (minimal extractiveness, maximum theater). This is the signature piton trajectory: atrophied function, maintained by performance.
 *
 * PERSPECTIVAL GAP:
 *   From the memorial operators' and tourism sector's seats, the stone is a cherished artifact that anchors identity and generates revenue — its value is precisely its symbolic, not operational, force. From aneyoshi residents' seats (both historical and post-2011), the stone is a background historical feature; their land-use decisions are constrained by modern building codes, hazard maps, and insurance regulations, not by the stone. From disaster planners' seats, the stone SHOULD be operative — it encodes multigenerational risk knowledge — but is excluded from the authority structure by the memorial-artifact framing. The engine will compute these seats differently: operators sit near beneficiary (low d), residents sit near symmetric (moderate d, neither paying nor benefiting from constraint operation), planners sit near observer (analytical d). This divergence IS the reading's point: the stone has become a thing of symbolic value rather than structural constraint, so seats experience it differently.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, no seat bears the cost of constraint adherence (extractiveness is minimal), so no seat has a target/payer relationship. Memorial operators and tourism benefit from the constraint's symbolic prestige (low d, beneficiary effect). Residents experience the stone as background history, not as a behavioral constraint they pay for or benefit from (moderate d, near symmetric). Planners are excluded from the structure entirely (their d is not computed in the operative constraint, only in the counterfactual). The constraint's directionality pattern reflects its atrophied state: it extracts almost nothing, so it has no targets, only symbolic beneficiaries and neutral observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a textbook MANDATROPHY case: the founding problem (intergenerational communication of tsunami risk) is DEAD — modern hazard science, building codes, and real-time warning systems have displaced the need for a 400-year-old stone to carry that knowledge. Yet the constraint persists, maintained by memorial operations and symbolic capital. The theater_ratio trajectory (0.05 → 0.78) is the diagnostic signature: as the original mandate died, theatrical maintenance rose to keep the constraint alive for cultural and economic reasons. This classification prevents the misreading that would label it ROPE (genuine coordination) or falsely classify the stone as still OPERANT — the piton category correctly names the decay and the performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_causality_ambiguity,
    'Is the 2011 survival of Aneyoshi attributable to residents'' adherence to the stone''s settlement-height directive, or to other factors (topographic elevation, chance, modern hazard warnings, tsunami characteristics)?',
    'Comparative analysis of settlement patterns in other Japanese coastal communities post-2011: communities without directional stones but with similar topography; communities with stones but below-stone settlement. If survival correlates with topography/warning-network rather than stone-adherence, the stone''s causal role is diminished.',
    'If survival is independent of stone-adherence, the piton reading is supported — the stone is memorial residue, not operant constraint. If survival is causally attributable to stone-guided settlement pattern, the behavioral-competence reading gains support and the stone may not be fully atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_causality_ambiguity, empirical, 'Whether the 2011 survival validates or contradicts the stone''s operational force').

omega_variable(
    institutional_succession_frame,
    'Is the stone''s decay a genuine institutional failure (the constraint became inoperant), or a FUNCTIONAL REPLACEMENT (modern building codes and hazard science simply took over the stone''s role)?',
    'Archaeological and institutional-history review: Did residents consciously abandon the stone as building norms modernized, or did the stone gradually become background noise as new systems overwrote it? Are there documented debates about the stone''s validity during the 20th century?',
    'If replacement: the piton reading is correct; the constraint''s mandate is dead but the symbol persists. If gradual erasure: institutional succession is normal, not decay. The theater_ratio remains high regardless, but the causal story differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_succession_frame, conceptual, 'Whether the stone''s atrophy is decay or normal institutional succession').

omega_variable(
    symbolic_capacity_as_extraction,
    'Does the memorial-operators'' extraction of symbolic capital (tourism revenue, heritage prestige, identity validation) constitute REAL EXTRACTION under this reading, or is symbolic benefit so diffuse that it should not be counted as extraction at all?',
    'Audit of memorial-site revenue flows: Do operators capture quantifiable economic benefit? Do residents experience the tourism infrastructure as a cost (congestion, land-use constraints, cultural commodification) or as neutral/positive? If residents bear costs and operators capture benefits, extractiveness should be higher.',
    'If symbolic value is real extraction, extractiveness should be higher (0.20–0.35 rather than 0.12), shifting the piton diagnosis. If it is truly diffuse and unextracted, the low extractiveness (0.12) stands, confirming the husk reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_capacity_as_extraction, preference, 'Whether symbolic-capital capture counts as constraint-mediated extraction').

omega_variable(
    kernel_disambiguation,
    'Is this constraint one reading of a contested kernel (aneyoshi_stone_commitment), or are the husk and competence readings two separate constraints with different ε values?',
    'The ε-invariance test (DP-001): If the two readings measure the SAME referent (the stone''s directive and its role in settlement decisions) and produce starkly different ε values (0.12 for husk, ~0.60+ for competence), they are one kernel read two ways. If they measure different referents (one the stone''s ceremonial function, one its historical land-use function), they are separate constraints and should be authored as separate files linked by network.affects_constraints.',
    'If kernel: both readings are live interpretations of a single persisting commitment whose legitimacy is contested. If separate constraints: each has its own ε-invariance and can be classified independently. The committer frame (Rules 1–4) applies only if kernel holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disambiguation, conceptual, 'Whether husk and competence readings are one contested kernel or two separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 1611, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1611, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1611, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1611, projected).
narrative_ontology:measurement(aney_tr_t1800, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1800, projected).
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1933, 0.35).
narrative_ontology:measurement_basis(aney_tr_t1933, projected).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 1980, 0.58).
narrative_ontology:measurement_basis(aney_tr_t1980, projected).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.72).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).
narrative_ontology:measurement(aney_tr_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 2024, 0.78).
narrative_ontology:measurement_basis(aney_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1611, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1611, 0.65).
narrative_ontology:measurement_basis(aney_be_t1611, projected).
narrative_ontology:measurement(aney_be_t1800, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement_basis(aney_be_t1800, projected).
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1933, 0.28).
narrative_ontology:measurement_basis(aney_be_t1933, projected).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement_basis(aney_be_t1980, projected).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.12).
narrative_ontology:measurement_basis(aney_be_t2011, observed).
narrative_ontology:measurement(aney_be_t2024, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 2024, 0.12).
narrative_ontology:measurement_basis(aney_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the aneyoshi_stone_commitment kernel family. The sibling reading (behavioral_competence_reading) interprets the stone as a live operational constraint on land-use that retained force across 78 years. This reading interprets it as a commemorative husk — symbolic prestige without behavioral constraint. Both readings reference the same kernel (the stone and its directive) but produce different ε values and classifications. Link via network.affects_constraints to enable contamination analysis: if one reading's empirical basis is challenged (e.g., survival causality is reattributed), the sibling reading's validity shifts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
