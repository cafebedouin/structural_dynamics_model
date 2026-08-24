% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Tsunami Warning Stone as Commemorative Husk
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   Stone markers (tsunami stones) erected after historical tsunamis in
 *   northeastern Japan originally functioned as live land-use prohibitions:
 *   'Do not build below this stone.' Over decades of high-growth development,
 *   compliance decayed. The stones were not removed; instead, they were
 *   reclassified as cultural heritage. Annual commemoration ceremonies,
 *   physical preservation, and tourism narratives now surround them. Building
 *   decisions proceed independently of stone locations — waterfront
 *   convenience and land value drive development. The commemorative husk
 *   provides moral cover for hazardous development while the actual
 *   regulatory function has evaporated. This reading instantiates the
 *   'commemorative_husk' interpretation of the stone_land_use_rule kernel:
 *   the constraint persists materially but extracts via symbolic
 *   substitution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.75).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.4).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.75).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, tangled_rope).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami Warning Stone as Commemorative Husk").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(stone_land_use_rule__commemorative_husk).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '615fa8e3-1b7f-46c7-94a6-7c69e4b082c6').
narrative_ontology:cs_kernel_codification('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', fixed_text).
narrative_ontology:cs_authority_grounding('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', lineage).
narrative_ontology:cs_interpretation_layer_present('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6').
narrative_ontology:cs_reading_relation('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', foundational, commemorative_function_supersedes_regulatory_function).
narrative_ontology:cs_axiom_status(commemorative_function_supersedes_regulatory_function, holdable).
narrative_ontology:cs_axiom_grounding('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', commemorative_function_supersedes_regulatory_function, conventional).
narrative_ontology:cs_axiom('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', secondary, waterfront_development_legitimated_by_commemoration).
narrative_ontology:cs_axiom_status(waterfront_development_legitimated_by_commemoration, holdable).
narrative_ontology:cs_axiom_grounding('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', waterfront_development_legitimated_by_commemoration, empirically_contingent).
narrative_ontology:cs_reference_frame('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', ancestral_hazard_lineage).
narrative_ontology:cs_drift_state('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', post_high_growth_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('615fa8e3-1b7f-46c7-94a6-7c69e4b082c6', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_governments).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, tourism_interests).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_tsunami_victims).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, coastal_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, disaster_insurance_pools).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, commemorative_adequacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build residential and commercial projects in hazardous zones below the stone line. The stone's commemorative status provides moral and marketing cover — 'we honor the past' — while actual hazard regulation is absent. Profits from waterfront premium locations; risk externalized to future occupants and insurers. Can shift investment to other regions if local politics shift.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Collect expanded property tax base from waterfront development. Maintain stones as designated heritage sites, funding physical upkeep and annual ceremonies. Avoid regulatory liability by classifying stones as commemorative rather than regulatory — no enforceable building prohibition exists. Bound by electoral cycles and prefectural oversight; cannot easily abandon waterfront revenue.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_governments, beneficiary,
    institutional, generational, constrained, regional).

% Market stones as cultural heritage destinations; operate guided tours, museums, and souvenir commerce around them. Benefit from waterfront amenities (restaurants, hotels, promenades) that draw visitors to stone sites. Can relocate marketing focus to other heritage assets if stone narratives shift.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, tourism_interests, beneficiary,
    organized, biographical, mobile, regional).

% Do not yet exist — future residents, workers, and visitors in developed hazardous zones. Bear mortality, injury, and displacement risk when tsunami exceeds commemorative memory. No voice in current land-use decisions; no organizational form to advocate. The stone's commemorative framing renders their risk invisible in present discourse.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_tsunami_victims, payer,
    powerless, generational, trapped, local).

% Live in homes below the stone line, many inherited or purchased before development pressure intensified. May believe the stone's presence signals safety or that commemorative ceremonies confer protection. Bear evacuation costs, property loss, and community fragmentation when disaster strikes. Exit constrained by property ties, aging population, and lack of affordable inland alternatives.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_residents, payer,
    moderate, biographical, constrained, local).

% Underwrite earthquake and tsunami risk for properties in hazardous zones. Premium models inadequately price the 'commemorative gap' — the divergence between stone-line awareness and actual building patterns. Bear concentrated financial losses when events exceed modeled probability. Cannot withdraw from mandated coverage zones without regulatory approval.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_insurance_pools, payer,
    institutional, generational, constrained, national).

% Maintain stones physically (cleaning, erosion repair, signage). Organize annual commemoration ceremonies attended by officials and media. Control the public narrative: stones are heritage, not hazard regulation. Members' identities are fused with the preservation mission — 'we are the ones who remember.' Exit would mean abandoning self-concept as guardians of ancestral memory.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, stone_preservation_societies, agenda_setter,
    organized, generational, identity_locked, local).

% Document the functional decay of stone-mediated risk communication across decades. Map the gap between commemorative form and regulatory void. Publish comparative analyses with other disaster cultures. No institutional stake in Japanese land-use outcomes; analytical seat only.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Memorializes tsunami history and community trauma; provides shared reference point for collective mourning and identity; maintains cultural heritage tourism and intergenerational storytelling about disaster.
% TRANSFER_FUNCTION: Moves risk from waterfront developers (who profit from hazardous building) to future tsunami victims and insurance pools; moves regulatory legitimacy from enforceable land-use rules to symbolic commemoration; moves development rights from constrained (by hazard) to unconstrained (by commemoration).
% ABSENT_VOICES: Future tsunami victims (do not yet exist); displaced former residents of hazardous zones (often relocated inland without return rights); geological hazard scientists whose risk models are excluded from planning deliberations; inland municipalities that receive displaced populations but have no say in coastal zoning.
% DISAPPEARANCE_RATIONALE: If the stones vanished overnight, the commemorative cover legitimating waterfront development would disappear. Communities would confront the raw reality of unregulated hazardous building without the moral buffer of 'we remember.' Pressure for actual land-use constraints, managed retreat, or explicit risk acceptance would increase. Development patterns would likely shift as the symbolic license evaporates.
% FOUNDING_PROBLEM: After historical tsunamis (notably 1896, 1933, 1960), communities needed to mark hazard lines and transmit intergenerational knowledge of safe building elevations in a pre-scientific, pre-zoning era.
% FOUNDING_PROBLEM_CORROBORATION: Geological survey records confirm tsunami recurrence intervals of 100-150 years; historical land-use records show compliance with stone lines decayed after 1960s high-growth development; municipal planning documents exclude stone lines from modern zoning maps; no independent body attests stones still regulate building — the Japan Society of Civil Engineers treats them as heritage, not hazard infrastructure.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the commemorative framework enables risk externalization: developers capture waterfront premium value while future victims and insurers bear the tail risk. Theater ratio is high (0.7) because physical maintenance and ceremonial performance of the stones are real activities, but they serve commemorative identity, not behavioral constraint — the ratio of performative-to-functional activity is large. Suppression is moderate (0.4): the constraint does not actively forbid alternatives (safe building sites exist), but the commemorative narrative suppresses public discourse about the regulatory void. Accessibility collapse is low (0.3) because alternatives (inland building, engineered defenses) remain physically and legally accessible — they are simply not chosen. Resistance is low (0.2) because few organized forces challenge the commemorative framing; the stones are beloved as heritage.
 *
 * PERSPECTIVAL GAP:
 *   From the preservation society's seat, the constraint is genuine coordination (identity_coordination): they maintain ancestral memory and community cohesion. From the future victim's seat, it is pure extraction (snare): a symbolic gesture that legitimates their endangerment. From the developer's seat, it is a convenient coordination device (rope-adjacent): a heritage marker that smooths permitting and marketing. The engine computes this per-seat divergence from the structural data — the claimed type (tangled_rope) captures the hybrid reality that no single seat experiences wholly.
 *
 * DIRECTIONALITY LOGIC:
 *   Waterfront developers, municipal governments, and tourism interests are structural beneficiaries (d near 0.0): they collect rents, revenue, and visitors from the commemorative-development complex. Future tsunami victims, coastal residents, and insurance pools are structural targets (d near 1.0): they bear the extracted risk with no voice and constrained exit. Stone preservation societies are agenda-setters with identity-locked exit: they administer the constraint's performative maintenance and cannot exit without abandoning their self-concept. Disaster anthropologists are analytical observers (d=0.5, analytical exit). The engine derives directionality from these structural positions; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational hazard transmission in a pre-scientific era) is dead — modern geology, zoning, and early warning systems solve it better. Yet the arrangement persists because the commemorative function coordinates community identity and tourism, while the regulatory vacuum extracts value for developers. The mandatrophy is resolved in the sense that the original mandate is acknowledged as obsolete, but the constraint is not retired — it is repurposed. This is not a scaffold (no sunset clause, no transition plan) but a tangled rope: the coordination function (commemoration) is real, and the extraction (risk externalization via commemorative cover) is asymmetric and actively maintained through narrative control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the stone_land_use_rule kernel a single constraint with contested readings, or are the behavioral_competence and commemorative_husk readings structurally distinct constraints that should be modeled separately?',
    'Test ε-invariance: if measuring the stone''s constraint function via building compliance rates yields ε≈0 (commemorative_husk) but measuring via evacuation behavior during drills yields ε>0.5 (behavioral_competence), the label ''stone_land_use_rule'' covers two constraints. Decompose per DP-001.',
    'If two constraints, the commemorative_husk reading''s high extractiveness and tangled_rope classification stand on their own; the behavioral_competence reading would be a separate mountain/rope story. If one constraint, the ε value must be reading-indexed (per OQ-26) and the classification becomes observer-relative — a result the framework rejects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label conflates structurally distinct constraints per the ε-invariance principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.4) structural — the commemorative narrative actively blocks regulatory revival — or internalized — residents and officials genuinely believe commemoration substitutes for protection?',
    'Post-commemoration survey: if suppression persists after explicit risk communication (e.g., hazard maps distributed, stone''s non-regulatory status legally clarified), reclassify as partially internalized. Track whether communities that lose stones (erosion, development) show different risk perception than those retaining them.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression (false security) with them. This would increase χ for coastal_residents and future_tsunami_victims seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the commemorative framing.').

omega_variable(
    commemorative_extraction_boundary,
    'Is the commemorative function genuine coordination (identity_coordination) that happens to enable extraction, or is commemoration itself the extraction mechanism — a performative cover constructed to legitimize hazardous development?',
    'Compare stone sites with active preservation societies vs. abandoned stones in similar hazard zones. If development intensity correlates with preservation activity (not just stone presence), commemoration is an active extraction enabler. If development proceeds regardless of commemoration, the extraction is independent and commemoration is a genuine but exploited coordination function.',
    'If commemoration is the extraction mechanism, the constraint is a snare (coordination story is cover). If commemoration is genuine coordination exploited by developers, it remains tangled_rope. This determines whether the coordination function deserves Boltzmann floor credit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_extraction_boundary, conceptual, 'Whether the commemorative function is genuine coordination or constructed cover for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.2).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.35).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.5).
narrative_ontology:measurement(ston_tr_t80, stone_land_use_rule__commemorative_husk, theater_ratio, 80, 0.6).
narrative_ontology:measurement(ston_tr_t100, stone_land_use_rule__commemorative_husk, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(ston_be_t80, stone_land_use_rule__commemorative_husk, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(ston_be_t100, stone_land_use_rule__commemorative_husk, base_extractiveness, 100, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__commemorative_husk, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(ston_su_t80, stone_land_use_rule__commemorative_husk, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(ston_su_t100, stone_land_use_rule__commemorative_husk, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.08).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, coastal_zoning_law).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, tsunami_early_warning_system).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, disaster_insurance_framework).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, managed_retreat_policy).

% DUAL FORMULATION NOTE:
% This constraint and the behavioral_competence reading form a constraint family decomposing the stone_land_use_rule kernel. The commemorative_husk reading has high ε (0.75) because the commemorative cover enables extraction; the behavioral_competence reading would have low ε if the stone actually constrains building. They share the same physical referent but different causal efficacies — the ε-invariance principle demands separate stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
