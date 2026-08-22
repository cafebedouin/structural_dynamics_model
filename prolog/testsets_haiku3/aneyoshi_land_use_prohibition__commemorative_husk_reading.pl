% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_commemorative_husk, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Prohibition as Commemorative Husk (Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use
 *
 * SUMMARY:
 *   The Aneyoshi stone memorial in Iwate Prefecture, Japan, marks a boundary
 *   reached by past tsunami and was operationally enforced as a land-use
 *   prohibition for approximately 78 years following its erection. Over
 *   subsequent decades, the stone's function atrophied into ritual and
 *   historical commemoration as memory carriers aged, younger residents
 *   lacked experiential knowledge of the constraint, and development
 *   interests treated it as cultural heritage rather than operative rule.
 *   This reading instantiates the stone AS IT NOW OPERATES: a symbol whose
 *   prohibition has decayed to near-zero behavioral force, while development
 *   proceeds on lands the stone originally interdicted. Development interests
 *   benefit from this decay (they capture land value); future residents
 *   become victims when geological cycles restore the constraint's original
 *   force through disaster. The measurement series shows extractiveness
 *   rising as development intensity increases and theater ratio climbing as
 *   ritual maintenance substitutes for behavioral enforcement—the classic
 *   piton trajectory.
 *
 * KEY AGENTS:
 *   - aneyoshi_village_council: Maintains the stone's cultural and spiritual function but lacks enforcement capacity
 *   - development_interests: Benefit from treating prohibition as non-binding historical artifact
 *   - future_residents_below_line: Will bear disaster cost when tsunami returns, becoming victims retroactively
 *   - prefectural_government: Holds authority to formalize prohibition into code but does not exercise it
 *   - disaster_memory_practitioners: Excluded from development decisions despite expert knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.81).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.22).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.73).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.73).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Stone Prohibition as Commemorative Husk (Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '70cd294e-eb29-409f-9868-c5783b45aa0f').
narrative_ontology:cs_kernel_codification('70cd294e-eb29-409f-9868-c5783b45aa0f', fixed_text).
narrative_ontology:cs_authority_grounding('70cd294e-eb29-409f-9868-c5783b45aa0f', extraction).
narrative_ontology:cs_interpretation_layer_present('70cd294e-eb29-409f-9868-c5783b45aa0f').
narrative_ontology:cs_reading_relation('70cd294e-eb29-409f-9868-c5783b45aa0f', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('70cd294e-eb29-409f-9868-c5783b45aa0f', foundational, symbolic_maintenance_substitutes_operational_enforcement).
narrative_ontology:cs_axiom_status(symbolic_maintenance_substitutes_operational_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('70cd294e-eb29-409f-9868-c5783b45aa0f', symbolic_maintenance_substitutes_operational_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('70cd294e-eb29-409f-9868-c5783b45aa0f', foundational, development_interest_benefit_from_prohibition_decay).
narrative_ontology:cs_axiom_status(development_interest_benefit_from_prohibition_decay, holdable).
narrative_ontology:cs_axiom_grounding('70cd294e-eb29-409f-9868-c5783b45aa0f', development_interest_benefit_from_prohibition_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('70cd294e-eb29-409f-9868-c5783b45aa0f', operational_land_use_prohibition_by_stone_force).
narrative_ontology:cs_drift_state('70cd294e-eb29-409f-9868-c5783b45aa0f', contemporary_memory_atrophy_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70cd294e-eb29-409f-9868-c5783b45aa0f', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, land_speculators).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, memory_carriers).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, memory_carriers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the stone memorial as a historical artifact and spiritual practice. Continues verbal warning transmission and occasional ritual reenactment of the prohibition's original purpose. Administers what remains of the constraint but lacks legal enforcement capacity or modern authority to prevent building on the prohibited lands. The council preserves the memorial's cultural meaning while the constraint's behavioral force has atrophied.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_village_council, agenda_setter,
    organized, generational, constrained, local).

% Developers and land speculators benefit from the prohibition's decay into symbol. They can acquire and build on lands the stone once interdicted, capturing substantial value because the prohibition no longer functions as a behavioral constraint. They rely on the stone being treated as historical curiosity rather than operational rule, allowing profitable conversion of high-risk tsunami zones into residential or commercial development.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests, beneficiary,
    powerful, biographical, arbitrage, regional).

% Will occupy buildings constructed on the prohibited lands, unaware that the stone marked a tsunami boundary. When catastrophe returns—geological cycles suggest 100-150 year intervals—these residents will bear the cost of the prohibition's symbolic degradation. They become victims retroactively when the constraint's original function (land-use safety) reasserts itself through physical disaster.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, immediate, trapped, local).

% Holds legal authority over land-use policy but has not formalized the stone's prohibition into modern building codes. Government officials can observe the constraint's decay into memorial and could restore or formalize it, but do so only after disaster cycles demonstrate its necessity. This creates a perverse temporal incentive: the constraint is remembered only after it fails.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, prefectural_government, observer,
    institutional, generational, analytical, regional).

% Scholars, seismic experts, and disaster-risk specialists who could validate the stone's operational necessity and advocate for its formalization. They are structurally excluded from development decisions: their warnings about geological cycles and past tsunami reach are treated as expert opinion rather than operative constraint, while development interests have decision power.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_memory_practitioners, excluded,
    moderate, civilizational, constrained, regional).

% Elder residents and cultural practitioners who maintain the stone's narrative and ritual meaning. They benefit from the constraint's survival as cultural memory and spiritual practice; they also carry the psychological cost of watching the constraint degrade into symbol while knowing its original purpose will reassert itself catastrophically. Their identity is fused with the memory-keeping role.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, memory_carriers, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, memory_carriers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone coordinates intergenerational knowledge transmission: it marks a boundary that previous communities learned catastrophically, encoding disaster experience into landscape and ritual so future residents inherit the warning without requiring first-hand disaster learning.
% TRANSFER_FUNCTION: Moves the cost of disaster-avoidance compliance from development interests (who benefit from building on high-value tsunami-zone land) to future residents (who will bear the disaster cost when the constraint's original force—geology—reasserts itself). The transfer is obscured because the stone has degraded to symbol, making the actual transfer invisible until catastrophe reveals it.
% ABSENT_VOICES: Disaster-risk specialists and seismic researchers whose expertise would restore the constraint's operational force are excluded from development decisions. Residents who will inhabit the built structures (temporally future) have no voice in the present decision to treat the prohibition as memorial rather than rule.
% DISAPPEARANCE_RATIONALE: If the stone were destroyed or the prohibition forgotten entirely, the immediate world would rearrange: development would accelerate unimpeded on the prohibited lands, the village's cultural memory would fragment. When tsunami returns, the world will rearrange catastrophically—past residents will have optimized for short-term land value, future residents will suffer for that optimization. The constraint's 'disappearance' is already incomplete; it persists as symbol while losing behavioral force.
% FOUNDING_PROBLEM: A past tsunami reached a certain elevation and destroyed community. Survivors marked the boundary with a stone to warn descendants: 'Do not build below this line.' The constraint solved the problem of intergenerational knowledge transfer when written records were unreliable and disaster cycles spanned generations.
% FOUNDING_PROBLEM_CORROBORATION: Geological studies confirm the stone marks an accurate historical tsunami reach; seismic analysis confirms the cycle interval (~120 years) and predicts future events. The village's own oral history attests the stone was operationally enforced for 78 years (behavioral_competence_reading's duration claim). Disaster-risk specialists and seismic institutes corroborate that the founding problem—intergenerational transmission of catastrophic-risk knowledge—remains structurally live. The prefectural government's failure to formalize the prohibition into building code is attested by land records and development permits issued on prohibited lands post-memory-decay.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 at interval end) because development interests benefit from building on high-value tsunami-zone land they could not access if the prohibition operated behaviorally; future residents become the cost-bearers. Suppression is low (0.22) because no active force maintains the prohibition—it persists through ritual and cultural practice, not coercion. Theater ratio is very high (0.73) because enforcement activity has almost entirely converted to ceremonial maintenance: village ritual reenactments, cultural education, memorial upkeep. None of this ritual activity functions to prevent development; it preserves the memory's cultural authenticity while the constraint's behavioral force erodes. The measurement series on the shared time grid shows extractiveness rising monotonically as development pressure builds and memory carriers age, while suppression remains flat (no enforcement machinery to intensify or decay, only theater maintenance). This is the piton pattern: function atrophied, persistence maintained through performance, costs diffuse and future-indexed.
 *
 * PERSPECTIVAL GAP:
 *   The village council's seat experiences the constraint as meaningful cultural preservation and spiritual practice—high theater value, low extractiveness from their perspective, because the council does not profit from development and their role is memory-keeping. The development interests' seat experiences the constraint as a successfully neutralized impediment—high extractiveness, because they capture the land value. The future residents' seat is temporally displaced; they will experience the constraint retroactively, at catastrophe, as a victim-seat (high extraction, zero ability to prevent it). The engine computes these divergences from the structural data: power differential (developer powerful vs. villager organized vs. future resident powerless), exit options (developer arbitrage vs. council constrained vs. future resident trapped), and the time-horizon mismatch (developers are biographical, village council generational, future residents imminent-crisis temporal).
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests have d near 0.0 (full beneficiary): they profit from the prohibition's behavioral decay, face no coercion, exit freely to other projects. Village council has d near 0.5 (symmetric): they maintain the constraint's meaning culturally but also suffer its decay instrumentally—their identity is fused with memory-keeping, so exit is identity_locked constrained. Future residents have d near 1.0 (full target): they will bear the constraint's original force (geology) when it reasserts, with zero exit option (trapped) and immediate time horizon. Memory practitioners sit near d=0.6 (partly target): they carry the knowledge and the psychological cost of watching the constraint decay, but are excluded from decision power, so their expertise is suppressed rather than extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandate drift: it was founded to solve a live problem (intergenerational transmission of catastrophic-risk knowledge across 120-year disaster cycles). That founding problem REMAINS LIVE at the structural level—geological cycles continue, future tsunami is inevitable. However, the mandate's behavioral embodiment has atrophied: the prohibition no longer guides land-use decisions. The village council still performs the mandate's ritual content, maintaining cultural memory and spiritual authenticity. But mandate performance has divorced from mandate function. The founding problem is dead AS A BEHAVIORAL DRIVER (nobody alters land-use decisions based on the stone anymore) while remaining live AS A STRUCTURAL REALITY (the geological hazard is still there). This is the piton's core dynamic: the mandatrophy is RESOLVED in the sense that we can name exactly what happened (memory carriers aged, development interests gained power to redefine the constraint as cultural rather than operative, ritual substituted for enforcement). The resolution mechanism is temporal and identity-linked: memory carriers' age and mortality, plus identity-fusion of younger residents with modernity/development rather than with the constraint's traditional meaning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_boundary,
    'At what point does a constraint''s persistent ritual and cultural maintenance cease to constitute behavioral enforcement and become pure theater?',
    'Compare actual land-use decisions: do any prospective developers consult the stone? Do any prefecture decisions reference it operationally? Do any building permits cite it as a constraint? Historical record of development permits and developer interviews establish whether the stone still functions as a decision-influencing rule or only as cultural reference.',
    'If the stone still influences ANY development decisions operationally, extractiveness drops and the constraint reclassifies from piton toward tangled_rope or snare (coordination function + asymmetric extraction). If it influences zero decisions, this reading''s piton classification holds and extractiveness remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_boundary, empirical, 'Whether the stone''s ritual maintenance constitutes operative constraint or pure cultural preservation.').

omega_variable(
    temporal_indexing_of_victims,
    'Does the constraint''s victim-status retroactively assign to residents who will occupy prohibited lands at the time of future disaster, or does victimhood accrue only when they are identifiable present agents?',
    'Philosophical/normative question about how to classify targets of a constraint whose harm is future and conditional on a physical event. Disaster anthropology and temporally-indexed victimhood literatures address this. No data resolution; framing choice.',
    'If future residents are considered victims NOW (this reading''s framing), extractiveness and harm asymmetry are higher. If they are not yet victims until catastrophe strikes (sibling framing), extractiveness scores differently. This is not empirical dispute but structural classification of temporal scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_indexing_of_victims, conceptual, 'Whether victims of a constraint include future residents whose harm is conditional and delayed.').

omega_variable(
    memory_decay_vs_active_forgetting,
    'Is the constraint''s behavioral atrophy a passive decay of memory as elder carriers age, or an active process of reframing by development interests that benefits from forgetting?',
    'Historical analysis of development decisions, prefectural policy evolution, media framing, and testimonies from agents on both sides. Did development interests actively suppress knowledge of the constraint, or did they simply exploit the fact that memory naturally decayed?',
    'If decay is passive, the constraint is a piton—inertial, atrophied, maintained theatrically. If reframing is active by development interests, the constraint may reclassify toward snare (active suppression and alternative-destruction by a beneficiary party to maintain their extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_decay_vs_active_forgetting, empirical, 'Whether the constraint''s behavioral decay is passive memory loss or active strategic forgetting.').

omega_variable(
    symbol_as_insufficient_coordination,
    'Does the stone''s function as cultural symbol and memory-carrier constitute a reduced but still-valid coordination mechanism for intergenerational disaster-risk knowledge? Or has symbolism entirely replaced function?',
    'Test: when next disaster strikes, will the stone''s symbolic presence (via news coverage, cultural memory suddenly reactivated) provide any protective benefit to residents? Or will the physical disaster proceed indifferent to the symbol''s prior meaning?',
    'If symbolic coordination retains residual protective value (e.g., cultural memory practices do reduce risk behavior partially), the constraint retains some coordination function and may not be pure piton. If symbol provides zero protection when disaster strikes, piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbol_as_insufficient_coordination, preference, 'Whether cultural-symbolic maintenance of a constraint constitutes residual coordination or is purely inertial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aneyoshi_husk_tr_t5, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(aneyoshi_husk_tr_t10, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(aneyoshi_husk_tr_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(aneyoshi_husk_tr_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement(aneyoshi_husk_tr_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 40, 0.73).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(aneyoshi_husk_be_t5, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(aneyoshi_husk_be_t10, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(aneyoshi_husk_be_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(aneyoshi_husk_be_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(aneyoshi_husk_be_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(aneyoshi_husk_su_t5, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(aneyoshi_husk_su_t10, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(aneyoshi_husk_su_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(aneyoshi_husk_su_t25, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 25, 0.21).
narrative_ontology:measurement(aneyoshi_husk_su_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint and behavioral_competence_reading are two instantiations of the same kernel (the Aneyoshi stone). They differ in their assignment of temporal scope: the behavioral_competence_reading treats the prohibition as persistently operational across 78 years and beyond (ε ~0.45); this reading treats it as decayed to symbol with high extractiveness (ε 0.81). The readings coexist as different parties' framings. The sibling reading influences this one: if behavioral competence were restored (e.g., by prefectural formalization of the prohibition into building code), extractiveness would drop substantially and this constraint would no longer be the primary description of the stone's current operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
