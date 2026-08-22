% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria Declaratory Reading: Objective Statehood Establishment
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention (1933) articulated four objective criteria for
 *   statehood: defined territory, permanent population, government, and
 *   capacity to conduct foreign relations. The declaratory reading interprets
 *   these criteria as constitutive of statehood as a legal fact — if met,
 *   statehood exists independent of recognition by other states. This reading
 *   directly contradicts the constitutive reading, which holds that statehood
 *   requires recognition by the international community. Under the
 *   declaratory reading, de facto authorities that satisfy objective criteria
 *   become structural beneficiaries (they acquire legal statehood), while
 *   established states with territorial claims and parent states seeking to
 *   prevent secession become payers (they lose the veto authority recognition
 *   provides). The constraint is presented as self-executing international
 *   law; it operates through mandatory enforcement by the international legal
 *   community against great-power political pressure to deny recognition
 *   despite objective qualification.
 *
 * KEY AGENTS:
 *   - de_facto_authorities (emerging political entities meeting objective criteria) — beneficiary, moderate power, identity-locked exit (commitment to territorial independence)
 *   - established_states_with_territorial_claims (states disputing boundary or legitimacy) — payer, powerful, constrained exit (cannot unilaterally opt out)
 *   - parent_states_losing_conditional_leverage (states from which secessionists seek independence) — payer and agenda-setter, institutional power, constrained exit
 *   - international_legal_community (jurists, courts, scholars) — agenda_setter maintaining the declaratory doctrine, institutional power
 *   - great_powers_exercising_recognition_veto (Security Council members, regional hegemons) — excluded from decision structure, powerful, trapped exit (their refusal to recognize cannot legally prevent statehood)
 *   - occupied_or_colonized_populations (peoples under foreign occupation) — beneficiary, powerless, trapped exit (occupation is structural constraint)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria Declaratory Reading: Objective Statehood Establishment").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf').
narrative_ontology:cs_kernel_codification('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', formalized).
narrative_ontology:cs_authority_grounding('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', lineage).
narrative_ontology:cs_interpretation_layer_present('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf').
narrative_ontology:cs_reading_relation('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', foundational, criteria_sufficiency_doctrine).
narrative_ontology:cs_axiom_status(criteria_sufficiency_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', criteria_sufficiency_doctrine, deontological).
narrative_ontology:cs_axiom('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', secondary, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', recognition_is_declaratory_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', objective_criteria_statehood_doctrine).
narrative_ontology:cs_drift_state('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', contemporary_recognition_denial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d7167ad-bfec-47e6-9d34-7fedb6d8e9bf', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, emerging_political_entities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, established_states_with_territorial_claims).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states_losing_conditional_leverage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, occupied_or_colonized_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political entities claiming independence that meet the four objective criteria (defined territory, permanent population, government capacity, capacity to conduct foreign relations). Under the declaratory reading, they acquire statehood as a legal fact upon meeting these criteria, independent of whether existing states recognize them. This reading shields them from recognition denial by powerful neighbors or former parent states that might use recognition as conditional leverage.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    moderate, generational, identity_locked, regional).

% Incumbent states that dispute territorial boundaries or the legitimacy of secessionist entities. The declaratory reading denies them the veto that constitutive readings provide — they cannot refuse recognition to block statehood. They bear the cost of a legal framework that treats territorial claims as settled by objective criteria rather than by their acceptance.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, established_states_with_territorial_claims, payer,
    powerful, generational, constrained, global).

% States from which separatist movements seek independence. The declaratory reading strips them of the leverage that conditional recognition provides — they cannot negotiate a secessionist entity's compliance with their demands by withholding recognition. The constraint works directly against their structural ability to condition state emergence on political concessions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_losing_conditional_leverage, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, parent_states_losing_conditional_leverage, agenda_setter).

% Jurists, international courts, and legal scholars who interpret and apply statehood doctrine. They maintain the declaratory reading through legal reasoning, court decisions, and scholarly consensus. They bear the burden of enforcing objective criteria against political pressure from powerful states demanding discretionary recognition authority.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_community, agenda_setter,
    institutional, generational, analytical, global).

% Permanent Security Council members and regional hegemons that historically wielded recognition as a foreign-policy tool. The declaratory reading excludes them from the decision structure — their refusal to recognize cannot legally prevent statehood if objective criteria are met. They would advocate for a constitutive reading that preserves their veto power but are structurally barred from the conversation by the declaratory framework itself.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, great_powers_exercising_recognition_veto, excluded,
    powerful, generational, trapped, global).

% Peoples under foreign military occupation or colonial administration seeking independence. The declaratory reading enables them to claim statehood through objective criteria without needing the occupying power's permission. Their benefit is structured: the constraint transfers the legitimacy question from discretionary recognition to objective, measurable conditions they can work to satisfy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, occupied_or_colonized_populations, beneficiary,
    powerless, generational, trapped, regional).

% United Nations bodies, regional organizations, and neutral arbitral tribunals that assess statehood claims. They witness and document whether entities meet the four criteria and whether the declaratory reading's enforcement holds against great-power political pressure to deny recognition despite objective qualification.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_institutional_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, international_legal_community).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-based method for determining statehood independent of political consensus: if objective criteria are met, statehood emerges as a legal fact rather than as a discretionary judgment by other states. Solves the problem of how to prevent powerful states from indefinitely blocking independence for territorial or political rivals.
% TRANSFER_FUNCTION: Transfers the authority to establish statehood from the discretionary judgment of existing states to the objective satisfaction of criteria (defined territory, permanent population, government, foreign-relations capacity). The constraint moves legitimacy from consensus-dependent to criteria-dependent, which shifts veto power away from established states toward those who can satisfy objective measurement.
% ABSENT_VOICES: Great powers and parent states that would exercise recognition as conditional leverage are structurally excluded from the decision structure — they would argue that recognition must remain discretionary, that political legitimacy (not just objective criteria) should gate statehood, and that their security interests warrant veto authority. They cannot speak within the declaratory framework because that framework denies them the standing to make that denial.
% DISAPPEARANCE_RATIONALE: If the declaratory reading disappeared and constitutive recognition returned as the sole standard, territorial entities meeting objective criteria but disfavored by great powers would lose statehood status overnight; parent states would regain conditional leverage; independence movements would need great-power consent rather than objective achievement; the legal landscape would revert to discretion-based rather than criteria-based determination.
% FOUNDING_PROBLEM: Historical pattern of great powers using recognition denial to prevent independence of rival or disfavored entities, and of parent states using recognition refusal as a tool to suppress secessionist movements — requiring objective criteria to prevent recognition from becoming a tool of great-power politics.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by post-WWII precedent: Soviet refusal to recognize Baltic independence during Cold War, Western refusal to recognize East Germany, China's non-recognition of Taiwan, and contemporary disputes over Kosovo, Western Sahara, and Palestine. International legal scholarship (beyond beneficiary-state advocacy) documents the historical pattern of recognition denial as political leverage. Neutral arbitral bodies and academic consensus outside great-power capitals attest the problem persists.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the declaratory reading shifts legitimacy authority away from great-power discretion toward objective criteria, which de facto authorities and occupied populations exploit to claim statehood without requiring their adversaries' permission. Suppression is also high (0.71) because enforcement of the declaratory reading against great-power refusal to recognize requires sustained legal pressure — the international legal community must actively defend objective criteria against constant political pressure from established states seeking to deny recognition. Theater is moderate (0.42): the objective criteria (territory, population, government, foreign capacity) have real content, but significant performance exists in how 'government capacity' and 'permanent population' are measured and who defines them. The suppression requirement rises over the interval (0.48 to 0.71) as great powers more actively resist the declaratory reading and as more entities successfully claim statehood under it, forcing greater enforcement effort. Extractiveness rises modestly (0.52 to 0.68) as the reading consolidates authority and becomes less contestable, making its redistribution of statehood authority more effective.
 *
 * PERSPECTIVAL GAP:
 *   From the de facto authority seat, the declaratory reading is liberatory — it converts their political struggle into a legal claim that depends on objective achievement rather than on powerful neighbors' discretionary permission. From the parent-state seat, it is deeply extractive — it strips them of the conditional leverage they historically wielded. From the great-power seat (excluded), it is illegitimate because it denies them the discretion international politics requires. From the international legal community's seat (agenda-setter), it is the correct reading of law. The engine should compute dramatically different types across these seats: snare or tangled_rope from the great-power perspective (if that seat were included), tangled_rope from the parent-state perspective (coordination through criteria + extraction through leverage loss), rope from the de facto authority perspective (genuine coordination benefit from objective rules). The declared power atoms and exit conditions encode these asymmetries; the engine computes the seat-level types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities: low directionality (d near 0.2–0.3), beneficiaries. They receive statehood status from the constraint without bearing costs; their exit is identity-locked (they cannot dissolve their territorial claim) but the constraint opens a path to legitimacy they previously lacked. Established states and parent states: high directionality (d near 0.75–0.85), targets. They lose the recognition veto; their exit is constrained (they cannot leave international law); they must reorganize around objective criteria rather than discretionary leverage. Great powers excluded: highest directionality (d = 1.0 if included as payers), but they are structurally excluded from the decision structure so the framework does not compute them. The international legal community: moderate directionality (d near 0.5), maintaining the constraint against pressure but also holding authority. The measurement trajectory shows suppression rising faster than extractiveness, indicating that enforcement is hardening even as the constraint's operative authority grows — the terrain is becoming more explicitly contested rather than settling into acquiescence.
 *
 * MANDATROPHY ANALYSIS:
 *   The declaratory reading avoids the mandatrophy trap by maintaining a clear coordination function (objective criteria prevent recognition from becoming a tool of great-power politics) alongside asymmetric extraction (shifting authority away from great powers). The founding problem (great powers using recognition denial as leverage) is live — it continues to drive requests for objective statehood criteria. However, tension exists: the constraint's persistence depends on the international legal community's active enforcement against great-power pressure, which means mandatrophy could emerge if great powers increasingly simply refuse to recognize entities that meet the criteria and suffer no legal consequence. The measurement series captures this risk: suppression rising faster than extractiveness suggests enforcement is becoming more theatrical (performance of criteria-adherence without actual acceptance), which is a mandatrophy signal. An omega variable addresses this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_enforcement_attrition,
    'Can the international legal community sustain enforcement of the declaratory reading against great-power refusal to recognize entities that meet objective criteria?',
    'Historical observation of whether great powers increasingly simply refuse recognition without legal consequence, or whether enforcement (through UN General Assembly resolutions, ICJ opinions, regional organization recognition) maintains declaratory statehood despite refusal. Measured by divergence between de facto statehood (meeting criteria) and formal diplomatic recognition.',
    'If enforcement attenuates, the constraint becomes theatrical — entities meet criteria but lack recognition in practice, and the extraction from parent states and great powers reverses (they regain veto authority through practice despite legal theory). If enforcement holds, the declaratory reading consolidates authority. This determines whether the constraint mandates (real law enforced) or theaters (ceremonial criteria maintained while discretion persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_enforcement_attrition, empirical, 'Whether declaratory statehood survives great-power recognition denial in practice.').

omega_variable(
    objective_criteria_measurement_contestation,
    'Can objective criteria (especially ''government capacity'' and ''permanent population'') be measured objectively, or do measurement disputes recreate discretionary judgment under the guise of objectivity?',
    'Empirical analysis of statehood disputes: do cases where an entity clearly fails one criterion get rejected unanimously, or do measurement contests emerge (e.g., does a de facto authority''s limited territorial control count as ''defined territory'')? If measurement is contested on nearly every case, the constraint has reinvented discretion as criteria-definition.',
    'If measurement is genuinely objective, the declaratory reading succeeds in removing discretion. If measurement is contestable, the constraint creates a new extraction mechanism: great powers extract by controlling the interpretation of objective criteria, shifting from refusal-to-recognize to claim-this-doesn''t-meet-criteria. This represents constraint-type drift from tangled_rope toward snare (pure extraction under an objectivity cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_criteria_measurement_contestation, empirical, 'Whether the four criteria admit objective measurement or recreate hidden discretion.').

omega_variable(
    reading_kernel_distinction,
    'Does the declaratory reading rest on a distinct reading of the same Montevideo kernel, or is it actually a different constraint (a different ε)?',
    'Conceptual: the declaratory reading asserts statehood follows from objective criteria alone (four conditions = statehood). The constitutive reading asserts recognition from existing states is necessary (criteria are evidence, not determinants). These are logically contradictory premises about what establishes statehood. Both readings cite the same 1933 convention, but they impose different interpretive frameworks. The kernel is the commitment to the four criteria; the reading is whether meeting them suffices or whether recognition is also required. This is a genuine kernel-reading pair (OQ-26 compatible: same referent — the four criteria — different ε measured by whether criteria sufficiency holds).',
    'If this is correctly framed as a declaratory reading, the constraint''s ε should remain stable regardless of great-power pressure (it measures the legal rule, not its acceptance). If it is actually two different constraints, they should be split into separate stories with different ε values. The answer determines whether temporal drift in suppression represents increased enforcement of a constant rule or increasing contestation indicating the reading is becoming unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether this is a reading of the Montevideo kernel or a separate constraint.').

omega_variable(
    parent_state_leverage_loss_structural,
    'Does the declaratory reading genuinely transfer leverage from parent states to de facto authorities, or does it create a new form of leverage (control over criteria interpretation) that parent states and great powers exercise instead?',
    'Comparative analysis: cases where a de facto authority meets objective criteria but is denied recognition by parent state and great powers (Palestine, Western Sahara, Taiwan) show whether declaratory statehood holds despite refusal, or whether it becomes ceremonial. If entities consistently denied recognition despite meeting criteria, the leverage transfer is incomplete and the extraction redistributes rather than eliminates.',
    'If the transfer is genuine, the constraint redistributes power from established to emerging entities and creates real asymmetry (beneficiary and payer seats experience fundamentally different constraints). If the transfer is incomplete, the constraint is hybrid-extractive: it claims to enable de facto authorities while actually subordinating them to a different form of leverage (criteria-gaming by great powers). This affects whether the reading coexists with or forecloses the constitutive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parent_state_leverage_loss_structural, empirical, 'Whether leverage actually transfers from parent states to de facto authorities or redistributes among great powers.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the declaratory reading''s core premise (criteria sufficiency) logically foreclose the constitutive reading''s core premise (recognition necessity), or can both coexist as competing frameworks?',
    'Logical analysis: if the declaratory reading asserts ''statehood exists when criteria are met'' and the constitutive reading asserts ''statehood exists when recognized'', these statements are logically contradictory — no single entity can simultaneously be a state-by-criteria and not-a-state-by-recognition. However, if both readings are applied to different cases (declaratory to some entities, constitutive to others), they coexist as competing frameworks rather than logically foreclosed alternatives. Foreclosure requires that adopting this reading makes the sibling reading impossible to hold; coexistence requires only that they are currently held by different parties.',
    'If genuinely foreclosed, the constraint represents a fundamental shift in international law from discretionary to criteria-based legitimacy — a one-way ratchet. If coexisting, it represents a contested framework where different states apply different readings simultaneously, creating a hybrid de facto rule. This distinction affects how to classify the reading_relations: does this reading ''foreclose'' the constitutive or ''coexist_with'' it?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether the declaratory and constitutive readings are logically incompatible or can coexist as competing frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mont_tr_t10, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(mont_tr_t20, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(mont_tr_t35, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(mont_tr_t50, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mont_be_t10, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mont_be_t20, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(mont_be_t35, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 35, 0.65).
narrative_ontology:measurement(mont_be_t50, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(mont_su_t10, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(mont_su_t20, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(mont_su_t35, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 35, 0.67).
narrative_ontology:measurement(mont_su_t50, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, international_recognition_regime).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, parent_state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% The Montevideo statehood criteria form a kernel — a persistent commitment that different parties interpret differently. The declaratory reading (this story) interprets the criteria as constitutive of statehood as a legal fact; the constitutive reading interprets them as evidence for recognition consideration; the hybrid reading adds normative legitimacy gates. All three constraints reference the same kernel text but produce different extractiveness and beneficiary structures. Each is a separate story with its own ε, stakeholders, and type. They are linked through the network: each reading affects how the others operate. The declaratory reading, by asserting criteria sufficiency, directly undermines the constitutive reading's discretionary authority and constrains the hybrid reading's legitimacy gates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
