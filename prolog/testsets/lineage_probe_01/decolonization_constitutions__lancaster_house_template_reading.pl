% ============================================================================
% CONSTRAINT STORY: decolonization_constitutions__lancaster_house_template_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decolonization_constitutions__lancaster_house_template_reading, []).

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
 *   constraint_id: decolonization_constitutions__lancaster_house_template_reading
 *   human_readable: Lancaster House Template: Constitutional Pre-Drafting in Decolonization
 *   domain: political/constitutional/decolonization
 *
 * SUMMARY:
 *   The Lancaster House template represents a specific reading of how
 *   decolonization constitutions operated: as pre-drafted institutional forms
 *   negotiated in London conference rooms, presented to independence
 *   movements as non-negotiable conditions of the independence date itself.
 *   This reading emphasizes the suppression of constituent-assembly
 *   authorship — the foreclosure of founding-moment sovereignty — and
 *   identifies extractiveness in the terms set where power still resided:
 *   colonial negotiators dictated institutional forms, entrenched minority
 *   protections as a mechanism of control over majority movements, and
 *   embedded Westminster supremacy in ways that constrained post-independence
 *   governance options. The constraint exhibits genuine coordination function
 *   (minority protections, stable institutional transition) bundled with
 *   extraction (authorship foreclosure, predetermined form, contingent on
 *   accepting colonial-negotiated terms). This is the tangled_rope signature:
 *   both real coordination and real extraction, requiring active enforcement
 *   (the independence date itself becomes the enforcement mechanism — accept
 *   the terms or wait), and asymmetric — some parties (colonial negotiators,
 *   protected minorities) benefit while others (constituent assemblies,
 *   founding movements) bear costs. The theater ratio is initially moderate
 *   (0.48) because the constitutional commitment is substantive at the moment
 *   of drafting, but rises over 15–20 years as Westminster forms are formally
 *   retained while actual power migrates to presidents, military councils, or
 *   party hierarchies (piton trajectory). This reading coexists with two
 *   sibling readings: the durable_adaptation reading (where Westminster forms
 *   transform through local rebuilding rather than transplant rejection) and
 *   the rapid_abandonment reading (where forms are repudiated within a
 *   decade). The three readings are live structural interpretations of the
 *   same historical process, held by different observers.
 *
 * KEY AGENTS:
 *   - Colonial Power Negotiators (institutional/arbitrage): Primary beneficiaries — control institutional form, protect investment frameworks, maintain Commonwealth alignment. Experience as pure coordination from their position.
 *   - Constituent Assembly / Independence Movement (organized/constrained): Primary victims of authorship suppression — lose founding-moment sovereignty, forced to accept pre-drafted terms as condition of independence date. Experience mixed extraction and coordination.
 *   - Protected Minority Groups (institutional/mobile): Secondary beneficiaries — gain formal constitutional protections that prevent immediate post-independence majoritarian dominance. Experience as rope (genuine coordination benefit).
 *   - Westminster Constitutional Form (institutional/arbitrage, piton perspective): Institutional substrate that persists ceremonially while functional power migrates elsewhere. Maintained through inertia and legitimacy cost of repudiation, not through continued utility.
 *   - International Witness Community (moderate/constrained): Experiences both coordination (clear constitutional baseline for recognition) and extraction (cannot force compliance if states repudiate template).
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent colonial choice as inevitable feature of decolonization process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decolonization_constitutions__lancaster_house_template_reading, 0.58).
domain_priors:suppression_score(decolonization_constitutions__lancaster_house_template_reading, 0.65).
domain_priors:theater_ratio(decolonization_constitutions__lancaster_house_template_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decolonization_constitutions__lancaster_house_template_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(decolonization_constitutions__lancaster_house_template_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(decolonization_constitutions__lancaster_house_template_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decolonization_constitutions__lancaster_house_template_reading, tangled_rope).
narrative_ontology:human_readable(decolonization_constitutions__lancaster_house_template_reading, "Lancaster House Template: Constitutional Pre-Drafting in Decolonization").
narrative_ontology:topic_domain(decolonization_constitutions__lancaster_house_template_reading, "political/constitutional/decolonization").

domain_priors:requires_active_enforcement(decolonization_constitutions__lancaster_house_template_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decolonization_constitutions__lancaster_house_template_reading, 'cba6780f-1c79-498a-a5c4-3481ba09c64a').
narrative_ontology:cs_kernel_codification('cba6780f-1c79-498a-a5c4-3481ba09c64a', fixed_text).
narrative_ontology:cs_authority_grounding('cba6780f-1c79-498a-a5c4-3481ba09c64a', extraction).
narrative_ontology:cs_interpretation_layer_present('cba6780f-1c79-498a-a5c4-3481ba09c64a').
narrative_ontology:cs_reading_relation('cba6780f-1c79-498a-a5c4-3481ba09c64a', decolonization_constitutions__durable_adaptation_reading, coexists_with).
narrative_ontology:cs_reading_relation('cba6780f-1c79-498a-a5c4-3481ba09c64a', decolonization_constitutions__rapid_abandonment_reading, coexists_with).
narrative_ontology:cs_axiom('cba6780f-1c79-498a-a5c4-3481ba09c64a', foundational, constituent_assembly_authorship_foreclosed).
narrative_ontology:cs_axiom_status(constituent_assembly_authorship_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('cba6780f-1c79-498a-a5c4-3481ba09c64a', constituent_assembly_authorship_foreclosed, empirically_contingent).
narrative_ontology:cs_axiom('cba6780f-1c79-498a-a5c4-3481ba09c64a', foundational, westminster_supremacy_as_extractive_precondition).
narrative_ontology:cs_axiom_status(westminster_supremacy_as_extractive_precondition, holdable).
narrative_ontology:cs_axiom_grounding('cba6780f-1c79-498a-a5c4-3481ba09c64a', westminster_supremacy_as_extractive_precondition, instrumental).
narrative_ontology:cs_reference_frame('cba6780f-1c79-498a-a5c4-3481ba09c64a', constituent_assembly_authorship_primacy).
narrative_ontology:cs_drift_state('cba6780f-1c79-498a-a5c4-3481ba09c64a', post_independence_institutional_adaptation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cba6780f-1c79-498a-a5c4-3481ba09c64a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(decolonization_constitutions__lancaster_house_template_reading, decolonization_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decolonization_constitutions__lancaster_house_template_reading, colonial_power_negotiators).
narrative_ontology:constraint_beneficiary(decolonization_constitutions__lancaster_house_template_reading, minority_protection_frameworks).
narrative_ontology:constraint_beneficiary(decolonization_constitutions__lancaster_house_template_reading, constitutional_continuity).
narrative_ontology:constraint_victim(decolonization_constitutions__lancaster_house_template_reading, constituent_assembly_authorship).
narrative_ontology:constraint_victim(decolonization_constitutions__lancaster_house_template_reading, founding_moment_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENT ASSEMBLY (SNARE) — Faces pre-drafted constitutional text negotiated in London, presented as non-negotiable condition of independence date. Cannot exit without losing the independence offer itself. Founders lose authorship rights; the constraint extracts founding-moment sovereignty and substitutes predetermined institutional form. Maximum experienced extraction — the legitimacy mechanism (democratic founding) is suppressed by the precondition (accept Lancaster House terms or delay independence indefinitely).
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENCE MOVEMENT (TANGLED ROPE) — Genuine coordination function: constitutional protections for minorities (negotiated in London) address real sectarian/ethnic tensions and make transition credible to international actors. But constrained by the take-it-or-leave-it structure: movement cannot substantially revise terms without losing independence date. Experiences both coordination (minority protections that enable coexistence) and extraction (authorship foreclosure, institutional form predetermined). Significant suppression of alternative constitutional pathways.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL POWER NEGOTIATORS (ROPE) — Pure coordination from this perspective: drafting Westminster-compatible constitutions with entrenched minority protections enables orderly transition, protects British investment, ensures Commonwealth alignment, and manages competing local factions. Negotiators see this as solving a coordination problem (how to transfer power without state collapse). Net beneficiary — can exit the arrangement if terms are not met (independence simply does not occur), while maintaining institutional forms and minority safeguards that serve imperial interests.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTECTED MINORITY GROUPS (ROPE) — Benefit from Westminster-entrenched constitutional protections negotiated in London as explicit condition of independence. These groups see the constraint as coordinating coexistence: without the externally-drafted constitutional limits, majority-driven dominance would be immediate post-independence. The constraint ensures voice and veto power on certain matters. Mobile exit option reflects that minorities can (in principle) exit if protections erode, though cost is high. The benefits (formal constitutional protection) are genuine and purchased through accepting the Lancaster House template.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL WITNESS COMMUNITY (TANGLED ROPE) — Coordination function: externally-drafted constitutions with entrenched rights chapters provide credible commitment signals to international actors about post-independence governance. Commonwealth membership, investment protection, and diplomatic recognition depend partly on Westminster-compatible institutional forms. But constrained by inability to force compliance if the new state repudiates the template (which many do within 10–20 years). Experiences both benefit (clear constitutional baseline for recognition) and extraction (institutional forms dictate governance without local revision capacity). Suppression operates through withholding recognition from departing states.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WESTMINSTER FORM AS INSTITUTIONAL SUBSTRATE (PITON) — The Westminster template persists in the formal constitutions of many ex-colonies decades after practical abandonment. In many cases, the form is maintained ceremonially (Queen as head of state, parliamentary supremacy) while real power migrates to presidents, military councils, or party hierarchies. Theater ratio is low (0.48) at the moment of drafting — the coordination function is real, minority protections are substantive, and the constitutional commitment is genuinely made. But the piton signature emerges over time: Westminster forms become vestigial, maintained for international legitimacy while actual governance operates through parallel structures. At civilizational scale, the form appears as institutional inertia — retained because the alternative (constitutional repudiation) is politically costly, not because it functions.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED CONSTRAINT (MOUNTAIN) — At civilizational scale, from a global vantage, the Lancaster House template reading might appear as an inevitable feature of managed decolonization: power transitions require institutional continuity, minority protections prevent state collapse, and Westminster forms are the only template with sufficient legitimacy to enable orderly handover. This perspective risks naturalizing what is actually a contingent choice by colonial negotiators — treating the extraction of founding-moment authorship as an immutable feature of the decolonization process itself. The false summit flag applies here: the constraint benefits identifiable parties (colonial negotiators, minority protections as institutional fact, continuity of Commonwealth investment frameworks) and operates through substantial suppression (constituent assembly authorship is foreclosed). The mountain reading naturalizes these contingent features as inevitable.
constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decolonization_constitutions__lancaster_house_template_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decolonization_constitutions__lancaster_house_template_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decolonization_constitutions__lancaster_house_template_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decolonization_constitutions__lancaster_house_template_reading, TR),
    TR >= 0.70.

:- end_tests(decolonization_constitutions__lancaster_house_template_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts authorship from constituent assemblies and imposes predetermined institutional form. But extractiveness is not maximal (0.72+) because genuine coordination benefits exist (minority protections that prevent state collapse, clear institutional pathway for transition, international credibility). The extraction is bundled with real coordination, not pure coercion. Measurement trajectory shows declining extractiveness over 15–20 years as local adaptation occurs and Westminster forms become vestigial — the extraction mechanism (forced acceptance of London-negotiated terms) loses force as institutions drift from their original template. Suppression (0.65): Moderately high. Constituent assemblies have no meaningful exit option — independence date is contingent on accepting the template. Colonial negotiators control the terms and can walk away (arbitrage exit). This asymmetry in exit options produces suppression: the offer is take-it-or-leave-it, and leaving it means indefinite colonial rule. However, suppression is not total (0.75+) because some movements successfully negotiate revisions, and the international pressure for decolonization limits how long colonial powers can withhold the date. Theater ratio (0.48): Moderate, initially. At the moment of drafting and independence, the constitutional commitment is substantive — minority protections are written into law, Westminster procedures are genuinely implemented, and the foundational moment has real institutional content. Theater rises to 0.71 by t=15 as the form becomes vestigial — the Queen remains head of state and parliamentary supremacy is formally retained while presidents, military councils, or party cadres exercise actual power. Claimed type (tangled_rope) is supported by the metric profile: ε ≥ 0.30, suppression ≥ 0.40, requires_active_enforcement = true (independence date acts as enforcement mechanism), beneficiaries present (negotiators, minorities), victims present (constituent assemblies), asymmetric structure confirmed by directionality data.
 *
 * PERSPECTIVAL GAP:
 *   The Lancaster House template generates maximum perspectival divergence because the structural positions are maximally asymmetric. Colonial negotiators see coordination (stable transition, Commonwealth preservation, investment protection). Constituent assemblies see extraction (authorship foreclosure, institutional form predetermined, founding moment alienated). Protected minorities see coordination (constitutional protections that prevent dominance). The independence movement sees mixed coordination and extraction (both the independence date and the minority protections are genuine benefits; the foreclosure of alternative institutional forms is genuine cost). The international community sees coordination for legitimacy. The Westminster form itself, viewed at civilizational scale, appears as vestigial (piton) — retained for legitimacy cost of repudiation, not for function. The analytical observer risks the false summit trap: viewing the constraint as inevitable natural law of managed decolonization rather than contingent choice by colonial negotiators with specific extractive interests.
 *
 * DIRECTIONALITY LOGIC:
 *   The Lancaster House template generates distinct directionality profiles for different observer positions. Colonial negotiators perceive d ≈ 0.10 (beneficiary + arbitrage exit → negative effective extraction from their perspective). Constituent assemblies perceive d ≈ 0.85 (victim + trapped exit → high experienced extraction). Protected minorities perceive d ≈ 0.35 (beneficiary + mobile/constrained exit, but conditional on accepting the template → moderate experienced benefit). The independence movement perceives d ≈ 0.60 (victim + constrained exit, but organized → moderate experienced extraction). The perspectival gaps are real and structural: beneficiaries and victims genuinely occupy different positions relative to the constraint. The colonial negotiators' arbitrage option (can walk away if terms are not accepted) gives them leverage; the constituent assembly's trapped status (no independence without the template) gives them none. The suppression mechanism operates by asymmetry: negotiators control the take-it-or-leave-it offer.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lancaster House template reading resolves mandatrophy by clarifying that the constraint is neither pure coordination (rope) nor pure extraction (snare), but hybrid (tangled_rope). This is not a failure of the classification system but a correct structural identification: colonial negotiators genuinely solve a coordination problem (how to transfer power without state collapse) while extracting authorship and institutional control. The coordination benefits are real (minority protections prevent sectarian violence; institutional stability enables investment). The extraction is real (constituent assemblies lose founding-moment sovereignty; institutional form is predetermined). Both dimensions must be present in the classification. The piton trajectory (theater rising, extractiveness falling) indicates that over time the coordination function degrades while the extraction persists in formal terms — the Westminster form is maintained not because it coordinates anything anymore, but because abandoning it is politically costly. The false summit analytical perspective reveals the risk: treating the constraint as inevitable natural law naturalizes what is actually a contingent institutional arrangement designed by actors with extractive interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorship_suppression_necessity,
    'Was the suppression of constituent-assembly authorship structurally necessary for managing decolonization, or was it an extractive choice by colonial negotiators?',
    'Counterfactual analysis: cases where independence movements retained authorship (rare); comparison of outcomes in cases where founders had revision capacity vs. cases with locked templates; archival evidence of colonial negotiators'' alternatives considered and rejected.',
    'If necessary: constraint recategorizes toward rope (pure coordination). If extractive choice: constraint remains snare/tangled_rope (extraction bundled with coordination). Classification outcome is contingent on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorship_suppression_necessity, empirical, 'Whether suppression of authorship was structurally necessary or an extractive choice').

omega_variable(
    minority_protection_credibility,
    'Did externally-drafted minority protections in Westminster templates actually prevent majority dominance, or were they merely theatrical commitments abandoned when politically inconvenient?',
    'Post-independence analysis of minority protection enforcement: cases where entrenched rights were honored (India, Ghana early period) vs. cases where they were repudiated (Uganda, Sri Lanka). Measurement of minority political power and access before/after independence date.',
    'If substantively honored: minority protection component is genuine coordination (supports tangled_rope). If abandoned quickly: minority protections were theater for international legitimacy (suggests snare classification). Mixed outcomes suggest piton trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_credibility, empirical, 'Whether minority protections in Westminster templates functioned as genuine coordination or theater').

omega_variable(
    alternative_template_availability,
    'What alternative constitutional templates were available to independence movements at the time, and why were Westminster forms preferred by colonial negotiators over alternatives?',
    'Historical analysis of negotiation records, rejected proposals, and comparative constitutional frameworks available in 1950s-1960s. Identification of alternatives (populist republics, federal models, unitary socialist constitutions) that colonial negotiators explicitly opposed.',
    'If alternatives existed and were actively rejected: Lancaster House template represents extracted choice, not inevitable form. If alternatives were genuinely unavailable: template reflects constraint of international legitimacy architecture rather than colonial extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_template_availability, empirical, 'Availability and treatment of alternative constitutional templates').

omega_variable(
    committer_reading_contest,
    'Is the Lancaster House template reading (pre-drafted, negotiated in London, extractive of authorship) the accurate structural characterization, or is it foreclosed by the durable_adaptation_reading or the rapid_abandonment_reading?',
    'Examination of the three readings'' axioms and reference frames: Do they coexist as live readings held by different parties? Does one logically foreclose another? What does the subsequent history (adaptation or abandonment) reveal about the initial template''s classification?',
    'If durable_adaptation_reading is correct: Lancaster House template is foundational but undergoes transformative revision by local actors — the reading coexists with this one, both live. If rapid_abandonment_reading is correct: Lancaster House template is rejected quickly, suggesting it was extraction without functional coordination — the reading coexists but reveals piton trajectory. Classification does not change, but the trajectory and resolution mechanism do.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_contest, conceptual, 'Structural relationship between Lancaster House template reading and its sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decolonization_constitutions__lancaster_house_template_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhtr_theater_t0_initial, decolonization_constitutions__lancaster_house_template_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(lhtr_theater_t5_formalization, decolonization_constitutions__lancaster_house_template_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(lhtr_theater_t15_vestigial, decolonization_constitutions__lancaster_house_template_reading, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(lhtr_extractiveness_t0_negotiation, decolonization_constitutions__lancaster_house_template_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(lhtr_extractiveness_t5_early_adaptation, decolonization_constitutions__lancaster_house_template_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lhtr_extractiveness_t15_institutional_drift, decolonization_constitutions__lancaster_house_template_reading, base_extractiveness, 15, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lhtr_suppression_t0_negotiation_precondition, decolonization_constitutions__lancaster_house_template_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(lhtr_suppression_t5_early_challenge, decolonization_constitutions__lancaster_house_template_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(lhtr_suppression_t15_institutional_decay, decolonization_constitutions__lancaster_house_template_reading, suppression_requirement, 15, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decolonization_constitutions__lancaster_house_template_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decolonization_constitutions__lancaster_house_template_reading, decolonization_constitutions__durable_adaptation_reading).
narrative_ontology:affects_constraint(decolonization_constitutions__lancaster_house_template_reading, decolonization_constitutions__rapid_abandonment_reading).
narrative_ontology:affects_constraint(decolonization_constitutions__lancaster_house_template_reading, westminster_transplant_rejection).
narrative_ontology:affects_constraint(decolonization_constitutions__lancaster_house_template_reading, commonwealth_institutional_capture).

% DUAL FORMULATION NOTE:
% The decolonization_constitutions kernel decomposes into three structurally distinct constraint stories: lancaster_house_template_reading (ε=0.58, tangled_rope, emphasizes extraction and suppression), durable_adaptation_reading (ε varies by case, rope/tangled_rope, emphasizes local transformation), and rapid_abandonment_reading (ε varies by timeline, snare/piton, emphasizes template rejection). These are not different measurements of one constraint but three distinct readings of how the same historical process operated. Each story has different beneficiary/victim structures, different measurement trajectories, and different terminal states. They are linked as a kernel family with reading_relations capturing the logical and structural relationships between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decolonization_constitutions__lancaster_house_template_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
