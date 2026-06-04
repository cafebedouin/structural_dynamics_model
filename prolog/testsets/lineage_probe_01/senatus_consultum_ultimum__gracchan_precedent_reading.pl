% ============================================================================
% CONSTRAINT STORY: senatus_consultum_ultimum__gracchan_precedent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senatus_consultum_ultimum_gracchan_precedent_reading, []).

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
 *   constraint_id: senatus_consultum_ultimum__gracchan_precedent_reading
 *   human_readable: Senatus Consultum Ultimum as Factional Suppression (Gracchan Precedent Reading)
 *   domain: legal/doctrinal/roman_republican_crisis
 *
 * SUMMARY:
 *   The senatus consultum ultimum (final decree) was a Roman Republican
 *   emergency mechanism invoked by the Senate to authorize consuls to act
 *   beyond normal legal constraints to preserve the state in crisis. This
 *   reading instantiates the Gracchan precedent interpretation: the decree
 *   was deployed first against Gaius Gracchus (127 BCE, landslide reformation
 *   crisis) and subsequently became a factional weapon of the senatorial
 *   oligarchy against popular reform tribunes. The structural claim of this
 *   reading is that the decree functioned as suppression of popular reform
 *   under emergency color from its first day — it was not a neutral crisis
 *   mechanism corrupted by later misuse, but a factional tool disguised as
 *   emergency from inception. The beneficiary is the senatorial order
 *   threatened by Gracchian redistribution; the victims are the Gracchans,
 *   their successors, and the tribunate as an institution. The decree's
 *   extractiveness (0.68) reflects that it converts political competition
 *   into proscription: the mechanism does not govern through incentives or
 *   law but through elimination of opponents. Its suppression (0.72) reflects
 *   the foreclosure of all exit options for targeted agents — the decree
 *   suspends normal legal protections, veto powers, and electoral succession,
 *   leaving targets only flight or death. Theater ratio (0.55) is moderate
 *   because the decree operates with formal senatorial authorization and
 *   constitutional language ('for the safety of the state') while functioning
 *   as systematic suppression.
 *
 * KEY AGENTS:
 *   - Senatorial oligarchy threatened by reform (institutional/arbitrage) — primary beneficiary; controls the decree's invocation and deployment
 *   - Gracchans and reform tribunes (powerless/trapped) — primary victims; face proscription, death, or exile; no legal exit
 *   - Provincial clients and land-hungry masses (moderate/constrained) — secondary victims; lose redistributive benefits and patronage networks when reform tribunes are eliminated
 *   - Republican constitutional order (institutional/constrained) — the form persists but function atrophies; becomes theatrical after repeated invocations
 *   - Factional rivals (organized/mobile) — organized agents who could theoretically dissolve or constrain the decree but do not; mechanism becomes normalized
 *   - Analytical observer (analytical/analytical) — risks naturalizing the decree as immutable statecraft necessity rather than contingent factional tool
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senatus_consultum_ultimum__gracchan_precedent_reading, 0.68).
domain_priors:suppression_score(senatus_consultum_ultimum__gracchan_precedent_reading, 0.72).
domain_priors:theater_ratio(senatus_consultum_ultimum__gracchan_precedent_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senatus_consultum_ultimum__gracchan_precedent_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(senatus_consultum_ultimum__gracchan_precedent_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(senatus_consultum_ultimum__gracchan_precedent_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senatus_consultum_ultimum__gracchan_precedent_reading, snare).
narrative_ontology:human_readable(senatus_consultum_ultimum__gracchan_precedent_reading, "Senatus Consultum Ultimum as Factional Suppression (Gracchan Precedent Reading)").
narrative_ontology:topic_domain(senatus_consultum_ultimum__gracchan_precedent_reading, "legal/doctrinal/roman_republican_crisis").

domain_priors:requires_active_enforcement(senatus_consultum_ultimum__gracchan_precedent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(senatus_consultum_ultimum__gracchan_precedent_reading, '814a9357-5f12-4005-8180-f26f50b6e614').
narrative_ontology:cs_kernel_codification('814a9357-5f12-4005-8180-f26f50b6e614', formalized).
narrative_ontology:cs_authority_grounding('814a9357-5f12-4005-8180-f26f50b6e614', extraction).
narrative_ontology:cs_interpretation_layer_present('814a9357-5f12-4005-8180-f26f50b6e614').
narrative_ontology:cs_reading_relation('814a9357-5f12-4005-8180-f26f50b6e614', senatus_consultum_ultimum__emergency_without_office_reading, coexists_with).
narrative_ontology:cs_reading_relation('814a9357-5f12-4005-8180-f26f50b6e614', senatus_consultum_ultimum__legality_contested_reading, influences).
narrative_ontology:cs_axiom('814a9357-5f12-4005-8180-f26f50b6e614', foundational, decree_factional_from_inception).
narrative_ontology:cs_axiom_status(decree_factional_from_inception, holdable).
narrative_ontology:cs_axiom_grounding('814a9357-5f12-4005-8180-f26f50b6e614', decree_factional_from_inception, empirically_contingent).
narrative_ontology:cs_axiom('814a9357-5f12-4005-8180-f26f50b6e614', foundational, reform_tribunal_death_as_continuous_precedent).
narrative_ontology:cs_axiom_status(reform_tribunal_death_as_continuous_precedent, holdable).
narrative_ontology:cs_axiom_grounding('814a9357-5f12-4005-8180-f26f50b6e614', reform_tribunal_death_as_continuous_precedent, empirically_contingent).
narrative_ontology:cs_reference_frame('814a9357-5f12-4005-8180-f26f50b6e614', senatorial_oligarchic_authority_preserved_through_emergency).
narrative_ontology:cs_drift_state('814a9357-5f12-4005-8180-f26f50b6e614', late_republic_civil_wars, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('814a9357-5f12-4005-8180-f26f50b6e614', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(senatus_consultum_ultimum__gracchan_precedent_reading, senatus_consultum_ultimum).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senatus_consultum_ultimum__gracchan_precedent_reading, senatorial_order_threatened_by_reform).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__gracchan_precedent_reading, gracchans_and_successors).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__gracchan_precedent_reading, tribunes_with_reform_mandate).
narrative_ontology:constraint_victim(senatus_consultum_ultimum__gracchan_precedent_reading, popular_assemblies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM VICTIMS (SNARE) — Trapped agents with no exit. Once the senatus consultum ultimum is invoked, the reform tribune faces proscription, death, or exile. The constraint operates through total suppression: refusal to recognize the tribune's legal authority; suspension of customary protections for magistrates; mobilization of executive force. Extractiveness is maximum because the outcome is predetermined — the decree is not a governing mechanism but a mechanism of elimination. The constraint kills its targets.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PROVINCIAL CLIENTS (TANGLED ROPE) — Constrained agents with partial agency. Provincial networks allied with reform tribunes receive benefits from redistributive land grants and tax policy shifts (genuine coordination function) but also bear the risk of proscription and loss of patronage if their patrons are killed under the decree. The constraint mixes coordination (the tribune solves the collective action problem of land shortage through legislative action) with asymmetric extraction (the decree uses emergency color to eliminate the tribune before these benefits are secured).
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENATORIAL ORDER (ROPE) — Institutional actors with arbitrage options. The senatorial oligarchy experiences the decree as coordination mechanism: it coordinates the collective action problem of defending property rights and power distribution against tribunes proposing redistribution. The decree solves the collective action problem of how to mobilize executive force without formal dictatorship. For the oligarchy, the constraint is pure coordination — no coercion required, no suppression experienced. They control the mechanism and benefit from its output.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUBLICAN FORM (PITON) — Constrained institutional actor. The Republican constitution survives the decree but in degraded form. The decree is invoked with increasing frequency and against decreasing provocation: first against Gaius Gracchus (genuine land redistribution crisis), later against minor agitations and eventually as routine suppression of electoral competition. The constitutional order persists theatrically — the forms (consulate, tribunes, Senate) continue, but their function has atrophied. Theater ratio is moderate-to-high because the Republic maintains the fiction of legality and constitutional process while suspending them in practice.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FACTIONAL RIVALRY (SCAFFOLD) — Organized actors with temporary sunset. The decree is structured as emergency mechanism, explicitly invoked for crisis resolution. If the underlying crisis (land shortage, power concentration, reform pressure) were resolved, the decree would lose its operative logic. Successor factions that control the Senate could theoretically dissolve the mechanism or constrain its application. However, the historical trajectory shows the opposite: the decree accumulates authority rather than expiring. As a scaffold, this perspective is aspirational — it represents what the mechanism *was designed* to be (temporary crisis response) rather than what it *became* (permanent factional weapon).
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — Civilizational perspective that risks treating the decree as an immutable structural feature of Republican crisis management. From this view, emergency suspension of normal law is an invariant property of political systems under stress — every republic must have a mechanism for crisis response, and that mechanism will necessarily exceed normal legal bounds. This perspective naturalizes the decree as a logical law of statecraft. However, the structural data reveals this as a false summit: the decree's extractiveness and suppression indicate a contingent factional arrangement, not a natural necessity.
constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senatus_consultum_ultimum__gracchan_precedent_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senatus_consultum_ultimum__gracchan_precedent_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senatus_consultum_ultimum__gracchan_precedent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senatus_consultum_ultimum__gracchan_precedent_reading, TR),
    TR >= 0.70.

:- end_tests(senatus_consultum_ultimum__gracchan_precedent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The decree functions as proscription machinery — it converts political disagreement into legal justification for elimination. Gaius Gracchus faced execution as a 'threat to the state' despite his office as tribune. His successors faced similar fates. The extraction is severe because the mechanism strips targets of all legal protections and exit options. The trajectory shows escalation from 0.42 (first use against Gracchus, still genuinely tied to a substantive crisis) to 0.68 (routine invocation against minor agitations). This accumulation reflects that the factional weapon loses connection to actual emergency and becomes pure suppression. Suppression (0.72): Very high. The decree forecloses all normal political exits: tribunes with reform mandates cannot exercise their veto; reformers cannot appeal to popular assemblies (the assemblies are suppressed by decree); allies cannot protect patrons (proscription is collective). The only exits are flight or death. Suppression intensifies over time as the decree becomes normalized — early use still faces rhetorical opposition (emergency language required), but later use faces minimal resistance. Theater ratio (0.55): Moderate. The decree maintains formal constitutional language ('for the safety of the state,' invoking senatorial authority) and operates through explicit decree rather than naked violence. However, the theater is not as high as a piton (0.70+) because the decree's suppressive function is direct and acknowledged, even if justified differently by different parties. The mechanism does not hide what it does — it justifies what it does through emergency rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The senatorial oligarchy sees a rope: the decree solves their collective action problem (coordination against reform). The reform tribunes see a snare: the decree eliminates them without legal recourse. Provincial clients see tangled rope: the decree both enables (via reform benefits) and threatens (via proscription of their patrons) their interests. The piton perspective notes that the Republican form persists but function decays. The scaffold perspective (aspirational) sees the decree as sunset mechanism but historical reality contradicts this. The analytical observer risks seeing mountain (emergency is natural law) but the structural data reveals false summit: the decree is contingent factional tool, not immutable feature of statecraft. The measurement trajectory shows why: as extractiveness accumulates and theater rises, the gap between emergency-justified justification and factional suppression widens. Early invocations (Gracchus crisis) could plausibly claim genuine emergency; later invocations cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality d-value for each perspective derives from their structural position: beneficiaries of the mechanism have low d (negative f(d), experiencing anti-extraction); trapped victims have high d (maximum f(d), experiencing maximum extraction); constrained or mobile agents occupy intermediate positions. The senatorial oligarchy derives d ≈ 0.10 from their beneficiary status + arbitrage exit options (f(d) ≈ -0.10). The Gracchans derive d ≈ 0.95 from their victim status + trapped exit options (f(d) ≈ 1.42). Provincial clients derive d ≈ 0.70 from their victim status + constrained exit options (f(d) ≈ 1.00). The decree's effective extraction (χ) is scaled by these directionality values: the senatorial beneficiaries experience χ ≈ 0.68 × (-0.10) × 1.0 = anti-extraction (the mechanism benefits them); the trapped victims experience χ ≈ 0.68 × 1.42 × 1.0 = severe extraction; constrained clients experience χ ≈ 0.68 × 1.00 × 1.0 = moderate-to-high extraction. The perspectival gap between beneficiary rope and victim snare is the mechanism's full extractive power made visible through context-dependent classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by demonstrating that the decree exhibits genuine mixed coordination and extraction dynamics, but the reading's core claim is that extraction dominates from inception. The rope perspective (senatorial coordination) is real — the decree does solve a collective action problem. But the snare perspective (victim suppression) is the mechanism's primary function and effect. The reading argues this is not accidental (that coordination gets corrupted over time) but structural (that the coordination benefit to the oligarchy always depended on suppressing reform). The mandatrophy is resolved by showing that both perspectives are correct simultaneously: the decree coordinates senatorial power precisely by suppressing popular reform. It is snare to the targets because it is rope to the beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_versus_factional_intent,
    'Is the senatus consultum ultimum fundamentally an emergency mechanism that was *secondarily* weaponized by factions, or a factional weapon *disguised* as emergency from its inception?',
    'Historical analysis of the original decree''s language and context (Gaius Gracchus crisis, 121 BCE); comparison with subsequent invocations to trace intentional escalation vs. incremental drift; examination of legislative precedents and senatorial debate records if extant.',
    'If fundamentally emergency: the decree is a rope/scaffold that was corrupted by usage — classify as piton (degraded coordination). If originally factional: the decree is a snare from its first day — mountain-level extraction masked by emergency theater. This reading instantiates the second position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_versus_factional_intent, empirical, 'Whether decree was emergency tool co-opted for faction or factional weapon dressed as emergency').

omega_variable(
    tribunes_institutional_death_date,
    'At what point does the tribune''s magisterial office become meaningless — when does the institution die as a check on power?',
    'Chronological analysis: number of tribunes serving full terms without proscription or forced abdication; correlation with decree invocations; examination of whether later tribunes still exercise veto or defensive functions.',
    'If tribunes retain functional authority after Gracchus: the decree is snare but the system retains compartmentalized constraints. If tribuneship becomes nominal office: the decree has crossed into totalizing extraction, and the Republican form (piton perspective) is purely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunes_institutional_death_date, empirical, 'When tribuneship becomes institutionally ineffective due to decree suppression').

omega_variable(
    reading_foreclosure_via_legal_precedent,
    'Does this reading (decree as factional suppression from day one) logically foreclose the legality_contested_reading (that the Republic never agreed on its lawfulness)?',
    'Examination of the relationship between factional intent and legal status: can a mechanism be both intentionally factional AND genuinely contested in legality by different parties? Or does factional origin settle the legal question (making it inherently unlawful, foreclosing claims of ambiguity)?',
    'If foreclosure holds: this reading and legality_contested_reading cannot both be true. If no foreclosure: both readings remain live (different parties genuinely disagreed on legality despite factional origins). This determines whether the kernel has 2 or 3 live readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_legal_precedent, conceptual, 'Whether factional-origin thesis forecloses legality-contested thesis').

omega_variable(
    natural_law_false_summit_candidate,
    'Is the analytical perspective''s mountain classification (emergency suspension is a natural law of statecraft) a genuine structural observation or a naturalization that serves senatorial interests?',
    'Comparative analysis of republics and polities: do all systems require emergency suspensions? Are there constitutional systems that survive without one? Does the naturalization hide that the senatus consultum ultimum is *specifically* designed for senatorial control rather than generic crisis response?',
    'If natural law holds: emergency mechanisms are inevitable. If naturalization: the decree is a contingent factional tool, and the mountain perspective is a false summit that the engine should detect and flag via FSM signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_candidate, conceptual, 'Whether emergency-suspension naturalization masks factional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senatus_consultum_ultimum__gracchan_precedent_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scu_gracchan_theater_t0_first_invocation, senatus_consultum_ultimum__gracchan_precedent_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(scu_gracchan_theater_t5_accumulation, senatus_consultum_ultimum__gracchan_precedent_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(scu_gracchan_theater_t10_routine_invocation, senatus_consultum_ultimum__gracchan_precedent_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(scu_gracchan_extract_t0_gaius_crisis, senatus_consultum_ultimum__gracchan_precedent_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scu_gracchan_extract_t5_post_gaius, senatus_consultum_ultimum__gracchan_precedent_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(scu_gracchan_extract_t10_late_republic, senatus_consultum_ultimum__gracchan_precedent_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(scu_gracchan_suppress_t0_first_use, senatus_consultum_ultimum__gracchan_precedent_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(scu_gracchan_suppress_t5_escalation, senatus_consultum_ultimum__gracchan_precedent_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(scu_gracchan_suppress_t10_normalized, senatus_consultum_ultimum__gracchan_precedent_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senatus_consultum_ultimum__gracchan_precedent_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__gracchan_precedent_reading, senatus_consultum_ultimum__emergency_without_office_reading).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__gracchan_precedent_reading, senatus_consultum_ultimum__legality_contested_reading).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__gracchan_precedent_reading, gracchian_land_redistribution_constraint).
narrative_ontology:affects_constraint(senatus_consultum_ultimum__gracchan_precedent_reading, tribunate_veto_suppression).

% DUAL FORMULATION NOTE:
% Three constraint stories share the senatus_consultum_ultimum kernel. This reading (gracchan_precedent) emphasizes factional origins and continuous suppression; emergency_without_office emphasizes legal structural danger; legality_contested emphasizes unresolved legal status. Each reading has its own ε and classification type. All three are linked via network.affects_constraints and instantiate different interpretive positions within the same kernel. The gracchan_precedent reading is upstream (causally) of the legality_contested reading: Cicero's exile for executing citizens under the decree makes sense only if the decree's legal status was genuinely contested, and such contestation arises from the decree's factional origins.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(senatus_consultum_ultimum__gracchan_precedent_reading, institutional, 0.08).
constraint_indexing:directionality_override(senatus_consultum_ultimum__gracchan_precedent_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
