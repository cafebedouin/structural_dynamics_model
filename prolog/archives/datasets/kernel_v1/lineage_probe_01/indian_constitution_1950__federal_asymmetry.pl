% ============================================================================
% CONSTRAINT STORY: indian_constitution_1950__federal_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_constitution_1950__federal_asymmetry, []).

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
 *   constraint_id: indian_constitution_1950__federal_asymmetry
 *   human_readable: Indian Constitution 1950: Federal Asymmetry — Strong Centre and Uneven State Autonomy
 *   domain: political/constitutional/federalism
 *
 * SUMMARY:
 *   The Indian Constitution of 1950 institutionalizes federal asymmetry as a
 *   foundational structural principle: a strong national centre with plenary
 *   power, paired with subordinate states (themselves reorganized by language
 *   for coordination purposes) and directly-ruled union territories that have
 *   no guaranteed path to autonomy. This reading focuses on the federal
 *   structure itself as a constraint — who benefits from strong central
 *   authority, who bears the cost of suppressed state sovereignty, and
 *   whether the asymmetry is a structural necessity or a contingent choice.
 *   The constraint is instantiated through multiple mechanisms: Article 368
 *   (constitutional supremacy, no state may secede), Article 356 (emergency
 *   dissolution of state governments), the appointment of federal governors,
 *   control of union territories, the doctrine that no amendment may alter
 *   the basic federal structure, and the asymmetric distribution of taxation
 *   and spending power. The extractiveness (0.52) reflects that central
 *   authority extracts significant autonomy from states and territories, but
 *   does not achieve total extraction — states retain legislative and
 *   executive powers in defined domains, and the Supreme Court sometimes
 *   constrains central override. The suppression (0.68) reflects that
 *   sovereignty claims are constitutionally foreclosed, alternative federal
 *   arrangements are not available, and emergency powers can be activated on
 *   disputed grounds.
 *
 * KEY AGENTS:
 *   - Union Government (Executive and Parliament): Primary beneficiary (institutional/arbitrage) — maintains strong centre, controls emergency powers, manages state reorganization
 *   - Regional State Governments: Secondary beneficiary and constrained victim (organized/constrained) — gain linguistic recognition and local representation; lose sovereignty and face central override
 *   - Union Territory Residents: Primary victims (powerless/trapped) — denied elected state government; subject to direct central rule; no constitutional guarantee of statehood
 *   - State Sovereignty Maximalists: Victims (powerless/trapped) — secession foreclosed by Article 368; sovereignty claims delegitimized by constitutional design
 *   - Supreme Court: Institutional arbiter (institutional/constrained) — coordinating but also extracted; bound by basic structure doctrine it created; constrained in reviewing emergency measures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the strong centre as inevitable necessity rather than power distribution choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_constitution_1950__federal_asymmetry, 0.52).
domain_priors:suppression_score(indian_constitution_1950__federal_asymmetry, 0.68).
domain_priors:theater_ratio(indian_constitution_1950__federal_asymmetry, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_constitution_1950__federal_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(indian_constitution_1950__federal_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indian_constitution_1950__federal_asymmetry, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_constitution_1950__federal_asymmetry, tangled_rope).
narrative_ontology:human_readable(indian_constitution_1950__federal_asymmetry, "Indian Constitution 1950: Federal Asymmetry — Strong Centre and Uneven State Autonomy").
narrative_ontology:topic_domain(indian_constitution_1950__federal_asymmetry, "political/constitutional/federalism").

domain_priors:requires_active_enforcement(indian_constitution_1950__federal_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(indian_constitution_1950__federal_asymmetry, '0cbd8495-77fb-4aac-8c06-1b90742bf191').
narrative_ontology:cs_kernel_codification('0cbd8495-77fb-4aac-8c06-1b90742bf191', formalized).
narrative_ontology:cs_authority_grounding('0cbd8495-77fb-4aac-8c06-1b90742bf191', lineage).
narrative_ontology:cs_interpretation_layer_present('0cbd8495-77fb-4aac-8c06-1b90742bf191').
narrative_ontology:cs_reading_relation('0cbd8495-77fb-4aac-8c06-1b90742bf191', indian_constitution_1950__amendment_and_basic_structure, coexists_with).
narrative_ontology:cs_reading_relation('0cbd8495-77fb-4aac-8c06-1b90742bf191', indian_constitution_1950__directive_principles_part_iv, coexists_with).
narrative_ontology:cs_reading_relation('0cbd8495-77fb-4aac-8c06-1b90742bf191', indian_constitution_1950__fundamental_rights_part_iii, coexists_with).
narrative_ontology:cs_reading_relation('0cbd8495-77fb-4aac-8c06-1b90742bf191', indian_constitution_1950__social_revolution_provisions, coexists_with).
narrative_ontology:cs_axiom('0cbd8495-77fb-4aac-8c06-1b90742bf191', foundational, union_integrity_primacy).
narrative_ontology:cs_axiom_status(union_integrity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0cbd8495-77fb-4aac-8c06-1b90742bf191', union_integrity_primacy, instrumental).
narrative_ontology:cs_axiom('0cbd8495-77fb-4aac-8c06-1b90742bf191', foundational, state_sovereignty_non_transferable).
narrative_ontology:cs_axiom_status(state_sovereignty_non_transferable, holdable).
narrative_ontology:cs_axiom_grounding('0cbd8495-77fb-4aac-8c06-1b90742bf191', state_sovereignty_non_transferable, deontological).
narrative_ontology:cs_reference_frame('0cbd8495-77fb-4aac-8c06-1b90742bf191', strong_federation_constitutional_order).
narrative_ontology:cs_drift_state('0cbd8495-77fb-4aac-8c06-1b90742bf191', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0cbd8495-77fb-4aac-8c06-1b90742bf191', '').
narrative_ontology:cs_kernel_id(indian_constitution_1950__federal_asymmetry, indian_constitution_1950).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_constitution_1950__federal_asymmetry, union_integrity_project).
narrative_ontology:constraint_beneficiary(indian_constitution_1950__federal_asymmetry, central_government).
narrative_ontology:constraint_victim(indian_constitution_1950__federal_asymmetry, state_sovereignty_maximalists).
narrative_ontology:constraint_victim(indian_constitution_1950__federal_asymmetry, linguistic_minorities_before_reorganization).
narrative_ontology:constraint_victim(indian_constitution_1950__federal_asymmetry, union_territory_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNION TERRITORY RESIDENT (SNARE) — No path to statehood guaranteed; direct rule by federal administration; no elected state legislature with sovereignty; extractive relationship with federal authority is total and suppressed. Trapped by constitutional denial of electoral participation and state-level autonomy. Maximum extraction experienced by those without exit.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAXIMAL-AUTONOMY MOVEMENT (SNARE) — Suppressed by Article 368 (constitutional supremacy of union), Article 356 (emergency dissolution of state legislatures), and the doctrine that no state may secede. The constraint embeds the suppression: sovereignty claims are not merely regulated but denied as ontologically invalid. Trapped by the constitutional foreclosure of exit.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL STATE GOVERNMENT (TANGLED ROPE) — Genuine coordination function: linguistic reorganization enables local representation and cultural accommodation. But active enforcement: central override (Article 356), central financial control, appointment of governors, and constitutional veto of state legislation constrain autonomy sharply. Benefits from recognized statehood and revenue allocation; suffers from suppressed sovereignty. Mixture of coordination and extraction.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UNION GOVERNMENT (ROPE) — Benefits from strong federalism architecture. Experiences the constraint as coordination: linguistic states enable local legitimacy while union maintains integrity. Can arbitrage between granting autonomy and withdrawing it (Articles 356, 368). Net beneficiary — extraction flows toward union; suppression is a tool wielded, not a constraint experienced.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME COURT (TANGLED ROPE) — Coordinating function: adjudicates federal disputes, protects rights, reviews emergency measures. But extracted: bound by the basic structure doctrine (created by itself but now inescapable), constrained by deference to union emergency claims, and caught between reviewing and sustaining the federal asymmetry. Acts as both enforcer and victim of the constitutional architecture.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, federal asymmetry toward a strong centre is a structurally inevitable feature of holding together a continental-scale, multi-lingual, post-colonial state. No genuine alternative exists: weak federalism risked partition and disintegration, as happened in Pakistan, Yugoslavia, and the USSR. The 'strong centre with uneven edges' structure is an emergent property of the problem itself, not a choice. However, the beneficiary declarations and suppression metrics contradict the mountain classification — the engine will identify this as a false summit concealing contingent institutional choices as natural necessity.
constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_constitution_1950__federal_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_constitution_1950__federal_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_constitution_1950__federal_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(indian_constitution_1950__federal_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The union extracts significant authority from states through emergency powers, financial control, and constitutional supremacy. But extraction is not total: states retain real legislative and executive powers within their domains (concurrent and state lists). The measurement trajectory (0.38 → 0.52 over 75 years) reflects that emergency Article 356 invocations increased during the 1960s-1990s (independence cohort: 0.38; post-emergency era: 0.48; contemporary: 0.52), showing that extraction has accumulated as central authority normalized emergency intervention. Suppression (0.68): High. State sovereignty is constitutionally foreclosed — Article 368 prohibits amendments that violate the basic federal structure (established by court doctrine, not explicit text). Union territories have no guaranteed autonomy path. Secession is legally impossible. Autonomy claims are constitutionally delegitimized. However, suppression is not absolute: state governments can and do mobilize politically; linguistic reorganization created legitimate venues for regional representation; some Supreme Court interventions have constrained central override. Theater ratio (0.35): Low-to-moderate. The federal structure is a substantive architecture, not primarily performative. States exercise real powers; disputes are genuinely decided; reorganization had real consequences for governance and representation. But some theater is present: emergency powers (Article 356) can be used on contested grounds; union territory governance includes performative development claims; federalism doctrine sometimes naturalizes what are political choices.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The union government sees coordination (Rope) — linguistic states and centralized authority together solve the problem of governing continental-scale diversity. The Supreme Court sees mixed functions (Tangled Rope) — it coordinates disputes but is extracted by its own doctrines. Regional state governments see constrained autonomy (Tangled Rope) — they have real powers but face emergency vulnerability. Union territory residents see total subordination (Snare) — no autonomy path, no exit option, direct extraction. State sovereignty maximalists see constitutional foreclosure (Snare) — their sovereignty claims are legally impossible. The analytical observer risks seeing structural necessity (Mountain) — holding together a multi-lingual post-colonial state appears to require strong federal centre — but the beneficiary declarations and suppression metrics reveal this as a false summit: the union integrity beneficiary shows that asymmetry serves identifiable interests, not immutable necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The union government (institutional/arbitrage) experiences low directionality (d ≈ 0.15): it is a clear beneficiary with multiple exit options (can grant or revoke autonomy, can invoke emergency powers, can reorganize territories). Regional state governments (organized/constrained) experience moderate directionality (d ≈ 0.50): they gain benefits from recognized statehood and linguistic autonomy but lose by suppressed sovereignty and emergency vulnerability. Union territory residents (powerless/trapped) experience high directionality (d ≈ 0.90): trapped with no exit path and no autonomous governance; pure targets of central extraction. State sovereignty maximalists (powerless/trapped) experience maximum directionality (d ≈ 0.95): the constraint exists to suppress their claims; their exit (secession) is constitutionally foreclosed. The Supreme Court (institutional/constrained) experiences moderate directionality (d ≈ 0.55): it benefits from its coordinating role but is extracted by being bound to uphold emergency measures and basic structure limits of its own making.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by recognizing that the federal asymmetry reading coexists with but does not foreclose other readings of the Constitution kernel. This reading focuses on federal structure; it is consistent with reading the Constitution as having a basic structure limit (another reading), having justiciable rights (another reading), having non-justiciable directives (another reading), or having social revolution provisions (another reading). The federation reading does NOT claim that these other commitments don't exist or don't matter. Rather, it claims that whatever the Constitution's other functions are, it instantiates a federal architecture with strong centre and suppressed autonomy claims. The mandatrophy dissolves by treating each reading as a distinct constraint story, linked via the kernel, not competing for the 'true' classification. The federal asymmetry is a snare/tangled_rope at most perspectives and a mountain (false summit) at the analytical observer — this perspectival spectrum IS the resolution, not a failure of classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_necessity_vs_choice,
    'Is strong central federalism a structural necessity for holding together a multi-lingual post-colonial state, or a contingent institutional choice that shaped political outcomes?',
    'Counterfactual analysis: comparison with other post-colonial federations (Canada, Australia, Belgium, Malaysia); historical reconstruction of the Constituent Assembly debates; modeling of exit probabilities under different federalism architectures',
    'If necessity: mountain classification confirmed; asymmetry is an immutable property of the problem. If choice: snare/tangled_rope classification confirmed; asymmetry reflects power asymmetries among framers and benefited institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_necessity_vs_choice, conceptual, 'Whether strong centre federalism is structurally necessary or contingently chosen').

omega_variable(
    state_reorganization_coordination_vs_extraction,
    'Did linguistic state reorganization primarily coordinate local governance or extract autonomy by making states legible and controllable units?',
    'Historical analysis of pre-reorganization vs post-reorganization state autonomy; comparison of linguistic states'' policy freedom with their resource allocation; study of whether reorganization enabled or suppressed regional movements',
    'If coordination-dominant: tangled_rope with lower extractiveness; linguistic states are genuine beneficiaries. If extraction-dominant: snare with higher extractiveness; linguistic states are controlled units.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_reorganization_coordination_vs_extraction, empirical, 'Linguistic reorganization as coordination or control mechanism').

omega_variable(
    article_356_emergency_activation_threshold,
    'What triggers emergency dissolution of state governments under Article 356? Political disagreement or genuine constitutional breakdown?',
    'Audit of Article 356 invocations: partisan alignment analysis (how often dismissed the ruling opposition state), frequency of dismissals preceding elections, restoration timelines, Supreme Court upholding vs striking down',
    'If mostly partisan: suppression score rises to 0.80+; snare classification confirmed. If mostly genuine emergency: suppression drops to 0.40; tangled_rope with lower suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_356_emergency_activation_threshold, empirical, 'Article 356 activation patterns and partisan application').

omega_variable(
    union_territory_statehood_path_authenticity,
    'Is there a genuine path to statehood for union territories, or are territories constitutionally foreclosed from autonomy?',
    'Study of territory-to-state conversions (Goa, Himachal Pradesh, Haryana); timeline and triggers for elevation; current territories'' demonstrated statehood readiness; Supreme Court rulings on territory elevation rights',
    'If path is genuine: union territories experience constrained rather than trapped exit; classification shifts toward tangled_rope. If foreclosed: trapped classification confirmed; higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(union_territory_statehood_path_authenticity, empirical, 'Whether union territories have authentic path to statehood').

omega_variable(
    reading_contest_federal_asymmetry_vs_basic_structure,
    'Does the federal asymmetry reading foreclosed by or coexist with the basic structure reading (where courts limit what amendments can change)?',
    'Constitutional jurisprudence: does basic structure doctrine protect federalism as immutable or permit restructuring? Can an amendment abolish states? Can Parliament redraw federal boundaries? Can union territories be denied statehood indefinitely?',
    'If basic structure protects federalism: federal asymmetry is entrenched; foreclosed by judicially-enforced limits. If basic structure permits restructuring: federal asymmetry coexists with amendment-driven alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_federal_asymmetry_vs_basic_structure, conceptual, 'Interaction between federalism constraint and basic structure doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_constitution_1950__federal_asymmetry, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(ic1950_fed_be_t0, indian_constitution_1950__federal_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ic1950_fed_be_t25, indian_constitution_1950__federal_asymmetry, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(ic1950_fed_be_t75, indian_constitution_1950__federal_asymmetry, base_extractiveness, 75, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ic1950_fed_su_t0, indian_constitution_1950__federal_asymmetry, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ic1950_fed_su_t25, indian_constitution_1950__federal_asymmetry, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(ic1950_fed_su_t75, indian_constitution_1950__federal_asymmetry, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_constitution_1950__federal_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(indian_constitution_1950__federal_asymmetry, indian_constitution_1950__basic_structure_doctrine).
narrative_ontology:affects_constraint(indian_constitution_1950__federal_asymmetry, indian_constitution_1950__emergency_powers).
narrative_ontology:affects_constraint(indian_constitution_1950__federal_asymmetry, indian_states_autonomy_movements).

% DUAL FORMULATION NOTE:
% The federal asymmetry reading is one of five decomposed constraint stories from the Indian Constitution 1950 kernel. Each reading has its own extractiveness, suppression, and beneficiary/victim structure. The federal asymmetry focuses on the structural architecture (states, territories, central authority); it is downstream of the basic structure doctrine reading (which constrains how the federal structure can be amended) and upstream of specific autonomy movements (which mobilize against federal constraints). The five readings form a constraint family linked by the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_constitution_1950__federal_asymmetry, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
