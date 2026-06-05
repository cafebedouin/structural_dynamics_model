% ============================================================================
% CONSTRAINT STORY: federal_fiction__nationality_form_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_fiction__nationality_form_reading, []).

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
 *   constraint_id: federal_fiction__nationality_form_reading
 *   human_readable: Federal Fiction: Nationality Form Reading (USSR Federal Structure as Identity Institutionalization)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Soviet federal structure instantiated a paradox: republics with
 *   titular nations, official languages, and national cadres were
 *   simultaneously decorative (all real power flowed through Moscow's
 *   all-union ministries, the unified party, and the planning apparatus) and
 *   functional (they provided institutional scaffolding for titular national
 *   identity and, crucially, became genuine exits in 1991 when the center
 *   collapsed). This constraint describes the federal forms from the
 *   perspective of those who experienced them as enabling national
 *   institutionalization — the reading that the forms 'did real identity
 *   work' despite being contained within a unitary command structure. The
 *   extractiveness of the forms became payable in 1991: the decorative
 *   republics became actual states, the nominal right of secession (Article
 *   17) became enforceable, and the titular nations found their institutional
 *   infrastructure already in place. The constraint exhibits the full range
 *   of DR types depending on structural position: the titular elites
 *   experienced pure coordination (Rope), the all-union center experienced
 *   mixed coordination-extraction (Tangled Rope), the supranational merger
 *   vision experienced pure extraction (Snare), and non-titular minorities
 *   experienced asymmetric institutional disadvantage (Snare). The
 *   measurement trajectory shows increasing suppression (rising enforcement
 *   intensity as nationalist sentiment grew) and rising theater ratio
 *   (federal structures became increasingly performative as their real
 *   functionality contradicted the unitary ideology).
 *
 * KEY AGENTS:
 *   - Titular Elites: Primary beneficiary (institutional/arbitrage) — reproduce national identity through republican institutions, national cadres, titulary language policies
 *   - Codified National Identities: Primary beneficiary (institutional/arbitrage) — institutionalized through federal structure, national territories, official languages
 *   - Supranational Merger Vision: Primary victim (powerless/identity_locked) — the vision of Soviet people as unified socialist nation undermined by the very federal categories meant to contain nationalism; identity-locked because internationalism internalized federal categories
 *   - Non-Titular Minorities: Secondary victim (powerless/trapped) — trapped within federal structure privileging titular nations, asymmetric citizenship in practice despite formal equality
 *   - All-Union Center: Mixed actor (organized/arbitrage) — coordinates through federal forms while extracting control through unified party and planning apparatus; benefits from decorative nature of borders
 *   - Soviet Intellectual Class: Mixed actor (moderate/constrained) — constrained by conformity requirements but benefits from federal structure's legitimacy and career pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable feature of multinational governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_fiction__nationality_form_reading, 0.58).
domain_priors:suppression_score(federal_fiction__nationality_form_reading, 0.68).
domain_priors:theater_ratio(federal_fiction__nationality_form_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_fiction__nationality_form_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federal_fiction__nationality_form_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federal_fiction__nationality_form_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_fiction__nationality_form_reading, tangled_rope).
narrative_ontology:human_readable(federal_fiction__nationality_form_reading, "Federal Fiction: Nationality Form Reading (USSR Federal Structure as Identity Institutionalization)").
narrative_ontology:topic_domain(federal_fiction__nationality_form_reading, "political/legal/constitutional").

domain_priors:requires_active_enforcement(federal_fiction__nationality_form_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_fiction__nationality_form_reading, 'f659f2b8-132d-4319-86ad-bee0de6f814e').
narrative_ontology:cs_kernel_codification('f659f2b8-132d-4319-86ad-bee0de6f814e', fixed_text).
narrative_ontology:cs_authority_grounding('f659f2b8-132d-4319-86ad-bee0de6f814e', extraction).
narrative_ontology:cs_interpretation_layer_present('f659f2b8-132d-4319-86ad-bee0de6f814e').
narrative_ontology:cs_reading_relation('f659f2b8-132d-4319-86ad-bee0de6f814e', federal_fiction__centralized_reality_reading, coexists_with).
narrative_ontology:cs_reading_relation('f659f2b8-132d-4319-86ad-bee0de6f814e', federal_fiction__secession_dead_letter_reading, coexists_with).
narrative_ontology:cs_axiom('f659f2b8-132d-4319-86ad-bee0de6f814e', foundational, federal_forms_institutionalized_nationality).
narrative_ontology:cs_axiom_status(federal_forms_institutionalized_nationality, holdable).
narrative_ontology:cs_axiom_grounding('f659f2b8-132d-4319-86ad-bee0de6f814e', federal_forms_institutionalized_nationality, empirically_contingent).
narrative_ontology:cs_axiom('f659f2b8-132d-4319-86ad-bee0de6f814e', foundational, nationality_institutionalization_enabled_1991_exits).
narrative_ontology:cs_axiom_status(nationality_institutionalization_enabled_1991_exits, holdable).
narrative_ontology:cs_axiom_grounding('f659f2b8-132d-4319-86ad-bee0de6f814e', nationality_institutionalization_enabled_1991_exits, empirically_contingent).
narrative_ontology:cs_reference_frame('f659f2b8-132d-4319-86ad-bee0de6f814e', federal_forms_as_enabling_structure).
narrative_ontology:cs_drift_state('f659f2b8-132d-4319-86ad-bee0de6f814e', contemporary_post_1991, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f659f2b8-132d-4319-86ad-bee0de6f814e', '').
narrative_ontology:cs_kernel_id(federal_fiction__nationality_form_reading, federal_fiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_fiction__nationality_form_reading, titular_elites).
narrative_ontology:constraint_beneficiary(federal_fiction__nationality_form_reading, codified_national_identities).
narrative_ontology:constraint_victim(federal_fiction__nationality_form_reading, supranational_merger_vision).
narrative_ontology:constraint_victim(federal_fiction__nationality_form_reading, non_titular_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-TITULAR MINORITY (SNARE) — Trapped within a federal structure that privileges titular nations and codifies their languages as official. Cannot exit the frame without losing institutional identity recognition. The federal form creates asymmetric citizenship categories in practice even while denying them on paper. Maximum extraction: institutional identity is both the cage and the only vehicle for claiming rights.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TITULAR ELITE (ROPE) — Experiences the federal form as pure coordination: the republics, titular languages, and national cadres provide institutional scaffolding for their political power and cultural reproduction. The federation enables their rule at the republican level while containing it within the union framework. Net beneficiary — the form legitimizes their authority and institutionalizes their nationality.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SOVIET INTELLECTUAL CLASS (TANGLED ROPE) — Constrained by the need for ideological conformity and all-union cultural authority, but also benefits from the federal structure's legitimacy as a framework for socialist modernization. Experiences the federal form as both enabling (national institutions provide career pathways) and extractive (suppression of non-sanctioned national movements). Moderate power with constrained exits due to career dependency on the union system.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPRANATIONAL MERGER VISION (SNARE) — Trapped by the institutionalization of national difference within federal forms. The vision of the Soviet people as a unified socialist nation was systemically undermined by the very federal structures meant to contain nationalism. Identity-locked: Soviet internationalism internalized the federal categories it was supposed to transcend. No organizational capacity to resist the codification of nationality at the institutional level. The form itself became the barrier to merger.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALL-UNION CENTER (TANGLED ROPE) — Coordinated with republics through the federal form while extracting control through the unified party, planning apparatus, and security services. The all-union center benefited from the decorative nature of federal borders (real administrative power flowed through Moscow) while maintaining the republican institutions that kept titular elites invested in the system. Extraction disguised as coordination; coordination enabled by the form's ambiguity.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, federal structures that institutionalize nationality are seen as immutable features of governance in multinational states: any attempt to create supranational unity while maintaining territorial organization necessarily produces this form. This perspective naturalizes what is structurally a contingent institutional choice. The engine will identify this as a false summit: the federal fiction was neither inevitable nor immutable — it was a specific choice to institutionalize nationality within a framework designed to contain it.
constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_fiction__nationality_form_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_fiction__nationality_form_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federal_fiction__nationality_form_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federal_fiction__nationality_form_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The federal forms extract in multiple ways: they suppress non-titular nationalism through privileging titular languages and cadres; they contain nationalist sentiment within institutional boundaries; they provide legitimacy to all-union control over republican policy. But extraction is not maximal because the forms genuinely do enable some degree of national institutionalization and provide real benefits to titular elites. The trajectory shows rising extractiveness as nationalist sentiment accumulates — the forms become increasingly extractive as they are required to do more suppressive work. Suppression (0.68): High. The federal structure suppresses supranational merger visions by institutionalizing national difference; suppresses non-titular minorities through asymmetric institutional privilege; suppresses nationalist movements by containing them within nominal 'union' framework. The trajectory shows rising suppression intensity over time as enforcement machinery must work harder to contain nationalist currents. Theater ratio (0.65): High-moderate. The federal forms are substantially performative: the illusion of republican autonomy and national self-determination masks unitary control through the party and planning apparatus. The theater ratio rises as the gap between formal federal structure and actual unitary control grows over time. Claimed type (Tangled Rope): The constraint has both genuine coordination (the forms do enable titular national institutionalization and republican-level governance) and asymmetric extraction (suppression of nationalism, containment within unitary framework, asymmetric privilege for titulary nations). The presence of beneficiaries (titular elites), victims (supranational vision, non-titular minorities), and active enforcement (nationality policy, cadre selection) satisfy the Tangled Rope gate.
 *
 * PERSPECTIVAL GAP:
 *   The Titular Elite (Rope) and the All-Union Center (Tangled Rope) experience sharply different extractions from the same structural form. For the titular elite, the federal structure enables their rule — it is coordination. For the all-union center, the structure enables control while requiring performative respect for nominally federal boundaries — it is mixed. The Supranational Merger Vision (Snare, identity_locked) experiences the forms as pure extraction because they institutionalize the very national boundaries meant to dissolve into socialist unity. The analytical observer (Mountain) risks naturalizing this as immutable — that any multinational federal system must institutionalize nationality — when it was actually a specific historical choice to create that particular structural configuration. The gap between the Rope perspective (beneficiary who experiences coordination) and the Snare perspective (victim who experiences pure extraction) reveals that the forms simultaneously enabled and suppressed, coordinated and extracted, depending entirely on one's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Titular elites derive low d (0.1–0.2) because they are beneficiaries with arbitrage options — they can shift resources and power between republican and all-union levels. The all-union center derives moderate d (0.4–0.5) because it benefits from the form (coordination function) while using it for extraction (control). The supranational merger vision derives very high d (0.92–0.98) because it is trapped (exit would require abandoning internationalism itself, which is identity-locked into the framework) and fully targeted by the forms. Non-titular minorities derive high d (0.85–0.95) because they are trapped within asymmetric federal structures privileging titulary nations. The Soviet intellectual class derives moderate d (0.55–0.65) because they experience both benefit (career pathways, institutional legitimacy) and cost (conformity requirements, suppression of heterodox nationalism). These d values feed the sigmoid f(d) to compute experienced extractiveness χ for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that the federal forms had genuine dual functionality: they were simultaneously decorative (from the all-union center's perspective) and functional (from the titular elite's perspective). The forms did real identity work — they institutionalized nationality, provided administrative scaffolding for national states, codified languages, created cadre hierarchies structured by nationality. But they did this work *within* a unitary command structure that gave Moscow final authority over all decisions. The 1991 collapse reveals the truth: the forms were functional enough that when the unitary center lost enforcement capacity, they immediately became independent states. If they had been purely decorative, the titular elites would not have had viable state structures to exit into. The mandatrophy — is this Rope (pure coordination) or Snare (pure extraction) or Tangled Rope (mixed)? — is resolved by recognizing that it is all three simultaneously, from different perspectives. The forms coordinated national institutionalization for titular elites. They extracted from the supranational vision. They mixed extraction and coordination from the all-union center's view. The presheaf of perspectives *is* the answer; no single type captures the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_functionality_ambiguity,
    'Did the federal forms genuinely enable titular nations to institutionalize identity, or did they primarily function as decorative boundaries masking unitary control?',
    'Comparative institutional analysis: examination of decision-making authority in republican legislatures vs. all-union ministries; analysis of titular language policy enforcement; tracking of national cadre promotion pathways vs. all-union nomenklatura control',
    'If forms were genuinely functional: classification shifts toward Rope for titular elites and lower extraction overall. If decorative: classification remains Tangled Rope/Snare and extraction metrics increase. The 1991 exit reveals the answer — the forms became functional when all-union control collapsed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(form_functionality_ambiguity, empirical, 'Whether federal forms genuinely enabled national institutionalization or merely decorated unitary control').

omega_variable(
    internationalism_capture_mechanism,
    'Was Soviet internationalism genuinely a supranational vision that federal forms undermined, or was it always a cover story for Russian-dominated unification?',
    'Historical analysis of internationalism discourse vs. practice; examination of which nations benefited from all-union programs; analysis of cultural policies favoring Russian language and culture',
    'If internationalism was genuine vision: supranational merger vision is correctly identified as victim. If always Russian expansionism: the victim category is misframed, and extraction metrics should reflect cultural assimilation rather than national suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internationalism_capture_mechanism, conceptual, 'Whether Soviet internationalism was genuine supranational vision or cover for Russification').

omega_variable(
    nationalist_elite_consciousness,
    'To what degree did titular elites consciously use federal forms to institutionalize national identity, versus unconsciously benefiting from structural incentives toward nationality?',
    'Analysis of pre-1991 nationalist dissident thought; examination of republican elite policy choices that hardened national boundaries; post-1991 trajectories revealing which elites had prepared exit strategies',
    'If conscious strategy: titular elites are full beneficiaries with agency. If structural unconsciousness: beneficiary relationship is more passive, and the forms are better understood as structural attractor than deliberate choice. Affects directionality: conscious beneficiary has higher d (more intentional extraction) than unconscious beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_elite_consciousness, conceptual, 'Degree to which titular elites consciously weaponized federal forms versus structurally benefiting').

omega_variable(
    reading_vs_centralized_reality,
    'Does this reading that ''federal forms did real identity work'' coexist logically with the centralized reality reading that all power flowed through Moscow''s all-union ministries, or do they foreclose each other?',
    'Structural analysis: the readings coexist if federal forms could simultaneously be decorative (from center''s perspective) and functional (from titular elite''s perspective). They foreclose if the all-union power monopoly made federal functionality impossible.',
    'If coexist: both readings remain live perspectives on the same constraint. If foreclose: one reading''s core premise rules out the other, indicating a fundamental disagreement about the constraint''s nature. The 1991 collapse suggests coexistence — the forms were decorative from Moscow''s view but became functional exits when central control dissolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_centralized_reality, conceptual, 'Logical relationship between nationality form reading and centralized reality reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_fiction__nationality_form_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federal_fiction__nationality_form_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fede_tr_t20, federal_fiction__nationality_form_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(fede_tr_t40, federal_fiction__nationality_form_reading, theater_ratio, 40, 0.72).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federal_fiction__nationality_form_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fede_be_t20, federal_fiction__nationality_form_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(fede_be_t40, federal_fiction__nationality_form_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federal_fiction__nationality_form_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t20, federal_fiction__nationality_form_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fede_su_t40, federal_fiction__nationality_form_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_fiction__nationality_form_reading, identity_coordination).
narrative_ontology:affects_constraint(federal_fiction__nationality_form_reading, federal_fiction__centralized_reality_reading).
narrative_ontology:affects_constraint(federal_fiction__nationality_form_reading, federal_fiction__secession_dead_letter_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest about the nature of Soviet federalism. All three readings share structural elements (federal forms, titular nationalism, all-union control) but have fundamentally different ε values reflecting different premises about what those forms accomplished: centralized_reality_reading (ε≈0.35, decorative borders masking unitary control) emphasizes the infrastructure of Moscow power; secession_dead_letter_reading (ε≈0.72, suppression of exit right) emphasizes Article 17's non-functionality as pure extraction mechanism; nationality_form_reading (ε=0.58) emphasizes the real institutional work the forms did toward national identity. These are not three measurements of the same constraint — they are three different constraints sharing a kernel. The network links them to show they are in logical relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_fiction__nationality_form_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
