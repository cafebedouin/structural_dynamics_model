% ============================================================================
% CONSTRAINT STORY: amun_priesthood_authority_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amun_priesthood_authority_substrate, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: amun_priesthood_authority_substrate
 *   human_readable: Amun Priesthood Authority Substrate at Karnak
 *   domain: ancient_religion/institutional_authority
 *
 * SUMMARY:
 *   The Amun priesthood at Karnak exemplifies how institutional authority can
 *   accumulate gradually through legitimate specialization until it becomes a
 *   binding constraint on the kernel-bearing authority itself. Over
 *   approximately 1200 years (late 18th Dynasty through the Late Period, c.
 *   1350–660 BCE), the priesthood transformed from a specialized religious
 *   function within the pharaonic administrative apparatus into a
 *   quasi-independent institutional authority structurally parallel to the
 *   pharaonic court. The priesthood accumulated three distinct forms of
 *   power: (1) land holdings through tax collection and donation, eventually
 *   controlling an estimated 20–30% of arable Egypt; (2) ritual centrality as
 *   the canonical interpreter of Amun's will, legitimating pharaonic rule
 *   through coronation and divine endorsement; and (3) operational authority
 *   over grain storage, agricultural calendar maintenance, and resource
 *   redistribution during seasonal crises. This constraint exemplifies the
 *   commitment-system pattern: a kernel-bearing authority (the pharaonic
 *   succession and divine kingship doctrine) came to depend operationally on
 *   an interpretive infrastructure (the Amun priesthood) that accumulated
 *   enough power to constrain the kernel-bearing authority's own freedom. The
 *   constraint is not pure extraction because the priesthood genuinely
 *   coordinates critical functions; it is not pure coordination because the
 *   priesthood's accumulated leverage enables extraction well above the
 *   coordination cost. The theater ratio rising from 0.32 to 0.78 reflects
 *   how the priesthood's actual operational power increasingly diverges from
 *   formal deference to pharaonic authority — by the Late Period, the
 *   priesthood maintained the theater of subordination (state processions,
 *   official inscriptions attributing authority to the pharaoh) while
 *   exercising actual control, creating a piton dynamic where performative
 *   legitimacy masks hollowed executive authority.
 *
 * KEY AGENTS:
 *   - Amun Priesthood Hierarchy: Primary beneficiary (institutional/arbitrage) — accumulates land, ritual authority, and resource control; experiences constraint as enabling and legitimate institutional specialization
 *   - Pharaonic Executive: Primary victim (powerful/constrained) — nominally sovereign but operationally constrained by priesthood's control of legitimation and resource infrastructure; trapped in identity-locked dependency on religious legitimacy
 *   - Subordinate Priesthood Members: Secondary victim (powerless/trapped) — bound by oath and ritual obligation; identity constituted through priesthood; no viable exit
 *   - Regional Temple Authorities: Tertiary victim (moderate/constrained) — benefit from centralized coordination but extract significant surplus through mandatory tribute and interpretive restrictions
 *   - Late Period Pharaohs: Degraded actor (institutional/arbitrage, piton perspective) — maintain ritual theater of authority while priesthood controls actual redistribution; authority has atrophied through institutional power accumulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amun_priesthood_authority_substrate, 0.58).
domain_priors:suppression_score(amun_priesthood_authority_substrate, 0.65).
domain_priors:theater_ratio(amun_priesthood_authority_substrate, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, extractiveness, 0.58).
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(amun_priesthood_authority_substrate, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amun_priesthood_authority_substrate, tangled_rope).
narrative_ontology:human_readable(amun_priesthood_authority_substrate, "Amun Priesthood Authority Substrate at Karnak").
narrative_ontology:topic_domain(amun_priesthood_authority_substrate, "ancient_religion/institutional_authority").

domain_priors:requires_active_enforcement(amun_priesthood_authority_substrate).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amun_priesthood_authority_substrate, 'e56fd44e-192c-4088-8edd-a795fa3d9f84').
narrative_ontology:cs_kernel_codification('e56fd44e-192c-4088-8edd-a795fa3d9f84', fixed_text).
narrative_ontology:cs_authority_grounding('e56fd44e-192c-4088-8edd-a795fa3d9f84', lineage).
narrative_ontology:cs_interpretation_layer_present('e56fd44e-192c-4088-8edd-a795fa3d9f84').
narrative_ontology:cs_reading_relation('e56fd44e-192c-4088-8edd-a795fa3d9f84', aten_heresy_cosmological_challenge, forecloses).
narrative_ontology:cs_reading_relation('e56fd44e-192c-4088-8edd-a795fa3d9f84', late_period_degraded_pharaonic_authority, influences).
narrative_ontology:cs_reading_relation('e56fd44e-192c-4088-8edd-a795fa3d9f84', distributed_temple_authority_reading, coexists_with).
narrative_ontology:cs_axiom('e56fd44e-192c-4088-8edd-a795fa3d9f84', foundational, amun_centrality_canonical_cosmology).
narrative_ontology:cs_axiom_status(amun_centrality_canonical_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('e56fd44e-192c-4088-8edd-a795fa3d9f84', amun_centrality_canonical_cosmology, theological).
narrative_ontology:cs_axiom('e56fd44e-192c-4088-8edd-a795fa3d9f84', foundational, priesthood_interpretive_monopoly_legitimacy).
narrative_ontology:cs_axiom_status(priesthood_interpretive_monopoly_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e56fd44e-192c-4088-8edd-a795fa3d9f84', priesthood_interpretive_monopoly_legitimacy, conventional).
narrative_ontology:cs_axiom('e56fd44e-192c-4088-8edd-a795fa3d9f84', secondary, accumulation_through_ritual_specialization_necessary).
narrative_ontology:cs_axiom_status(accumulation_through_ritual_specialization_necessary, overridden).
narrative_ontology:cs_axiom_grounding('e56fd44e-192c-4088-8edd-a795fa3d9f84', accumulation_through_ritual_specialization_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('e56fd44e-192c-4088-8edd-a795fa3d9f84', canonical_pharaonic_divine_kingship).
narrative_ontology:cs_drift_state('e56fd44e-192c-4088-8edd-a795fa3d9f84', late_period_institutional_shift, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e56fd44e-192c-4088-8edd-a795fa3d9f84', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amun_priesthood_authority_substrate, amun_priesthood_hierarchy).
narrative_ontology:constraint_victim(amun_priesthood_authority_substrate, pharaonic_executive_authority).
narrative_ontology:constraint_victim(amun_priesthood_authority_substrate, resource_redistribution_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE PRIESTHOOD MEMBER (SNARE) — Trapped within hierarchical obedience to senior priests; career and identity are constituted through the priesthood; exit means abandoning religious vocation and social position entirely. The member experiences maximum suppression through oath-binding, ritual obligation, and social isolation from non-priestly populations. Extraction flows upward through labor obligation and ritual prerogative without meaningful coordination benefit.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL TEMPLE AUTHORITY (TANGLED ROPE) — Temples outside Karnak benefit from the centralized Amun authority structure for resource coordination and ritual legitimacy, but are constrained by mandatory tribute, restricted independent ritual innovation, and subordination to the central hierarchy. They see genuine coordination functions (shared ritual calendar, resource pooling across droughts) alongside asymmetric extraction (Karnak keeps surplus, controls canonical interpretation).
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENIOR PRIESTHOOD COUNCIL (ROPE) — Net beneficiaries experiencing the constraint as coordination of religious function. They marshal land holdings, labor, and ritual centrality to solve collective-action problems (harvest coordination, flood management through ritual calendar, wealth stability). The institutional structure appears to them as enabling legitimate specialization and resource aggregation. High agency, exit via gradual power transfer to successors.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARAONIC EXECUTIVE (TANGLED ROPE) — The pharaoh retains nominal authority but faces binding constraints on actual redistribution capacity and military mobilization. The Amun priesthood coordinates essential functions (religious calendar, land tenure records, resource storage) that the pharaoh's apparatus needs, but the priesthood's accumulated leverage means exit is costly — destabilizing the priesthood risks collapse of the administrative substrate. The pharaoh experiences both coordination benefit (delegated land management, ritual legitimation of rule) and severe extraction (wealth drainage, constraint on independent economic policy).
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEGRADED ROYAL AUTHORITY AT LATE PERIOD (PITON) — By the Late Period (post-700 BCE), pharaonic authority had substantially atrophied as an executive force. The priesthood maintained formal deference to the pharaoh through ritual theater (state processions, coronation ceremonies, official inscriptions crediting pharaonic authority) while exercising actual redistribution control. The piton classification reflects the performative maintenance of pharaonic authority even as its operative power has been hollowed out.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, institutional authority structures accumulate interpretive and operational power through long cycles — this is inherent to how legitimacy-bearing institutions function. Ritual centrality compounds across generations; land holdings compound through tax collection; interpretive authority compounds through canonical text control. The constraint appears as a natural law of institutional dynamics. However, the structural data reveals beneficiaries (priesthood) and victims (pharaonic authority, resource function), suggesting this is a false summit naturalization.
constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amun_priesthood_authority_substrate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amun_priesthood_authority_substrate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amun_priesthood_authority_substrate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amun_priesthood_authority_substrate, TR),
    TR >= 0.70.

:- end_tests(amun_priesthood_authority_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the priesthood's ability to channel surplus toward their own hierarchy while maintaining coordination of essential functions. The value increased substantially over the 1200-year interval (from 0.15 to 0.65), modeling the rent-seeking pattern: initial extraction was genuinely functional (coordination surplus ≤ cost), but later extraction accumulated extractive overhead above functional necessity. Suppression (0.65): Moderate-high. The priesthood maintained organizational suppression through oath-binding, hierarchical obedience structures, and threat to religious status (excommunication, loss of burial rites). However, suppression was not total — the priesthood could not physically coerce the pharaoh's court or prevent the rise of competing religious centers. The suppression operated through legitimacy dependency rather than military force, making it cognitively sustainable rather than externally visible. Theater ratio (0.68): Moderate-high and rising. The constraint's performative content increased as the priesthood's actual power grew — formal deference to pharaonic authority intensified precisely as executive pharaonic power atrophied. Late Period state processions, coronation ceremonies, and official royal decrees crediting the pharaoh with Amun's blessing were largely theatrical; the priesthood controlled the actual ritual meaning and could withdraw legitimation if the pharaoh overstepped. The rise in theater from 0.32 to 0.78 marks the transition from functional authority (priesthood's power roughly matched its operational necessity) to piton institutional inertia (priesthood maintains formal structure even as the underlying executive function has shifted).
 *
 * PERSPECTIVAL GAP:
 *   The most striking perspectival gap is between the Senior Priesthood Council (rope perspective) and the Pharaonic Executive (tangled rope perspective). The priesthood experiences the same institutional structure as enabling coordination — delegating land management, coordinating harvest timing through ritual calendar, stabilizing wealth storage — exactly the legitimate specialization function that justifies their authority. The pharaoh, constrained by the priesthood's control of religious legitimation and unable to exit without destabilizing the entire administrative apparatus, experiences the same structure as extraction. Both perspectives are structurally accurate: the priesthood IS coordinating essential functions (coordination is real), AND the priesthood IS extracting surplus (extraction is real). The gap reveals that the constraint is neither pure coordination nor pure extraction, but a hybrid where the institutional structure serves both functions simultaneously. The Degraded Royal Authority perspective (piton) adds a temporal dimension: the pharaonic authority that once balanced priesthood power has itself become performative, suggesting the constraint is in a late-stage trajectory where the kernel-bearing authority (pharaonic rule) is being hollowed out by the interpretive infrastructure (priesthood) that was supposed to serve it.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic executive's directionality is the most complex case in this story. The pharaoh is nominally powerful (institutional power atom) with ostensibly high exit options (could theoretically appoint different priests, shift administrative centers, embrace alternative religions). However, the pharaoh is identity_locked on religious legitimacy — pharaonic rule was constituted through Amun's endorsement, and abandoning that legitimacy means ceasing to be a pharaoh in the Egyptian cultural frame. This identity lock makes the pharaoh's structural mobility irrelevant: they cannot exercise their nominal exit options without becoming a different person (a tyrant rather than a divinely-ordained ruler). The derivation chain yields d ≈ 0.70 for this agent (powerful + constrained + identity_locked + victim status), producing moderate-to-high experienced extraction. The priesthood's directionality is low (institutional + arbitrage + beneficiary status → d ≈ 0.15), producing minimal or negative experienced extraction (they see the constraint as enabling). The subordinate priesthood member's directionality is maximum (powerless + trapped + victim status → d ≈ 0.95), producing the maximum experienced extraction in the snare perspective. The regional temple authority's directionality is moderate (moderate + constrained + victim status → d ≈ 0.65), producing moderate extraction. These derivations explain the perspectival gaps: the same institutional structure yields radically different experienced extractiveness depending on the agent's structural position and constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through commitment-system structure: the pharaonic authority is the kernel-bearing power (the source of cosmic legitimacy, the point where divine will enters the political order), but the Amun priesthood is the interpretive infrastructure (the means by which that cosmic legitimacy is adjudicated and communicated). Over time, the infrastructure accumulated enough operational power to constrain the kernel-bearing authority itself. The constraint is tangled rope because the priesthood genuinely coordinates essential functions (the coordination half explains why the pharaoh doesn't simply eliminate them) while also extracting rent (the extraction half explains why the pharaoh, if they could exit cheaply, would do so). The false summit (mountain perspective) naturalizes this dynamic as inevitable institutional mathematics, but the structural data reveals it as a contingent historical outcome: it required (1) long institutional continuity (1200+ years), (2) integrated legitimacy (religious authority = political authority), and (3) pharaonic succession weakening. Other institutional structures might have prevented this accumulation. The mandatrophy is resolved by recognizing that mandatrophy itself — the question 'is this coordination or extraction?' — is exactly what the commitment system embodies: the priesthood IS coordinating, AND IS extracting, and the boundary between legitimate specialization and parasitic accumulation is precisely where the political struggle between pharaonic and priestly authority occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntarism_vs_coercion_mechanism,
    'Was the priesthood''s authority accumulation driven by pharaonic delegation (rational institutional choice) or by coercive extraction that pharaohs were too weak to resist?',
    'Epigraphic analysis of royal decree language (voluntary delegation vs necessity language); correlation between priesthood land acquisition and pharaonic military capacity; comparison of priestly growth rates during strong vs weak pharaonic regimes',
    'If voluntary delegation: constraint is Rope from pharaonic perspective (coordination). If coercive extraction: constraint is Snare (pharaoh is trapped by necessity to maintain religious apparatus). Classification hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntarism_vs_coercion_mechanism, empirical, 'Whether priesthood authority was delegated voluntarily or extracted coercively').

omega_variable(
    functional_essentiality_boundary,
    'How much of the Amun priesthood''s operational authority was genuinely required for agricultural coordination, resource storage, and ritual calendaring versus how much represented extractive surplus above functional necessity?',
    'Comparative institutional analysis: identify minimum authority structure needed for documented coordination functions; measure observed priesthood apparatus size against this minimum; estimate extractive overhead',
    'If functional overlay is small (<15%): constraint is primarily Rope. If substantial (>40%): constraint is primarily Snare. Boundary determines whether the priesthood is best modeled as legitimate specialization or parasitic accumulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_essentiality_boundary, conceptual, 'Functional necessity vs extractive surplus in priesthood authority').

omega_variable(
    natural_law_false_summit,
    'Is institutional power accumulation through interpretive authority a law of institutional dynamics, or is it a contingent outcome of specific Egyptian political conditions (weak pharaonic succession, integrated land/religious legitimacy, long institutional continuity)?',
    'Comparative study of other ancient priesthoods (Greek temple networks, Babylonian clergy, Hebrew temple authorities): do they show equivalent accumulation under different political conditions? Counterfactual: what would have prevented priesthood power accumulation in Egypt?',
    'If universal law: mountain classification is correct. If contingent: constraint is tangled_rope throughout, and the mountain perspective is a false summit naturalizing extractive institutional dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit, conceptual, 'Whether institutional power accumulation is universal law or contingent outcome').

omega_variable(
    ritual_legitimacy_dependency_trap,
    'Did the pharaoh remain dependent on Amun priesthood legitimation for maintaining rule, and did this dependency trap the pharaoh into accepting extraction?',
    'Analysis of pharaonic legitimation narratives: correlation between pharaonic authority claims and Amun priesthood endorsement; examination of pharaohs who attempted to bypass priesthood (Akhenaten, later Roman emperors); assessment of religious authority alternatives available to pharaohs',
    'If high dependency: pharaonic authority is trapped (identity_locked on legitimacy through Amun); constraint is tangled_rope with identity_locked exit for pharaoh. If alternatives existed: pharaonic constraint is constrained rather than trapped, opening exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_legitimacy_dependency_trap, empirical, 'Pharaonic dependency on Amun priesthood legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amun_priesthood_authority_substrate, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amun_priest_tr_t0, amun_priesthood_authority_substrate, theater_ratio, 0, 0.32).
narrative_ontology:measurement(amun_priest_tr_t3, amun_priesthood_authority_substrate, theater_ratio, 3, 0.42).
narrative_ontology:measurement(amun_priest_tr_t6, amun_priesthood_authority_substrate, theater_ratio, 6, 0.55).
narrative_ontology:measurement(amun_priest_tr_t9, amun_priesthood_authority_substrate, theater_ratio, 9, 0.68).
narrative_ontology:measurement(amun_priest_tr_t12, amun_priesthood_authority_substrate, theater_ratio, 12, 0.78).

% Extraction over time
narrative_ontology:measurement(amun_priest_be_t0, amun_priesthood_authority_substrate, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(amun_priest_be_t3, amun_priesthood_authority_substrate, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(amun_priest_be_t6, amun_priesthood_authority_substrate, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(amun_priest_be_t9, amun_priesthood_authority_substrate, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(amun_priest_be_t12, amun_priesthood_authority_substrate, base_extractiveness, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amun_priesthood_authority_substrate, resource_allocation).
narrative_ontology:boltzmann_floor_override(amun_priesthood_authority_substrate, 0.22).
narrative_ontology:affects_constraint(amun_priesthood_authority_substrate, pharaonic_succession_continuity).
narrative_ontology:affects_constraint(amun_priesthood_authority_substrate, late_period_state_fragmentation).

% DUAL FORMULATION NOTE:
% The Amun priesthood authority substrate is upstream of pharaonic succession continuity — the priesthood's control of legitimation became a constraint on which pharaonic successors could maintain rule. It is also upstream of Late Period state fragmentation — as pharaonic authority atrophied relative to priesthood power, the state apparatus fragmented into competing institutional authorities (priesthood, military, regional temples), preventing unified action. These three constraints form a family: the priesthood authority story explains how institutional power accumulation occurs; the succession continuity story explains how that power constrains the pharaonic line; the state fragmentation story explains the downstream political consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amun_priesthood_authority_substrate, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
