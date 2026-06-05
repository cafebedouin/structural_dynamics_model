% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Divine Legitimacy via Amun-Ra Polytheistic Priesthood
 *   domain: religious_studies/political_economy/ancient_history
 *
 * SUMMARY:
 *   Ancient Egypt's religious legitimacy flows through established priestly
 *   interpretation of a polytheistic cosmology centered on Amun-Ra as chief
 *   patron. This reading instantiates one specific institutional
 *   configuration: priesthoods maintain interpretive monopoly over the
 *   theology; Amun-Ra hierarchically encompasses regional deities; the
 *   pharaoh's legitimacy depends on priestly validation; temple economies
 *   expand through the coordination function of unified orthopraxy. This is
 *   structurally distinct from two sibling readings: (1)
 *   atenist_monotheistic_reading, where Akhenaten and the priesthood of Aten
 *   attempted to establish monotheistic authority (Amun-Ra framework
 *   forecloses this by making polytheism the natural order); (2)
 *   folk_syncretistic_reading, where autonomous local cults resist
 *   subordination to Amun-Ra hierarchy and blend deities into syncretic
 *   hybrids (the polytheistic reading suppresses this by redefining
 *   syncretism as theological error, not valid local interpretation). The
 *   polytheistic Amun-Ra framework exhibits the tangled-rope structure:
 *   priesthood coordinates Egypt's religious diversity through hierarchical
 *   theology (genuine coordination function), while simultaneously extracting
 *   power, resources, and interpretive monopoly (asymmetric extraction). The
 *   pharaoh benefits from the unified legitimacy (coordination gain) but is
 *   constrained by priestly veto power (extraction cost). Regional traditions
 *   benefit from inclusion in the state-sponsored cosmology but lose autonomy
 *   (snare structure). Folk practitioners are trapped outside the orthodox
 *   framework (snare structure). The measurement trajectory shows increasing
 *   extractiveness and suppression over two centuries: as the Amun-Ra
 *   framework solidifies, theater ratio (performative ritual content) rises,
 *   suppression requirements increase (folk traditions must be more actively
 *   suppressed as they persist), and base extractiveness drifts upward (the
 *   priesthood extracts more resources as its interpretive monopoly
 *   stabilizes). This measurement pattern is diagnostic of institutional
 *   capture — a coordination mechanism (polytheistic ordering) transforms
 *   into an extraction mechanism (priestly monopoly) as institutional inertia
 *   accumulates.
 *
 * KEY AGENTS:
 *   - Amun-Ra Priesthood: Primary beneficiary (institutional/arbitrage) — controls interpretive authority, temple landholdings, ritual monopoly; gains from framework that legitimates their power and expands temple economies.
 *   - Regional Temple Networks: Secondary beneficiary (organized/constrained) — benefit from Amun-Ra framework providing centralized legitimacy and pilgrimage flows; constrained by requirement to harmonize local deities with Amun-Ra hierarchy.
 *   - Pharaonic Authority: Powerful/constrained (powerful/constrained) — benefits from priestly validation and unified religious order; constrained by priestly veto over legitimacy.
 *   - Folk Syncretic Practitioners: Primary victim (powerless/trapped) — excluded from orthodox framework, face suppression, cannot maintain autonomous shrine traditions without state sanction withdrawal.
 *   - Regional Deity Traditions: Secondary victim (powerless/trapped) — pre-existing autonomous cults subordinated to Amun-Ra hierarchy, lose independent cosmological status.
 *   - Analytical Observer: Universal/civilizational view (analytical/analytical) — risks naturalizing the polytheistic framework as the inevitable way to organize religious diversity, when it is a specific institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.48).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.52).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Divine Legitimacy via Amun-Ra Polytheistic Priesthood").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious_studies/political_economy/ancient_history").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '045550fc-b0cc-41b4-8907-60dd97b326fa').
narrative_ontology:cs_kernel_codification('045550fc-b0cc-41b4-8907-60dd97b326fa', fixed_text).
narrative_ontology:cs_authority_grounding('045550fc-b0cc-41b4-8907-60dd97b326fa', lineage).
narrative_ontology:cs_interpretation_layer_present('045550fc-b0cc-41b4-8907-60dd97b326fa').
narrative_ontology:cs_reading_relation('045550fc-b0cc-41b4-8907-60dd97b326fa', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('045550fc-b0cc-41b4-8907-60dd97b326fa', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('045550fc-b0cc-41b4-8907-60dd97b326fa', foundational, polytheistic_hierarchy_natural).
narrative_ontology:cs_axiom_status(polytheistic_hierarchy_natural, holdable).
narrative_ontology:cs_axiom_grounding('045550fc-b0cc-41b4-8907-60dd97b326fa', polytheistic_hierarchy_natural, instrumental).
narrative_ontology:cs_axiom('045550fc-b0cc-41b4-8907-60dd97b326fa', foundational, priestly_interpretive_mediation_required).
narrative_ontology:cs_axiom_status(priestly_interpretive_mediation_required, holdable).
narrative_ontology:cs_axiom_grounding('045550fc-b0cc-41b4-8907-60dd97b326fa', priestly_interpretive_mediation_required, deontological).
narrative_ontology:cs_reference_frame('045550fc-b0cc-41b4-8907-60dd97b326fa', amun_ra_hierarchical_polytheism).
narrative_ontology:cs_drift_state('045550fc-b0cc-41b4-8907-60dd97b326fa', temple_institutionalization_intensification_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('045550fc-b0cc-41b4-8907-60dd97b326fa', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_interpretive_authority).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_rule_dependent_on_divine_validation).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretic_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_deity_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRIEST (SNARE) — Local practitioners bound to Amun-Ra orthopraxy despite prior autonomous shrine traditions. Cannot maintain independent cult without losing legitimacy access, state protection, and ritual material support. Suppression is primarily structural: state enforcement via temple hierarchy and resource monopoly. Full extraction experienced because exit (maintaining autonomous local shrine) triggers state sanction.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL DEITY AUTONOMY (SNARE) — Pre-existing regional cults (e.g., Sobek in Fayum, Khnum at Elephantine) are hierarchically subordinated to Amun-Ra cosmology. Pressured to accept incorporation as manifestations of Ra, or lose state temple patronage and pilgrimage traffic. No exit pathway: rejection of Amun-Ra polytheistic framework results in marginalization. Experienced extraction is severe because the constraint redefines the victim's own theological autonomy.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPLE ESTATE SYSTEM (ROPE) — Beneficiary. Amun-Ra priesthood controls vast landholdings, agricultural surplus, and ritual monopoly. The polytheistic reading (many-god framework with clear hierarchy) legitimates distributed temple economies across Egypt while maintaining centralized priestly interpretive authority. Sees the constraint as coordination: managing Egypt's religious diversity through hierarchical theological ordering. Net benefit because extraction is minimal from their position; they experience the constraint as enabling their institutional expansion.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARAONIC AUTHORITY (TANGLED ROPE) — Powerful but structurally constrained. Pharaoh benefits from priestly validation (divine legitimacy) and from the coordinating function of Amun-Ra orthopraxy (religious unity reduces rebellion risk). But pharaoh is constrained by priestly veto power: priesthood can withdraw ritual sanction, leading to legitimacy collapse. Extraction flows toward the pharaoh (benefits from unified cosmology), but pharaoh pays a price (must accommodate priestly interests, cannot unilaterally redefine the cosmology). This is the classic tangled-rope structure: genuine coordination (shared interest in stable theology) paired with asymmetric extraction (priesthood controls the legitimacy supply).
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTER-REGIONAL PRIESTLY NETWORKS (TANGLED ROPE) — Organized bodies of interpreters maintain distributed authority over theology across temples. Benefit from Amun-Ra framework (legitimates their interpretive role, increases pilgrimage, centralizes ritual authority in priestly class). Constrained by need to accommodate regional variation (cannot impose pure standardization without triggering local resistance). Extraction embedded in coordination: the requirement that priesthood interpret and harmonize regional deities with Amun-Ra serves both priestly interests (maintains interpretive monopoly) and genuine coordination (manages religious diversity).
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Civilizational perspective risks reading the Amun-Ra framework as an immutable natural law: divine order is hierarchical, polytheistic systems require authoritative interpretation, priestly mediation is necessary for social stability. From this view, the constraint appears structurally inevitable — how else could ancient states manage religious diversity? However, the structural data reveals this as a false summit: specific beneficiaries (temples, priesthood, pharaonic legitimacy), specific victims (folk practitioners, regional autonomy), and active enforcement (priestly validation requirements) indicate a constructed institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__amun_polytheistic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The Amun-Ra priesthood extracts substantial institutional benefits (interpretive monopoly, land control, ritual resource flows) from the framework, but the extraction is not maximal because there is genuine coordination function present: the polytheistic hierarchy does solve the problem of managing Egypt's diverse regional deities without violence. The beneficiaries (priesthood, pharaonic authority) actively use the coordination function; they are not pure rent-seekers. However, the extraction component is real and substantial: priesthood gains power well beyond what minimal coordination would require. The 0.48 value reflects that this is a hybrid constraint: genuine coordination paired with significant asymmetric extraction. Suppression (0.52): Moderate-high. Suppression operates primarily through resource monopoly (state temples control ritual materials, training, pilgrimage infrastructure) and through legitimacy denial (priesthood delegitimizes folk and syncretic practices as theologically invalid). Active coercion exists but is not the primary mechanism — most suppression operates through cultural delegitimization and institutional exclusion. The rising measurement trajectory (0.38 → 0.52 over 200 years) suggests that as the Amun-Ra framework solidifies, suppression requirements increase because folk traditions become more obviously subordinated and require active management. Theater ratio (0.58): Moderate-high. Ritual performativity increases substantially over the interval as the framework becomes more elaborate: priestly dress, ceremonial complexity, and textual standardization all increase. Early in the period (t=0), the polytheistic framework is relatively lean — priests can coherently claim that their role is essential for managing cosmological relationships. By t=200, much of the ritual apparatus is performative: the theology is so standardized that actual innovation becomes dangerous (heresy risk), and priests maintain the framework more through ritual theater than through active interpretation. This measurement pattern (theater rising from 0.42 to 0.58) is consistent with institutional degradation — the framework begins as genuine coordination and transforms into inertial performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival incommensurability across power asymmetries. The priesthood sees Rope — they experience the constraint as solving a genuine coordination problem (how to manage religious diversity) with minimal coercion. The pharaoh sees Tangled Rope — they benefit from the coordination but pay an extraction cost (priestly veto over legitimacy). Regional traditions see Snare — they are subordinated, their autonomy is extracted, and they have no exit path. Folk practitioners see Snare with maximum extraction — excluded entirely, suppressed through resource monopoly and cultural delegitimization. The analytical observer risks seeing Mountain (this is how social order must be organized), but the structural data reveals false summit: specific beneficiaries, specific victims, and measured suppression requirements indicate a constructed institutional arrangement that could be otherwise. The perspectival gap is so pronounced that different observers literally cannot agree on whether the constraint is beneficial, harmful, or neutral — each perspective is measuring a different emergent property from the same underlying structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality (d) from the agent's structural position relative to the constraint. The priesthood (institutional/arbitrage) has d ≈ 0.10 — they are primary beneficiaries with exit options (can pursue power through alternative institutional roles), so f(d) is negative (constraint subsidizes them). The pharaoh (powerful/constrained) has d ≈ 0.50 — mixed position, benefits from legitimacy but constrained by priestly veto, so f(d) ≈ 0.65 (moderate effective extraction). Folk practitioners (powerless/trapped) have d ≈ 0.95 — victims with no exit, so f(d) ≈ 1.42 (maximum experienced extraction). Regional deity traditions (powerless/trapped) have d ≈ 0.90 — victims with minimal exit, f(d) ≈ 1.28. The analytical observer (analytical/analytical) has canonical d ≈ 0.73, f(d) ≈ 1.15 — can see the full structure but risks naturalizing it. The directionality framework reveals the core asymmetry: powerless agents experience chi significantly higher than their base_extractiveness value, because f(d) amplifies extraction for trapped victims. Beneficiaries experience chi lower than base_extractiveness (negative f(d)) or neutral (f(d) ≈ 0.65 for constrained beneficiaries). This asymmetry is the diagnostic signal that the polytheistic framework functions as tangled rope (mixed coordination and extraction) from institutional perspectives but appears as pure snare from powerless perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the tangled-rope classification correctly captures the constraint's dual structure: it is simultaneously a coordination mechanism (solving the genuine problem of managing religious diversity) and an extraction mechanism (priesthood extracts power and resources). The measurement trajectory shows how coordination can drift into extraction: over 200 years, theater_ratio rises (performative content increases), suppression_requirement rises (active management of alternatives increases), and base_extractiveness rises (priesthood extracts more). This pattern is consistent with institutional capture — the original coordination function becomes progressively subordinated to extraction as the beneficiaries (priesthood) consolidate their institutional position. The alternative classifications from other perspectives (snare from powerless agents, rope from priesthood, mountain from analytical observer) are all structurally valid given those agents' position and exit options, but they obscure the core tangled-rope structure visible from the pharaonic and inter-regional institutional perspective. The mandatrophy is not 'which type is correct?' but 'the constraint exhibits genuine coordination and genuine extraction simultaneously; the question is whether coordination decreases over time (as measurements suggest) or whether they remain balanced.' The rising theater and suppression trajectories suggest the constraint is degrading from tangled rope toward snare — priesthood is increasingly extracting beyond what coordination requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polytheistic_framework_universality,
    'Is the polytheistic Amun-Ra framework genuinely the only coherent cosmology for organizing Egypt''s diverse regional deities, or does this appear inevitable only because the alternatives (monotheism, radical syncretism, federation of autonomous cults) were politically suppressed?',
    'Historical counterfactual: comparison with known religious systems that managed multi-deity contexts through different frameworks (Mesopotamian city-god federations, later Hellenistic syncretism). Analysis of textual evidence for priesthood''s explicit choice to adopt polytheistic hierarchy rather than alternative organizational schemes.',
    'If genuinely inevitable: constraint approaches Mountain legitimacy; polytheistic ordering is natural to managing cosmological diversity. If politically contingent: constraint confirms as Tangled Rope; the priesthood strategically chose Amun-Ra polytheism to maximize their interpretive authority, suppressing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polytheistic_framework_universality, conceptual, 'Whether polytheistic hierarchy is inevitable or politically contingent').

omega_variable(
    folk_syncretism_suppression_mechanism,
    'How much of the suppression of folk syncretic practices is enforced through state coercion (priestly control of temples, denial of ritual resources, legal penalties) versus through cultural legitimacy loss (folk practitioners internalize priestly delegitimization)?',
    'Textual analysis of priestly condemnation language; archaeological evidence of shrine destruction vs. abandonment; oral tradition records of folk practitioners'' self-descriptions (do they frame their exclusion as external force or as loss of cosmic validity?)',
    'If primarily coercive: suppression = 0.52 is accurate structural measure. If primarily internalized: effective suppression is higher (folk practitioners police themselves); structural suppression drops but cultural capture rises. Affects directionality of regional_deity_autonomy victim role.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(folk_syncretism_suppression_mechanism, empirical, 'Whether folk suppression is coercive or internalized').

omega_variable(
    reading_kernel_contingency,
    'This constraint is one reading of the divine_legitimacy_substrate kernel. Is the Amun-Ra polytheistic reading itself a contingent historical choice by priesthoods, or does it represent the most natural/stable interpretation of Egypt''s underlying theological reality?',
    'Textual evidence showing priesthood explicitly adopting or debating polytheistic framework choice; comparison with moments when alternative readings (atenism, folk syncretism) gained traction and were suppressed; analysis of whether priesthood rhetoric positions polytheism as discovery or as choice.',
    'If contingent choice: this reading is structurally equivalent to its sibling readings (atenist_monotheistic_reading, folk_syncretistic_reading); the three coexist_with each other and represent different institutional power configurations, not different truths. If natural/discovered: this reading forecloses the sibling readings; polytheistic interpretation is THE correct reading and alternatives are errors or corruptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contingency, conceptual, 'Whether polytheistic reading is natural interpretation or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divleg_amun_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(divleg_amun_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(divleg_amun_tr_t200, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 200, 0.58).

% Extraction over time
narrative_ontology:measurement(divleg_amun_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(divleg_amun_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(divleg_amun_be_t200, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(divleg_amun_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(divleg_amun_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(divleg_amun_su_t200, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 200, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.18).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_authority_validation_mechanism).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_land_monopoly).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel has three structurally distinct instantiations, each with different ε values and different institutional beneficiaries. The amun_polytheistic_reading (ε=0.48, Tangled Rope) is the institutionally dominant reading in historical Egypt. The atenist_monotheistic_reading (ε ≈ 0.65, Snare) was a failed attempt to consolidate pharaonic authority by displacing priesthood. The folk_syncretistic_reading (ε ≈ 0.35, Rope) represented autonomous local ordering without centralized priesthood authority. These three are not perspectives on one constraint but three distinct constraints grounded in the same kernel — they answer the same legitimacy question through incompatible institutional frameworks. The polytheistic reading affects the other readings by suppressing them (priesthood enforced orthodoxy), affecting downstream constraints (temple monopoly, pharaonic validation mechanisms) by providing the theological justification for those institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
