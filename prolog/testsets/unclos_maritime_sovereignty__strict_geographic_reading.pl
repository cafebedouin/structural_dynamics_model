% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Strict Geographic Reading (Natural Features Only)
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   The UNCLOS maritime sovereignty constraint operates at the intersection
 *   of geography, international law, and geopolitical strategy. The strict
 *   geographic reading — that only naturally formed features above water at
 *   high tide generate territorial sea and exclusive economic zones (EEZ) —
 *   has structured maritime law since 1982. However, the rise of intentional
 *   artificial island construction (Southeast Asian land reclamation,
 *   island-building for geopolitical expansion) has exposed deep structural
 *   tensions in the reading. This constraint story focuses specifically on
 *   the strict reading: artificial construction does not alter legal status.
 *   It is one of three competing readings of the same underlying UNCLOS
 *   kernel text, each with different beneficiaries, victims, and
 *   extractiveness profiles. The strict reading benefits naval powers and
 *   non-claimant states by constraining expansion; it harms expansionist
 *   coastal states that have invested in artificial island infrastructure;
 *   and it creates coordination benefits (clear, litigable rules) alongside
 *   extraction costs (enforcement burden, jurisdictional rigidity). The
 *   constraint exhibits high suppression (0.68) because states with
 *   expansionist interests face strong institutional pressure to comply with
 *   the strict reading, even as they contest it. The theater ratio (0.45)
 *   reflects moderate performativity: the natural/artificial distinction is
 *   increasingly manipulable through hybrid engineering (creating features
 *   that appear natural but are intentionally designed), yet the adjudicative
 *   process remains substantially functional.
 *
 * KEY AGENTS:
 *   - Naval Powers and Non-Claimant States: Institutional beneficiary (institutional/arbitrage) — preserve open ocean access and constrain others' expansion through strict reading
 *   - Expansionist Coastal States: Primary victim (moderate/constrained) — invested in artificial islands under assumption of legal recognition; extraction occurs through prohibition of effect
 *   - Small Island States: Secondary victim (powerless/trapped) — marginal maritime territory; cannot use artificial enhancements; no exit from strict reading enforcement
 *   - International Courts (ICJ, ITLOS): Institutional operator (organized/mobile) — adjudicate feature classification; see coordination benefit but also enforcement burden
 *   - UNCLOS Treaty System: Institutional framework (institutional/arbitrage) — maintains formal rule structure through inertia despite increasing contestation
 *   - Global Maritime Commons: Collective victim (powerless/trapped) — bears compression of high-seas access through aggregate jurisdictional expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.68).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Maritime Sovereignty: Strict Geographic Reading (Natural Features Only)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65').
narrative_ontology:cs_kernel_codification('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', fixed_text).
narrative_ontology:cs_authority_grounding('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', lineage).
narrative_ontology:cs_interpretation_layer_present('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65').
narrative_ontology:cs_reading_relation('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', foundational, natural_formation_determines_sovereignty).
narrative_ontology:cs_axiom_status(natural_formation_determines_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', natural_formation_determines_sovereignty, conventional).
narrative_ontology:cs_axiom('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', foundational, intentional_construction_strips_sovereignty_claim).
narrative_ontology:cs_axiom_status(intentional_construction_strips_sovereignty_claim, holdable).
narrative_ontology:cs_axiom_grounding('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', intentional_construction_strips_sovereignty_claim, deontological).
narrative_ontology:cs_reference_frame('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', geographic_determinism_1982).
narrative_ontology:cs_drift_state('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', contemporary_artificial_island_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('11cbe5a7-bee7-4ee0-80b3-d2b9f3837f65', '2026-02-26T14:32:15Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_coalition).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, island_sovereignty_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATES WITH ARTIFICIAL CLAIMS (SNARE) — Trapped by the strict reading's exclusion of artificial features. States that invested in maritime infrastructure (dikes, reclaimed land, artificial islands) to expand territorial reach cannot leverage those improvements under this constraint. No exit option: either abandon costly infrastructure or accept it produces zero legal effect. High suppression: international enforcement pressure prevents deviation. Maximum extraction experienced by this agent.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COASTAL STATES WITH MIXED FEATURES (TANGLED ROPE) — Benefits from the coordination function: the strict reading provides clear, litigable rules that reduce maritime boundary disputes. But constrained by extraction: the reading excludes enhancements that would be natural expansions of their territory. Moderate extraction because they retain EEZ based on natural features, but artificial extensions are prohibited. The constraint both enables coordination (clear rules) and extracts (limitations on expansion).
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NAVAL POWERS AND NON-CLAIMANT STATES (ROPE) — Pure beneficiary with arbitrage options. The strict geographic reading preserves open ocean access by denying artificial islands the status of geographic features. Naval powers can navigate freely; non-claimant states benefit from constrained expansion by others. Low effective extraction because these actors have mobility — they can shift strategies or exit maritime disputes. Net beneficiary.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LAW COMMUNITY (TANGLED ROPE) — Benefits from the constraint's coordination function: the strict reading provides objective, litigable criteria (natural formation, above water at high tide) that reduce boundary disputes and render maritime law predictable. But also constrained by enforcement overhead: courts must adjudicate disputed feature classification (Is a dike-assisted island 'natural'? Does submerged feature count?). The coordination benefit (clear rules) is genuine and substantial; the extraction (litigation burden) is real but moderate.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNCLOS TREATY ADMINISTRATION (PITON) — The strict geographic reading functions as a degraded institutional mechanism. The treaty text's geographic criteria (natural formation, above water at high tide) were clear in 1982 when land reclamation was marginal, but are increasingly performative as intentional artificial island construction becomes geopolitical strategy. The UNCLOS dispute resolution system persists through inertia and institutional commitment, but the core distinction (natural vs. artificial) is increasingly contested and manipulated. Theater ratio rises as states construct hybrid features (dikes creating 'natural-looking' islands) designed to evade the geographic test. The treaty persists because states lack agreed alternatives, not because its geographic criteria remain functionally adjudicable.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL MARITIME COMMONS (SNARE) — The commons experiences extraction through jurisdictional expansion even under the strict reading. Each state maximizes territorial claims within the strict geographic boundary; the aggregate effect is compression of high-seas access and fragmented governance. The commons cannot organize or exit; it bears the cost of jurisdictional multiplication. The strict reading mitigates this extraction compared to expansive readings, but extraction still occurs.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The strict geographic reading risks naturalizing a specific legal doctrine (the 1982 UNCLOS distinction between natural and artificial features) as a matter of geographic necessity. The claim would be: only naturally formed features CAN generate territorial sovereignty because artificial construction doesn't alter fundamental geographic reality. However, the structural data contradicts this. The 'natural feature' requirement is a choice made by treaty drafters to solve a specific coordination problem in 1982 (preventing unlimited artificial expansion), not a discoverable law of geography. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_maritime_sovereignty__strict_geographic_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, TR),
    TR >= 0.70.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The strict geographic reading excludes artificial features from legal effect, which is a form of extraction for expansionist states — they invested in infrastructure expecting legal recognition and receive none. However, extraction is not maximal (not 0.72+) because the constraint also provides coordination benefits (predictable rules reduce disputes). The extractiveness rises over the 30-year interval (0.35 → 0.58) as artificial island construction becomes geopolitically normalized and states increasingly feel constrained by the strict reading's exclusion. Suppression (0.68): High. Expansionist states face strong institutional enforcement pressure (treaty obligations, court decisions, naval power enforcement by non-claimant states) to comply with the strict reading. International law institutions are substantially aligned around the reading; states attempting unilateral reinterpretation face diplomatic and legal sanctions. Theater ratio (0.45): Moderate. The natural/artificial distinction is increasingly performative because states deploy hybrid features (dikes + sediment management to create 'naturally appearing' islands) that technically comply with the geographic test while evading its spirit. However, the adjudicative process remains substantially functional — courts can and do distinguish intentional construction from natural formation through geological expertise. Theater is lower than in piton cases because the underlying rule (natural vs. artificial) remains meaningful in practice.
 *
 * PERSPECTIVAL GAP:
 *   The strict geographic reading creates a profound perspectival divide: beneficiaries (naval powers, non-claimant states) see a coordination mechanism that enables predictable maritime law; victims (expansionist coastal states, small island states) see pure extraction — investment in infrastructure that yields zero legal benefit. The international law community sees tangled_rope: genuine coordination function (clear rules) combined with enforcement burden (difficult adjudication of hybrid features). The UNCLOS treaty system itself exhibits piton characteristics: it persists through institutional inertia and binding legal force, but the geographic criteria that once seemed natural and objective are increasingly contested and manipulated. The analytical observer risks seeing this as a natural law (geography determines sovereignty), when in fact it is a contingent treaty choice that benefits specific actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure drives the directionality computation. Naval powers and non-claimant states are beneficiaries with arbitrage options (they can shift strategies or pressure other states through multiple channels); their derived d is low (~0.15), producing negative or minimal χ. Expansionist coastal states are victims with constrained exit (they invested in islands but cannot unilaterally reinterpret the treaty); their derived d is high (~0.72), producing substantial χ. Small island states are victims with no exit (trapped); their d is very high (~0.92). The international law community occupies a middle position: they derive benefit from coordination (clear rules reduce litigation uncertainty) but bear enforcement burden; moderate d (~0.52). The piton perspective (UNCLOS treaty system) has high d (~0.85) despite institutional power, because the system itself is constrained by the treaty text it administers — it cannot easily revise rules even if the rules are increasingly dysfunctional.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through kernel reading differentiation. The mandatrophy question — 'Is this constraint coordination or extraction?' — is NOT resolvable within a single reading. The strict reading embeds both: genuine coordination benefit (clear, litigable rules) for the beneficiary coalition (naval powers), genuine extraction (prohibition of artificial expansion) for the victim coalition (expansionist states). The constraint is correctly classified as tangled_rope at the moderate institutional perspective (coastal states with mixed features) because they experience both coordination (predictable rules) and extraction (expansion limitation). The engine's mandatrophy resolution here is that all three readings are legitimate, and the actual governance constraint is determined by which reading's institutional coalition is strongest. Currently, the strict reading is dominant (courts uphold it, naval powers enforce it), but the measurements show rising extraction (0.35 → 0.58) as the reading's exclusion of artificial features becomes increasingly costly to expansionist states. The omega variables document that the kernel reading could shift if customary practice diverges from treaty text (omega: alternative_reading_adoption_pressure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_feature_boundary_ambiguity,
    'What constitutes a ''naturally formed'' feature when human activity has shaped coastlines for millennia through dikes, dredging, and sediment management?',
    'Historical geology + human impact analysis: distinguish between features whose current form is primarily determined by natural processes vs. those intentionally constructed or materially modified by human engineering. Establish threshold for ''natural'' (e.g., >80% natural genesis, <20% human modification).',
    'If threshold is strict (>90% natural): many existing islands and landforms become reclassified as artificial. If threshold is permissive (<50% natural): the natural/artificial distinction collapses. Classification trajectory: mountain → tangled_rope → snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_feature_boundary_ambiguity, empirical, 'Boundary between naturally-formed and artificially-constructed features').

omega_variable(
    enforcement_capability_drift,
    'Can international courts and dispute resolution mechanisms reliably adjudicate ''natural feature'' status when states deploy geological obfuscation (constructing features that appear natural but are intentionally engineered)?',
    'Longitudinal analysis of ICJ and ITLOS decisions on feature classification; tracking of contested cases that required expert geologic testimony; assessment of reversal rates on appeal. Measurement of treaty state compliance with court decisions on artificial islands (e.g., did parties accept or challenge rulings?).',
    'If enforcement remains reliable (>85% compliance with court decisions): constraint maintains tangled_rope structure. If enforcement degrades (<65% compliance): constraint slides toward snare as suppression tightens to compensate for lost adjudicative power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capability_drift, empirical, 'Adjudicative reliability for natural feature determination').

omega_variable(
    alternative_reading_adoption_pressure,
    'Will treaty practice (actual state behavior and custom international law development) shift toward the expansive_construction_reading or hybrid_effective_control_reading as artificial island construction becomes geopolitically normalized?',
    'Opinio juris analysis: tracking of state claims, ICJ precedent formation, customary practice drift. Monitor whether courts begin accepting hybrid/effective-control arguments for artificial features despite strict treaty text. Measure whether the minority reading becomes majority practice.',
    'If strict reading remains binding: snare/piton perspectives remain stable. If expansive or hybrid reading becomes customary law: UNCLOS text remains unchanged but its interpretive practice drifts, and the strict reading becomes a false treaty — still formally binding but substantially eroded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_adoption_pressure, conceptual, 'Whether sibling readings will become dominant through customary practice').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates ONE reading of the UNCLOS maritime sovereignty kernel. The sibling readings (expansive_construction_reading, hybrid_effective_control_reading) represent alternative legitimate interpretations of the same treaty text. Which reading is correct — or is the kernel text genuinely ambiguous?',
    'Historical analysis of treaty drafting intent; state practice surveys; court decisions over time. Assess whether the treaty text itself determines the reading or whether the text permits multiple readings.',
    'If text determines strict reading: this reading is the authoritative constraint. If text permits multiple readings: the constraint exists in a superposition across three stories, with actual governance following the strongest institutional power coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the UNCLOS kernel text determines or permits multiple readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unclos_strict_tr_t15, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(unclos_strict_tr_t30, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(unclos_strict_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_strict_be_t15, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(unclos_strict_be_t30, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(unclos_strict_su_t15, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(unclos_strict_su_t30, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_artificial_island_sovereignty).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_constraint).

% DUAL FORMULATION NOTE:
% The UNCLOS maritime sovereignty kernel decomposes into three structurally distinct constraints: strict_geographic_reading (this file, ε=0.58), expansive_construction_reading (ε=0.42, beneficiaries shift toward expansionist states), and hybrid_effective_control_reading (ε=0.48, beneficiaries shift toward powerful states with enforcement capacity). These are not perspectives on one constraint — they are competing constraints grounded in different treaty readings. The strict reading dominates current institutional practice but shows rising extraction over time (T17 signal). The expansive and hybrid readings are doctrinal alternatives held by minority coalitions of states and scholars. Link all three files with mutual affects_constraints entries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
