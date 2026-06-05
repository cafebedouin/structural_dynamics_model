% ============================================================================
% CONSTRAINT STORY: spatial_enforcement_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spatial_enforcement_mechanism, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: spatial_enforcement_mechanism
 *   human_readable: Spatial Enforcement Mechanism in Stone-Inscribed Land-Use Rules
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   Stone-inscribed spatial enforcement mechanisms represent a particular
 *   institutional solution to a perennial problem in disaster-prone regions:
 *   how to transmit land-use knowledge across generations without written
 *   recording technology. Communities facing repeated tsunamis, earthquakes,
 *   or floods encode boundary knowledge in stone markers, oral narratives,
 *   and ritual practice. This constraint describes the mechanism through
 *   which that encoding becomes enforceable — the spatial rules are embedded
 *   in landscape, transmitted through kinship and apprenticeship, and
 *   maintained through informal sanctions and exclusion. The constraint
 *   operates at the intersection of disaster anthropology (how communities
 *   preserve institutional memory), land-use governance (how boundary rules
 *   are encoded and enforced), and institutional memory (how knowledge is
 *   transmitted and validated). From the perspective of long-term
 *   institutional holders (elders, lineage keepers), the spatial mechanism is
 *   coordination that solves real boundary disputes and encodes irreplaceable
 *   ecological knowledge. From the perspective of newcomers and migrants, it
 *   is extraction: spatial restrictions whose logic is inaccessible,
 *   enforcement that feels arbitrary, and knowledge systems closed to
 *   outsiders. From a civilizational view, the mechanism appears as a natural
 *   law of disaster-prone societies — how else could spatial rules persist
 *   without written technology? This reading instantiates the natural-law
 *   interpretation of a contested kernel about whether stone-inscribed rules
 *   represent discovered boundaries or socially constructed arrangements.
 *
 * KEY AGENTS:
 *   - Long-term Survivors and Institutional Knowledge Holders: Primary beneficiaries (institutional/arbitrage) — their knowledge remains irreplaceable and valuable; they control access to spatial rules and benefit from knowledge asymmetry
 *   - Newcomers and Migrants: Primary victims (powerless/trapped) — arrive to opaque spatial landscape; face extraction through resource exclusion and penalty for unknown boundary violations; no exit option below severe cost
 *   - Transgenerational Coordination System: Abstract victim (powerless/trapped) — when institutional knowledge holders die and transmission breaks, spatial rules persist as theater rather than functional governance; the coordination function collapses but enforcement persists
 *   - Post-Disaster Communities: Secondary agents (moderate/constrained) — adaptively learn spatial rules through socialization but experience mixed coordination and extraction as rules are hoarded while also providing genuine boundary guidance
 *   - Formalization Coalitions: Organized agents (organized/constrained) — researchers, government agencies, NGOs attempting to codify spatial rules in written form with explicit sunset — converting opaque oral/stone transmission into accessible documentation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable property of disaster-prone societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spatial_enforcement_mechanism, 0.52).
domain_priors:suppression_score(spatial_enforcement_mechanism, 0.58).
domain_priors:theater_ratio(spatial_enforcement_mechanism, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spatial_enforcement_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(spatial_enforcement_mechanism, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(spatial_enforcement_mechanism, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spatial_enforcement_mechanism, tangled_rope).
narrative_ontology:human_readable(spatial_enforcement_mechanism, "Spatial Enforcement Mechanism in Stone-Inscribed Land-Use Rules").
narrative_ontology:topic_domain(spatial_enforcement_mechanism, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(spatial_enforcement_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(spatial_enforcement_mechanism, fixed_text).
narrative_ontology:cs_authority_grounding(spatial_enforcement_mechanism, lineage).
narrative_ontology:cs_interpretation_layer_present(spatial_enforcement_mechanism).
narrative_ontology:cs_kernel_id(spatial_enforcement_mechanism, stone_land_use_rule).
narrative_ontology:cs_axiom(spatial_enforcement_mechanism, foundational, stone_inscribed_rules_as_social_construction).
narrative_ontology:cs_axiom_status(stone_inscribed_rules_as_social_construction, holdable).
narrative_ontology:cs_axiom_grounding(spatial_enforcement_mechanism, stone_inscribed_rules_as_social_construction, deontological).
narrative_ontology:cs_axiom(spatial_enforcement_mechanism, secondary, extractiveness_contingent_on_knowledge_asymmetry).
narrative_ontology:cs_axiom_status(extractiveness_contingent_on_knowledge_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding(spatial_enforcement_mechanism, extractiveness_contingent_on_knowledge_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame(spatial_enforcement_mechanism, spatial_rules_as_community_covenant).
narrative_ontology:cs_drift_state(spatial_enforcement_mechanism, post_disaster_environmental_mismatch, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spatial_enforcement_mechanism, long_term_survivors).
narrative_ontology:constraint_beneficiary(spatial_enforcement_mechanism, institutional_knowledge_holders).
narrative_ontology:constraint_victim(spatial_enforcement_mechanism, newcomers_and_migrants).
narrative_ontology:constraint_victim(spatial_enforcement_mechanism, transgenerational_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT/NEWCOMER (SNARE) — Arrives to a landscape where stone markers and oral enforcement rules are opaque, embedded in local knowledge that cannot be accessed from outside. Faces extraction through trespass penalties, displacement, or resource denial without meaningful option to understand or exit. The constraint's suppression is maximum from this position — material barriers (stone markers on unfamiliar terrain), social barriers (exclusion from knowledge transmission), and institutional barriers (informal enforcement with no appeal mechanism) combine. No exit option below severe cost.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADAPTIVE COMMUNITY MEMBER (TANGLED ROPE) — Learned the spatial rules through childhood immersion or gradual socialization. Experiences genuine coordination function (stone markers reduce repetitive boundary conflicts, codify seasonal access patterns) but also asymmetric extraction (rule knowledge is hoarded by elders, access to resource zones is restricted). Constrained exit — leaving the territory means material loss, but staying means accepting subordinate knowledge position. Both coordination benefit and extraction present.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LONG-TERM INSTITUTIONAL HOLDER (ROPE) — Elders, lineage keepers, or institutional memory carriers benefit from the constraint's encoding. Stone markers and oral transmission create coordination that ensures their knowledge remains valuable and irreplaceable. They experience the constraint as pure coordination because the institutional structure is built around their epistemic authority. Arbitrage position — they can encode/decode the rules and can choose to transmit or withhold knowledge. Net beneficiary with full agency.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-DISASTER INSTITUTIONAL MEMORY (PITON) — After major disaster (tsunami, earthquake, flood), the spatial enforcement mechanism persists even as its functional coordination purpose has degraded. Stone markers remain but the ecological/climatic conditions they were calibrated to have shifted. Oral knowledge holders have died. The enforcement becomes largely performative — maintaining the stone monument and repeating the rules as cultural practice rather than functional land-use governance. Theater ratio is high because the ritual persists despite reduced functional verification of whether the spatial restrictions still match post-disaster conditions.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSITIONAL GOVERNANCE COALITION (SCAFFOLD) — Government agencies, NGOs, or research-and-development projects implement spatial enforcement through formal mapping, written codification, and training programs. Experiences the constraint as temporary scaffolding — formalizing oral/stone-inscribed rules into written, accessible form with explicit sunset: once inclusive spatial knowledge is distributed and internalized, the formal enforcement infrastructure can be withdrawn. Low effective extraction because the coalition has explicit agenda to sunset the mechanism. Theater is moderate — there is genuine coordination benefit from formalization, not pure ritual.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, spatial enforcement mechanisms appear as inevitable structures: humans in disaster-prone zones must encode boundary knowledge somehow; stone and oral transmission are natural solutions to preservation across generations without written technology; the asymmetry in knowledge access is inherent to how transmission systems work. This perspective naturalizes what is actually a contingent institutional arrangement. The engine will classify this as a false summit if beneficiary data is sufficient.
constraint_indexing:constraint_classification(spatial_enforcement_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spatial_enforcement_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spatial_enforcement_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spatial_enforcement_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(spatial_enforcement_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(spatial_enforcement_mechanism, TR),
    TR >= 0.70.

:- end_tests(spatial_enforcement_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits substantial extraction asymmetry — knowledge holders monopolize interpretation, newcomers face exclusion and penalties, resource access is mediated through kinship/insider status. However, extractiveness is not maximal (0.70+) because genuine coordination function exists: stone markers do reduce boundary disputes and encode seasonal access patterns that prevent resource depletion. The measurement trajectory shows rising extractiveness over the interval (0.42 → 0.52 → 0.58), reflecting environmental degradation post-disaster (stone markers become mismatched to hazard zones, enforcement becomes more punitive as ecological stress rises). Suppression (0.58): High. Barriers to exit and alternatives are structural: migrants face material displacement cost, social barriers (exclusion from knowledge transmission), institutional barriers (informal enforcement with no appeal mechanism), and identity costs (accepting subordinate knowledge position). Newcomers cannot easily learn the rules, and asking about spatial boundaries reveals outsider status. Stone markers are physical barriers to access; oral transmission is a cognitive barrier. Theater ratio (0.61): Moderate-high. Post-disaster, theater rises as environmental conditions diverge from rule-encoding. Communities continue enforcing spatial restrictions not because ecological conditions justify them but because the rules are embedded in ritual, story, and monument. The formalization coalition observes that much enforcement activity is performative — maintaining the stone, repeating the story — rather than addressing current hazard zones.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates three distinct classification types from structural agents. Long-term institutional holders see Rope — the mechanism solves coordination problems and ensures their knowledge remains central. Newcomers and migrants see Snare — maximum extraction with no exit and no access to the knowledge that would unlock exit. Adaptive community members who learned the rules through immersion see Tangled Rope — mixed benefit from coordination (they understand the rules, which reduces their anxiety about boundaries) and mixed cost from extraction (they occupy subordinate knowledge position). The post-disaster perspective sees Piton — the ritual persists through institutional inertia while its functional coordination purpose has degraded. The formalization coalition sees Scaffold — a temporary enforcement structure being replaced by explicit, accessible written codification with a sunset timeline. The analytical observer risks seeing Mountain — treating spatial enforcement as an inevitable property of disaster-prone societies. But structural data reveals this as a false summit: the extractiveness and suppression metrics are contingent on knowledge hoarding and institutional memory concentration, not on immutable properties of disaster management.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) derives from its structural position. Long-term holders (beneficiary + arbitrage exit) experience low d → negative chi → they perceive coordination. Newcomers (victim + trapped exit) experience high d → high chi → they perceive extraction. Adaptive members (victim + constrained exit, but partly beneficiary through coordination) experience moderate d → moderate chi → mixed perception. The magnitude of chi in the snare perspective reflects not just the base extraction (0.52) but the interaction of trapped exit (which amplifies experienced extraction) and the victim status (which derives d from resource exclusion and knowledge asymmetry). The formalization coalition perceives lower chi because their constrained exit comes with agency and organized power — they see a way to sunset the mechanism. The piton perspective derives from theater ratio (0.61 at t=5) — the mechanism persists through inertia even as functional verification of whether rules match post-disaster hazards has degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This constraint is one reading of a contested kernel about whether stone-inscribed land-use rules represent natural laws of disaster-prone societies or contingent institutional arrangements. The mandatrophy manifests as the tension between the Mountain classification (civilizational view treating spatial enforcement as inevitable) and the Tangled Rope/Snare classifications (showing extractive asymmetries in knowledge and access). The reading instantiates the 'spatial enforcement as contingent institutional mechanism' interpretation of the kernel. The foundational axiom is 'stone_inscribed_rules_as_social_construction' — the reading holds that spatial boundaries are socially made, not discovered or natural. This axiom is holdable and grounded in practice: communities actively choose whether to enforce, interpret, and transmit spatial rules; the rules are revised when ecological conditions change; knowledge holders can choose whether to teach or hoard. If this axiom is abandoned, the reading collapses into the alternative natural-law reading (where boundaries are discovered/inevitable). The mandatrophy resolves through empirical investigation: does the formalization coalition succeed in making spatial rules functionally equivalent through written codification? If yes, the constraintʻs extractiveness was contingent on knowledge asymmetry and can be eliminated — Scaffold classification confirmed. If no, knowledge asymmetry is structural (oral transmission is irreducible) and extraction persists — Tangled Rope remains. The reading itself is not mandatrophic (it has a coherent extractiveness value); the system is mandatrophic (six types from six perspectives). The reading's role is to disambiguate which interpretation of the kernel — natural law or constructed — is operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stone_marker_interpretive_ambiguity,
    'Are stone markers primarily a coordination mechanism (encoding shared boundaries to reduce conflict) or primarily an extraction mechanism (encoding elders'' control over knowledge and resource access)?',
    'Comparative ethnography: do communities with stone-inscribed rules show lower boundary conflict rates than those without? Do newcomers/migrants experience the markers as clarifying or excluding? Historical analysis: what portion of enforcement actions target true boundary violations vs. knowledge-based exclusion?',
    'If coordination-primary: constraint may reclassify from Snare (migrant view) to Rope even from powerless perspective. If extraction-primary: coordination function is theater masking asymmetric knowledge control. Affects whether spatial mechanism is a genuine natural law or a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stone_marker_interpretive_ambiguity, empirical, 'Whether stone markers function primarily as coordination or as knowledge-control mechanism').

omega_variable(
    oral_transmission_irreplaceability,
    'Is oral knowledge transmission required to interpret stone markers, or can written documentation fully capture the spatial rules?',
    'Formalization experiments: communities where researchers document spatial rules in writing and teach from written text without elder intermediation. Success metric: do newcomers trained via documentation achieve equivalent spatial competence to those trained orally? Do conflicts over spatial interpretation decrease or persist?',
    'If oral transmission is irreplaceable: knowledge asymmetry is structural (constraint remains extraction-dominant). If documentation is sufficient: the constraint''s extractive mechanism is contingent (could be eliminated by formalization). Scaffolding timeline depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_transmission_irreplaceability, empirical, 'Whether oral transmission is structurally necessary for spatial rule enforcement').

omega_variable(
    disaster_regime_shift_detection,
    'Do post-disaster environmental changes invalidate stone-marker spatial rules, and if so, how quickly does institutional memory detect and adapt?',
    'Post-disaster monitoring: satellite imagery, ecological surveys, and enforcement pattern analysis before and after major disasters (tsunami, earthquake, landslide). Track: do stone-inscribed zones match post-disaster hazard zones? Do institutions revise boundaries, or do they enforce outdated spatial rules? Timeline: how long does adaptation take (months, years, decades)?',
    'If markers become mismatched rapidly: theater ratio rises (enforcement becomes performative). If institutions adapt flexibly: functional coordination persists. Affects piton classification validity and measurement trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disaster_regime_shift_detection, empirical, 'Whether stone-inscribed rules adapt to post-disaster environmental shifts').

omega_variable(
    knowledge_control_as_authority_grounding,
    'Is the spatial enforcement mechanism grounded in genuine coordination (solving a shared problem) or in the authority of elders/knowledge holders (where enforcement legitimacy depends on maintaining knowledge scarcity)?',
    'Authority decay analysis: when elders die or migrate, do spatial rules persist because they are intrinsically functional, or do they persist only as theater (performed but not enforced)? Do younger knowledge holders actively teach spatial rules to ensure continuity, or do they guard knowledge to maintain their own status? What happens when institutional memory is disrupted (war, epidemic, forced migration)?',
    'If authority-grounded: the constraint''s legitimacy is fragile — when authority holders disappear, enforcement collapses (piton or rapid scaffold). If coordination-grounded: rules persist because they solve problems (rope or tangled_rope). Critical for understanding whether this is a natural law or a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_control_as_authority_grounding, empirical, 'Whether spatial enforcement legitimacy depends on knowledge-holder authority or coordination function').

omega_variable(
    kernel_reading_interpretation_ambiguity,
    'Is this constraint a reading of ''stone-inscribed land-use rules as natural law'' (fixed text kernel) or as ''spatial enforcement practice as contingent institutional arrangement'' (distributed, evolving kernel)?',
    'Ethnographic focus: do communities treat stone markers as discovered/natural (immutable, discovered boundaries) or as constructed/social (made by ancestors, revisable if collective agreement permits)? Do institutions attempt to reinterpret or revise stone-inscribed rules, and how do they justify the reinterpretation?',
    'If natural-law reading dominates: spatial enforcement mechanism is mountain-classified. If social-construction reading dominates: mechanism is tangled_rope or snare. The reading frames whether spatial enforcement is inevitable or contingent. This omega documents the reading ambiguity that the ''kernel_context'' field addresses in free text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_interpretation_ambiguity, conceptual, 'Whether stone-inscribed rules are treated as natural law or contingent social arrangements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spatial_enforcement_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spatial_tr_t0, spatial_enforcement_mechanism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(spatial_tr_t2, spatial_enforcement_mechanism, theater_ratio, 2, 0.55).
narrative_ontology:measurement(spatial_tr_t5, spatial_enforcement_mechanism, theater_ratio, 5, 0.61).
narrative_ontology:measurement(spatial_tr_t10, spatial_enforcement_mechanism, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(spatial_be_t0, spatial_enforcement_mechanism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spatial_be_t2, spatial_enforcement_mechanism, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(spatial_be_t5, spatial_enforcement_mechanism, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(spatial_be_t10, spatial_enforcement_mechanism, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spatial_enforcement_mechanism, resource_allocation).
narrative_ontology:affects_constraint(spatial_enforcement_mechanism, post_disaster_institutional_memory).
narrative_ontology:affects_constraint(spatial_enforcement_mechanism, knowledge_asymmetry_in_hazard_zones).

% DUAL FORMULATION NOTE:
% Spatial enforcement mechanism is one component of a constraint family describing how disaster-prone communities encode and transmit land-use knowledge. The parent constraint is the broader 'institutional memory transmission system'; this story focuses on the spatial-enforcement subcomponent (how boundaries are marked and enforced). Sibling constraints examine the ecological knowledge encoding (why particular zones have restrictions) and the transgenerational transmission failure (what happens when institutional memory holders die). Each has its own ε and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
