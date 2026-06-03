% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Reading of Cultural Property Authority
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   The universal heritage reading of the cultural property kernel grounds
 *   legitimate authority in an institution's capacity to preserve artifacts
 *   and maximize universal access, regardless of the artifact's geographic
 *   origin or the claims of source-country or indigenous communities. This
 *   reading emerged in post-WWII international law and became doctrine
 *   through UNESCO instruments and the 1970 Convention on the Means of
 *   Prohibiting and Preventing the Illicit Import, Export and Transfer of
 *   Ownership of Cultural Property. The constraint operates between major
 *   museums and holding institutions (beneficiaries under this reading),
 *   source-country successor states and indigenous communities (victims under
 *   this reading), and the international legal regime that enforces the
 *   doctrine. The reading treats repatriation claims as particularist threats
 *   to a putative universal good — preservation and shared access. However,
 *   the structural data reveals the universal heritage reading as a tangled
 *   rope with significant extraction: claimant states and communities bear
 *   high legal costs (protracted litigation), diplomatic friction, identity
 *   harm (their artifacts reframed as belonging to 'humanity'), and epistemic
 *   erasure (museum interpretation controls meaning). The constraint requires
 *   active enforcement through international law, museum litigation
 *   strategies, and academic authority claims that position Western
 *   institutions as uniquely capable stewards. Suppression has declined over
 *   the measurement interval (0.55 → 0.48) as indigenous repatriation
 *   movements and postcolonial legal scholarship have increased
 *   source-country agency. Theater ratio has risen (0.42 → 0.55), reflecting
 *   that museum stewardship claims are increasingly performative as digital
 *   access and decentralized preservation challenge the monopoly on
 *   preservation and interpretation. Extractiveness has risen (0.35 → 0.52)
 *   as the costs to source countries have accumulated while the coordination
 *   benefits (shared preservation, universal access) have been contested.
 *   This constraint is one reading of a three-way dispute: the universal
 *   heritage reading coexists with the sovereign repatriation reading
 *   (grounding authority in postcolonial state succession) and the indigenous
 *   stewardship reading (grounding authority in community cultural
 *   continuity). None of these readings forecloses the others within a single
 *   framework — they remain genuinely live positions held by different
 *   institutional actors and normative traditions.
 *
 * KEY AGENTS:
 *   - Major Museums (British Museum, Louvre, Metropolitan, Getty): Primary beneficiaries (institutional/arbitrage) — benefit from legal doctrine legitimizing retention, gain soft power and research authority, attract international funding based on collection prestige
 *   - Successor Claimant States (Egypt, Greece, Nigeria, India, etc.): Primary victims (powerful/constrained) — bear legal costs of repatriation claims, face diplomatic friction in multilateral negotiations, experience identity harm from their artifacts being reframed as global property rather than national heritage
 *   - Indigenous Communities (Aboriginal Australians, Native Americans, Pacific Island groups, etc.): Primary victims (powerless/trapped) — additional layer of extraction via epistemic erasure (museum interpretation displaces community meaning-making), legal barriers to access, spiritual harm from object removal from ceremonial context
 *   - Western Collecting Nations (UK, France, US, etc.): Secondary beneficiaries with dual role (powerful/mobile) — benefit from soft power and cultural diplomacy claims, but face rising repatriation costs and legitimacy erosion (universities divesting, museums under scrutiny)
 *   - International Legal Regime (UNESCO, UNIDROIT, ICOMOS): Institutional enforcer (institutional/arbitrage) — maintains the doctrine through conventions, guidelines, and certification of museum authority; benefits from coordination function but also enforces extraction
 *   - Curatorial Profession and Art History Academia: Institutional defenders (institutional/constrained) — identity-fused with museum stewardship role; experience the constraint as piton (performative tradition persisting through professional inertia)
 *   - International Law Reform Movement (repatriation NGOs, postcolonial scholars, indigenous advocates): Organized challengers (organized/constrained) — see universal heritage reading as temporary scaffold with sunset; building alternative doctrine (strong repatriation rights, indigenous authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.52).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.48).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Reading of Cultural Property Authority").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'c14ee99b-bcf7-4443-a3d9-c3326aba3008').
narrative_ontology:cs_kernel_codification('c14ee99b-bcf7-4443-a3d9-c3326aba3008', formalized).
narrative_ontology:cs_authority_grounding('c14ee99b-bcf7-4443-a3d9-c3326aba3008', extraction).
narrative_ontology:cs_interpretation_layer_present('c14ee99b-bcf7-4443-a3d9-c3326aba3008').
narrative_ontology:cs_reading_relation('c14ee99b-bcf7-4443-a3d9-c3326aba3008', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c14ee99b-bcf7-4443-a3d9-c3326aba3008', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('c14ee99b-bcf7-4443-a3d9-c3326aba3008', foundational, universal_artifact_preservation_necessity).
narrative_ontology:cs_axiom_status(universal_artifact_preservation_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c14ee99b-bcf7-4443-a3d9-c3326aba3008', universal_artifact_preservation_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c14ee99b-bcf7-4443-a3d9-c3326aba3008', foundational, universal_epistemic_access_value).
narrative_ontology:cs_axiom_status(universal_epistemic_access_value, holdable).
narrative_ontology:cs_axiom_grounding('c14ee99b-bcf7-4443-a3d9-c3326aba3008', universal_epistemic_access_value, instrumental).
narrative_ontology:cs_reference_frame('c14ee99b-bcf7-4443-a3d9-c3326aba3008', universal_preservation_authority).
narrative_ontology:cs_drift_state('c14ee99b-bcf7-4443-a3d9-c3326aba3008', contemporary_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c14ee99b-bcf7-4443-a3d9-c3326aba3008', '2026-02-27T14:33:22Z').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, major_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_heritage_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, collecting_nations).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_claimant_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, source_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOURCE COUNTRY / INDIGENOUS CLAIMANT (SNARE) — Trapped by international legal doctrine that treats their claim as particularist vs the 'universal good' of preservation. High extraction: legal costs (protracted repatriation litigation), diplomatic friction, identity harm (their cultural artifacts framed as belonging to 'humanity' rather than them), and epistemic erasure (museum framing controls interpretation). No exit from global legal regime that privileges holding institutions. Powerless in multilateral enforcement.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAJOR MUSEUMS / HOLDING INSTITUTIONS (ROPE) — Institutional beneficiaries (arbitrage exit). Experience the universal heritage framework as pure coordination: it legitimizes retention, attracts international funding, positions them as stewards of global patrimony. Zero extraction from their perspective — the constraint solves their coordination problem (how to justify possession of expropriated artifacts). Legal doctrine, UNESCO guidelines, and academic framing all support their authority claim.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WESTERN COLLECTING NATIONS (TANGLED ROPE) — Powerful institutional actors with complex position. They benefit from the universal heritage framing (legitimizes their retention of artifacts, supports soft power and cultural diplomacy). But they also face extraction: increasing repatriation costs, growing international criticism, eroding legitimacy claims as postcolonial critiques intensify. Mobile exit (can negotiate bilateral repatriations) but generational time horizon reveals costs accumulating — universities facing divestment pressure, museums under scrutiny. Mixed coordination (shared global heritage framework) and extraction (protecting national museums from repatriation liability).
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LAW REFORM MOVEMENT (SCAFFOLD) — Organized agents (NGOs, postcolonial scholars, repatriation advocates) see the universal heritage doctrine as a temporary institutional arrangement with a functional sunset. Constrained (face institutional inertia and museum resistance) but see a pathway: UNESCO Conventions on the Return of Cultural Property (1970, 1995, 2007) represent incremental revision. This perspective treats the universal heritage reading as a transitional doctrine that will be superseded by stronger source-country authority norms. Low effective extraction because the reform movement has agency and sees revision mechanisms.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CURATORIAL PROFESSIONALISM / ACADEMIC TRADITION (PITON) — The universal heritage reading persists largely through institutional inertia and professional identity fusion: curators' identity is bound to the museum's stewardship role, art historians' fields depend on artifact access, conservation science positions museums as necessary intermediaries. Theater ratio (0.55) reflects that museum stewardship and academic access claims are increasingly performative — private collectors and digital archives challenge the exclusivity claim. The constraint persists because the curatorial profession has 'become' the universal heritage doctrine; abandoning it would require identity dissolution, not just policy change.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational frame, the universal heritage reading can appear as a natural law: artifacts stored in stable, climate-controlled, well-funded institutions ARE preserved better than in resource-limited source countries. Shared global access IS epistemically valuable. These claims appear immutable — laws of conservation science and information economics. However, the structural data reveals this as a false summit: the 'necessity' of centralized preservation and the 'truth' of superior access are constructed by differential funding (Western investment in museum infrastructure), legal regimes (international law favoring holding institutions), and epistemic authority structures (whose preservation standards count). The engine will flag this as FSM: beneficiaries exist, eroding the mountain classification.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__universal_heritage_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, TR),
    TR >= 0.70.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The universal heritage reading exhibits moderate-to-high extraction. Source countries and indigenous communities bear legal costs (protracted repatriation litigation can run millions and take decades), diplomatic friction (repatriation claims strained UK-Egypt relations, US-Native American relations), identity harm (artifacts reframed as belonging to 'humanity' erases their significance as national/community property), and epistemic erasure (museum interpretation controls narrative). The beneficiaries (major museums) experience this extraction as coordination — the universal heritage framing legitimizes their retention. The rise in extractiveness over the measurement interval (0.35 → 0.52) reflects accumulating costs: accumulated repatriation litigation outcomes, diplomatic precedents, growing NGO pressure, and indigenous organizing. Suppression (0.48): Moderate. Legal barriers to repatriation exist (difficulty establishing provenance, CITES-like regulations favor holding institutions, international law enforcement asymmetries), but suppression has declined as source countries have increased legal capacity and indigenous communities have built organizing power (creating agency). The decline (0.55 → 0.48) reflects UNESCO Conventions increasing repatriation pathways, India's successful repatriation litigation, and the British Museum's growing repatriation commitments. Theater ratio (0.55): Moderate-to-high. Museum stewardship claims are increasingly performative. The preservation argument relies on Western-specific standards of climate control and archival practice, not universal preservation necessity. Digital access undermines the 'universal access' claim — most humans access cultural artifacts through images, not in situ museum visits. The rise in theater (0.42 → 0.55) reflects that the performative content has increased as the coordination function has been undermined by technology (digital reproduction) and institutional alternatives (source-country museums improving capacity). Claimed type (tangled_rope): Justified by the combination of genuine coordination (preservation, access) and asymmetric extraction (legal costs, identity harm, epistemic authority concentrated in Western institutions). The constraint requires active enforcement (holds in international law, litigation, professional norms). Beneficiaries and victims are clearly identifiable and asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence across all six types. The source country/indigenous claimant sees a snare (pure extraction: legal costs, diplomatic friction, identity harm, no exit from global legal regime). The major museum sees rope (pure coordination: legitimization, funding, authority). The Western collecting nation sees tangled rope (benefits mixed with rising extraction costs). The international law reform movement sees a scaffold (temporary institutional arrangement with a sunset path). The curatorial profession sees a piton (performative tradition maintained through professional identity inertia). The civilizational analytical observer risks seeing a mountain (universal preservation necessity) — but the structural data reveals this as a false summit because identifiable beneficiaries exist (museums gain authority and funding). The perspectival gap is not merely observational — it reveals that the universal heritage reading naturalizes the beneficiary's interests (museums, Western collecting nations) while treating claimant interests as particularist threats to a universal good.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's position in the extraction flow. Major museums: beneficiaries with arbitrage exit (can negotiate bilateral repatriations) → low d → negative χ (experience the constraint as beneficial coordination). Source countries/indigenous claimants: victims with constrained/trapped exit (legal barriers, diplomatic costs, identity lock) → high d → high χ (experience high extraction). Western collecting nations: positioned as both beneficiaries (benefit from authority claim, soft power) and facing extraction costs (repatriation liability, legitimacy erosion) → moderate d → moderate χ (mixed experience). International law reform movement: organized with constrained exit (institutional inertia in museum resistance) but seeing sunset pathway → low-moderate d (agency present) → low χ (scaffolding perspective with exit path visible). Curatorial profession: identity-locked (professional identity fused to museum stewardship role) → high d for identity_locked exit option (cannot exercise structural mobility due to cognitive capture). The piton and mountain perspectives derive from different interpretations of the same structural reality: one sees performance masking function (piton), the other mistakes contingency for necessity (false-summit mountain).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through cross-position analysis: the constraint is classified as six distinct types depending on the observer's structural position, and these classifications are not contradictions but perspectival readings of asymmetric extraction. The coordination function (preservation, access) is genuine — museums DO perform these services. But the extraction asymmetry is also genuine — benefits and costs are not equally distributed. The tangled_rope classification (Constraint's claimed type) captures both: this is a coordination mechanism (legitimate function) with asymmetric extraction (unjust distribution). The false-summit mountain perspective reveals the critical diagnostic: when the analytical observer mistakes coordination necessity for natural law, they are naturalizing a contingent institutional arrangement that benefits identifiable parties (museums, Western institutions). The FSM engine signature will flag this: beneficiaries are declared, so the mountain classification triggers false-summit evaluation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preservation_sufficiency_threshold,
    'What standard of preservation justifies overriding source-country authority claims? Is climate-controlled storage necessary, or is cultural continuity maintenance (even at lower technical preservation standards) equally legitimate?',
    'Longitudinal comparison of artifact condition across holdings (museum collections vs source-country or indigenous-controlled collections); ethnographic assessment of cultural transmission integrity in source contexts; willingness-to-accept studies on source communities'' preservation preferences.',
    'If Western preservation standards are necessary and source contexts cannot meet them: universal heritage reading''s extraction is justifiable coordination cost. If source-country preservation is adequate for cultural transmission: extraction is unjustified, and reading forecloses indigenous stewardship and sovereign repatriation readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_sufficiency_threshold, empirical, 'Whether centralized Western preservation is uniquely necessary or culturally particularist').

omega_variable(
    universal_access_epistemic_value,
    'Does universal access to centralized museum holdings actually produce greater scholarly understanding and cultural appreciation than decentralized source-country access supplemented by digital reproduction?',
    'Comparative analysis of scholarly output using centralized vs decentralized collections; citation patterns; diversity and intellectual authority of interpretive scholarship (whose interpretations dominate); accessibility metrics (who actually accesses collections — wealthy tourists and scholars vs source-community members).',
    'If universal access (in centralized institutions) produces superior scholarship: justifies extraction. If decentralized + digital access produces equivalent or superior scholarship: access claim fails, and reading appears as pure extraction masquerading as epistemic coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_epistemic_value, empirical, 'Whether centralized collections produce superior scholarship vs decentralized access').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is this constraint fundamentally a coordination mechanism (solving the collective-action problem of artifact preservation) or an extraction mechanism (legitimizing Western institutional benefit and control)?',
    'Analysis of who bears costs (source countries: legal costs, diplomatic friction, identity harm, epistemic erasure) vs who benefits (museums: funding, soft power, research access, authority). If cost-bearer = different population from beneficiary, extraction is primary. If cost-bearer = beneficiary, coordination is primary.',
    'If extraction primary: constraint should reclassify as snare under all perspectives where costs are measured. If coordination primary: constraint justifies high suppression and legitimizes holding-institution authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether constraint is coordination or extraction').

omega_variable(
    committer_reading_underdetermination,
    'The universal heritage reading is ONE committer interpretation of the cultural property kernel. The indigenous stewardship and sovereign repatriation readings are incommensurable frameworks grounding legitimate authority in different sources (community continuity vs state succession). Which reading''s core premise is correct: universal access (universal_heritage_reading), indigenous control (indigenous_stewardship_reading), or state repatriation (sovereign_repatriation_reading)?',
    'This is a conceptual omega — not empirically resolvable. The resolution depends on which foundational axiom you hold: whether cultural property is a global commons (universal), a sovereign national resource (repatriation), or a sacred/identity trust (indigenous stewardship). These axioms are incommensurable within a single framework. Cross-position analysis reveals that each reading naturalizes its own beneficiary''s interests: universal access naturalizes museum authority, sovereignty naturalizes state authority, stewardship naturalizes community authority.',
    'The three readings are in genuine logical conflict — this reading (universal_heritage_reading) coexists with but does NOT resolve the conflict with indigenous_stewardship_reading and sovereign_repatriation_reading. The engine will classify this constraint structure as having high perspectival coupling precisely because the readings are incommensurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_underdetermination, conceptual, 'Which foundational axiom (universal access, state sovereignty, indigenous stewardship) grounds legitimate authority?').

omega_variable(
    identity_fusion_in_museum_piton,
    'Is the curatorial profession''s attachment to the universal heritage doctrine a genuine coordination function (stewardship identity is necessary to perform preservation), or identity fusion that binds practitioners to a particular institutional arrangement?',
    'Ethnographic study of curator career identity; analysis of whether repatriation aligns curators'' identity with source communities (cooperative stewardship) or dissolves their professional identity; comparison of career outcomes and identity satisfaction across repatriation-accepting vs repatriation-resistant institutions.',
    'If genuine coordination: piton classification is structural (functional degradation while identity persists). If identity fusion: the piton is a manifestation of cognitive capture, and the universal heritage reading''s extractiveness is higher than measured (suppression of alternative institutional arrangements through professional identity lock).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_museum_piton, empirical, 'Whether curator professional identity is coordination-functional or identity-fused').

omega_variable(
    false_summit_mountain_naturalization,
    'Is the mountain perspective (analytical/civilizational) truly identifying an immutable natural law of preservation and access, or naturalizing a contingent institutional arrangement (Western-funded museums, international law favoring holding institutions, academic prestige concentrated in Western institutions)?',
    'Counterfactual analysis: if Western museum funding dried up and source countries invested in distributed local preservation infrastructure, would centralized preservation be ''necessary''? If legal regimes changed (international law enforced repatriation), would universal access via centralized institutions remain ''superior''? Historical analysis of when universal heritage doctrine emerged (post-WWII, accompanying decolonization resistance) and what interests it served.',
    'If mountain classification is legitimate: preservation science and access economics justify holding-institution authority. If false summit: the constraint is a tangled_rope with high extraction, and the universal heritage reading is one beneficiary-serving interpretation among incommensurable alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_naturalization, conceptual, 'Whether mountain perspective reveals natural law or naturalizes institutional contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cultprop_univ_theater_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cultprop_univ_theater_t25, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(cultprop_univ_theater_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(cultprop_univ_extractiveness_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cultprop_univ_extractiveness_t25, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(cultprop_univ_extractiveness_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cultprop_univ_suppression_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cultprop_univ_suppression_t25, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(cultprop_univ_suppression_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, museum_authority_and_legitimacy).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, postcolonial_legal_doctrine_enforcement).

% DUAL FORMULATION NOTE:
% The cultural property legal corpus is a three-reading kernel: universal_heritage_reading, sovereign_repatriation_reading, and indigenous_stewardship_reading. Each reading is a distinct constraint story with its own ε, beneficiary/victim structure, and classification. They are not measurements of the same constraint from different angles — they are incommensurable frameworks grounding authority in different sources (universal access/preservation, state succession, community continuity). The universal_heritage_reading (this story) treats the other readings' authority claims as particularist threats. The sovereign_repatriation_reading treats the universal reading's preservation claim as colonial preservation of colonialism. The indigenous_stewardship_reading treats both as erasure of indigenous epistemic authority. Network links document these family relationships and mutual influence (each reading's success undermines the others' legitimacy premises).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
