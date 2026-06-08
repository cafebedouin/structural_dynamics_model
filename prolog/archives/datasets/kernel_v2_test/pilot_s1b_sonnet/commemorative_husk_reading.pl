% ============================================================================
% CONSTRAINT STORY: commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commemorative_husk_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: commemorative_husk_reading
 *   human_readable: Aneyoshi Stone as Commemorative Husk (Post-Commitment Decay Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone memorial in Japan's Iwate Prefecture bears the
 *   inscription 'Remember the calamity of the great tsunamis. Do not build
 *   any homes below this point.' Erected after the 1933 Sanriku tsunami, the
 *   stone was intended as a perpetual behavioral constraint on settlement
 *   patterns. This reading—the commemorative husk reading—models the stone as
 *   a commitment system that has undergone generational decay: the monument
 *   persists as a visible cultural artifact, maintained through heritage
 *   preservation funding and tourism infrastructure, but no longer exerts
 *   behavioral constraint on land-use decisions. The stone survives as
 *   symbolic capital extracted by the tourism economy and regional identity
 *   narrative while providing no protection to coastal residents who build
 *   below the marked line. Theater ratio (0.85) reflects that stone
 *   maintenance is almost entirely performative—preservation funding,
 *   heritage site designation, and touristic framing—with minimal functional
 *   connection to actual disaster preparedness or land-use enforcement. The
 *   2011 Tōhoku tsunami created an attribution narrative: Aneyoshi survived
 *   because residents 'heeded the stone,' but this reading treats that
 *   attribution as potentially erroneous—survival may have resulted from
 *   topographic luck, wave dynamics, or other factors, with the stone
 *   retrospectively credited. The commemorative husk reading is one of two
 *   structural interpretations of the same kernel; the sibling behavioral
 *   competence reading models the stone as a functioning constraint that
 *   successfully coordinated evacuation behavior.
 *
 * KEY AGENTS:
 *   - Coastal Residents Below Stone Line: Primary victims (powerless/trapped) — bear tsunami risk while stone functions as decoration; cannot exit (housing decisions constrained by economics, not by ancestral warnings)
 *   - Tourism and Cultural Heritage Sector: Primary beneficiaries (institutional/arbitrage) — extract economic value from stone's symbolic capital through heritage tourism, preservation grants, regional identity branding
 *   - Regional Identity Narrative: Secondary beneficiary (institutional/arbitrage) — vindicated by stone's persistence; 'our ancestors were wise' narrative gains authority from monument's survival regardless of behavioral function
 *   - Disaster Preparedness Community: Secondary victim (moderate/constrained) — stone's symbolic authority crowds out evidence-based risk assessment; attribution error (crediting survival to stone rather than topography) suppresses investment in genuine preparedness infrastructure
 *   - Municipal Planning Authority: Organized institutional actor (organized/mobile) — maintains stone through heritage budget allocations but permits construction below line when economic pressure demands; sees stone's function as atrophied but continues theater through institutional inertia
 *   - Epistemic Commons (Disaster Preparedness): Abstract collective victim (powerless/trapped) — contaminated by attribution error; false positive (stone credited with causal efficacy it may not possess) degrades collective knowledge about what actually works
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commemorative_husk_reading, 0.78).
domain_priors:suppression_score(commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(commemorative_husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(commemorative_husk_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commemorative_husk_reading, piton).
narrative_ontology:human_readable(commemorative_husk_reading, "Aneyoshi Stone as Commemorative Husk (Post-Commitment Decay Reading)").
narrative_ontology:topic_domain(commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commemorative_husk_reading, '84129d9f-d19d-47eb-8ce6-56e7d705be4e').
narrative_ontology:cs_kernel_codification('84129d9f-d19d-47eb-8ce6-56e7d705be4e', fixed_text).
narrative_ontology:cs_authority_grounding('84129d9f-d19d-47eb-8ce6-56e7d705be4e', extraction).
narrative_ontology:cs_reading_relation('84129d9f-d19d-47eb-8ce6-56e7d705be4e', commemorative_husk_reading__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('84129d9f-d19d-47eb-8ce6-56e7d705be4e', foundational, monuments_decay_to_symbolic_husks).
narrative_ontology:cs_axiom_status(monuments_decay_to_symbolic_husks, holdable).
narrative_ontology:cs_axiom_grounding('84129d9f-d19d-47eb-8ce6-56e7d705be4e', monuments_decay_to_symbolic_husks, empirically_contingent).
narrative_ontology:cs_axiom('84129d9f-d19d-47eb-8ce6-56e7d705be4e', secondary, symbolic_persistence_enables_extraction).
narrative_ontology:cs_axiom_status(symbolic_persistence_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('84129d9f-d19d-47eb-8ce6-56e7d705be4e', symbolic_persistence_enables_extraction, instrumental).
narrative_ontology:cs_reference_frame('84129d9f-d19d-47eb-8ce6-56e7d705be4e', immediate_post_1933_commitment).
narrative_ontology:cs_drift_state('84129d9f-d19d-47eb-8ce6-56e7d705be4e', contemporary_pre_2011, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('84129d9f-d19d-47eb-8ce6-56e7d705be4e', '').
narrative_ontology:cs_kernel_id(commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, tourism_economy).
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, cultural_heritage_preservation_sector).
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, regional_identity_narrative).
narrative_ontology:constraint_victim(commemorative_husk_reading, coastal_residents_below_stone_line).
narrative_ontology:constraint_victim(commemorative_husk_reading, epistemic_commons_disaster_preparedness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commemorative_husk_reading, disaster_preparedness_community).
narrative_ontology:constraint_vindicates(commemorative_husk_reading, monuments_outlive_meanings).
narrative_ontology:constraint_vindicates(commemorative_husk_reading, symbolic_persistence_without_behavioral_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live and build below the stone's marked line. Housing decisions follow economic logic—land cost, proximity to employment, family ties—not ancestral warnings. The stone is visible as a cultural landmark but does not constrain where they build. Bear tsunami risk. Cannot easily relocate due to economic constraints and community ties.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, coastal_residents_below_stone_line, payer,
    powerless, biographical, trapped, local).

% Operate tour companies, souvenir shops, hospitality businesses that derive revenue from heritage tourism centered on the stone. The stone attracts visitors interested in disaster history and cultural resilience narratives. Collect economic value from the monument's symbolic capital. Can redirect investment to other heritage sites if the stone becomes less profitable.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, tourism_economy, beneficiary,
    institutional, immediate, arbitrage, regional).

% Manage heritage site designation, administer preservation grants, maintain the physical monument, produce interpretive materials. The stone is a flagship example of traditional disaster wisdom and justifies ongoing funding for cultural preservation programs. Collect budget allocations and institutional authority from the monument's existence. Can reallocate resources to other heritage projects without significant cost.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, cultural_heritage_preservation_sector, beneficiary,
    institutional, generational, arbitrage, national).

% A non-agent entity (the regional identity claim 'our ancestors were wise and we honor their warnings') that gains authority from the stone's persistence. The narrative is deployed in public discourse, regional branding, and political rhetoric. Beneficiary classification reflects that the narrative is vindicated by the monument's survival, but it is not a real-world actor and is excluded from directionality computation.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, regional_identity_narrative, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_non_agent(commemorative_husk_reading, regional_identity_narrative).

% Engineers, emergency planners, risk analysts who design and implement evidence-based disaster preparedness systems. Face epistemic contamination: the stone's symbolic authority and the attribution narrative ('Aneyoshi survived because of the stone') crowd out systematic analysis of what actually protects communities. Investment in seawalls, early warning systems, and evacuation infrastructure is suppressed by the belief that traditional warnings suffice. Can challenge the narrative but at political and social cost.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, disaster_preparedness_community, payer,
    moderate, generational, constrained, national).

% Issue construction permits, allocate municipal budgets, maintain the stone as a heritage site. Permit construction below the stone line when economic pressure demands it—the stone's directive has no enforceable legal weight. Allocate heritage preservation budget to stone maintenance (signage, pathways, cleaning) while land-use decisions proceed independently. See the stone's function as atrophied but continue theater through institutional inertia and tourism revenue considerations. Can redirect funding or rezone without significant cost.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, municipal_planning_authority, agenda_setter,
    organized, generational, mobile, regional).

% The collective epistemic resource about what disaster preparedness interventions actually work. Contaminated by attribution error: if the stone is credited with causal efficacy it may not possess, the false positive degrades the commons. Has no advocate and no mechanism to correct the error. Cannot exit the contamination. Non-agent: the commons is an abstract collective good, not a real-world actor, and is excluded from directionality computation.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, epistemic_commons_disaster_preparedness, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(commemorative_husk_reading, epistemic_commons_disaster_preparedness).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: coordinate settlement patterns to minimize tsunami casualties by establishing a clear behavioral boundary (do not build below this line). In this reading, that function has atrophied—the stone no longer constrains land-use.
% TRANSFER_FUNCTION: Economic and symbolic value flows from coastal residents (who bear risk without protection) and disaster preparedness community (whose evidence-based resources are crowded out) to tourism economy, heritage preservation sector, and regional identity narrative (who collect revenue, funding, and authority from the monument's symbolic persistence).
% ABSENT_VOICES: Future coastal residents who will experience the next tsunami are structurally excluded—they cannot participate in current land-use decisions and have no advocate in the heritage preservation vs functional preparedness trade-off. The dead (1933 tsunami victims whose memory the stone supposedly honors) are symbolically present but obviously cannot contest how their legacy is used. If the stone's behavioral force has genuinely eroded and the attribution narrative is false, then residents are excluded from the truth: the consensus ('the stone works') arises not because it is real but because dissenting evidence (land-use data, topographic analysis) is not in the conversation.
% DISAPPEARANCE_RATIONALE: If the stone disappeared overnight, the tourism economy and heritage sector would rearrange (loss of a revenue-generating landmark). Municipal preservation budgets would redirect. Regional identity narratives would need a different anchor. In that sense, arrangements depend on the stone. However, land-use patterns—the actual settlement behavior the stone was meant to govern—would likely remain unchanged, because in this reading the stone already exerts no behavioral constraint. The verdict is contested because different stakeholders have different dependencies: beneficiaries depend on the stone's symbolic presence, but victims do not benefit from its behavioral absence.
% FOUNDING_PROBLEM: The stone was erected after the 1933 Sanriku tsunami, which killed thousands. The founding problem was prevention of future tsunami casualties through intergenerational transmission of safe settlement patterns. The directive 'do not build below this point' was meant to embed disaster preparedness in land-use decisions across generations who had not experienced the 1933 event.
% FOUNDING_PROBLEM_CORROBORATION: Status is contested between two framings: (1) The founding problem is DEAD if safe settlement patterns can now be achieved through modern engineering (seawalls, early warning systems, building codes), making the ancestral directive obsolete. The stone persists as a cultural relic while the real preparedness work happens elsewhere. (2) The founding problem is LIVE if intergenerational knowledge transmission about tsunami risk remains necessary and no alternative mechanism has replaced the stone's intended function. Corroboration: Disaster preparedness engineers (outside the beneficiary set) can assess whether contemporary coastal communities have effective tsunami risk mitigation independent of the stone. If Aneyoshi's 2011 survival is attributable to the stone's behavioral influence, the problem is live. If attributable to topography or luck, the problem is live but the stone is not solving it. Heritage sector (beneficiary) claims the problem is live and the stone is the solution, but they have an extractive interest in that claim. Post-2011 land-use data (construction permits above vs below line, 2011-2025) would corroborate whether the stone has regained behavioral force or remains a husk.
narrative_ontology:disappearance_verdict(commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(commemorative_husk_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL RESIDENT (PITON) — The stone exists as a visible artifact but exerts no behavioral constraint on where to build. Housing decisions follow economic logic (land cost, job proximity) not ancestral warnings. The stone is cultural decoration maintained through tourism funding and heritage preservation theater. High theater ratio, low functional constraint. Victim of extraction: bears tsunami risk while beneficiaries collect from the monument's symbolic capital without providing protection.
constraint_indexing:constraint_classification(commemorative_husk_reading, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TOURISM/HERITAGE SECTOR (ROPE) — The stone functions as genuine coordination infrastructure: it attracts visitors, anchors regional identity narratives, and provides economic value through heritage tourism. The constraint coordinates stakeholder activity around preservation funding, tour routes, and cultural programming. Low extractiveness from this perspective: the coordination function is real and the sector experiences net benefit.
constraint_indexing:constraint_classification(commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: DISASTER PREPAREDNESS COMMUNITY (SNARE) — The stone's persistence as 'proof' that traditional warnings work creates epistemic contamination: survival of Aneyoshi in 2011 is attributed to the stone rather than to luck, topography, or other factors. This attribution error suppresses investment in genuine preparedness infrastructure and evidence-based risk assessment. The community experiences extraction: the stone's symbolic authority crowds out systematic analysis. Moderate power because they have technical capacity but are politically constrained; exit is possible but costly (requires challenging cultural narratives).
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MUNICIPAL PLANNING (PITON) — The stone exists in the regulatory environment as a heritage site requiring maintenance funding but providing no enforceable land-use constraint. Zoning decisions proceed independently of the stone's directive. The authority maintains the stone through budget allocations (theater) while permitting construction below the line when economic pressure demands it. Mobile exit: can redirect funding or rezone without significant cost. Piton classification from recognition that the stone's function has atrophied.
constraint_indexing:constraint_classification(commemorative_husk_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / DECAY-AS-NATURAL-LAW (MOUNTAIN) — From a civilizational perspective, commitment decay over generational timescales may appear as an immutable feature of human institutions: all monuments eventually lose their behavioral force and become symbolic husks. This framing naturalizes the decay process as inevitable. However, the structural data contradicts this: the decay is maintained by identifiable beneficiaries (tourism sector, heritage industry) who extract value from the stone's symbolic persistence while suppressing its behavioral function. The analytical perspective risks false summit: treating contingent institutional arrangements (heritage preservation funding, tourism economy, cultural identity politics) as laws of institutional physics.
constraint_indexing:constraint_classification(commemorative_husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commemorative_husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commemorative_husk_reading, TR),
    TR >= 0.70.

:- end_tests(commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The stone's persistence as a monument generates economic and symbolic value captured by tourism sector, heritage preservation industry, and regional identity narrative—none of whom provide actual tsunami protection. Coastal residents bear the risk (victim status) while beneficiaries collect from the monument's symbolic capital. The extraction is substantial but not maximal because some residual coordination function may exist (weak behavioral influence on some residents, genuine cultural transmission value). The 1950-2010 trajectory shows increasing extractiveness as the monument's behavioral force atrophied while its symbolic-economic value grew. Suppression (0.15): Low. The constraint exerts minimal coercive force—residents are not prevented from building below the stone line by any enforcement mechanism. The suppression trajectory (0.45 → 0.15 over 60 years) reflects decay of social enforcement: in 1950 there may have been genuine community pressure against violating the stone's directive; by 2010 that pressure has almost entirely eroded. The low terminal suppression is diagnostic of piton classification: the constraint persists through inertia and theater, not through enforcement. Theater ratio (0.85): Very high. The stone is maintained almost entirely through performative mechanisms: heritage site designation, preservation funding, tourism infrastructure (signage, visitor pathways, interpretive plaques), regional identity narratives, and commemorative rituals. Functional disaster preparedness activity around the stone (enforcement of building restrictions, systematic evacuation planning tied to the stone's location) is minimal. The 1950-2010 trajectory (0.35 → 0.85) shows theater replacing function: early in the interval some behavioral constraint may have existed; by 2010 the stone is primarily a museum piece.
 *
 * PERSPECTIVAL GAP:
 *   The tourism/heritage sector sees rope (genuine coordination—the stone anchors regional identity and economic activity). Coastal residents and disaster preparedness community see snare or piton (extraction without protection, or atrophied function maintained as theater). Municipal planning sees piton (degraded function sustained by institutional inertia). The analytical observer risks mountain (naturalizing decay as inevitable institutional physics) but structural data reveals false summit: the decay is not a law of nature but a maintained arrangement benefiting identifiable actors. The gap between the beneficiary's rope and the victim's piton/snare is diagnostic: when one party experiences coordination and another experiences theater-without-function from the same constraint, the theater is serving an extractive purpose. The perspectival gap also reveals the kernel reading structure: the same monument is simultaneously 'functioning behavioral constraint' (behavioral competence reading, sibling constraint) and 'decayed symbolic husk' (this reading) depending on which observable is measured—land-use patterns vs cultural transmission, construction permits vs tourism revenue, actual vs attributed causation in 2011 survival.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure drives directionality: Tourism sector and heritage industry are clear beneficiaries with arbitrage exit options—they collect economic value from the stone's symbolic capital and can redirect investment if the monument becomes less profitable. Their derived d is low, producing low or negative effective extraction (they experience the constraint as coordination). Coastal residents below the stone line are victims with trapped exit options—they bear tsunami risk while receiving no protection from the stone, and cannot easily relocate due to economic constraints. Their derived d is high, producing high effective extraction (they experience maximum burden). Disaster preparedness community is a secondary victim with constrained exit—they face epistemic contamination (attribution error crowds out evidence-based analysis) but have agency to challenge the narrative at some cost. Their d is moderate-high, producing moderate extraction. The municipal planning authority is an organized actor with mobile exit—they maintain the stone through budget allocation but face minimal cost from redirecting resources or ignoring the stone's directive. Their d is low, producing low extraction. The analytical observer risks naturalizing the decay process (treating generational commitment erosion as inevitable) despite identifiable beneficiaries maintaining the husk structure for extractive purposes.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved: The stone's original mandate (behavioral constraint on settlement to prevent tsunami casualties) has outlived its function in this reading. The monument persists not because it prevents casualties but because it generates tourism revenue, anchors regional identity, and receives heritage preservation funding. The mandate-function gap is the extraction mechanism: beneficiaries maintain the symbolic artifact while victims bear the risk the artifact was meant to eliminate. This is a clean piton case: atrophied mandate sustained as performance, with identifiable extractive beneficiaries. The theater ratio (0.85) quantifies the mandate-function gap. The suppression decay (0.45 → 0.15) shows the original mandate's enforcement eroding over time. The mandatrophy is resolved not by sunset (constraint termination) but by conversion to extractive husk—the form persists, the function is gone, and someone profits from the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the Aneyoshi stone a decayed commitment (commemorative husk) or a functioning behavioral constraint (behavioral competence)?',
    'Land-use pattern analysis: residential construction rates below vs above stone line 1950-2010; interview data on decision factors for building site selection; correlation between stone visibility and actual evacuation behavior in 2011.',
    'If commemorative husk: high extractiveness (tourism/heritage sectors collect from symbolic capital without providing protection). If behavioral competence: low extractiveness (stone coordinates genuine preparedness). Readings coexist across different observer positions: residents may see husk while external analysts see competence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Whether stone functions as behavioral constraint or symbolic artifact').

omega_variable(
    attribution_error_magnitude,
    'To what extent is Aneyoshi''s 2011 survival attributed to the stone versus other factors (topography, luck, wave dynamics)?',
    'Comparative analysis: survival rates in communities with vs without stone markers controlling for elevation, distance from shore, and wave characteristics; counterfactual modeling of 2011 tsunami impact across different topographies.',
    'If attribution is accurate: stone''s coordination function is real, extractiveness lower. If attribution error: stone''s symbolic authority crowds out evidence-based preparedness, extractiveness higher. High attribution error strengthens the piton classification: the monument''s reputation exceeds its causal contribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_error_magnitude, empirical, 'Accuracy of causal attribution for 2011 survival').

omega_variable(
    commitment_revival_possibility,
    'Can the 2011 tsunami event re-activate the stone''s behavioral force, or does the commemorative husk structure resist re-commitment?',
    'Longitudinal tracking post-2011: construction permits above vs below stone line 2011-2025; municipal ordinance changes referencing the stone; funding allocation patterns for stone maintenance vs active preparedness infrastructure.',
    'If revival occurs: scaffold trajectory (temporary decay followed by renewal). If husk structure persists: piton confirmation (decay is sticky, institutional inertia dominates). This omega addresses whether the reading is stable or a snapshot of a transitional state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_revival_possibility, empirical, 'Whether 2011 event can reverse commitment decay').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the stone artifact itself, or the interpretive tradition about ancestral warnings?',
    'Historical analysis: when did ''the stone'' become the authoritative referent versus ''what the ancestors said''? Textual analysis of how the commitment is cited in public discourse. This is a framing choice, not an empirical fact.',
    'If kernel = stone artifact: cs_structure correctly models a formalized kernel with fixed_text authority grounding. If kernel = interpretive tradition: cs_structure should use lineage authority grounding with distributed kernel. The commemorative husk reading assumes kernel = stone artifact (the physical monument is the stabilized commitment). The behavioral competence reading might assume kernel = interpretive tradition (living practice sustained by lineage).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether CS kernel is stone artifact or interpretive tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commemorative_husk_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1950, commemorative_husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1965, commemorative_husk_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(theater_1980, commemorative_husk_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(theater_1995, commemorative_husk_reading, theater_ratio, 45, 0.78).
narrative_ontology:measurement(theater_2010, commemorative_husk_reading, theater_ratio, 60, 0.85).

% Extraction over time
narrative_ontology:measurement(extract_1950, commemorative_husk_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(extract_1965, commemorative_husk_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(extract_1980, commemorative_husk_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(extract_1995, commemorative_husk_reading, base_extractiveness, 45, 0.7).
narrative_ontology:measurement(extract_2010, commemorative_husk_reading, base_extractiveness, 60, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1950, commemorative_husk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(suppress_1980, commemorative_husk_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(suppress_2010, commemorative_husk_reading, suppression_requirement, 60, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The commemorative_husk_reading and behavioral_competence_reading form a constraint family modeling the same kernel (Aneyoshi stone) from two structurally distinct framings. The ε values differ by a wide margin: husk reading has high extraction (tourism/heritage capture symbolic capital), competence reading has low extraction (stone coordinates genuine preparedness). The observables differ: husk reading measures land-use patterns and heritage funding flows; competence reading measures cultural transmission and attributed causation. Both readings are empirically testable via the omega variables. The constraint family models the kernel reading structure: contested commitments where different parties hold different structural interpretations of the same monument.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
