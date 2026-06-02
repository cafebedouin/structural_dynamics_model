% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Cultural Property Legal Corpus: Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   Under the indigenous stewardship reading, cultural artifacts held by
 *   museums, colonial successor states, and other non-indigenous institutions
 *   represent extracted epistemic authority and spiritual sovereignty. The
 *   constraint operates between indigenous communities (stewards with
 *   legitimate authority grounded in cultural continuity, ceremonial
 *   practice, and epistemological integration of objects) and holding
 *   institutions (museums, states) that claim authority through possession,
 *   international law, and universal heritage ideology. The reading treats
 *   the legal corpus governing cultural property — UNESCO conventions,
 *   national patrimony laws, museum acquisition protocols — as a regime that
 *   naturalizes extraction: institutions that hold artifacts are treated as
 *   legitimate stewards while indigenous communities are reduced to
 *   'stakeholders' or 'source communities' within their own cultural
 *   inheritance. The constraint's core mechanism is institutional authority
 *   asymmetry: museums and states command legal standing, curatorial
 *   expertise, and resource capacity, while indigenous communities must
 *   petition for return within frameworks designed to legitimize retention.
 *   Theater ratio (0.58) reflects that much institutional discourse around
 *   'universal heritage,' 'preservation,' and 'scholarly access' performs
 *   coordination while actually legitimizing extraction — the rhetoric of
 *   stewardship masks the power asymmetry. Extractiveness has accumulated
 *   over the interval as repatriation frameworks have formalized without
 *   fundamentally shifting authority: NAGPRA (1990), UNDRIP (2007), and
 *   Kunming-Montreal Biodiversity Agreement (2022) have created paths for
 *   repatriation but within institutional terms that require communities to
 *   prove legitimacy rather than institutions to prove theirs.
 *
 * KEY AGENTS:
 *   - Indigenous Communities: Primary victims (powerless/trapped) — lose spiritual authority, ceremonial capacity, epistemic sovereignty over their own cultural inheritance; cannot command return within existing frameworks
 *   - Indigenous Cultural Continuity: Primary victim / abstract commons (powerless/trapped) — epistemic commons of indigenous knowledge transmitted through artifacts and ceremony; treats itself as resource available for institutional extraction
 *   - Museums and Collecting Institutions: Primary beneficiaries (institutional/arbitrage) — extract curatorial authority, scholarly legitimacy, institutional prestige, and often revenue from holding artifacts; can repatriate if incentives shift but no current incentive exists
 *   - Colonial Successor States: Secondary beneficiary (institutional/arbitrage) — claim national patrimony authority through territorial sovereignty; use state law to supersede indigenous authority; coordinate with museums through national cultural property law
 *   - Indigenous Rights Coalitions: Organized resistance (organized/constrained) — have some agency through international advocacy, legal challenges, repatriation protocols, but face persistent institutional resistance and resource asymmetry
 *   - Analytical Observer: Risks naturalizing institutional inevitability (analytical/analytical) — false summit candidate; treats state-centric international law as immutable feature of how cultural property must be governed rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.68).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.75).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Legal Corpus: Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, 'be0be06f-8ce0-4a23-973e-83a8a5fe5013').
narrative_ontology:cs_kernel_codification('be0be06f-8ce0-4a23-973e-83a8a5fe5013', formalized).
narrative_ontology:cs_authority_grounding('be0be06f-8ce0-4a23-973e-83a8a5fe5013', extraction).
narrative_ontology:cs_interpretation_layer_present('be0be06f-8ce0-4a23-973e-83a8a5fe5013').
narrative_ontology:cs_reading_relation('be0be06f-8ce0-4a23-973e-83a8a5fe5013', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('be0be06f-8ce0-4a23-973e-83a8a5fe5013', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('be0be06f-8ce0-4a23-973e-83a8a5fe5013', foundational, indigenous_continuity_legitimacy).
narrative_ontology:cs_axiom_status(indigenous_continuity_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('be0be06f-8ce0-4a23-973e-83a8a5fe5013', indigenous_continuity_legitimacy, deontological).
narrative_ontology:cs_axiom('be0be06f-8ce0-4a23-973e-83a8a5fe5013', foundational, institutional_possession_illegitimacy).
narrative_ontology:cs_axiom_status(institutional_possession_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('be0be06f-8ce0-4a23-973e-83a8a5fe5013', institutional_possession_illegitimacy, deontological).
narrative_ontology:cs_reference_frame('be0be06f-8ce0-4a23-973e-83a8a5fe5013', indigenous_epistemic_stewardship).
narrative_ontology:cs_drift_state('be0be06f-8ce0-4a23-973e-83a8a5fe5013', post_colonial_institutional_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be0be06f-8ce0-4a23-973e-83a8a5fe5013', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_cultural_continuity).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_epistemic_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped by global legal architecture and institutional power asymmetry. Cannot exit the regime that holds their sacred artifacts; cannot command repatriation through any mechanism commensurate with their authority claim. Trapped exit option because the structural barriers (sovereign immunity of museums and states, international law precedent favoring possession, resource asymmetry) are insurmountable within current frameworks. Maximum extraction: communities lose spiritual authority, ceremonial capacity, and epistemic sovereignty over their own cultural inheritance.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS CULTURAL CONTINUITY (SNARE) — Abstract collective good that cannot organize or exit. The epistemic commons of indigenous knowledge — transmitted through material artifacts, ceremony, and place — is treated as a resource available for extraction. Communities cannot protect this commons from institutional appropriation, commodification, or interpretive colonization. Trapped: no exit option exists for the continuity itself; it is embedded in the constraint.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MUSEUMS AND COLLECTING INSTITUTIONS (ROPE) — Institutional beneficiaries experiencing the constraint as coordination: universal heritage ideology legitimates global curation, scholarly access, and institutional sustainability through artifact holding. Museums perceive the constraint as solving a genuine coordination problem — how to preserve and study culturally significant objects. Under this reading, they see themselves as performing a coordination function while actually extracting authority from indigenous stewardship claims. Arbitrage exit means institutions can repatriate if incentives shift, but they have no motivation to do so within the universal heritage frame.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLONIAL SUCCESSOR STATES (ROPE) — Institutional beneficiaries experiencing the constraint as solving the coordination problem of national cultural authority. Successor states use international legal frameworks (UNESCO, national patrimony law) to claim stewardship of all artifacts from their territorial jurisdiction, displacing indigenous authority claims with state sovereignty claims. The constraint coordinates state-to-state artifact management while extracting from indigenous communities. Arbitrage: states can negotiate international agreements, but the constraint requires no change to their extraction benefits.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIGENOUS RIGHTS COALITIONS (TANGLED ROPE) — Organized agents (UNDRIP signatories, indigenous legal networks, repatriation coalitions) have agency and some exit capacity through international advocacy, legal challenges, and soft power. The constraint both enables (provides coordination mechanism for collective claims) and extracts (narrow victories, slow bureaucratic repatriation). Constrained exit: coalitions can pursue repatriation, but success requires sustained resource expenditure and faces institutional resistance. The constraint's mixed nature reflects that coordination mechanisms (international frameworks, repatriation protocols) do exist, but extraction remains high because they operate within colonial legal structures.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL INEVITABILITY (MOUNTAIN) — From a civilizational analytical perspective, the constraint appears as an immutable feature of state-centric international law: once artifacts enter museum/state custody, international frameworks treat possession as the default, reversal as exceptional. The mountain perspective risks naturalizing what is actually a contingent institutional choice — the decision to treat state sovereignty and museum custody as the legitimate default rather than indigenous stewardship. This is a false summit candidate: the engine will detect beneficiaries (museums, states) and flag this as naturalization of institutional extraction.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__indigenous_stewardship_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Under the indigenous stewardship reading, the extraction narrative is clear: institutions (museums, states) hold artifacts that communities claim as their epistemic and spiritual property; the communities cannot recover them through proportionate mechanisms; the legal apparatus treats institutional holding as the default rather than exceptional. The extractiveness is not maximal (0.72+) because some repatriation has occurred, some communities have achieved partial restoration of authority, and the institutional opposition is not totalizing — it is strong but not absolute. Suppression (0.75): High. Structural barriers to indigenous reclamation are severe: sovereign immunity protects state museums, international law precedent favors possession, resource asymmetry prevents community legal campaigns, and institutional definitions of 'legitimate steward' exclude indigenous epistemology. The suppression mechanisms are formal (law), epistemic (museums' curatorial authority claimed as objective expertise), and material (cost of litigation, repatriation logistics). Theater ratio (0.58): Moderate. Institutional discourse around preservation, universalism, and scholarly access performs legitimation while masking extraction. However, the reading does not treat the entire regime as theater — the constraints are genuinely institutional and legal, not mere performance. The theater ratio reflects that much institutional rhetoric is performative cover for extraction, but the extraction mechanism itself is structural, not merely theatrical. The rising trajectory across the interval reflects that repatriation frameworks, while creating some paths to return, have also formalized the institutional authority over cultural property — NAGPRA, for example, created a repatriation mechanism but within a framework that treats museums as presumptive stewards unless proven otherwise.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the irreducibility of the stewardship reading within institutional frameworks designed around universal heritage. Communities classify as snare; institutions classify as rope; the gap is not a measurement or perspective issue but a fundamental disagreement about legitimacy. The stewardship reading asserts indigenous authority is prior (communities are the rightful stewards); the universal heritage reading (sibling) asserts institutional authority is legitimate (museums are appropriate stewards for global humanity). These readings foreclose each other within a single framework — a court cannot simultaneously hold that indigenous communities have primary stewardship authority and that institutions have legitimate possession. The constraint exemplifies why indexical classification matters: the same structural fact (artifacts held by institutions) produces incompatible classifications depending on whether one accepts stewardship axioms (indigenous continuity = legitimate authority) or heritage axioms (institutional curation = legitimate preservation). The analytical observer's mountain perspective risks collapsing this incommensurability into inevitability — treating the question 'who is the legitimate steward?' as already settled by international law, rather than recognizing the legal corpus itself as contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) maps to their structural relationship to the extraction flow. Indigenous communities: victims with trapped exit → d ≈ 0.95 → maximum experienced extraction (f(d) ≈ 1.42). Museums: beneficiaries with arbitrage exit → d ≈ 0.05 → negative experienced extraction, frame it as coordination benefit (f(d) ≈ -0.12). Successor states: beneficiaries with arbitrage exit → d ≈ 0.08 → low experienced extraction, frame as state coordination. Rights coalitions: organized agents with constrained exit → d ≈ 0.45 → moderate experienced extraction (f(d) ≈ 0.50). The directed graph of extraction flows from communities → institutions. Scope modifier σ(S) = 1.2 (global scope) amplifies effective extractiveness: χ = ε × f(d) × σ(S). For communities: χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (exceeds 1.0 because scope amplification on maximum directional extraction overflows the normalized range; interpreted as maximum perceivable extraction). For institutions: χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative extraction; they perceive subsidy/benefit from the regime). The directionality asymmetry is the structural core of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being constructed as a pure snare from the powerless community perspective and rope from the institutional perspective. The snare classification (ε=0.68, suppression=0.75) is justified: institutions extract epistemic authority without proportionate mechanism for community reclamation. The rope classification from institutions' view reflects their genuine perception of coordination value — they do preserve artifacts, enable study, prevent loss. But under the stewardship reading, this coordination function is parasitic on extraction: the institutions could not provide preservation services if they had not taken the artifacts in the first place, and the communities could provide their own preservation within their own epistemological frameworks if they retained custody. The mandatrophy is resolved by recognizing that the snare classification stands within the stewardship reading's axioms: the constraint is extractive because it denies communities legitimate authority over their own cultural inheritance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_authenticity_criterion,
    'What constitutes legitimate ''cultural continuity'' for purposes of indigenous stewardship authority — continuous unbroken practice, reconstructed/revived practice, or diasporic/displaced community claim?',
    'Comparative analysis of repatriation cases: outcomes vary dramatically based on whether stewardship requires continuous practice (Ainu, Aboriginal Australian cases) vs. reconstructed practice (some Haudenosaunee repatriations) vs. diaspora claims (Holocaust restitution models). Establish which criterion this reading commits to and trace its structural implications.',
    'If strict continuity required: many communities disqualified due to colonial disruption. If reconstructed/revived practice accepted: stewardship scope expands significantly. If diaspora recognized: authority disperses across multiple communities. Classification sensitivity: broad definition → higher victim count → higher suppression metrics → snare dominates; narrow definition → lower victim count → potential rope reclassification for some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stewardship_authenticity_criterion, conceptual, 'Definition of cultural continuity for stewardship eligibility').

omega_variable(
    sacred_versus_cultural_distinction,
    'Does stewardship authority differ between artifacts deemed ''sacred'' (ceremonial objects, human remains) vs. ''cultural'' (artwork, tools, regalia)? Does this reading collapse the distinction or maintain it?',
    'Corpus analysis of repatriation protocols: NAGPRA (US) treats human remains and sacred objects differently than cultural patrimony; international law (UNESCO) often treats all as equivalent. Determine whether this reading''s axioms support a unified stewardship claim or differentiated authority by artifact type.',
    'Unified claim (all cultural artifacts subject to stewardship): simpler logic, broader extraction narrative, cleaner snare classification. Differentiated claim (sacred objects = high stewardship authority; cultural objects = negotiable): more complex extraction narrative, potential tangled_rope reclassification for cultural artifacts, preserves museum coordination function for non-sacred items.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_versus_cultural_distinction, conceptual, 'Distinction between sacred and cultural stewardship authority').

omega_variable(
    temporal_legitimacy_boundary,
    'When did the extraction begin? Does this reading treat all artifacts held by external parties as extracted (from contact onward), or only those taken without consent (colonial era onward), or only those held against explicit community claim (post-UNDRIP)?',
    'Temporal analysis of repatriation case outcomes: cases based on explicit colonial seizure (documented appropriation, forced removal) resolve more consistently than cases based on generic ''held by non-indigenous institution.'' Identify the temporal boundary this reading commits to.',
    'Broad boundary (all external holding = extraction from contact): maximizes ε and suppression, strongest snare case. Narrow boundary (only against-explicit-claim = extraction post-UNDRIP): reduces ε slightly, allows rope reclassification for historical holdings, narrower victim set. The boundary choice structurally determines who counts as victim and what historical period is relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_legitimacy_boundary, conceptual, 'Temporal origin boundary for extraction narrative').

omega_variable(
    state_versus_museum_differentiation,
    'Under this reading, are colonial successor states and museums equally illegitimate holders, or does the reading differentiate state sovereignty claims from private/international institutional claims?',
    'Institutional analysis: some repatriation frameworks treat state national museums differently from international or private institutions. Determine whether stewardship authority implicitly defers to successor-state claims while rejecting museum claims, or rejects both equally.',
    'Equal rejection (both states and museums are extractors): consistent snare narrative, both are victims'' antagonists. Differentiated: state museums = constrained legitimacy (partial successor-state authority), private institutions = full illegitimacy. This choice affects the directionality of the ''colonial successor state'' perspective: fully illegitimate → institutional victim; partially legitimate → institutional beneficiary with constrained authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_versus_museum_differentiation, conceptual, 'Differentiation between successor-state and museum extraction authority').

omega_variable(
    kernel_reading_alternative_frames,
    'What alternative readings of the cultural property legal corpus kernel exist, and how does the indigenous stewardship reading''s legitimacy depend on excluding or subordinating them?',
    'This omega is routed to cs_structure.reading_relations and cs_structure.axioms: documents that universal_heritage and sovereign_repatriation readings coexist as live alternatives; this reading forecloses universal_heritage (they contradict on who is the legitimate steward) but coexists with sovereign_repatriation (both prioritize indigenous agency, differ on mechanism).',
    'If readings coexist: constraint is genuinely contested, extraction depends on which reading''s institutional framework dominates. If this reading forecloses others: extraction is stable within this frame. If this reading is foreclosed by others in practice: the snare classification is vindicated — the reading''s axioms are holdable but institutionally overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_frames, conceptual, 'Alternative kernel readings and this reading''s relationship to them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(culprop_steward_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(culprop_steward_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(culprop_steward_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(culprop_steward_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(culprop_steward_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(culprop_steward_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(culprop_steward_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(culprop_steward_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(culprop_steward_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% The cultural property legal corpus is a contested kernel with multiple readings. This file instantiates the indigenous_stewardship_reading with ε=0.68 (snare-dominant). The universal_heritage_reading (sibling, separate file) treats institutional stewardship as legitimate, ε≤0.35 (rope-dominant). The sovereign_repatriation_reading (sibling, separate file) treats state authority as primary, intermediate ε. The three readings are structurally interdependent: any institutional framework must choose between stewardship axioms (community continuity = legitimacy), heritage axioms (institutional curation = legitimacy), or repatriation axioms (state succession = legitimacy). These are not perspective variations on one constraint; they are different constraints grounded in incompatible readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.08).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
