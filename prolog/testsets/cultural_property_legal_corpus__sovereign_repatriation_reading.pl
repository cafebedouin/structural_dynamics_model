% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Authority: Successor States' Legal Claim to Expropriated Cultural Property
 *   domain: international_law/cultural_property/postcolonial_studies
 *
 * SUMMARY:
 *   The sovereign repatriation reading grounds authority in postcolonial
 *   legal doctrine: colonial expropriation was an illegitimate taking;
 *   successor states inherit the legal title of expropriated peoples;
 *   repatriation is the restoration of property to rightful owners. This
 *   reading has become institutionalized in UNESCO conventions (1970
 *   Convention on the Means of Prohibiting and Preventing the Illicit Import,
 *   Export and Transfer of Ownership of Cultural Property), regional
 *   protocols (African Union heritage framework), and domestic legislation
 *   (Peru's Cultural Heritage Law, Egypt's repatriation statutes). The
 *   constraint operates between successor states (beneficiaries seeking
 *   restoration of symbolic capital and national identity authority) and
 *   holding institutions (primarily Western museums, which benefit from
 *   curatorial authority, visitor access, and scholarly prestige, and thus
 *   resist transfer). The extraction mechanism is institutional control over
 *   cultural symbols: by holding the artifacts, major institutions retain
 *   curating authority, determine how objects are interpreted, control
 *   educational narratives, and extract epistemic and economic value (museum
 *   revenue, scholarly publication, cultural influence). Repatriation claims
 *   impose costs on holding institutions (loss of unique objects, diminished
 *   collections, declining visitor traffic at major museums) and demand
 *   infrastructure investment from successor states (conservation facilities,
 *   secure storage, trained staff). The suppression mechanism includes
 *   diplomatic pressure (countries restrict institutional access, threaten
 *   sanctions), institutional resistance (legal defenses, slow-walking
 *   negotiations, selective repatriations), and doctrinal claims ('universal
 *   heritage' framing that naturalizes Western stewardship). Theater has
 *   risen over the measurement interval as the universal heritage doctrine
 *   has become increasingly performative — invoked selectively to resist
 *   repatriations deemed 'inconvenient' while enabling transfers deemed
 *   'diplomatically useful' (e.g., Western museums readily return items to
 *   allied nations but resist claims from previously colonized states). The
 *   theater rise reflects the doctrine's erosion as a legitimate normative
 *   framework even as it persists through institutional inertia.
 *
 * KEY AGENTS:
 *   - Successor States (institutional/arbitrage): Primary beneficiaries of the sovereign repatriation reading. Seek restoration of national cultural authority, symbolic capital, and control over educational narratives. Extract epistemic legitimacy and identity authority through repatriation.
 *   - Metropolitan and National Museums (institutional/arbitrage): Primary extractors within this reading. Benefit from curatorial control, institutional prestige, and collections' uniqueness. Resistance to transfer reflects institutional self-interest.
 *   - UNESCO and Repatriation Coalitions (organized/constrained): Organized agents driving the reading's implementation. See repatriation as a transitional framework with sunset — their goal is to erode institutional resistance through diplomatic and legal pressure until repatriation becomes the default norm.
 *   - Expropriated Collectives (powerless/trapped): Symbolic victims of the constraint. Denied access to cultural inheritance; unable to influence curatorial decisions. Repatriation claims are made by successor states on behalf of these collectives, creating an agency gap — the state claims to represent the collective but may instrumentalize artifacts for state ideology rather than cultural restoration.
 *   - Universal Heritage Epistemic Commons (powerless/trapped): Abstract victim of the repatriation reading. If artifacts leave major museums, global access may decline, specialist scholarship becomes location-dependent, international students and researchers face barriers. The commons has no institutional advocate.
 *   - Analytical Observers (analytical/analytical): Risk naturalizing property rights and Western institutional stewardship as immutable law rather than contingent legal frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.48).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.62).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Authority: Successor States' Legal Claim to Expropriated Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/postcolonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '230f2f24-79db-4971-8eb6-c5a1922cc825').
narrative_ontology:cs_kernel_codification('230f2f24-79db-4971-8eb6-c5a1922cc825', formalized).
narrative_ontology:cs_authority_grounding('230f2f24-79db-4971-8eb6-c5a1922cc825', extraction).
narrative_ontology:cs_interpretation_layer_present('230f2f24-79db-4971-8eb6-c5a1922cc825').
narrative_ontology:cs_reading_relation('230f2f24-79db-4971-8eb6-c5a1922cc825', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('230f2f24-79db-4971-8eb6-c5a1922cc825', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('230f2f24-79db-4971-8eb6-c5a1922cc825', foundational, colonial_expropriation_illegitimate_transfer).
narrative_ontology:cs_axiom_status(colonial_expropriation_illegitimate_transfer, holdable).
narrative_ontology:cs_axiom_grounding('230f2f24-79db-4971-8eb6-c5a1922cc825', colonial_expropriation_illegitimate_transfer, deontological).
narrative_ontology:cs_axiom('230f2f24-79db-4971-8eb6-c5a1922cc825', foundational, legal_succession_authorizes_repatriation_claim).
narrative_ontology:cs_axiom_status(legal_succession_authorizes_repatriation_claim, holdable).
narrative_ontology:cs_axiom_grounding('230f2f24-79db-4971-8eb6-c5a1922cc825', legal_succession_authorizes_repatriation_claim, deontological).
narrative_ontology:cs_reference_frame('230f2f24-79db-4971-8eb6-c5a1922cc825', colonial_expropriation_illegal_wrongful_taking).
narrative_ontology:cs_drift_state('230f2f24-79db-4971-8eb6-c5a1922cc825', contemporary_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('230f2f24-79db-4971-8eb6-c5a1922cc825', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, national_cultural_identity_projects).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPROPRIATED COLLECTIVE MEMORY (SNARE) — Trapped in foreign institutional spaces; cannot exit the alienation condition; bears full cost of symbolic dispossession and epistemic erasure. The collective inheritance is held hostage to institutional conservation agendas and curatorial authority.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUCCESSOR STATE RECLAMATION EFFORT (TANGLED ROPE) — Constrained by diplomatic pressure, Western institutional resistance, and the costs of building repatriation infrastructure. Also benefits from repatriation when achieved: restoration of national cultural authority, symbolic legitimacy, educational control. Moderate extraction from holding institutions; significant coordination function for national identity projects.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR HOLDING INSTITUTION (ROPE) — Institutional beneficiary able to arbitrage between retention (curatorial authority, visitor access, scholarly prestige) and selective repatriation (diplomatic relations, institutional legitimacy, tax benefits). Experiences repatriation claims as a coordination problem to be managed, not as illegitimate extraction. Low experienced extraction — significant exit options and negotiating power.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REPATRIATION MOVEMENT & UNESCO FRAMEWORK (SCAFFOLD) — Organized agents (repatriation coalitions, UNESCO, cultural return commissions) see the claim structure as a transitional authority framework with built-in sunset: as successor states build capacity for conservation and access, the moral legitimacy of holding institutions erodes. UNESCO conventions establish sunset logic — repatriation is structured as temporary trusteeship ending in transfer.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNIVERSAL HERITAGE DOCTRINE (PITON) — The 19th-century logic that Western institutions are the 'rightful guardians of humanity's cultural heritage' persists through institutional inertia despite being intellectually discredited. The doctrine continues as theater — invoked selectively to resist particular repatriations while enabling others based on strategic institutional interest. High theater ratio reflects the gap between the doctrine's normative force (which has eroded) and its continued deployment.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, property rights to cultural artifacts appear as natural law: whoever holds the object holds the legitimate authority over it. Possession is nine-tenths of the law; institutional stewardship is self-evidently the best preservation model. This perspective risks naturalizing what is actually a contingent legal framework (Eurocentric property law) and a historical injustice (colonial expropriation). The engine will identify this as a false summit.
constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_property_legal_corpus__sovereign_repatriation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, TR),
    TR >= 0.70.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48, moderate-high): The sovereign repatriation reading imposes significant costs on holding institutions — loss of unique objects, reduced collections, potential visitor decline. But the reading is framed as restitution (undoing historical extraction) rather than new expropriation, which moderates the extractiveness score. Additionally, some holding institutions benefit from selective repatriation (diplomatic gains, institutional legitimacy), suggesting that extraction is asymmetric across the institutional population. The measurement trajectory shows extraction rising from 0.28 to 0.48 over the interval, reflecting the doctrine's increasing institutional pressure as successor states build legal capacity and diplomatic coalitions. Suppression (0.62, high): Multiple suppression mechanisms operate — diplomatic pressure, institutional resistance framed as concern for 'preservation standards,' legal defenses (statute of limitations, 'good faith' acquisition), and selective repatriation (only yielding items deemed non-essential). Successor states face barriers: holding institutions control the artifacts and can deny access pending capacity certification; Western courts often uphold institutional possession; repatriation negotiations are power-asymmetric (wealthy institutions negotiate with under-resourced states). Theater ratio (0.58, moderate-high): The universal heritage doctrine's performance has increased over the interval. Institutions selectively invoke 'best preservation practice' to resist some claims while accepting others based on diplomatic calculus. Repatriation agreements often include restrictive conditions (loan requirements, visitor guarantees, collaborative access) that preserve institutional control under the guise of cooperation. The theater rise reflects the doctrine's intellectual discrediting (scholars recognize that Western preservation claims are culturally situated, not universal standards) while its institutional deployment continues.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a clear perspectival gap between the successor state beneficiary (Tangled Rope) and the holding institution (Rope). From the successor state perspective, repatriation is constrained by institutional resistance and diplomatic cost — a mixed coordination/extraction dynamic where achieving repatriation requires sustained pressure against institutional inertia. From the holding institution perspective, repatriation is a manageable coordination problem: negotiate selective transfers, frame remaining holdings as collaborative stewardship, maintain scholarly prestige through partnership rather than possession. The expropriated collective (Snare) experiences maximum extraction — they are excluded from curatorial authority and cannot influence whether artifacts stay or return; their interests are instrumentalized by both successor states (using them as repatriation justification) and holding institutions (using them as reasons to resist transfer for 'safekeeping'). The analytical observer risks naturalizing this as immutable property law when it is actually a contestable legal framework. The repatriation movement and UNESCO organize to create a sunset condition (Scaffold) — as successor states build capacity, the moral legitimacy of holding institutions erodes, and repatriation becomes the default. The universal heritage doctrine persists as theater (Piton) — intellectually degraded but institutionally maintained.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign repatriation reading's directionality emerges from the structural asymmetry: successor states are beneficiaries claiming restoration of stolen property; holding institutions are extractors defending curatorial authority; the expropriated collectives are victims whose interests are mediated by state claims. From the successor state perspective (moderate/constrained): d ≈ 0.55, producing moderate effective extraction (constrained exit + victim status + moderate power). From the holding institution perspective (institutional/arbitrage): d ≈ 0.15, producing low/negative effective extraction (arbitrage exit + beneficiary status + institutional power — they can negotiate selectively and choose which claims to contest). From the expropriated collective perspective (powerless/trapped): d ≈ 0.95, producing maximum experienced extraction (trapped exit + victim status + powerless). The constraint appears as Snare from the collective's structural position but Rope from the institution's position, revealing that the perspectival gap tracks real differences in power and exit capacity. This directionality distribution is the core diagnostic feature: a pure coordination problem (Rope) from the powerful party and a snare from the powerless party indicates that the coordination function masks extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining clear structural asymmetry: the sovereign repatriation framing does assert a coordination function (legitimate stewardship by successor states, restoration of cultural authority to rightful agents), AND it asserts an asymmetric extraction (holding institutions benefit from curatorial control and must yield it under pressure). The Tangled Rope classification emerges from both elements. Unlike a pure Snare (extraction without coordination function — artifacts remain dispersed, successor states gain nothing), or a pure Rope (coordination without extraction — holding institutions voluntarily transfer with no cost), the sovereign repatriation reading instantiates both: it coordinates the return of cultural property AND it extracts institutional power from museums. The mandatrophy is resolved by recognizing that repatriation simultaneously restores legitimate authority to successor states and imposes costs on holding institutions — both functions are real, not one masking the other. The constraint does coordinate a legitimate righting of historical wrongs while extracting institutional prerogatives. This is precisely what Tangled Rope is: hybrid coordination-extraction where suppressing the coordination function is no longer tenable (institutions can no longer claim universal stewardship justifies keeping expropriated artifacts) but the extraction mechanism remains salient (repatriation costs institutions significant curatorial authority and prestige).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contestation_readings,
    'Is the kernel (cultural_property_legal_corpus) best interpreted via sovereign repatriation, universal heritage, or indigenous stewardship logics? Are these readings logically incommensurable or strategically coexistent?',
    'Examining actual legal outcomes: do courts/institutions apply all three readings simultaneously (coexistence), or do they foreclose readings through hierarchical authority claims? Mapping which reading dominates by object type, regional context, and institutional power.',
    'If readings coexist: the constraint is a multi-pole dispute with no resolution path, and classification varies by stakeholder power. If one reading forecloses others: the constraint stratifies into layers (reading-dependent classification cascades). If readings influence rather than foreclose: repatriation is a negotiated process where each reading constrains others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_readings, conceptual, 'Logical relationship between the three sibling readings of the cultural property kernel').

omega_variable(
    extraction_vs_restitution_semantics,
    'Is sovereign repatriation authority a restitution mechanism (undoing historical extraction) or does declaring repatriation create NEW extraction from holding institutions (expropriation in reverse)?',
    'Semantic and historical analysis: examining whether the repatriation reading frames itself as correcting the original crime (restitution) or as asserting a competing property claim (extraction). How do successor states and holding institutions rhetorically frame the transaction?',
    'If restitution framing: extractiveness should be lower (correction, not new taking). If extraction framing: extractiveness should be higher (one expropriation replacing another). Current epsilon (0.48) assumes mixed framing — restitution semantically but with institutional extraction costs. Reframing would shift epsilon by ±0.15.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_restitution_semantics, conceptual, 'Whether repatriation is framed as restitution or as new extraction').

omega_variable(
    institutional_capacity_versus_legal_claim,
    'Does the sovereign repatriation reading''s legitimacy depend on successor state capacity for conservation and access, or is it a pure legal claim independent of capacity?',
    'Examining repatriation conditions in actual agreements: do holding institutions make transfer conditional on capacity certification? Do successor states frame the claim as capacity-dependent or as inherent legal right? Longitudinal tracking of repatriated objects: do successor states maintain conservation standards, or does degradation occur?',
    'If capacity-dependent: the reading concedes partial legitimacy to the holding institution logic. If pure legal claim: the reading forecloses institutional quality arguments. Current constraint treats capacity as secondary (mentioned in scaffold perspective as sunset condition, not in this reading''s foundational axioms). This may underestimate the constraint''s actual contested terrain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_versus_legal_claim, empirical, 'Whether sovereign repatriation claims are capacity-conditioned or legal absolutes').

omega_variable(
    false_summit_natural_law_risk,
    'Does this reading''s mountain perspective (property rights as natural law) genuinely reflect inescapable logic, or does it naturalize a contingent Eurocentric legal framework that benefits holding institutions?',
    'Comparative analysis: examining non-Western property concepts that may not map to Western possession-based ownership. Identifying where ''natural'' property law derives from: evolutionary inevitability or institutional design. Examining whether the mountain''s ''immutability'' persists when the beneficiary changes (does a successor state claiming property rights invoke ''natural law'' while Western institutions invoke ''best practice''?).',
    'If false summit confirmed: the mountain classification is naturalization disguising a tangled_rope or snare from the institutional perspective. The engine''s FSM detector should flag this. If natural law holds: the property framework is structurally inevitable and the repatriation reading is constrained by inescapable logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether the mountain perspective naturalizes contingent legal frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(culprop_sov_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(culprop_sov_tr_t3, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(culprop_sov_tr_t6, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(culprop_sov_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(culprop_sov_be_t3, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(culprop_sov_be_t6, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(culprop_sov_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(culprop_sov_su_t3, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(culprop_sov_su_t6, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, identity_coordination).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel is instantiated as three separate constraint stories, each representing a distinct reading with its own epsilon and structural data. The sovereign_repatriation_reading (this constraint, ε=0.48) reflects the legal authority claim of successor states. The universal_heritage_reading (ε≈0.32, Rope/Scaffold) reflects institutional stewardship and knowledge-commons arguments. The indigenous_stewardship_reading (ε≈0.55, Tangled Rope/Snare) reflects direct community authority claims, distinct from state succession. These are not alternative measurements of the same constraint but structurally distinct claims with different beneficiaries, extraction mechanisms, and institutional support. All three are linked via network edges to show their family relationship and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
