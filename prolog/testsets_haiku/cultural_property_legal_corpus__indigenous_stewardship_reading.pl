% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Indigenous Stewardship of Cultural Artifacts (Kernel Reading)
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the
 *   cultural_property_legal_corpus kernel. The kernel is contested across
 *   three structurally distinct claims: (1) the
 *   indigenous_stewardship_reading (THIS story) holds that legitimate
 *   authority over cultural artifacts rests with indigenous communities that
 *   maintain continuous cultural practice and relationship to the sacred
 *   items; (2) the sovereign_repatriation_reading holds that authority
 *   belongs to successor states that claim historical continuity with
 *   expropriated peoples; (3) the universal_heritage_reading holds that
 *   authority rests with institutions maximizing preservation and universal
 *   human access regardless of origin. These are not three viewpoints on the
 *   same constraint; they are three structurally different constraints with
 *   different beneficiary sets, different extraction profiles, and different
 *   ε values. Each instantiates a different legitimate authority kernel. The
 *   indigenous stewardship reading produces the highest ε because, under this
 *   reading, artifacts held by anyone OTHER THAN indigenous communities
 *   represent illegitimate extraction from those communities. Museums,
 *   collectors, and successor states all become targets and extractors in
 *   this framing.
 *
 * KEY AGENTS:
 *   - Indigenous communities: Legitimate stewards under this reading; powerless in the global legal system; identity-locked by their relationship to sacred artifacts and cultural continuity; bear costs of exclusion and repatriation burden.
 *   - Northern hemisphere museums: Institutional agenda-setters holding large non-Western collections; extractors holding property they lack legitimate claim to; benefit from prestige and revenue; suppress indigenous repatriation claims.
 *   - Successor states: Institutional agenda-setters claiming sovereignty over artifacts; extractors asserting state authority to hold/trade/display; lack community legitimacy under this reading but benefit from sovereignty assertion.
 *   - Auction houses and dealers: Secondary beneficiaries profiting from commodification; mobility allows arbitrage across jurisdictions; suppress provenance scrutiny and repatriation claims.
 *   - International legal authorities (UNESCO, heritage bodies): Agenda-setters enforcing the procedural rules that make repatriation costly and difficult; institutionalize state and museum authority.
 *   - Indigenous advocacy organizations: Organized observers providing legal resources; operate from outside institutional apparatus; mediate indigenous claims to international forums.
 *   - Private collectors: Secondary beneficiaries holding artifacts with minimal provenance; benefit from weak repatriation enforcement; high mobility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.82).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.79).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Indigenous Stewardship of Cultural Artifacts (Kernel Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '80bc67a2-fe3c-4c26-b248-1e046261ef33').
narrative_ontology:cs_kernel_codification('80bc67a2-fe3c-4c26-b248-1e046261ef33', fixed_text).
narrative_ontology:cs_authority_grounding('80bc67a2-fe3c-4c26-b248-1e046261ef33', extraction).
narrative_ontology:cs_interpretation_layer_present('80bc67a2-fe3c-4c26-b248-1e046261ef33').
narrative_ontology:cs_reading_relation('80bc67a2-fe3c-4c26-b248-1e046261ef33', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('80bc67a2-fe3c-4c26-b248-1e046261ef33', cultural_property_legal_corpus__sovereign_repatriation_reading, influences).
narrative_ontology:cs_axiom('80bc67a2-fe3c-4c26-b248-1e046261ef33', foundational, indigenous_epistemic_authority).
narrative_ontology:cs_axiom_status(indigenous_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('80bc67a2-fe3c-4c26-b248-1e046261ef33', indigenous_epistemic_authority, deontological).
narrative_ontology:cs_axiom('80bc67a2-fe3c-4c26-b248-1e046261ef33', foundational, sacred_communal_property_inalienable).
narrative_ontology:cs_axiom_status(sacred_communal_property_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('80bc67a2-fe3c-4c26-b248-1e046261ef33', sacred_communal_property_inalienable, deontological).
narrative_ontology:cs_reference_frame('80bc67a2-fe3c-4c26-b248-1e046261ef33', indigenous_community_stewardship_authority).
narrative_ontology:cs_drift_state('80bc67a2-fe3c-4c26-b248-1e046261ef33', contemporary_globalized_museum_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('80bc67a2-fe3c-4c26-b248-1e046261ef33', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, auction_houses_dealers).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, private_collectors).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, northern_hemisphere_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_self_determination).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_epistemic_authority).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, sacred_communal_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sacred and communal knowledge claims to artifacts their ancestors created and that embody ongoing cultural practice. Under this reading, they are the legitimate authority and stewards. Simultaneously, they bear the cost of being excluded from artifacts held by museums and successor states, of having to mount costly repatriation claims, and of the institutional violence of having their epistemic authority denied. Their exit from identity as indigenous peoples is foreclosed; their exit from the global legal system's foreign property regimes is constrained by state power.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary,
    powerless, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, payer).

% Hold large collections of non-Western artifacts, often acquired through colonial-era extraction or dubious purchase. Under this reading, they are extractors: they hold property with no legitimate claim, benefit from possession through prestige and admission revenue, and actively suppress indigenous repatriation claims through legal resources and institutional inertia. Repatriation represents a loss of collection prestige and generates internal resistance from curators and donors. Exit would require policy shift and collection disposal, which carries career and funding risk.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, northern_hemisphere_museums, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, northern_hemisphere_museums, agenda_setter).

% Claim sovereignty over cultural artifacts from territories they govern, including artifacts created by indigenous peoples within their borders. Under this reading, they are also extractors: they lack the communal legitimacy claimed by indigenous communities, yet they assert state authority to hold, trade, or display these artifacts. They benefit from assertion of sovereignty and from revenue or prestige. Their exit would mean ceding cultural authority to indigenous communities, which contradicts the nation-state consolidation project.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, agenda_setter).

% Profit from the commodification and sale of artifacts, with limited enforcement of provenance or indigenous claims. They benefit from the current legal regime that treats cultural property as tradeable goods. Under this reading, they are secondary extractors: the commodification itself violates the sacred/communal status, and they have institutional incentive to suppress repatriation claims and provenance scrutiny.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, auction_houses_dealers, beneficiary,
    powerful, biographical, mobile, global).

% UNESCO, UNESCO convention bodies, and national heritage authorities set international cultural property law. Under this reading, they enforce a hybrid regime that nominally acknowledges indigenous rights while institutionalizing state and museum authority. They set the procedural and evidentiary rules that make repatriation costly and difficult for indigenous claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_legal_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Organize indigenous communities for repatriation campaigns, provide legal resources, and articulate the indigenous stewardship reading to international audiences. They operate from outside the institutional legal apparatus and carry the burden of mounting challenges to museums and states with vastly greater resources.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_advocacy_organizations, observer,
    organized, civilizational, constrained, global).

% Hold private collections of indigenous artifacts, often with minimal provenance documentation. They benefit from the current legal regime's treatment of cultural property as private goods and from the weak enforcement of repatriation claims. They have mobility to relocate collections across jurisdictions to avoid compliance.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, private_collectors, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, northern_hemisphere_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a principled framework for determining legitimate stewardship and authority over sacred and communal cultural artifacts: places that authority with the communities whose ancestors created the artifacts and who maintain living cultural practice, rather than dispersing it across museums, states, and private collectors with no community ties.
% TRANSFER_FUNCTION: Moves control, access, prestige, and revenue from indigenous communities to the institutional holders (museums, states, auction houses, private collectors). Indigenous communities lose the ability to direct how sacred items are handled, displayed, or used; they also lose potential revenue and prestige. Museums and states gain prestige, admission revenue, scholarly authority, and the psychological benefit of possessing the 'world's cultural heritage.' Auction houses and dealers gain profit from commodification and sale.
% ABSENT_VOICES: Indigenous peoples without formal state recognition or strong organized advocacy are largely absent from repatriation negotiations; their voices are mediated through advocacy organizations and state-level indigenous affairs offices, both of which filter and translate claims. Diaspora indigenous communities separated from artifact-holding regions by colonial borders have difficulty establishing standing in the relevant legal forums. Descendant communities of displaced or decimated peoples sometimes cannot be located or authenticated by the institutional standards museums apply.
% DISAPPEARANCE_RATIONALE: If this reading were to become institutionalized (artifacts recognized as legitimately steward by indigenous communities, legal authority shifted accordingly), museums would lose portions of their collections, states would lose cultural property assertions, and the global art market would contract—acquisition of indigenous artifacts would require community consent, which would drastically reduce legal supply. The institutional and economic arrangement organizing 'world cultural heritage' would reorganize around community stewardship, with substantial redistribution of prestige and control. Institutions would have to fundamentally change their acquisition and retention policies.
% FOUNDING_PROBLEM: Colonial extraction of sacred and communal cultural artifacts from indigenous peoples, accompanied by the erasure of indigenous authority and the assertion of European museum authority as the legitimate custodian of 'world culture.' The founding problem was the violent dispossession and the delegitimation of indigenous communities' own claims to their artifacts.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous communities attest the dispossession and delegitimation are ongoing—artifacts remain in museums without community consent, repatriation claims are rejected on technicalities, and children in source communities grow up seeing their sacred items displayed under foreign institutional labels. Decolonial scholars, anthropologists working with indigenous communities, and UNESCO-affiliated experts on indigenous rights attest to the ongoing structural dispossession. Legal historians document the colonial-era mechanisms of extraction. The founding problem is corroborated from outside the indigenous advocacy set by international human rights bodies and by some museum professionals who have shifted positions toward stewardship models.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.82 at interval end) because, under this reading, the constraint's operation systematically denies legitimate authority to indigenous communities and transfers control to parties with no communal legitimacy claim. The measurement series shows extractiveness rising over the 40-year interval (0.68 to 0.82), modeling the accumulation of institutional extraction as museums expand collections, as auction markets globalize, and as successor states consolidate cultural nationalism claims—simultaneously, the constraint's actual implementation involves more sophisticated legal defenses of museum possession (philanthropic framing, universal heritage rhetorics, funding model dependence on collections). Suppression is high (0.79 at interval end, rising from 0.68) because the constraint's persistence requires active suppression: enforcement through legal costs (repatriation claims are expensive), through institutional gatekeeping (museums control authentication and provenance standards), through epistemic suppression (museums assert their authority to interpret what artifacts 'mean'), and through state power (successor states leverage immigration and diplomatic power to limit indigenous advocacy). Theater ratio is elevated (0.58 at interval end, rising from 0.42) because increasingly museums deploy public-facing stewardship narratives, repatriation committees, and community engagement rhetoric—performative gestures that nominally acknowledge indigenous authority while institutionally maintaining museum control. The rising theater trajectory models the Goodhart drift: as repatriation pressure mounts, museums shift from denying indigenous claims to staging the appearance of consultation while retaining actual control. Accessibility collapse (0.71) reflects that, under this reading, indigenous communities' alternatives to museum/state stewardship have largely collapsed—artifacts are in foreign collections, recovery requires expensive legal action, and the global property regime treats artifacts as tradeable goods, not communal property. Resistance (0.68) reflects that indigenous communities and their advocates mount significant opposition, but resistance is structurally asymmetric: communities have limited institutional power and legal resources; museums and states control the rules and institutions.
 *
 * PERSPECTIVAL GAP:
 *   The indigenous communities seat and the museum/state seats will compute dramatically different classifications from the same structural data. From the indigenous communities' position, they are being extracted from—their legitimate authority is denied, their artifacts are held without consent, and they bear the cost of fighting for repatriation. From the museum and state seats, the arrangement is portrayed as stewardship, preservation, universal human benefit, and legitimate public authority. The engine computes per-seat classifications from power, exit, and directionality; the indigenous powerless-identity-locked seat computes as a victim; the institutional powerful seats compute as beneficiaries/agenda-setters. The perspectival gap is the whole story: under the indigenous stewardship reading, the same institutional arrangement that museums and states experience as legitimate public authority indigenous communities experience as extraction. This gap is the definitional structure of the tangled rope classification from the target seat: there is real coordination happening (preservation of artifacts, centralized access), but it is asymmetrically distributed—indigenous communities are coordinated-away-from, not coordinated-with.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Indigenous communities are identified as both beneficiaries (the reading asserts they SHOULD be benefiting from stewardship and authority) and victims (they are currently excluded and bearing costs). This dual classification reflects the core injustice the reading articulates: indigenous communities should be the beneficiaries, but the current institutional arrangement extracts from them. The engine's derivation from beneficiary/victim + exit will produce a high d-value (near 1.0, target) for indigenous communities because they are identified as victims, their exit is identity_locked (cannot leave indigenous identity), and they are powerless in the global legal system. Museums and states will compute lower d-values (beneficiaries in practice, agenda-setters, powerful, constrained exit by policy rather than identity) but still positive extraction because they are NOT identified as beneficiaries under this reading—the reading denies their legitimacy. No directionality override is needed; the structural data produces the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial dispossession and delegitimation of indigenous authority) is LIVE and actively being contested. Repatriation campaigns, UNESCO protocols, and indigenous advocacy are ongoing responses to a problem that has not been solved. The disappearance verdict is world_rearranges: if indigenous communities gained actual authority, the global cultural property regime would reorganize. However, there is a mandatrophy risk: museums and states might declare the founding problem 'resolved' by adopting performative stewardship rhetoric (community consultation committees, repatriation policies that rarely result in actual return, 'inclusive' curatorial framing) while preserving institutional control. The rising theater ratio (0.42 to 0.60) models this risk: the appearance of addressing indigenous authority claims while maintaining extraction. A piton reading is possible if theater rises to ~0.75+ and extractiveness plateaus while suppression remains stable—theaters without function. The Tangled Rope classification is correct because the arrangement does coordinate (artifact preservation) but asymmetrically extracts (indigenous communities excluded from authority and bearing costs). The extraction is not hidden; it is defended through legitimacy claims (museum expertise, state sovereignty, universal heritage). Unlike a snare, there is a genuine coordination function; unlike a rope, the coordination benefits are not shared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_community_definition,
    'How are ''indigenous communities'' defined for stewardship authority under this reading, and who authenticates that definition? Does stewardship rest with descendants of the artifact creators, with living communities in the geographic region of origin, with formal nation-to-nation relationships, or with some other criterion?',
    'Clarification from indigenous advocacy organizations, UNESCO indigenous protocols, and specific repatriation case outcomes showing which communities are recognized as legitimate stewards and on what grounds.',
    'Different definitions of indigenous community would change the beneficiary set and the scope of extraction. If stewardship requires demonstrated genealogical continuity, fewer communities qualify but the legitimacy claim is stronger; if stewardship rests on geographic/cultural proximity, more communities might qualify but proof becomes contested. The extraction profile shifts based on who is included/excluded from legitimate authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_community_definition, conceptual, 'Definitional ambiguity in what constitutes ''the indigenous community'' with stewardship authority.').

omega_variable(
    sacred_vs_communal_tension,
    'Does stewardship authority rest on the sacred status of artifacts (requiring restricted access, spiritual authority, and specific ritual use) or on communal ownership (allowing broader access, community decision-making, and public display)? These may conflict when communities want to display sacred items or when display violates sacred protocols.',
    'Examination of repatriated artifacts and how indigenous communities have chosen to use them—some communities display repatriated items in community museums with restricted hours, others keep them completely restricted, others use them in living practice. Comparative study of how different communities balance sacred and communal dimensions.',
    'If the reading privileges sacred stewardship, repatriation should enforce indigenous protocols around access and display, which may limit public access relative to museum display. If it privileges communal stewardship, communities have discretion to display or restrict as they choose. The extraction dynamic changes: museums argue restricted access violates universal heritage; indigenous communities argue unrestricted display violates sacred status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacred_vs_communal_tension, conceptual, 'Tension between sacred protocols and communal access rights in stewardship authority.').

omega_variable(
    institutional_repatriation_compliance,
    'When museums adopt voluntary repatriation policies or join repatriation committees, do these constitute genuine acknowledgment of indigenous stewardship authority or performative theater that maintains institutional control while appearing responsive?',
    'Tracking of repatriation committee composition, decision timelines, approval rates, and actual artifact return rates against applications over 10+ year periods. Post-repatriation follow-up: do museums maintain relationships with communities, or does repatriation represent an institutional exit from engagement?',
    'If repatriation policies are theater (low approval rates, high evidentiary barriers, slow timelines, continued institutional narrative about ''proper stewardship''), the suppression rises and theater ratio rises—piton risk increases. If repatriation policies represent genuine devolution of authority, the classification may shift toward Rope or even voluntary Coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_repatriation_compliance, empirical, 'Whether institutional repatriation responses represent genuine authority devolution or performative suppression.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the museum/preservation function structurally inseparable from institutional (museum/state) control, or could preservation and access coordination occur under indigenous stewardship authority?',
    'Examination of community museums, indigenous-run conservation projects, and hybrid stewardship models (e.g., co-curated exhibitions, shared legal authority). Evidence from communities that have gained repatriation and taken on preservation responsibilities.',
    'If preservation requires institutional expertise and scale that communities cannot provide, the constraint carries real coordination costs that justify some institutional involvement (moves toward rope classification). If preservation can be achieved under indigenous stewardship (with technical support contracts if needed), the constraint is primarily extraction with a coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether artifact preservation necessarily requires institutional control or is separable from stewardship authority.').

omega_variable(
    committer_kernel_contest,
    'This constraint is ONE READING of the cultural_property_legal_corpus kernel. The sibling readings (universal_heritage_reading, sovereign_repatriation_reading) differ in their ε values and beneficiary structures. What are the conditions under which indigenous stewardship is foreclosed, influences, or coexists with these sibling readings?',
    'Comparative analysis of repatriation cases, UNESCO protocols, and state practices showing whether jurisdictions recognize multiple readings as coexistent or enforce one reading as canonical. Examination of legal conflicts between indigenous claims and state sovereignty claims, and between indigenous claims and universalist heritage claims.',
    'If indigenous stewardship forecloses state repatriation (one framework cannot hold both claims), the constraint''s legitimacy competes directly against state sovereignty—outcome determines which reading prevails. If readings coexist (different parties hold them simultaneously), the constraint persists as a site of ongoing contest without resolution. If indigenous stewardship influences but does not foreclose the other readings (e.g., reshaping what counts as legitimate state repatriation or heritage stewardship), the readings occupy a hybrid legal space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Kernel-level contest: logical relationships between indigenous stewardship and competing readings of legitimate cultural authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t5, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement_basis(cult_tr_t5, observed).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(cult_tr_t10, observed).
narrative_ontology:measurement(cult_tr_t15, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(cult_tr_t15, observed).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(cult_tr_t20, observed).
narrative_ontology:measurement(cult_tr_t25, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(cult_tr_t25, observed).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(cult_tr_t30, observed).
narrative_ontology:measurement(cult_tr_t35, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 35, 0.59).
narrative_ontology:measurement_basis(cult_tr_t35, projected).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement_basis(cult_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t5, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement_basis(cult_be_t5, observed).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(cult_be_t10, observed).
narrative_ontology:measurement(cult_be_t15, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(cult_be_t15, observed).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(cult_be_t20, observed).
narrative_ontology:measurement(cult_be_t25, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(cult_be_t25, observed).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(cult_be_t30, observed).
narrative_ontology:measurement(cult_be_t35, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(cult_be_t35, projected).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(cult_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t5, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(cult_su_t5, observed).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(cult_su_t10, observed).
narrative_ontology:measurement(cult_su_t15, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(cult_su_t15, observed).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(cult_su_t20, observed).
narrative_ontology:measurement(cult_su_t25, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement_basis(cult_su_t25, observed).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(cult_su_t30, observed).
narrative_ontology:measurement(cult_su_t35, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 35, 0.79).
narrative_ontology:measurement_basis(cult_su_t35, projected).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(cult_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the cultural_property_legal_corpus kernel. The three readings—indigenous_stewardship_reading, universal_heritage_reading, and sovereign_repatriation_reading—decompose the contested kernel into three structurally distinct constraints with different ε values and beneficiary sets. The readings coexist as live positions held by different institutional and political actors. Indigenous stewardship produces the highest ε because under this reading any institutional (museum or state) holding represents illegitimate extraction. Sovereign repatriation produces moderate-high ε because under that reading state authority is legitimate but museums holding sovereignty-violating collections represent extraction. Universal heritage produces lower ε because under that reading institutional holding is legitimate for preservation and access purposes. The three stories are linked by network.affects_constraints to enable contamination analysis—changes in one reading's institutional standing affect the others' operative space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
