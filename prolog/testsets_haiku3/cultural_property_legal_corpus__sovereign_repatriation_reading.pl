% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_sovereign_repatriation, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Cultural Property Repatriation (State-Centered Reading)
 *   domain: international_law/post-colonial_studies
 *
 * SUMMARY:
 *   This constraint instantiates the sovereign repatriation reading of the
 *   contested cultural property kernel. Under this reading, cultural
 *   artifacts acquired during colonialism are sovereign property of successor
 *   states claiming historical and cultural continuity with expropriated
 *   peoples. Colonial acquisition is recast as illegitimate extraction;
 *   holding institutions become occupiers of stolen property; and
 *   repatriation authority flows from state sovereignty and historical
 *   continuity. The reading coordinates a genuine function—resolving
 *   contested ownership claims and restoring symbolic authority—while
 *   asymmetrically extracting from holding institutions, whose loss of
 *   collections and narrative authority is the mechanism that implements the
 *   coordination. This is the reading's ε-invariant formulation; sibling
 *   readings (indigenous stewardship, universal heritage) would instantiate
 *   different constraints with different beneficiary structures and different
 *   ε values. Do not merge the readings.
 *
 * KEY AGENTS:
 *   - Successor states: institutional beneficiaries claiming historical continuity; gain narrative authority and symbolic capital
 *   - Holding institutions: powerful payers bearing repatriation costs and curation authority loss
 *   - Global museum networks: organized payers and enforcers, coordinating repatriation standards
 *   - Indigenous communities: excluded, powerless, would reject state sovereignty on behalf of community stewardship
 *   - Global civil society: observers, providing provenance documentation and alternative frames
 *   - Holding-nation publics: moderate beneficiaries of reconciliation, bearers of access loss
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.58).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Cultural Property Repatriation (State-Centered Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/post-colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'cab398f2-c4e7-411e-a5a0-2d599d63011d').
narrative_ontology:cs_kernel_codification('cab398f2-c4e7-411e-a5a0-2d599d63011d', formalized).
narrative_ontology:cs_authority_grounding('cab398f2-c4e7-411e-a5a0-2d599d63011d', extraction).
narrative_ontology:cs_interpretation_layer_present('cab398f2-c4e7-411e-a5a0-2d599d63011d').
narrative_ontology:cs_reading_relation('cab398f2-c4e7-411e-a5a0-2d599d63011d', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('cab398f2-c4e7-411e-a5a0-2d599d63011d', cultural_property_legal_corpus__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('cab398f2-c4e7-411e-a5a0-2d599d63011d', foundational, colonial_acquisition_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('cab398f2-c4e7-411e-a5a0-2d599d63011d', colonial_acquisition_illegitimate, deontological).
narrative_ontology:cs_axiom('cab398f2-c4e7-411e-a5a0-2d599d63011d', foundational, successor_state_historical_continuity).
narrative_ontology:cs_axiom_status(successor_state_historical_continuity, holdable).
narrative_ontology:cs_axiom_grounding('cab398f2-c4e7-411e-a5a0-2d599d63011d', successor_state_historical_continuity, empirically_contingent).
narrative_ontology:cs_axiom('cab398f2-c4e7-411e-a5a0-2d599d63011d', foundational, state_sovereignty_trumps_institutional_possession).
narrative_ontology:cs_axiom_status(state_sovereignty_trumps_institutional_possession, holdable).
narrative_ontology:cs_axiom_grounding('cab398f2-c4e7-411e-a5a0-2d599d63011d', state_sovereignty_trumps_institutional_possession, conventional).
narrative_ontology:cs_reference_frame('cab398f2-c4e7-411e-a5a0-2d599d63011d', post_colonial_state_sovereignty_framework).
narrative_ontology:cs_drift_state('cab398f2-c4e7-411e-a5a0-2d599d63011d', contemporary_repatriation_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cab398f2-c4e7-411e-a5a0-2d599d63011d', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, national_identity_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_networks).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_nation_publics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nations claiming historical continuity with expropriated peoples assert sovereign ownership over cultural artifacts held abroad. They frame repatriation as restoration of identity capital and correction of colonial extraction. Benefits include symbolic authority over national heritage narrative, tourism revenue from domestic museums, and diplomatic leverage in international forums. Exit would mean abandoning claims to cultural legitimacy and national narrative authority.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, mobile, national).

% Major museums, universities, and cultural institutions hold artifacts acquired through colonial channels. Under this reading they are recast as extractors — occupying and profiting from illegitimate possession. They face legal claims, political pressure, and reputational costs. Their exit options include voluntary repatriation (costly, sets precedent), litigating to retain collections (expensive, damages reputation), or negotiating loans and co-custody arrangements (maintains control but transfers authority narratives).
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions, payer,
    powerful, biographical, constrained, global).

% International professional bodies and accreditation systems that coordinate museum practices face pressure to establish repatriation standards and protocols. They simultaneously collect research access and curatorial prestige from large collections. Under repatriation demands they become enforcers of state sovereignty claims but also lose jurisdictional authority over collection management and research.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_networks, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_networks, beneficiary).

% Communities whose cultural artifacts were expropriated are formally excluded from this reading's negotiation set — repatriation flows to successor states, not directly to communities. They would argue for community custodianship, sacred use protocols, and non-display provisions, but lack standing in state-to-institution frameworks. Their exclusion is structural to the reading: sovereignty is attributed to the state, not the people.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, civilizational, trapped, local).

% Human rights organizations, indigenous advocacy networks, and heritage preservation groups monitor repatriation claims and practices. They provide independent documentation of colonial provenance, challenge state sovereignty readings when they exclude indigenous community voices, and propose alternative frameworks (universal heritage, community stewardship). They take no direct position but supply evidence and normative pressure.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, global_civil_society, observer,
    organized, biographical, analytical, global).

% Citizens of nations where major museums are based. Under this reading they bear the cost of repatriation (artifacts leave collections, museum missions shift) but may benefit from cultural reconciliation narratives and improved international standing. Their voice is mediated through domestic politics and museum advocacy; they have no formal seat at repatriation negotiations.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_nation_publics, beneficiary,
    moderate, biographical, mobile, national).

% Institutions, scholars, and conservationists arguing that cultural artifacts should remain accessible to humanity regardless of origin. They contest the state sovereignty reading, claiming preservation and universal access are higher goods. They actively mount counter-arguments but lack institutional power to enforce alternative frameworks absent legal change.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal and diplomatic framework for resolving claims over artifacts taken during colonialism: allocates authority (to successor states), defines legitimacy (historical continuity and cultural membership), and structures negotiation (bilateral repatriation claims, UNESCO conventions, national repatriation legislation).
% TRANSFER_FUNCTION: Transfers cultural artifacts from holding institutions in former colonial powers to successor states in formerly colonized regions, along with associated symbolic authority over heritage narrative, museological control, and identity-capital investment returns. Holding institutions transfer physical custody but also lose research access, curatorial prestige, and collection-based revenue.
% ABSENT_VOICES: Indigenous communities whose artifacts are repatriated to successor states rather than to the communities themselves; universal-heritage advocates and conservation networks that would argue preservation and access trump geographic origin; alternative stewardship models (community museums, sacred non-display protocols, rotating custody) are excluded from the state-to-institution negotiation set.
% DISAPPEARANCE_RATIONALE: If this reading's legal and diplomatic framework vanished, holding institutions would no longer face repatriation claims or international pressure; successor states would lose the sovereignty-grounded authority to demand return; the terms under which cultural legitimacy is contested would shift radically. Museums would reorganize around new collection principles, and the narratives that govern which nation 'owns' the symbolic capital of an artifact would be re-litigated under alternative frameworks (universal heritage, community stewardship).
% FOUNDING_PROBLEM: Colonial powers systematically removed cultural artifacts from colonized territories without consent, using coercive acquisition to build metropolitan museums and establish cultural dominance. The expropriated peoples were left without their own heritage institutions and narrative authority over their cultural identity.
% FOUNDING_PROBLEM_CORROBORATION: Successor-state governments attest the problem is live and ongoing—artifact repatriation is continuous political and legal work. Historical scholarship outside benefiting parties (colonial historians, archival researchers, human rights documentation) confirms systematic colonial expropriation. However, the contested status appears in disagreement from holding institutions (claiming artifacts were 'collected,' 'preserved,' 'acquired legitimately') and universal-heritage advocates (claiming the founding problem conflates repatriation justice with access restrictions). The corroboration is divided: post-colonial scholars and successor-state entities corroborate; holding institutions and preservation networks dispute both the framing and the status.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at moderate-high (0.58 at interval end) because holding institutions lose valuable artifacts and authoritative voice over heritage narratives, while successor states capture symbolic capital and narrative control. However, ε does not reach snare-level because repatriation involves genuine coordination—resolving contested ownership requires rules and frameworks, not just coercion. Suppression is moderate (0.42) because the reading relies on legal and diplomatic machinery rather than coercive closure; alternatives (museums retain collections, keeping artifacts as universal heritage, universal-heritage framing dominates) exist and mount active resistance (0.71). Theater is moderate (0.38) because succession-state narratives of historical continuity are sometimes contested (alternative genealogies exist; some successor states have weak claim-lines) and holding institutions' 'conservation mission' rhetoric persists even under repatriation pressure. The measurement series shows extractiveness accumulating over the interval (0.38→0.58) as repatriation legislation expands and museums face mounting pressure; theater plateaus (0.38) as the repatriation narrative stabilizes into a new normal; suppression holds steady (0.42) as institutions adapt enforcement and diplomatic frameworks without escalation. All metrics authored on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (successor states) experiences this as legitimate restoration of colonial injustice and recovery of identity capital—a form of justice coordination. The payer seat (holding institutions) experiences it as coercive dispossession—their access rights and curatorial authority are unilaterally redefined by external actors claiming superior historical title. The excluded seat (indigenous communities) would experience both as inadequate: the state-to-institution transfer bypasses the communities whose artifacts they are, subordinating community stewardship to state sovereignty. The engine computes each seat's type independently from the power, exit, and beneficiary/victim declarations; the perspectival gap appears as divergent classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are directional beneficiaries (d→0): they gain narrative authority, international standing, and identity-capital returns with minimal cost—exit would mean abandoning claims to cultural legitimacy. Holding institutions are directional targets (d→1): they lose artifacts, curatorial authority, and research access; their exit is constrained because retaining collections faces legal challenge and reputational cost. Indigenous communities (excluded from this reading) would have even higher d (full targets) if included, because repatriation flows to states rather than to communities, and the state-sovereignty frame actively suppresses community stewardship alternatives. The identity-locked exit dimension is particularly salient for indigenous communities: their identity is fused with artifacts and stewardship practice, making exit from the constraint's effects (remaining excluded from repatriation) impossible even if they objected to state-sovereignty framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial expropriation) is LIVE: artifacts remain held abroad, repatriation claims continue, and no party contests that colonialism happened or that artifacts were taken without consent. However, a mandatrophy signal appears in the READING contest: the repatriation reading competes with universal-heritage and indigenous-stewardship readings that offer alternative solutions to the same problem. Under the repatriation reading alone, the founding problem is live and the constraint persists as functional coordination. But the corpus-level contest means the founding problem (how should contested artifacts be resolved?) is being answered differently depending on which reading governs. This is not mandatrophy of the constraint itself, but rather frame-contest over what counts as solving the founding problem. The three readings are sibling answers to one kernel question, not different stages of one solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    successor_state_legitimacy_genealogy,
    'Do successor states have legitimate historical continuity with expropriated peoples, or is the connection a post-hoc political construction?',
    'Genealogical and institutional analysis: trace continuity of state institutions, territorial control, cultural transmission, and self-identification. Compare claimant populations'' own assertions of continuity vs. state claims. Examine cases where colonial boundaries created artificial successor states with weak continuity (e.g., postcolonial nations assembling diverse ethnic groups with no unified pre-colonial ancestor state).',
    'If continuity is weak or constructed, the state''s claim to beneficiary status weakens; repatriation authority might devolve to communities or be contested. If continuity is strong, the state''s beneficiary role is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_state_legitimacy_genealogy, empirical, 'Whether successor states represent legitimate cultural continuity with expropriated peoples or are administrative impositions.').

omega_variable(
    community_stewardship_vs_state_sovereignty_foreclosure,
    'Does this reading''s state-sovereignty frame logically foreclose indigenous community stewardship, or can both coexist?',
    'Examine legal and normative frameworks: can a state hold sovereignty AND communities maintain sacred/custodial authority simultaneously (e.g., through co-custody, community veto on display, repatriation to communities not to national museums)? Or does state sovereignty structurally require state-level control and exclude community authority?',
    'If foreclosure is structural (state sovereignty = no competing authority), this reading forecloses the indigenous-stewardship sibling. If co-governance is possible, the readings coexist rather than foreclose. This determines the reading_relations value (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_stewardship_vs_state_sovereignty_foreclosure, conceptual, 'Whether state sovereignty and indigenous community authority are logically incompatible or can coexist.').

omega_variable(
    preservation_access_vs_repatriation_tradeoff,
    'Is the constraint''s ε (repatriation costs, access loss, institutional extraction) justified by the restoration of identity capital and symbolic authority, or does it represent genuine zero-sum loss?',
    'Empirical measurement of repatriated artifacts: conservation outcomes, community use, museum access post-repatriation vs. pre-repatriation. Measurement of symbolic capital: successor-state investment in heritage institutions, identity-framing changes, international recognition shifts. Compare constraints'' ε across different repatriation trajectories.',
    'If identity-capital gains equal or exceed institutional losses, the constraint''s extractiveness is justified by coordination function. If losses exceed gains, the constraint approaches pure extraction (Snare). This affects mandatrophy and type boundaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preservation_access_vs_repatriation_tradeoff, empirical, 'Whether repatriation''s symbolic and identity-capital benefits offset institutional costs and access losses.').

omega_variable(
    universal_heritage_influence_vs_foreclosure,
    'Does this reading''s state-sovereignty frame influence (but not foreclose) the universal-heritage sibling by changing resource availability and institutional power, or do the two readings directly contradict each other?',
    'Observe institutional responses: as repatriation frameworks strengthen, do museums shift to emphasizing ''universal access'' and ''preservation'' more loudly (adaptation to constraint), or do they attempt to preserve sovereignty-based arguments against repatriation (direct contradiction)? Examine legal and diplomatic outcomes: does acceptance of state sovereignty in some cases make universal-heritage arguments weaker in others, or does each case remain independently contested?',
    'If the relationship is influence, the reading_relations value is ''influences''; if contradiction, it is ''forecloses''. Influences is more likely: state sovereignty and universal heritage are not logically incompatible, but state-sovereignty frameworks do make universal-access institutional models harder to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_influence_vs_foreclosure, conceptual, 'Whether the repatriation reading''s effects on the universal-heritage reading are structural pressure (influences) or logical contradiction (forecloses).').

omega_variable(
    identity_fusion_indigenous_communities,
    'Is the identity-locked exit option for indigenous communities structural (their identity is constituted through artifact stewardship and cannot be exited), or is it internalized suppression that could be revised if the constraint''s framing changed?',
    'Post-constraint-change observation: if states or communities shift repatriation frameworks to include community custodianship, do communities'' identity-locked status dissolve (revealing it was internalized attachment to a particular repatriation frame) or persist (revealing it is structural identity fusion)? Compare communities'' own statements about whether identity requires physical custody vs. narrative authority vs. spiritual access.',
    'If identity-lock is structural, communities'' d value remains high (near-target) even if repatriation occurs, because they remain excluded from authority-granting in this reading''s framework. If internalized, d could shift if framing changes. This affects the constraint''s suppression characterization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_indigenous_communities, conceptual, 'Whether indigenous identity-lock is structural or internalized constraint-attachment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cult_tr_t7, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(cult_tr_t14, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 14, 0.34).
narrative_ontology:measurement(cult_tr_t21, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement(cult_tr_t28, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 28, 0.38).
narrative_ontology:measurement(cult_tr_t35, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 35, 0.38).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cult_be_t7, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 7, 0.44).
narrative_ontology:measurement(cult_be_t14, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 14, 0.51).
narrative_ontology:measurement(cult_be_t21, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(cult_be_t28, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 28, 0.57).
narrative_ontology:measurement(cult_be_t35, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cult_su_t7, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 7, 0.37).
narrative_ontology:measurement(cult_su_t14, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 14, 0.4).
narrative_ontology:measurement(cult_su_t21, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 21, 0.42).
narrative_ontology:measurement(cult_su_t28, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 28, 0.42).
narrative_ontology:measurement(cult_su_t35, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 35, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.18).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel contains three ε-invariant readings: sovereign_repatriation_reading (state sovereignty, moderate ε, tangled rope), indigenous_stewardship_reading (community authority, different ε, different beneficiary/victim), and universal_heritage_reading (institutions and humanity, different ε, different type). Each reading instantiates a distinct constraint because each has a distinct ε (referent = the standing arrangement under contest, assessed by the reading's own lights) and a distinct beneficiary/victim structure. The three constraints are linked by network.affects_constraints to indicate they are competing answers to the same kernel question, not separate domains. The sibling stories should be authored with awareness that each reading's ε refers to the SAME physical state of affairs (artifacts held abroad) but evaluates it through different normative lenses: the repatriation reading sees it as illegitimate extraction requiring state-level restitution; the stewardship reading sees it as community dispossession requiring community-level return; the heritage reading sees it as opportunity cost on access and preservation. Do not merge the readings or average their ε values—each is an independent constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
