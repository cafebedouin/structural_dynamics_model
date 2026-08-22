% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Sovereign Repatriation Reading of Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereign-repatriation reading of the
 *   cultural property legal corpus kernel: cultural artifacts are the
 *   sovereign property of successor states, colonial acquisition was
 *   illegitimate extraction, and legitimate authority to reclaim and hold the
 *   objects rests with states that assert historical continuity with the
 *   expropriated peoples. Under this reading, the legal and diplomatic
 *   apparatus built around instruments like the 1970 UNESCO Convention
 *   functions as a hybrid: it genuinely reverses specific documented colonial
 *   thefts (coordination function, real and historically grounded), while
 *   simultaneously routing the resulting symbolic and material capital
 *   through state institutions that often do not share direct descent,
 *   custodial tradition, or even friendly relations with the actual
 *   originating community. The state becomes the collecting agent of a claim
 *   it did not itself suffer in any lived sense, converting restitution into
 *   an instrument of modern nation-building.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.44).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'a679cae0-ad7a-424c-b502-b89cfcb46a7e').
narrative_ontology:cs_kernel_codification('a679cae0-ad7a-424c-b502-b89cfcb46a7e', distributed).
narrative_ontology:cs_authority_grounding('a679cae0-ad7a-424c-b502-b89cfcb46a7e', distributed).
narrative_ontology:cs_reading_relation('a679cae0-ad7a-424c-b502-b89cfcb46a7e', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('a679cae0-ad7a-424c-b502-b89cfcb46a7e', cultural_property_legal_corpus__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('a679cae0-ad7a-424c-b502-b89cfcb46a7e', foundational, state_continuity_confers_restitution_standing).
narrative_ontology:cs_axiom_status(state_continuity_confers_restitution_standing, holdable).
narrative_ontology:cs_axiom_grounding('a679cae0-ad7a-424c-b502-b89cfcb46a7e', state_continuity_confers_restitution_standing, conventional).
narrative_ontology:cs_axiom('a679cae0-ad7a-424c-b502-b89cfcb46a7e', foundational, colonial_acquisition_is_categorically_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_is_categorically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('a679cae0-ad7a-424c-b502-b89cfcb46a7e', colonial_acquisition_is_categorically_illegitimate, deontological).
narrative_ontology:cs_reference_frame('a679cae0-ad7a-424c-b502-b89cfcb46a7e', colonial_era_custodial_seizure).
narrative_ontology:cs_drift_state('a679cae0-ad7a-424c-b502-b89cfcb46a7e', post_1970_unesco_convention_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a679cae0-ad7a-424c-b502-b89cfcb46a7e', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, national_museum_authorities_of_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, originating_subnational_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, encyclopedic_museum_visitor_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, encyclopedic_museums_and_former_colonial_powers).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_acquisition_illegitimacy_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_continuity_with_expropriated_peoples_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Files formal repatriation claims through diplomatic channels and international bodies (UNESCO 1970 Convention, bilateral treaties), asserting unbroken sovereign continuity with pre-colonial polities. Collects returned artifacts into national collections, builds new state museums to house them, and converts the objects into instruments of national identity-building and international prestige. Can choose which claims to press and when, giving it leverage other parties lack.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, beneficiary).

% Receives repatriated objects, gains international visibility and tourism revenue, and administers the resulting collections. Benefits directly from every successful claim regardless of whether the returned object originated with a group the current state actually maintains continuity with.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, national_museum_authorities_of_successor_states, beneficiary,
    institutional, generational, mobile, national).

% Holds contested artifacts acquired during colonial administration, often under duress, coercive treaty, or outright seizure. Faces diplomatic pressure, legal claims, and reputational cost for retention; must negotiate loans, restitution agreements, or permanent transfer. Exit is constrained by legal exposure, public opinion, and dependency on cultural diplomacy relationships with claimant states.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, encyclopedic_museums_and_former_colonial_powers, payer,
    powerful, biographical, constrained, global).

% The actual descendant lineage, clan, or indigenous nation that produced or held the artifact before colonial seizure, frequently a group with an ambiguous or adversarial relationship to the modern successor state now claiming it. Objects returned under this reading go to national capitals and state museums, not to the originating community's own custody, control, or sacred practice; the community has no standing in the sovereign-to-sovereign restitution process and cannot compel the artifact's return to itself rather than to the state.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, originating_subnational_communities, payer,
    powerless, civilizational, trapped, local).

% Global audiences who lose access to consolidated, comparative encyclopedic collections as objects disperse to national museums under state claims; access shifts from a single major institution to fragmented state repositories with varying hours, funding, and openness.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, encyclopedic_museum_visitor_publics, payer,
    moderate, biographical, constrained, global).

% Adjudicates and mediates claims under conventions like the 1970 UNESCO Convention and UNIDROIT, weighing state sovereignty claims against competing indigenous, universalist, and provenance arguments. Its rulings shape which claims succeed and set precedent for the field.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_cultural_property_tribunals_and_unesco_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and diplomatic mechanism for reversing specific, documented colonial-era seizures of cultural property, giving claimant states standing and process where none previously existed and holding institutions a predictable framework for evaluating and resolving claims.
% TRANSFER_FUNCTION: Moves physical custody, symbolic capital, and tourism/prestige value of artifacts from holding institutions in former colonial powers to the national museum and government apparatus of successor states, on the basis of state-to-state historical continuity claims rather than claims lodged by the specific descendant communities the objects were taken from.
% ABSENT_VOICES: Originating subnational and indigenous communities are structurally absent from the sovereign-to-sovereign claims process — the 1970 Convention and its diplomatic apparatus recognize states as claimants, not clans, nations-within-states, or diasporic descendant groups, many of whom have fraught or adversarial relationships with the successor state now asserting continuity with them.
% DISAPPEARANCE_RATIONALE: Successor states and their museum authorities would experience the disappearance of this legal architecture as a major loss of an active, functioning restitution pathway and would argue the world rearranges substantially against them. Encyclopedic museums would argue the underlying moral and political pressure to repatriate would persist through other channels (bilateral negotiation, public campaigns) even without this specific doctrinal framework, so the world would not fully reset. Originating subnational communities are divided: some would see no change since the framework never served their direct claims anyway; others fear losing what leverage the framework indirectly provides via state advocacy.
% FOUNDING_PROBLEM: Colonial administrations and their agents removed cultural, religious, and historical artifacts from occupied territories through seizure, coercive purchase, or destruction-threat extraction, and post-independence states had no established legal path to contest possession by museums and collectors in the former colonial metropoles.
% FOUNDING_PROBLEM_CORROBORATION: Successor state governments and their allied legal scholars attest the founding problem remains live — the majority of contested colonial-era holdings remain in European and North American institutions. Independent provenance researchers and some UNESCO-affiliated legal scholars, sitting outside both the claimant states and the holding institutions, corroborate that the original extraction was real and largely undisputed as fact, but note the current claims mechanism increasingly serves state nation-building agendas rather than the specific harmed communities, a divergence documented in academic critiques of the framework rather than asserted only by the successor states themselves.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (rising from 0.28 to 0.52 across the interval) because a large share of the framework's operation is genuine reversal of documented colonial-era seizure — that share is not extraction, it is coordination doing real work. The residual extraction is the growing gap between what the state receives and what the originating community receives: as more claims succeed and states build national museums around them, national governments capture growing symbolic and economic value while originating communities remain outside the claims process entirely. Suppression (0.44) reflects the structural exclusion of subnational and indigenous claimants from standing in the sovereign-to-sovereign process, not coercive enforcement against them, which is why it sits below extraction rather than above it. Theater ratio (0.38) tracks a real but partial drift: some repatriation ceremonies and diplomatic signings substitute symbolic transfer for functional benefit to the community that would actually reconnect with the object.
 *
 * PERSPECTIVAL GAP:
 *   From the successor state's seat, this reading is coordination completing unfinished decolonization. From the originating subnational community's seat, the same mechanism looks like a second dispossession — the artifact returns to sovereign territory but not to the hands, altars, or ceremonies it was taken from. The engine's per-seat computation should register this asymmetry without either seat's account overriding the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor state governments and their national museum authorities are structural beneficiaries: they set the claims agenda, collect the artifacts, and convert them into prestige and tourism capital, so directionality sits near the beneficiary end. Encyclopedic museums and former colonial powers are targets bearing the transfer cost, with constrained exit given legal and reputational exposure. Originating subnational communities are also targets — arguably the deepest ones — because the framework's coordination benefit (righting colonial wrongs) is captured upstream by the state before it reaches them; their exit is trapped since they have no independent standing to press claims themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial-era extraction of cultural property without any legal remedy — was real and in the historical cases underlying most claims remains largely unaddressed, so this is not simple mandatrophy where the problem has vanished. What has drifted is the beneficiary: the mandate was framed as righting a historical wrong against expropriated peoples, but the operative claims mechanism vindicates states rather than peoples. Classifying this as tangled_rope rather than snare or rope preserves both truths simultaneously — real coordination function (documented colonial extraction genuinely reversed) plus asymmetric extraction (value captured by state apparatus rather than descendant community) — instead of collapsing the story into either pure villainy or pure justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_versus_community_continuity,
    'Does the modern successor state asserting a repatriation claim actually maintain meaningful continuity with the specific community the artifact was taken from, or is state continuity a legal fiction papering over discontinuity between the colonial-era community and the current national government?',
    'Case-by-case historical and anthropological assessment of whether the claimant state''s population, governance, and cultural practice descend from or remain connected to the originating community, versus cases where the state is a post-colonial successor entity with a different ethnic, religious, or political composition than the group actually harmed.',
    'Where continuity is genuine, this reading''s coordination function is strong and extraction is closer to a byproduct of scale; where continuity is fictive, the reading functions closer to a second extraction layered on top of the colonial one, shifting the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_versus_community_continuity, empirical, 'Whether asserted state-community continuity is historically real or a legitimating fiction.').

omega_variable(
    committer_kernel_framing_location,
    'This story reads the cultural property kernel as a state-sovereignty contest; the sibling readings relocate the same kernel onto community stewardship and universal-access grounds respectively. Where exactly does the disagreement live — is it a factual dispute about who suffered the original extraction, or a normative dispute about which unit (state, community, or humanity) is the correct bearer of restitution rights?',
    'Comparative analysis of the three sibling constraint files'' beneficiary/victim structures and extractiveness values would show whether the readings converge on facts and diverge only on normative unit-of-analysis, or diverge on facts as well.',
    'If the disagreement is purely normative (unit-of-analysis), all three readings can be simultaneously factually accurate and only in normative competition, supporting coexists_with relations across the kernel. If factual disagreement is present (e.g., disputed provenance or disputed community identity), one reading''s factual claims could undermine a sibling''s structural premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_framing_location, conceptual, 'Locating whether kernel disagreement is normative (unit of restitution) or factual (provenance/identity).').

omega_variable(
    diplomatic_leverage_asymmetry,
    'Do successor states use repatriation claims partly as diplomatic leverage or soft-power currency independent of the artifacts'' cultural significance to the originating community, and if so how much of measured extractiveness reflects that instrumental use versus genuine restitution?',
    'Tracking whether repatriated objects are prioritized for high-profile diplomatic exchanges and national museum flagship displays versus quieter transfer to regional or community custodianship would separate instrumental from restitutive use.',
    'High instrumental use would support a higher extractiveness reading and strengthen the case that national governments are capturing identity capital disproportionate to community benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diplomatic_leverage_asymmetry, empirical, 'Whether repatriation functions partly as state soft-power currency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2018, 0.36).
narrative_ontology:measurement(cult_tr_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement(cult_be_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1980, 0.33).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1990, 0.36).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2018, 0.43).
narrative_ontology:measurement(cult_su_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the single natural-language label 'cultural property restitution law,' per the epsilon-invariance principle: measuring the constraint by 'does the state get the object back' yields this moderate-extraction tangled_rope reading; measuring by 'does the actual descendant community regain custody and use' yields the indigenous_stewardship_reading (typically higher extraction against communities, since the state-capture problem intensifies); measuring by 'is preservation and public access maximized' yields the universal_heritage_reading (which frames dispersal to national museums as a preservation and access loss rather than a justice gain). All three describe the same underlying dispute over physical custody of the same artifacts but assign wholly different beneficiaries, victims, and epsilon values, so they are authored as separate linked files rather than one story with a hidden measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
