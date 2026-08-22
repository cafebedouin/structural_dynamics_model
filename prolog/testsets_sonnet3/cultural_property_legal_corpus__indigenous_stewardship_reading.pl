% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Cultural Property Legal Corpus — Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   The existing international cultural property legal corpus (UNESCO 1970,
 *   UNIDROIT 1995, bilateral repatriation agreements, museum deaccessioning
 *   policy) recognizes states and holding institutions as the operative
 *   parties to disputes over sacred and communal indigenous objects. Read
 *   from the indigenous stewardship standpoint, this entire arrangement is a
 *   structure of continued extraction: it launders the original colonial
 *   taking into a permanent institutional or state title that the actual
 *   continuity-bearing community can access only if a state chooses, as a
 *   matter of its own interest, to act on the community's behalf.
 *
 * KEY AGENTS:
 *   - indigenous_source_communities: primary target (powerless/trapped) — bears the extraction; excluded from standing entirely
 *   - diasporic_descendant_communities: secondary target (powerless/trapped) — doubly excluded, no state proxy available at all
 *   - museum_holding_institutions: primary beneficiary (institutional/arbitrage) — retains possession, prestige, revenue
 *   - colonial_successor_states: co-beneficiary under this reading (institutional/arbitrage) — captures repatriation as state nation-building without transferring custody downward
 *   - international_courts_and_treaty_bodies: agenda-setter (institutional/analytical) — the adjudicative architecture itself structurally excludes sub-state claimants
 *   - indigenous_rights_advocacy_networks: excluded voice (moderate/constrained) — documents and organizes but has no binding forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.78).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Legal Corpus — Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, 'a5088f29-033f-45d2-94d5-b3d8169e3276').
narrative_ontology:cs_kernel_codification('a5088f29-033f-45d2-94d5-b3d8169e3276', distributed).
narrative_ontology:cs_authority_grounding('a5088f29-033f-45d2-94d5-b3d8169e3276', distributed).
narrative_ontology:cs_reading_relation('a5088f29-033f-45d2-94d5-b3d8169e3276', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5088f29-033f-45d2-94d5-b3d8169e3276', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('a5088f29-033f-45d2-94d5-b3d8169e3276', foundational, continuity_bearing_community_is_sole_legitimate_claimant).
narrative_ontology:cs_axiom_status(continuity_bearing_community_is_sole_legitimate_claimant, holdable).
narrative_ontology:cs_axiom_grounding('a5088f29-033f-45d2-94d5-b3d8169e3276', continuity_bearing_community_is_sole_legitimate_claimant, deontological).
narrative_ontology:cs_axiom('a5088f29-033f-45d2-94d5-b3d8169e3276', foundational, successor_state_title_does_not_entail_community_legitimacy).
narrative_ontology:cs_axiom_status(successor_state_title_does_not_entail_community_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a5088f29-033f-45d2-94d5-b3d8169e3276', successor_state_title_does_not_entail_community_legitimacy, conventional).
narrative_ontology:cs_reference_frame('a5088f29-033f-45d2-94d5-b3d8169e3276', pre_colonial_community_custodianship).
narrative_ontology:cs_drift_state('a5088f29-033f-45d2-94d5-b3d8169e3276', contemporary_repatriation_diplomacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a5088f29-033f-45d2-94d5-b3d8169e3276', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, diasporic_descendant_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, state_title_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_universal_custodianship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The communities who created, use, or maintain lineage continuity with the sacred and communal objects in question. Under existing law they have no standing to compel return unless a successor state chooses to litigate or negotiate on their behalf, and that state's title may not track their own claim of continuity at all. They cannot access, use ceremonially, or exercise stewardship over their own cultural patrimony while it sits in vaults or display cases thousands of miles away, cataloged under someone else's provenance narrative.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities, payer,
    powerless, civilizational, trapped, regional).

% Descendants dispersed by colonization, displacement, or genocide who retain cultural or genealogical connection to the objects but lack any state apparatus to assert claims on their behalf at all — they are doubly excluded, first by the original taking and second by a legal framework that only recognizes state or institutional claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, diasporic_descendant_communities, payer,
    powerless, civilizational, trapped, global).

% Hold the objects, control access, set loan and repatriation policy, and derive prestige, funding, scholarly capital, and visitor revenue from continued possession. They frame their custodianship as preservation and universal access, and can indefinitely delay or condition any return through provenance disputes, conservation claims, or negotiated partial arrangements that keep ultimate title with the museum.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_holding_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_holding_institutions, agenda_setter).

% States that inherited the colonial administrative apparatus and, under existing treaty and property law, are treated as the legitimate negotiating party for repatriation claims — even where the artifacts belong to communities the state itself has historically marginalized, forcibly assimilated, or displaced. They can pursue repatriation as a nation-building or diplomatic project without transferring the returned objects' custody, meaning, or use rights back to the originating community.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter).

% Adjudicate cultural property disputes under conventions (UNESCO 1970, UNIDROIT 1995) that recognize state parties as the operative claimants. They apply doctrines of state title and museum custodianship that structurally cannot recognize a sub-state indigenous community as a rights-holder without a state or NGO intermediary, reproducing the exclusion at the level of the adjudicative forum itself.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_courts_and_treaty_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Transnational indigenous rights organizations that argue for direct community standing under instruments like UNDRIP, but have no binding forum in which to press claims independent of a sympathetic state's willingness to act as proxy. They document violations, petition, and organize occupations or public pressure campaigns, but the formal repatriation architecture does not seat them as parties.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_rights_advocacy_networks, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The existing legal corpus does coordinate something real: it establishes stable, internationally recognized custody and provenance records that prevent chaotic looting disputes between museums and states, and it gives some claimants (states) a forum to negotiate returns at all.
% TRANSFER_FUNCTION: Continued physical possession, interpretive authority, ceremonial and economic value, and cultural narrative control flow to museums and successor states; the communities with the deepest continuity claim receive none of these unless a state chooses, as a matter of its own diplomatic interest, to pass custody downward.
% ABSENT_VOICES: Indigenous source communities and diasporic descendant groups are structurally absent from the adjudicative process itself — international conventions recognize state parties, not sub-state communities, as claimants, so a community whose own government is indifferent or hostile has no forum at all.
% DISAPPEARANCE_RATIONALE: If the current state/museum-centered title framework vanished and were replaced by direct community stewardship recognition, provenance would have to be re-traced to specific communities rather than nations, thousands of objects currently displayed or stored under national or museum title would face community-initiated claims, and the entire economics of encyclopedic museums (built substantially on colonial-era acquisitions) would be destabilized.
% FOUNDING_PROBLEM: The corpus was built to stop uncontrolled looting and black-market trafficking of antiquities after WWII-era plunder, and to give some legal traction to newly independent states seeking return of objects taken under colonial administration.
% FOUNDING_PROBLEM_CORROBORATION: UN Special Rapporteurs on cultural rights and indigenous rights (a source outside both the museum and the successor-state benefiting parties) have repeatedly found that state-centered repatriation frameworks fail to secure community-level restitution even after formal transfer to a state, corroborating that the original anti-looting problem has been partially addressed while a distinct, unaddressed problem — recognition of sub-state community title — was never solved and is treated by both museums and states as outside their obligation.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.86, the highest among the three sibling readings, because under this reading's own premise the class of illegitimate holders is maximal: both museums (holding physical custody without community-recognized title) AND successor states (claiming diplomatic standing without necessarily representing the specific continuity-bearing community) are extractors. Suppression (0.78) reflects that the adjudicative forums themselves (treaty bodies, courts) are structured to only recognize state parties, which is an active, institutionalized barrier rather than mere neglect. Theater ratio (0.42) captures the growing volume of repatriation ceremonies, loan agreements, and 'collaborative stewardship' partnerships that perform partial acknowledgment while retaining ultimate institutional or state title — a rising theatricality documented across the interval as public pressure for repatriation has grown faster than actual transfer of title to communities.
 *
 * PERSPECTIVAL GAP:
 *   From the museum and successor-state seats, the current corpus looks like careful, negotiated custodianship-in-transition — cooperative, lawful, incrementally responsive. From the indigenous community seat under this reading, the identical structure is a continuation of the original taking by other means: the objects never left custody of parties without legitimate claim, they simply changed which illegitimate party holds title. The engine computes these as different seat-level classifications from the same structural data; this divergence is the point of the reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Museum holding institutions and colonial successor states are declared beneficiaries because, under this reading's premise of community-continuity title, both retain possession or negotiating power without the underlying legitimacy the reading requires — this places both near the full-beneficiary end of directionality despite their very different institutional character. Indigenous source communities and diasporic descendants are declared victims: trapped exit (no legal standing to act unilaterally), civilizational time horizon (claims spanning generations of dispossession), and regional-to-global scope, which the engine will read as high effective extraction given their structural powerlessness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding anti-looting and anti-trafficking function of the corpus is largely resolved — black-market antiquities trafficking is comparatively well-suppressed relative to mid-20th-century baselines. But the corpus's mandate has not sunset; it has been repurposed to adjudicate an entirely different problem (community title recognition) using an apparatus (state-to-state or state-to-museum negotiation) that structurally cannot solve it. This is a case where mandatrophy is NOT simply 'unresolved' — it is actively obscured by the theater of state-led repatriation ceremonies that create the appearance of the founding problem being addressed while the actual continuity-bearing claimants remain outside the forum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_proxy_legitimacy,
    'When a successor state repatriates an object and retains or redistributes custody through its own national museum system, does that transfer discharge the community''s claim under the indigenous stewardship reading, or does it merely relocate the extraction from a foreign museum to a domestic one?',
    'Track post-repatriation custody chains: does the object return to community control, ceremonial use, and community-determined access, or does it enter a state or national museum with the community again excluded from title?',
    'If state repatriation systematically fails to transfer custody to the actual community, successor states should be treated as co-extractors (as authored here) rather than as allies of the indigenous claim, which sharply changes assessment of celebrated ''repatriation successes.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_proxy_legitimacy, empirical, 'Whether state repatriation discharges or merely relocates the extraction this reading identifies.').

omega_variable(
    continuity_verification_authority,
    'Who has legitimate authority to determine which community holds the requisite ''cultural continuity'' when multiple descendant or successor groups contest a single object''s proper stewardship?',
    'Comparative analysis of existing community-led title determinations (e.g., NAGPRA cultural affiliation processes) versus court or treaty-body determinations, assessing which produces outcomes the affected communities themselves recognize as legitimate.',
    'If continuity determination requires an external adjudicator, the indigenous stewardship reading risks reproducing exactly the intermediary-authority problem it accuses the state-centered corpus of creating; if communities can self-certify through accepted internal processes, the reading is more structurally distinct from its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_verification_authority, conceptual, 'Whether continuity-based title requires an external adjudicator, which would partly collapse this reading''s distinctiveness from state-title models.').

omega_variable(
    kernel_framing_choice,
    'Is the cultural property legal corpus better understood as a SINGLE contested kernel with three readings (as authored here), or as three genuinely separate legal instruments that happen to be colloquially conflated under one label?',
    'Trace whether the same treaty texts and case law (UNESCO 1970, UNIDROIT 1995, national NAGPRA-style statutes) are cited across all three reading communities as the shared object of interpretation, versus each reading drawing on structurally distinct legal instruments.',
    'If the same texts are shared and reinterpreted, the kernel framing holds and this story''s ε-invariance depends on holding the standing arrangement fixed while only the interpretive premise varies (as done here). If the instruments are actually distinct, this story may itself require further decomposition rather than treatment as one reading among three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the three declared readings share one kernel text or are colloquially conflated distinct instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cult_tr_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(cult_tr_t36, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(cult_tr_t48, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 48, 0.39).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(cult_be_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(cult_be_t36, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 36, 0.82).
narrative_ontology:measurement(cult_be_t48, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 48, 0.84).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 60, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(cult_su_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(cult_su_t36, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 36, 0.74).
narrative_ontology:measurement(cult_su_t48, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 48, 0.76).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the cultural_property_legal_corpus kernel. sovereign_repatriation_reading treats successor states as the legitimate claimant against museums (moderate-high ε, states as beneficiary-victims of colonial extraction); universal_heritage_reading treats preservationist institutions as legitimate regardless of origin (lowest ε, near-mountain/rope framing). This reading treats sub-state continuity-bearing communities as sole legitimate claimant, which is why ε here is authored highest: it is the only reading under which BOTH the museum and the state are extractors simultaneously. The three stories share the same standing arrangement (the existing legal corpus and current custody patterns) as their common ε referent; they diverge only in whose premise of legitimate title is applied to that fixed arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
