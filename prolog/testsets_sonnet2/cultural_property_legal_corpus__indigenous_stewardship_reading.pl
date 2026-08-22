% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Cultural Property Regime — Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates the indigenous-stewardship reading of the
 *   contested cultural-property kernel: legitimate authority over sacred and
 *   communal artifacts rests with the community that maintains living
 *   cultural continuity with them, not with the colonial museum that holds
 *   them or the post-colonial state that claims territorial succession. Under
 *   this reading both museums AND successor states are extractors — the
 *   state's claim of national sovereignty over an object is treated as
 *   structurally analogous to the museum's claim of institutional
 *   stewardship: neither is the community whose ongoing ceremonial or lineage
 *   relationship the object exists within. This produces the highest epsilon
 *   of the three sibling readings, because it denies legitimate standing to
 *   every party currently holding or negotiating over the objects except the
 *   community itself, which typically holds none of them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.79).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Regime — Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '9fe23178-8750-4ada-8561-dc36b2f023ea').
narrative_ontology:cs_kernel_codification('9fe23178-8750-4ada-8561-dc36b2f023ea', distributed).
narrative_ontology:cs_authority_grounding('9fe23178-8750-4ada-8561-dc36b2f023ea', distributed).
narrative_ontology:cs_reading_relation('9fe23178-8750-4ada-8561-dc36b2f023ea', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fe23178-8750-4ada-8561-dc36b2f023ea', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('9fe23178-8750-4ada-8561-dc36b2f023ea', foundational, community_continuity_grounds_title).
narrative_ontology:cs_axiom_status(community_continuity_grounds_title, holdable).
narrative_ontology:cs_axiom_grounding('9fe23178-8750-4ada-8561-dc36b2f023ea', community_continuity_grounds_title, deontological).
narrative_ontology:cs_axiom('9fe23178-8750-4ada-8561-dc36b2f023ea', foundational, territorial_succession_does_not_confer_cultural_authority).
narrative_ontology:cs_axiom_status(territorial_succession_does_not_confer_cultural_authority, holdable).
narrative_ontology:cs_axiom_grounding('9fe23178-8750-4ada-8561-dc36b2f023ea', territorial_succession_does_not_confer_cultural_authority, conventional).
narrative_ontology:cs_reference_frame('9fe23178-8750-4ada-8561-dc36b2f023ea', community_continuity_authority).
narrative_ontology:cs_drift_state('9fe23178-8750-4ada-8561-dc36b2f023ea', post_undrip_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('9fe23178-8750-4ada-8561-dc36b2f023ea', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_curatorial_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, diaspora_indigenous_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, art_market_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sacred and communal objects acquired during colonial expropriation, punitive expeditions, or coerced sale, and set the terms under which any claim for return is even considered (provenance thresholds, legal title tests, conservation arguments). They control the objects physically, control the archives that would prove or disprove the circumstances of acquisition, and derive prestige, ticket revenue, and research access from continued possession. Under this reading they have no legitimate claim at all — the community that maintains living cultural and spiritual continuity with the object is the only party with standing.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_curatorial_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_curatorial_institutions, beneficiary).

% Assert sovereign title to artifacts removed from their territory, negotiate state-to-state repatriation on the basis of national succession, and often route returned objects into state or national museums rather than the originating community. Under this reading, national-territorial succession is not the same thing as unbroken cultural continuity — the state can be exactly as extractive as the colonial museum if it treats the community's sacred object as a national trophy rather than returning it to the community that maintains the practice.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, beneficiary).

% Maintain the living ceremonial, spiritual, or lineage-based relationship to the object — the object is not a historical curiosity to them but an active participant in ongoing practice. They bear the cost of the object's absence: interrupted ceremonies, severed intergenerational transmission, and the indignity of having to petition foreign institutions or their own nominal national government for something they never ceded. Exit is not available — there is no substitute object and no forum that treats their standing as dispositive rather than advisory.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities, payer,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities, beneficiary).

% Displaced descendants of the source community, often without formal recognition from the successor state, who nonetheless carry the same continuity claim. They are systematically excluded from repatriation negotiations that treat the state or a narrowly defined resident community as the only eligible claimant, losing standing twice — once to the original expropriation, once to the modern negotiation's narrow eligibility rules.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, diaspora_indigenous_descendants, payer,
    powerless, generational, trapped, global).

% Auction houses, private collectors, and dealers who trade in artifacts whose provenance runs through colonial-era acquisition. They profit from the ambiguity between the sovereign-repatriation and universal-heritage framings, since neither fully forecloses market circulation the way community standing would. Under this reading their transactions are trafficking in stolen sacred property regardless of documented sale chains, since no downstream buyer can cure a title that was never legitimately alienated from the community in the first place.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, art_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Adjudicate restitution claims under existing international instruments (UNESCO 1970, UNIDROIT 1995, national repatriation statutes) that were drafted primarily around state-to-state and institution-to-state relationships. They can hear community claims but frequently lack a jurisdictional category for community-as-rightsholder distinct from state-as-rightsholder, which is precisely the gap this reading identifies as the mechanism of ongoing extraction.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_tribunals, observer,
    institutional, generational, analytical, continental).

% Some experts within holding institutions privately support community-standing claims but are structurally positioned inside the institutions that benefit from continued possession, and are rarely given a formal voice in tribunal proceedings that treat institutional and state positions as the only parties of record.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, anthropological_and_curatorial_experts, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The surface coordination story is preservation and safe custody of fragile, high-value cultural objects on behalf of humanity or a nation. Under this reading, that story is largely pretextual for objects that carry ongoing sacred or ceremonial function: preservation was never the community's request, and the actual coordination problem — maintaining living cultural continuity — is one the current custodians are structurally unable to solve because they are not part of the practicing community.
% TRANSFER_FUNCTION: Physical custody, exhibition revenue, scholarly access, and national prestige move from the indigenous source community (which loses the object's active ceremonial and lineage function) to museums (which gain collection value and visitor revenue) and to successor states (which gain a symbol of national patrimony they can display as their own).
% ABSENT_VOICES: Diaspora descendants and non-state-recognized community factions are almost never named parties in state-to-state or museum-to-state restitution negotiations; anthropological staff who would corroborate the community's continuity claim are institutionally quieted by employment dependence on the holding institution.
% DISAPPEARANCE_RATIONALE: If the current custody arrangement dissolved and physical objects were returned to the standing of the communities maintaining continuity with them, museum collections would shrink materially, national patrimony narratives built around held artifacts would lose their anchor objects, and ceremonial and lineage practices interrupted for generations would resume with the returned objects as active participants rather than display items — a substantial rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: Colonial-era removal, looting, and forced sale stripped source communities of sacred and communal objects; the subsequent legal and museological apparatus was built to legitimate and administer that removed collection rather than to test whether the removal was ever legitimate.
% FOUNDING_PROBLEM_CORROBORATION: UN Special Rapporteur reports on the rights of indigenous peoples and multiple national truth-and-reconciliation commission findings, produced by bodies outside both the holding museums and the successor states, corroborate that community continuity claims persist unresolved; the museums and states themselves largely characterize the matter as legally settled by twentieth-century title transfers, which is precisely the corroboration gap this reading identifies.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.88 by interval end) and rising, reflecting an increasingly asserted (not settled) recognition gap: as international discourse on indigenous rights has hardened (UNDRIP, national reconciliation processes), the mismatch between what museums/states retain and what community-standing claims would require has become more visible and more contested, not less. Suppression (0.79) captures the structural exclusion of community claimants from tribunals built around state and institutional parties. Theater ratio rises to 0.44 as some institutions adopt visible 'consultation' and provenance-review programs whose function is increasingly reputational rather than restitutive — a rising theater signal alongside rising extraction is exactly the drift pattern this reading treats as evidence the underlying claim, not merely its handling, is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the museum and successor-state seats, current custody is lawful, often multilaterally negotiated, and increasingly generous (loan programs, co-stewardship pilots). From the source-community seat, the same arrangement is the continuation of an original wrong through updated paperwork. The engine should compute divergent per-seat types from these structural positions; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous source communities and diaspora descendants are the clearest targets: trapped exit, powerless, civilizational time horizon (the loss compounds across generations), bearing the cost of interrupted practice. Museums and successor states are both coded as beneficiaries under this reading specifically because the reading denies the state's sovereignty claim the same legitimacy it denies the museum's institutional claim — this is the reading's structural signature relative to the sovereign-repatriation reading, which would instead code the successor state as a victim of colonial extraction and a legitimate claimant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial removal without community consent) remains live by outside corroboration (UN and truth-commission findings) even though the apparatus built to manage the removed collections treats the matter as substantially resolved by twentieth-century title transfers. That status/verdict mismatch — status effectively contested-toward-dead from the custodian side, world_rearranges from the community side — is the mandatrophy signature this reading is built to surface: an arrangement that persists because its administrators declare the founding grievance closed, not because it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_identification_ambiguity,
    'Which entity counts as ''the community maintaining cultural continuity'' when a community has fragmented, dispersed into diaspora, or lost internal consensus on the object''s status?',
    'Anthropological and community self-governance evidence establishing which body(ies) the practicing community itself recognizes as authoritative for the object in question, gathered independently of state or museum framing.',
    'If community identification is itself contested or fragmented, the reading''s clean beneficiary/victim split partially breaks down — internal community disputes could reproduce the same extraction dynamic at a smaller scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_identification_ambiguity, conceptual, 'Uncertainty over who legitimately speaks for a source community under this reading.').

omega_variable(
    reading_selection_is_the_dispute,
    'Is the indigenous-stewardship reading the structurally correct account of legitimate authority over these artifacts, or is it one of three defensible readings (alongside sovereign-repatriation and universal-heritage) with no framework-external tiebreaker?',
    'None available within existing international law instruments; this is the committer-level disagreement the kernel itself is built to hold. Resolution would require either a new international instrument that formally adopts community-standing as the operative legal category (rather than state or institutional title), or would remain permanently contested as a normative rather than empirical question.',
    'If community-standing is adopted as the operative legal category, successor-state claims currently treated as remediation (under the sovereign-repatriation reading) would themselves become subject to this reading''s extraction analysis. If it is not adopted, this reading remains an advocacy position rather than an operative legal standard, and the very high epsilon here describes moral/political claim strength rather than adjudicated legal extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_is_the_dispute, preference, 'Whether this reading, versus its two siblings, is the correct account of legitimate authority — the kernel-level disagreement itself.').

omega_variable(
    state_as_proxy_or_extractor,
    'Can a successor state ever legitimately act as proxy custodian for a community it has formal treaty or constitutional relations with, or does any state intermediation under this reading collapse into extraction by definition?',
    'Case-by-case examination of whether specific repatriation agreements route custody and control genuinely back to the community (with state facilitation only) versus routing objects into state national museums over community objection.',
    'If some state arrangements do function as genuine community proxies, the blanket beneficiary coding of colonial_successor_states in this story would need to be split into a proxy-legitimate subset and an extractive subset, which would lower aggregate epsilon for the reading in cases with strong community-state alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_as_proxy_or_extractor, empirical, 'Whether state custody can ever satisfy this reading''s community-continuity standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cult_tr_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(cult_tr_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cult_tr_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(cult_be_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 8, 0.75).
narrative_ontology:measurement(cult_be_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 24, 0.83).
narrative_ontology:measurement(cult_be_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 32, 0.86).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cult_su_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(cult_su_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(cult_su_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.08).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the cultural_property_legal_corpus kernel, decomposed per the ε-invariance principle: sovereign_repatriation_reading (successor states as legitimate claimants, colonial acquisition as state-level theft) and universal_heritage_reading (holding institutions as legitimate on preservation/access grounds). Each reading has its own epsilon, beneficiary/victim structure, and claimed type; this reading's structural distinctiveness is treating BOTH museums and successor states as extractors, yielding the highest epsilon of the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
