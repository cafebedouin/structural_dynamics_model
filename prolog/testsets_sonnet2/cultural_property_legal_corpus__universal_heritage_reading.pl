% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine of Cultural Property Custody
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates the universal-heritage reading of the cultural
 *   property kernel: the claim that legitimate custodial authority rests with
 *   whichever institution best maximizes preservation and universal access,
 *   independent of an artifact's place of origin. Under this reading,
 *   encyclopedic museums are not merely custodians defending a legal position
 *   but the structurally correct authority, and successor-state or community
 *   claims read as particularist threats to a public good ('humanity's shared
 *   heritage') that transcends any single nation or community's interest. As
 *   authored, the metrics track this reading's own operation, not its
 *   self-justifying rhetoric: extraction is substantial and rising because
 *   the doctrine's practical effect over the twentieth and twenty-first
 *   centuries has been to convert an initial conservation rationale into a
 *   durable legal shield that imposes recurring costs (litigation, diplomatic
 *   friction, identity harm) on claimant states and communities regardless of
 *   whether the original conservation rationale still applies to a given
 *   object.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.58).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine of Cultural Property Custody").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '44d786a7-5a10-41d2-bdea-31a7eebe0aa9').
narrative_ontology:cs_kernel_codification('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', distributed).
narrative_ontology:cs_authority_grounding('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', extraction).
narrative_ontology:cs_interpretation_layer_present('44d786a7-5a10-41d2-bdea-31a7eebe0aa9').
narrative_ontology:cs_reading_relation('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_reading_relation('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_axiom('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', foundational, preservation_capacity_grounds_custody).
narrative_ontology:cs_axiom_status(preservation_capacity_grounds_custody, holdable).
narrative_ontology:cs_axiom_grounding('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', preservation_capacity_grounds_custody, instrumental).
narrative_ontology:cs_axiom('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', foundational, geographic_origin_is_normatively_irrelevant_to_custody).
narrative_ontology:cs_axiom_status(geographic_origin_is_normatively_irrelevant_to_custody, holdable).
narrative_ontology:cs_axiom_grounding('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', geographic_origin_is_normatively_irrelevant_to_custody, conventional).
narrative_ontology:cs_reference_frame('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', encyclopedic_museum_founding_era_consensus).
narrative_ontology:cs_drift_state('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', post_1970_unesco_convention_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('44d786a7-5a10-41d2-bdea-31a7eebe0aa9', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institution_curatorial_staff).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, host_state_tourism_economies).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_claimant_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, diaspora_source_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities_of_origin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cosmopolitan_stewardship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds contested artifacts acquired during colonial-era expeditions, punitive campaigns, or unequal-treaty transactions. Sets acquisition, loan, and deaccession policy; funds legal defense against repatriation claims; produces the scholarly and curatorial framing ('universal museum,' 'shared heritage of mankind') that grounds refusal to return objects. Retains full physical and legal custody, draws visitor revenue and prestige from the collection, and can grant or withhold long-term loans as a substitute for restitution.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, beneficiary).

% Careers, publications, and professional standing are built on stewardship of the collection as currently constituted. Benefits from continued access to objects for research and exhibition; a large-scale repatriation would dissolve the collections and research programs their expertise is organized around.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_institution_curatorial_staff, beneficiary,
    moderate, biographical, constrained, national).

% Draws substantial tourism revenue and soft-power prestige from housing major world collections in national capitals. Governments face domestic constituencies benefiting from the status quo and have limited incentive to compel institutions they partly fund to repatriate high-draw objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, host_state_tourism_economies, beneficiary,
    powerful, generational, mobile, national).

% Pursue restitution through litigation, diplomatic negotiation, and international bodies (UNESCO 1970 Convention, bilateral cultural agreements) against institutions with superior legal resources, home-court jurisdiction, and control of provenance archives. Bear the recurring costs of legal proceedings, diplomatic friction, and the reputational framing of their claims as 'nationalist' or 'political' rather than restorative. Exit is constrained: they cannot simply walk away from artifacts central to national identity and historical memory, but have no unilateral mechanism to compel return.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_claimant_states, payer,
    moderate, generational, constrained, national).

% Communities whose ancestral objects sit in foreign vitrines have no standing to sue and are frequently not consulted even when their state pursues a claim on their behalf. Bear cultural and psychological costs of separation from ceremonial or sacred items but sit outside the state-to-institution negotiation entirely.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, diaspora_source_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, diaspora_source_communities, excluded).

% Bear the identity and continuity harm of objects used in living ritual, governance, or memory practice being held as static exhibits under a framing ('humanity's shared heritage') that erases their specific claim to those objects as still-functioning cultural property rather than historical curiosities.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities_of_origin, payer,
    powerless, civilizational, trapped, local).

% Mediate claims, issue advisory opinions, and broker return agreements, but possess no binding enforcement power over encyclopedic museums in states that have not ratified relevant conventions or that grandfather pre-1970 acquisitions. Their process legitimizes the current custody arrangement by channeling claims into slow, non-binding forums.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, unesco_and_bilateral_commissions, observer,
    institutional, generational, analytical, global).

% Auction houses and private dealers benefit from the same doctrine that keeps provenance scrutiny loose and title disputes unresolved; a strong repatriation norm would chill transactions in objects with contested colonial-era chains of custody.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable custodial and conservation infrastructure — climate control, scholarly cataloguing, security, and broad public access — for objects that might otherwise be dispersed, under-resourced, or inaccessible to global audiences; solves a genuine preservation and access-coordination problem for the subset of objects genuinely at conservation risk in their states of origin.
% TRANSFER_FUNCTION: Moves physical custody, exhibition revenue, scholarly capital, and narrative authority over cultural objects from communities and states of origin to the museums and states that currently hold them, while moving the political and cultural cost of contestation onto claimant states and descendant communities.
% ABSENT_VOICES: Descendant and diaspora communities are almost never direct parties to restitution negotiations, which occur state-to-institution; their view of the object as still-living ceremonial or governance property, rather than historical artifact, rarely enters the legal or curatorial record.
% DISAPPEARANCE_RATIONALE: If the universal-heritage doctrine lost its legitimating force, encyclopedic museums would lose their primary legal and rhetorical defense against restitution claims; provenance-based title would likely default toward state-of-origin or community claims, collections would shrink substantially, and the tourism and scholarly economies built around them would have to reorganize around loan-based or co-custodial models.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century encyclopedic museums framed comprehensive, centrally-held collections as solving fragmentation, looting-by-private-collectors, and unequal conservation capacity across a world of unstable states and limited scientific infrastructure — the object was safer, better studied, and more widely seen in London, Paris, or Berlin than left in situ.
% FOUNDING_PROBLEM_CORROBORATION: Conservation scientists and some UNESCO technical reports corroborate that certain acquisition-era conservation risks were real for some categories of objects at the time. However, independent historians of colonialism, postcolonial legal scholars, and the claimant states themselves attest that the conservation-risk framing was frequently pretextual relative to the scale of extraction, and that many origin states now possess conservation capacity the doctrine's continued invocation ignores; no corroboration exists from a party outside the museums' own scholarly and legal apparatus for the claim that current custody remains conservation-necessary rather than institutionally self-interested.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the bulk of contested holdings no longer face the conservation or instability risks that originally justified centralized custody in the doctrine's own founding logic, yet the doctrine continues to license retention and to recharacterize restitution claims as threats to universal access rather than as legitimate title disputes. Suppression (0.58) is moderate-high: institutions rely on procedural friction (slow, non-binding UNESCO forums; statutes of limitations; sovereign immunity doctrines; home-court jurisdiction) rather than overt coercion, which caps suppression below a snare-level but keeps it well above genuine rope levels. Theater ratio (0.44) and its rising trajectory capture the growing gap between the doctrine's stated preservation function (now often satisfiable via co-custody, digitization, or loan) and its actual operation as a legal-rhetorical shield — theatrical stewardship increasingly substitutes for the preservation function it claims. Accessibility collapse (0.50) is moderate: some alternatives exist (long-term loans, replica programs, joint stewardship agreements) but the doctrine as authored treats full title transfer as near-unthinkable, collapsing the strongest alternative. Resistance (0.72) is high and rising: claimant states, descendant communities, and a growing coalition of postcolonial legal scholarship actively contest the doctrine, which is itself evidence this is a constructed and contested arrangement rather than settled natural allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedic museums and their host-state tourism economies sit at the beneficiary end: they retain custody, revenue, and narrative authority, and their exit options are effectively arbitrage-grade (they can reframe, relitigate, or offer partial loans without ceding title). Curatorial staff and art-market intermediaries benefit indirectly through career and transactional structures built on the current custody regime. Successor claimant states are structural targets: their exit is constrained by the practical impossibility of abandoning claims to identity-central objects, and by the absence of any unilateral enforcement mechanism. Diaspora and descendant communities are the most trapped: powerless, excluded from the state-to-institution negotiation, and bearing continuity and identity harms this reading's framing (objects as 'humanity's' rather than a living community's) explicitly discounts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine conservation risk and fragmentation-era instability — was real for at least some holdings at acquisition. Reading this arrangement as a tangled_rope rather than a pure snare or a pure rope prevents two mislabelings: treating the entire arrangement as innocent coordination (ignoring that the doctrine now functions largely as a legal shield decoupled from actual conservation need) and treating it as pure extraction with no coordination residue (ignoring that some objects genuinely do benefit from centralized conservation and broad public access, and that abrupt wholesale reversal would itself create real preservation risk for a subset of holdings). The mandate has substantially outlived its original function for the majority of contested objects while retaining partial validity for a minority — this asymmetry is what tangled_rope classification is built to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_genuine_vs_constructed,
    'Is ''humanity''s shared heritage'' a genuine transcendent public good that legitimately overrides particularist claims, or is it a constructed doctrine that happens to legitimate the interests of whichever institutions currently hold contested objects?',
    'Compare the doctrine''s application across cases: does the ''universal access'' framing get invoked symmetrically (e.g., applied even against Western museums'' own claims to retain artifacts when a stronger universal-access argument favors a museum elsewhere), or does it correlate near-perfectly with which institution currently holds the object? Asymmetric application is evidence of constructed post-hoc justification rather than principled doctrine.',
    'If the doctrine is applied asymmetrically in favor of current holders, this reading functions closer to a false summit — a naturalized-sounding principle serving identifiable beneficiaries — strengthening the tangled_rope-to-snare direction. If applied symmetrically including against holders'' interests, the coordination function is more genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_genuine_vs_constructed, conceptual, 'Whether universal heritage is a principled standard or a beneficiary-correlated post-hoc rationale.').

omega_variable(
    conservation_risk_currency,
    'For the specific objects currently retained under this doctrine, does the originating conservation-risk rationale still hold today, given improved conservation capacity in many origin states?',
    'Object-by-object or category-by-category conservation capacity assessment in claimant states, compared against actual conservation conditions and resources at the holding institutions.',
    'Where conservation risk has genuinely lapsed, retention under this doctrine is extraction with no residual coordination function for that object; where risk remains real, some coordination function persists and full tangled_rope classification (rather than snare) remains warranted for that subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_risk_currency, empirical, 'Whether the founding conservation rationale remains empirically live for currently retained objects.').

omega_variable(
    reading_disagreement_locus,
    'Where exactly do the three kernel readings disagree — is it about facts (who acquired what, under what conditions), or about the normative unit of legitimate authority (institution vs. state vs. community)?',
    'Map cases where all three readings would agree on the acquisition facts but diverge only on which authority structure should hold custody as a result — isolating the normative disagreement from any factual one.',
    'If the disagreement is purely normative (which this omega suspects), no amount of provenance research resolves the kernel contest; the three readings are genuinely incommensurable framings requiring political/legal choice rather than empirical adjudication, which explains why the same evidentiary record supports multiple constraint stories with divergent ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_disagreement_locus, conceptual, 'Locating the kernel disagreement as a normative-authority dispute rather than a factual dispute over acquisition history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'the cultural property authority question' per the epsilon-invariance principle: universal_heritage_reading (this file — institutions as legitimate holders, ε=0.68, tangled_rope), sovereign_repatriation_reading (successor states as legitimate holders — expected higher ε on holding institutions and lower ε on states, likely snare or tangled_rope from the claimant-state seat), and indigenous_stewardship_reading (communities as legitimate holders — expected to surface descendant communities as primary beneficiaries and both museums and successor states as extractive intermediaries, potentially snare from the community seat). Each story shares the same underlying acquisition-history facts but assigns beneficiary/victim status and ε according to a different normative premise about where legitimate authority rests. The three are linked bidirectionally: legal and diplomatic pressure generated under one reading (e.g. a successful sovereign-repatriation claim) structurally changes the resource and legitimacy environment for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
