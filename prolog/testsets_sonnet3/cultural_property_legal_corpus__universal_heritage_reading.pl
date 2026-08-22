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
 *   human_readable: Universal Heritage Doctrine of Cultural Property Custodianship
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates the universal-heritage reading of the contested
 *   cultural-property kernel: the claim that cultural artifacts are
 *   humanity's shared heritage and that legitimate custodial authority rests
 *   with institutions best positioned to preserve and universally exhibit
 *   them, regardless of where those artifacts originated or how they were
 *   acquired. Under this reading, encyclopedic museums and their donor and
 *   market networks occupy the beneficiary seat, and the coordination story
 *   (genuine conservation science, genuine access expansion) is real but
 *   increasingly rides alongside asymmetric extraction from claimant states
 *   and descendant communities, whose restitution claims are treated within
 *   the reading's own logic as particularist threats to a public good rather
 *   than as equally legitimate normative starting points. The ε authored here
 *   (0.71, rising over the interval) reflects the standing arrangement as it
 *   currently operates under the universal-heritage doctrine's own
 *   institutional practice — not the sovereign-repatriation or
 *   indigenous-stewardship alternative arrangements, which are separate
 *   constraints (see kernel_context and network links) with their own ε
 *   values and their own claimant-favoring structural deltas.
 *
 * KEY AGENTS:
 *   - encyclopedic_museums: agenda_setter/beneficiary (institutional/arbitrage) — sets custodial terms, retains legal title, absorbs dispute costs indefinitely
 *   - successor_states_of_origin: payer (moderate/constrained) — bears diplomatic and legal costs of restitution pursuit under a framework structured against them
 *   - descendant_communities: payer (powerless/trapped) — bears identity and cultural-continuity harm with no direct standing
 *   - national_cultural_ministries_of_claimant_states: payer/agenda_setter (moderate/constrained) — redirects scarce state resources into structurally disadvantaged claims
 *   - unesco_and_multilateral_cultural_bodies: observer (institutional/analytical) — mediates without enforcement power, constrained by neutrality mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.71).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.62).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine of Cultural Property Custodianship").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'aa9b7e65-5432-49a3-a16f-135e70664671').
narrative_ontology:cs_kernel_codification('aa9b7e65-5432-49a3-a16f-135e70664671', distributed).
narrative_ontology:cs_authority_grounding('aa9b7e65-5432-49a3-a16f-135e70664671', extraction).
narrative_ontology:cs_interpretation_layer_present('aa9b7e65-5432-49a3-a16f-135e70664671').
narrative_ontology:cs_reading_relation('aa9b7e65-5432-49a3-a16f-135e70664671', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa9b7e65-5432-49a3-a16f-135e70664671', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('aa9b7e65-5432-49a3-a16f-135e70664671', foundational, custody_legitimacy_independent_of_origin).
narrative_ontology:cs_axiom_status(custody_legitimacy_independent_of_origin, holdable).
narrative_ontology:cs_axiom_grounding('aa9b7e65-5432-49a3-a16f-135e70664671', custody_legitimacy_independent_of_origin, instrumental).
narrative_ontology:cs_axiom('aa9b7e65-5432-49a3-a16f-135e70664671', foundational, preservation_capacity_supersedes_historical_title).
narrative_ontology:cs_axiom_status(preservation_capacity_supersedes_historical_title, holdable).
narrative_ontology:cs_axiom_grounding('aa9b7e65-5432-49a3-a16f-135e70664671', preservation_capacity_supersedes_historical_title, empirically_contingent).
narrative_ontology:cs_reference_frame('aa9b7e65-5432-49a3-a16f-135e70664671', postwar_conservation_crisis_framework).
narrative_ontology:cs_drift_state('aa9b7e65-5432-49a3-a16f-135e70664671', contemporary_restitution_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aa9b7e65-5432-49a3-a16f-135e70664671', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institution_curatorial_staff).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, donor_networks_of_major_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_states_of_origin).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, national_cultural_ministries_of_claimant_states).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universalism_of_cultural_heritage).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, preservation_maximization_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cosmopolitan_access_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold contested artifacts acquired during colonial-era expeditions, punitive campaigns, or unequal treaties. Frame their custodianship as serving universal access and superior conservation capacity. Set exhibition terms, loan conditions, and the interpretive framing under which the artifacts are displayed. Face repatriation claims through litigation, diplomatic pressure, and public campaigns, but retain legal title in most jurisdictions and can absorb the cost of prolonged disputes indefinitely.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, beneficiary).

% Pursue restitution through bilateral negotiation, UNESCO mechanisms, or domestic courts of the holding state, each of which is slow, expensive, and structured by the holding institution's home jurisdiction's property law. Bear diplomatic friction costs, legal fees measured in years of litigation, and a persistent framing in international discourse as making 'particularist' or 'nationalist' claims against a public good. Cannot compel return absent the holding state's cooperation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_states_of_origin, payer,
    moderate, generational, constrained, national).

% Experience the continued absence of ancestral or sacred objects as an ongoing harm to cultural continuity and identity — often distinct from and unaddressed by state-level diplomatic negotiations, which may not represent their specific claims at all. Have essentially no standing before museum boards or in most restitution frameworks that recognize states, not communities, as claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities, payer,
    powerless, generational, trapped, local).

% Staff and fund the sustained diplomatic and legal effort required to pursue claims, redirecting scarce budget from other cultural or development priorities. Sometimes must choose between full restitution demands and negotiated partial returns or long-term loans, because the universal-heritage framework structurally favors the latter as a 'compromise.'
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, national_cultural_ministries_of_claimant_states, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, national_cultural_ministries_of_claimant_states, agenda_setter).

% Benefit from provenance ambiguity and the doctrine's tendency to treat title challenges as exceptions rather than a governing presumption; auction houses and private collectors price contested items using the same legitimating framework museums use, since a favorable universal-access norm suppresses aggressive claim enforcement across the market generally.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Fund acquisitions, endowments, and legal defense of holdings; their philanthropic identity and reputational capital are bound up with the museums' continued possession of flagship contested items, and repatriation of major works could affect both institutional prestige and donor-naming arrangements they have paid for.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, donor_networks_of_major_museums, beneficiary,
    powerful, biographical, mobile, global).

% Mediate disputes, issue non-binding recommendations, and maintain conventions (1970 Convention, UNIDROIT) that claimant states invoke but which lack enforcement teeth against holding institutions in non-signatory or reservation-heavy jurisdictions. Their institutional survival depends partly on being seen as neutral, which limits how forcefully they can side with claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, unesco_and_multilateral_cultural_bodies, observer,
    institutional, generational, analytical, global).

% The sovereign-repatriation and indigenous-stewardship framings are treated within universal-heritage discourse as special pleading or as threats to the shared-heritage public good, rather than as equally coherent normative starting points; their advocates are heard in academic and activist fora but rarely control the museum boards, national courts, or acquisition committees that actually adjudicate individual cases.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, rival_kernel_readings, excluded,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralized, well-resourced institutions with conservation expertise, climate-controlled storage, and global public access infrastructure can, in principle, preserve fragile artifacts and make them available to far more people than dispersed local custodianship would reach — a genuine coordination problem in conservation science and public access exists and the doctrine offers one solution to it.
% TRANSFER_FUNCTION: Moves physical custody, interpretive authority, and the economic and reputational value of cultural property from communities and states of origin to holding institutions in wealthy states, while moving the diplomatic, legal, and identity-related costs of contesting that custody back onto claimant states and descendant communities.
% ABSENT_VOICES: Descendant and indigenous communities are almost never direct parties to restitution negotiations, which are conducted state-to-state or institution-to-state; their objection — that even a returned-to-state outcome may not restore the object to the community with the actual living relationship to it — is structurally unheard within this reading's own machinery.
% DISAPPEARANCE_RATIONALE: If the universal-heritage doctrine's legitimating authority collapsed overnight, the legal and rhetorical default in restitution disputes would shift toward presumptive return absent an origin-state waiver; museums would face materially different burdens of proof, insurance and loan practices would restructure around presumption of illegitimate title for colonial-era acquisitions, and a substantial fraction of encyclopedic museum collections would become subject to negotiated return rather than negotiated retention.
% FOUNDING_PROBLEM: In the mid-20th century, the international community faced genuine problems of wartime looting, illicit trafficking, and the risk that culturally significant objects would be destroyed, dispersed, or degraded absent institutions with resources and expertise to conserve them; the universal-heritage framing emerged partly from real conservation crises (postwar Europe, decolonization-era instability) and partly as a legitimating narrative for collections already assembled through colonial-era extraction.
% FOUNDING_PROBLEM_CORROBORATION: Holding institutions and some conservation scientists attest the founding problem (fragility, risk of loss, need for expert stewardship) remains live for specific object classes. Independent historians of museology, UNESCO working-group reports, and claimant-state legal scholars — sources outside the beneficiary set — argue the conservation-crisis justification has been substantially decoupled from its origin and now functions primarily to legitimate retention of items facing no current conservation risk in their states of origin, several of which have since built world-class conservation infrastructure.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.71 and rising because the doctrine's practical operation increasingly functions to legitimate retention of items with no current conservation justification, shifting from a genuine 1950s-era conservation-crisis rationale toward a legitimating overlay for colonial-era acquisitions. Suppression (0.62) reflects that alternatives — sovereign or community-based custodianship — are not merely disfavored but actively delegitimized in the doctrine's own discourse (framed as 'nationalist' or 'particularist'). Theater ratio is authored rising sharply (0.30 to 0.58) because an increasing share of the doctrine's conservation and access rhetoric is deployed defensively in response to specific restitution claims rather than reflecting genuine, apolitical conservation practice — the doctrine is increasingly invoked reactively rather than operating as steady-state policy. Accessibility collapse is moderate (0.45): alternative custodial arrangements are conceivable and increasingly practiced elsewhere (many states now have world-class conservation infrastructure), so the doctrine has not achieved the near-total foreclosure a mountain would show. Resistance is high (0.72): claimant states, descendant communities, and a growing scholarly consensus actively contest the doctrine's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the encyclopedic museum's seat, the arrangement looks like principled universalist stewardship performing a genuine coordination function — expert conservation, global public access. From the successor-state and descendant-community seats, the identical structure operates as enforced retention of expropriated property, legitimated after the fact by a doctrine whose main practical effect is to shift the burden of proof onto claimants. The engine computes this divergence from the structural power/exit data; the claimed_type (tangled_rope) is authored to reflect that both a real coordination function and a real asymmetric extraction coexist in the same structure, which is exactly the tangled-rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedic museums, their donor networks, and art-market intermediaries sit near the full-beneficiary end of directionality: they retain custody, set interpretive terms, and benefit reputationally and financially from continued possession, with mobile or arbitrage-grade exit from any single dispute. Successor states and cultural ministries sit toward the target end: constrained exit (they cannot compel return, must litigate or negotiate within a framework structured by the holding jurisdiction), bearing the diplomatic and legal cost directly. Descendant communities sit at the extreme target end: trapped exit options, powerless structural position, and harms (loss of cultural continuity) that are not even fully captured by state-level negotiation outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine conservation risk to fragile, at-risk artifacts amid postwar and decolonization-era instability — was real and partially still is for specific object classes. But for a large and growing share of contested holdings, the conservation-crisis justification has become decoupled from present conditions: many origin states now have conservation infrastructure equal to or exceeding that of holding institutions. The doctrine's mandate has not been formally revised to reflect this; instead the mandate's justification has drifted from 'these objects will be lost or damaged without us' to 'universal access is intrinsically superior to origin-based custody regardless of local capacity,' a more sweeping and less falsifiable claim. This is the mandatrophy pattern: a genuine transitional/coordination justification persisting as a permanent legitimating structure after its original problem has been substantially resolved for many of the objects it still governs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conservation_justification_vs_legitimation_overlay,
    'For any given contested holding, is the universal-heritage doctrine currently operating as a genuine, evidence-based conservation justification, or as a legitimation overlay applied after the fact to retain items facing no actual present conservation risk?',
    'Case-by-case technical assessment of conservation infrastructure and risk in the claimant state, compared against the holding institution''s stated justification at the time of any refusal to repatriate; a pattern of refusal persisting after equivalent conservation capacity is demonstrated in the claimant state would indicate legitimation overlay.',
    'If overlay dominates for most contested holdings, the doctrine''s coordination-function claim collapses toward pure legitimated extraction (moving the classification toward snare); if genuine conservation risk dominates, the tangled-rope classification (real coordination plus real extraction) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_justification_vs_legitimation_overlay, empirical, 'Whether the conservation rationale is live or has become a post-hoc legitimating story.').

omega_variable(
    kernel_reading_selection_and_its_stakes,
    'Is the universal-heritage reading a neutral evaluative framework, or is the choice of THIS reading over the sovereign-repatriation or indigenous-stewardship readings itself already a distribution of legitimacy that favors the parties who currently hold custody?',
    'Compare the three sibling readings'' beneficiary/victim structures and ε values (see network.affects_constraints); note whether any framing is treated as the ''default'' in international legal discourse and international courts absent explicit contestation, and by whom that default is set.',
    'If the universal-heritage reading functions as the uncontested doctrinal default in most fora (courts, museum policy, diplomatic practice) rather than one contested position among three, its status as ''shared heritage'' framing does structural work independent of its substantive merits — effectively pre-selecting the outcome before the sovereign or indigenous claims are even heard on their own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_its_stakes, conceptual, 'Whether treating universal-heritage as default framing is itself a legitimacy-distributing move.').

omega_variable(
    state_vs_community_claimant_mismatch,
    'Even where successor states succeed in restitution claims against the universal-heritage doctrine, does state-level repatriation adequately address the harm to descendant communities, or does it substitute one non-community custodian (the successor state) for another (the museum)?',
    'Track post-repatriation custodial arrangements: are returned objects placed under community stewardship, community-accessible national museums, or effectively re-centralized under state authority with limited community access?',
    'If state-level repatriation frequently fails to restore community access, the sovereign-repatriation reading may share more structural extraction with the universal-heritage reading than its own framing acknowledges — relevant to how the three sibling readings should be weighted against each other, not to this story''s own classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_vs_community_claimant_mismatch, empirical, 'Whether successor-state custody genuinely resolves the harm the indigenous-stewardship reading identifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.54).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.1).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the cultural_property_legal_corpus kernel. universal_heritage_reading treats holding institutions as beneficiaries and claimant states/communities as bearing the extraction; sovereign_repatriation_reading inverts this, treating successor states as the legitimate claimant and colonial-era acquisition as the extraction event; indigenous_stewardship_reading further disaggregates the claimant side, treating communities (not successor states) as the legitimate authority and potentially identifying successor-state custody itself as a secondary extraction relative to community stewardship. Each story carries its own stable ε, its own beneficiary/victim sets, and its own classification per the ε-invariance principle; none of the three ε values should be reconciled to the others — the divergence between them is the data the kernel-contest structure exists to preserve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
