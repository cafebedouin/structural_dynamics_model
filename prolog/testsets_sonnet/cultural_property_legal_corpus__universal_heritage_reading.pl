% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates the universal-heritage reading of the contested
 *   cultural-property kernel: the claim that legitimate custodial authority
 *   over cultural artifacts belongs to institutions best positioned to
 *   preserve them and grant universal access, independent of geographic or
 *   political origin. Under this reading, encyclopedic museums and the
 *   scholarly apparatus defending their holdings are the coordinating,
 *   agenda-setting seats; successor states and descendant communities
 *   pursuing repatriation are recast as particularist claimants whose
 *   demands, if honored, would fragment humanity's shared inheritance. The
 *   doctrine emerged and hardened specifically as decolonization-era
 *   repatriation claims mounted, which is itself evidence relevant to (but
 *   not dispositive of) the founding-problem question below. This is one
 *   clean reading among three siblings (sovereign_repatriation_reading,
 *   indigenous_stewardship_reading) — no attempt is made here to average or
 *   hedge across them; each sibling is its own constraint with its own ε and
 *   its own beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - encyclopedic_museums: primary agenda-setter and beneficiary (institutional/arbitrage) — retains custody, sets terms of engagement
 *   - successor_states: primary payer (institutional/constrained) — bears diplomatic and legal costs of pursuing claims under rules it did not write
 *   - descendant_communities: excluded payer (powerless/trapped) — lacks standing as a non-state actor, bears identity and cultural-continuity harm with no direct legal seat
 *   - art_market_intermediaries and holding_institution_donor_networks: secondary beneficiaries whose financial and reputational interests are served by title stability
 *   - universal_heritage_scholars: agenda-setting beneficiary — produces the doctrinal apparatus that legitimates the arrangement, often from within or adjacent to the benefiting institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.61).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine of Cultural Property Custody").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'f2e48b60-a60d-4ff2-97d7-c4f01b845667').
narrative_ontology:cs_kernel_codification('f2e48b60-a60d-4ff2-97d7-c4f01b845667', distributed).
narrative_ontology:cs_authority_grounding('f2e48b60-a60d-4ff2-97d7-c4f01b845667', extraction).
narrative_ontology:cs_interpretation_layer_present('f2e48b60-a60d-4ff2-97d7-c4f01b845667').
narrative_ontology:cs_reading_relation('f2e48b60-a60d-4ff2-97d7-c4f01b845667', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_reading_relation('f2e48b60-a60d-4ff2-97d7-c4f01b845667', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_axiom('f2e48b60-a60d-4ff2-97d7-c4f01b845667', foundational, custody_legitimacy_tracks_preservation_capacity_not_origin).
narrative_ontology:cs_axiom_status(custody_legitimacy_tracks_preservation_capacity_not_origin, holdable).
narrative_ontology:cs_axiom_grounding('f2e48b60-a60d-4ff2-97d7-c4f01b845667', custody_legitimacy_tracks_preservation_capacity_not_origin, instrumental).
narrative_ontology:cs_axiom('f2e48b60-a60d-4ff2-97d7-c4f01b845667', foundational, humanity_is_the_relevant_rights_holding_unit_for_cultural_artifacts).
narrative_ontology:cs_axiom_status(humanity_is_the_relevant_rights_holding_unit_for_cultural_artifacts, holdable).
narrative_ontology:cs_axiom_grounding('f2e48b60-a60d-4ff2-97d7-c4f01b845667', humanity_is_the_relevant_rights_holding_unit_for_cultural_artifacts, conventional).
narrative_ontology:cs_reference_frame('f2e48b60-a60d-4ff2-97d7-c4f01b845667', post_decolonization_custody_status_quo).
narrative_ontology:cs_drift_state('f2e48b60-a60d-4ff2-97d7-c4f01b845667', contemporary_restitution_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2e48b60-a60d-4ff2-97d7-c4f01b845667', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institution_donor_networks).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, origin_nation_cultural_ministries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_heritage_scholars).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_museum_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cosmopolitan_stewardship_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds contested objects acquired during colonial-era expeditions, punitive campaigns, or unequal-treaty purchases. Sets acquisition, loan, and deaccession policy; funds and cites universal-heritage scholarship that frames its holdings as serving humanity rather than any single nation. Retains full physical custody and interpretive control while claimants must litigate, petition, or negotiate on the museum's institutional terms and timeline.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, beneficiary).

% Governments of the territories from which objects were removed, seeking return through diplomatic requests, bilateral negotiation, or litigation in the holding institution's own legal jurisdiction. Bear sustained legal costs, diplomatic staff-time, and repeated procedural setbacks; the universal-heritage framing recasts their claims as parochial nationalism working against the object's optimal preservation and worldwide accessibility.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_states, payer,
    institutional, generational, constrained, national).

% The living communities whose ancestors made, used, or held sacred the contested objects. Often lack standing to bring claims directly under state-centric international law, and the universal-heritage doctrine further displaces them by treating the relevant unit as 'humanity' rather than the specific community of origin — erasing continuity claims that do not map onto a recognized sovereign state.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities, payer,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, descendant_communities, excluded).

% Auction houses, dealers, and appraisers whose business depends on a legal environment in which title acquired decades or centuries ago is treated as settled and non-retroactively contestable. Benefit from the universal-heritage doctrine's practical effect of stabilizing provenance chains against repatriation claims, which preserves market liquidity and valuation for antiquities and colonial-era acquisitions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, art_market_intermediaries, beneficiary,
    powerful, biographical, mobile, global).

% Trustees, major donors, and patron circles whose philanthropic identity and tax-advantaged giving are bound up with the museum's continued possession of prestige collections. Their reputational and financial interests are served by the doctrine that frames deaccessioning or return as a loss to universal culture rather than a correction of an extractive acquisition.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_institution_donor_networks, beneficiary,
    organized, biographical, arbitrage, national).

% Administers the 1970 Convention and related instruments, mediates disputes, and produces normative guidance, but has limited enforcement power over encyclopedic museums located in non-ratifying or selectively-compliant jurisdictions. Documents the gap between doctrine and restitution outcomes without being able to compel transfer.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, unesco_and_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Curators, legal scholars, and museum-affiliated academics who articulate and defend the cosmopolitan stewardship thesis in journals, expert testimony, and policy consultations, often with institutional funding or employment ties to the holding museums whose custody the thesis legitimates.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_heritage_scholars, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, universal_heritage_scholars, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internationally legible framework under which objects can be conserved, catalogued, and exhibited with predictable long-term custodianship, professional conservation standards, and broad public access rather than fragmented across dozens of under-resourced or politically unstable claimant jurisdictions.
% TRANSFER_FUNCTION: Moves interpretive authority, physical custody, tourism and prestige revenue, and market-liquidity benefits from origin communities and successor states to the museums, market intermediaries, and donor networks that currently possess or trade the objects; claimant states and communities absorb the diplomatic, legal, and identity costs of pursuing return under rules the current possessors wrote.
% ABSENT_VOICES: Descendant and indigenous communities whose objects are held rarely appear as direct parties in the legal or diplomatic process — international law recognizes states, not communities, as claimants, so communities without state backing (or whose states deprioritize the claim) have no seat at all. Their absence is structural, not incidental, since the universal-heritage framing's unit of analysis (humanity) is specifically constructed to bypass particularist claimants.
% DISAPPEARANCE_RATIONALE: If the universal-heritage doctrine lost its legal and normative force overnight, encyclopedic museums would face immediate, wide-scale repatriation claims with no doctrinal shield; acquisition-era title would be reopened, insurance and loan agreements would need renegotiation, and the antiquities market would likely see a liquidity contraction as provenance risk repriced across major collections. This is not a natural fact whose disappearance changes nothing — entire institutional and market arrangements are built on it holding.
% FOUNDING_PROBLEM: In the mid-20th century, decolonization produced newly independent states advancing repatriation claims against European and North American museums; the universal-heritage doctrine (articulated prominently in the 1980s-2000s, including the 2002 'Declaration on the Importance and Value of Universal Museums') was built to provide a legal and ethical rationale for museums to retain contested holdings while decolonization pressure mounted.
% FOUNDING_PROBLEM_CORROBORATION: The signatory museums and their affiliated scholars attest the doctrine protects genuine preservation and access goods that would be lost under fragmented national custody. Independent legal scholars outside the signatory institutions (e.g., critical heritage law scholarship, UN Special Rapporteur reports on cultural rights) and successor-state governments attest the doctrine was substantially a defensive instrument crafted by the very institutions whose holdings it legitimates, timed to the moment those holdings first came under sustained legal challenge.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68) and rising because the doctrine's practical effect is to convert what looks like a preservation-access good into a durable shield against restitution, with costs (legal, diplomatic, and identity-harm costs) landing disproportionately on claimant states and communities. Suppression (0.61) reflects the structural barrier facing non-state claimants (no direct standing) plus the practical difficulty of litigating against a well-resourced institutional defendant in its own jurisdiction. Theater ratio is moderate-to-high (0.52) because a substantial and growing share of the doctrine's public articulation — high-profile joint declarations, curatorial statements on 'universal culture' — functions as legitimating performance layered onto a genuine but partial conservation function; the conservation and access functions are real but do not require the specific custody-retention outcome the doctrine defends. Accessibility collapse is moderate (0.44) because alternative custodial arrangements (long-term loan, joint stewardship, shared title) are documented and occasionally implemented, meaning the doctrine has not fully foreclosed alternatives even though it structurally disfavors them. Resistance is high (0.72), tracking decades of sustained, organized challenge from successor states, UN bodies, and critical heritage scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the encyclopedic museum's seat, the arrangement looks like genuine coordination: professional conservation, worldwide public access, and protection against the risks of fragmented or under-resourced national custody. From the successor state's or descendant community's seat, the identical structure looks like an extraction mechanism dressed in cosmopolitan language — the same acquisition history, the same physical custody, the same legal shield, read as principled stewardship from one seat and as institutionalized retention of expropriated property from the other. The engine computes both seats from the same structural data; this story does not adjudicate between them, only documents the divergence as the universal-heritage reading's own claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedic museums and their donor and market networks sit near the full-beneficiary end: they retain physical custody, control interpretive framing, and benefit financially and reputationally from the arrangement's persistence. Successor states and descendant communities sit near the full-target end: they bear the costs (legal, diplomatic, temporal, identity) of contesting an arrangement built and defended by the party holding the object. Descendant communities are structurally worse off than successor states because they typically lack even the state-level standing that international law recognizes, which is why they are marked both payer and excluded rather than payer alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's founding problem (providing a defensible custodial framework as decolonization pressure mounted) is genuinely contested rather than simply dead: some conservation and access functions the doctrine cites are real and would be lost under fully fragmented custody, which is why founding_problem_status is authored as contested rather than dead. But the doctrine's specific timing — crystallizing into an explicit declaration precisely when legal challenges to holding institutions intensified — is itself the signal that at least part of its function is defensive rather than purely coordinating. Classifying this as tangled_rope rather than snare or mountain preserves that ambiguity: a genuine coordination function (centralized conservation expertise, stable public access) coexists with asymmetric extraction (claimant states and communities bear costs the doctrine's own defenders do not), both riding the same custody structure and requiring active enforcement (litigation defense, doctrinal advocacy) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_good_vs_institutional_self_interest,
    'Is the universal-heritage doctrine a genuine articulation of a preservation/access public good, or a constructed legitimation narrative produced substantially by the institutions it benefits, timed to deflect decolonization-era restitution claims?',
    'Compare doctrinal emergence timing against litigation and diplomatic pressure timelines; examine funding and institutional affiliation of doctrine''s primary authors (e.g., signatories of the 2002 Declaration); assess whether comparable preservation/access outcomes are achievable under joint-title or long-term-loan arrangements that do not require retention of contested title.',
    'If substantially self-interested, this reading''s claimed coordination function is largely cover for extraction, strengthening the case for reclassification toward snare; if genuinely a public good under-provided by fragmented custody, the tangled_rope classification (real coordination plus asymmetric cost) is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_good_vs_institutional_self_interest, conceptual, 'Whether universal-heritage doctrine is genuine public-good coordination or institutional self-legitimation.').

omega_variable(
    reading_selection_and_kernel_indeterminacy,
    'Given that the underlying kernel (who holds legitimate authority over cultural artifacts) has no single adjudicating body and three incompatible readings are simultaneously live in different fora (museum boards, UN bodies, national courts, indigenous governance structures), is the universal-heritage reading''s apparent dominance in Western legal and museological practice a function of its structural correctness or of which parties currently hold the objects and write the applicable law?',
    'Track outcomes in jurisdictions and fora where claimant states/communities have comparable legal resources to holding institutions (e.g., recent French and German restitution commissions) versus fora where they do not; a reading whose success rate tracks resource parity rather than doctrinal merit suggests power-driven rather than principle-driven dominance.',
    'If dominance tracks resource asymmetry rather than doctrinal merit, this reading''s practical hegemony is itself evidence of the extraction this story documents, independent of the doctrine''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_indeterminacy, conceptual, 'Whether this reading''s practical dominance reflects structural merit or resource asymmetry between disputing parties.').

omega_variable(
    descendant_community_standing_gap,
    'Is the exclusion of descendant communities (as opposed to successor states) from direct legal standing a neutral feature of the state-centric international legal system generally, or a feature specifically preserved because it forecloses the indigenous_stewardship_reading''s claimants from ever reaching a forum where this reading would have to contend with them directly?',
    'Examine whether reform proposals granting direct standing to non-state descendant communities have been actively opposed by universal-heritage-doctrine proponents, versus simply not prioritized as a general matter of international legal reform.',
    'Active opposition would indicate the standing gap is a load-bearing part of this reading''s persistence rather than incidental; mere non-prioritization would weaken that inference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(descendant_community_standing_gap, empirical, 'Whether descendant-community standing exclusion is incidental or structurally load-bearing for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(cult_tr_t1983, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1983, 0.33).
narrative_ontology:measurement(cult_tr_t1997, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1997, 0.4).
narrative_ontology:measurement(cult_tr_t2002, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2002, 0.47).
narrative_ontology:measurement(cult_tr_t2013, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2013, 0.5).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(cult_be_t1983, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1983, 0.48).
narrative_ontology:measurement(cult_be_t1997, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1997, 0.55).
narrative_ontology:measurement(cult_be_t2002, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2002, 0.61).
narrative_ontology:measurement(cult_be_t2013, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(cult_su_t1983, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1983, 0.46).
narrative_ontology:measurement(cult_su_t1997, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1997, 0.52).
narrative_ontology:measurement(cult_su_t2002, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2002, 0.58).
narrative_ontology:measurement(cult_su_t2013, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.1).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'cultural property legal corpus' kernel per the ε-invariance principle: the universal_heritage_reading, sovereign_repatriation_reading, and indigenous_stewardship_reading each instantiate structurally distinct claims with different beneficiary/victim sets and different ε profiles, and must not be collapsed into a single averaged constraint. This file influences its siblings by structurally foreclosing or pressuring their operating conditions: to the extent universal-heritage doctrine prevails in holding-institution jurisdictions, it raises the practical cost (legal, diplomatic) of successor states and indigenous communities ever reaching a forum where their readings would be adjudicated on the merits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
