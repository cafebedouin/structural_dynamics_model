% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Cultural Artifacts as Sacred/Communal Property of Indigenous Communities — Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the indigenous_stewardship_reading of
 *   the cultural_property_legal_corpus kernel. It treats cultural artifacts
 *   as the sacred/communal property of the indigenous communities that
 *   created and maintain living relationships with them. Legitimate authority
 *   rests solely with those communities — not with colonial successor states
 *   that inherited looted collections, not with museums that hold them, and
 *   not with universal heritage frameworks that naturalize their current
 *   distribution. The standing arrangement under contest is the global system
 *   of state-centric cultural property law and museum governance; this
 *   reading assesses that arrangement's extractiveness at 0.88 because
 *   artifacts are held by parties with no legitimate claim under its lights.
 *   The constraint is classified as a snare: the coordination story
 *   (preservation, access, stewardship) is cover for a system that actively
 *   suppresses indigenous authority and extracts cultural, epistemic, and
 *   economic value.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.91).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Artifacts as Sacred/Communal Property of Indigenous Communities — Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '8c50c902-2f61-40ac-bf6c-d66e2565b07e').
narrative_ontology:cs_kernel_codification('8c50c902-2f61-40ac-bf6c-d66e2565b07e', distributed).
narrative_ontology:cs_authority_grounding('8c50c902-2f61-40ac-bf6c-d66e2565b07e', lineage).
narrative_ontology:cs_reading_relation('8c50c902-2f61-40ac-bf6c-d66e2565b07e', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c50c902-2f61-40ac-bf6c-d66e2565b07e', cultural_property_legal_corpus__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('8c50c902-2f61-40ac-bf6c-d66e2565b07e', foundational, communal_cultural_sovereignty).
narrative_ontology:cs_axiom_status(communal_cultural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('8c50c902-2f61-40ac-bf6c-d66e2565b07e', communal_cultural_sovereignty, deontological).
narrative_ontology:cs_axiom('8c50c902-2f61-40ac-bf6c-d66e2565b07e', foundational, cultural_continuity_as_title).
narrative_ontology:cs_axiom_status(cultural_continuity_as_title, holdable).
narrative_ontology:cs_axiom_grounding('8c50c902-2f61-40ac-bf6c-d66e2565b07e', cultural_continuity_as_title, deontological).
narrative_ontology:cs_reference_frame('8c50c902-2f61-40ac-bf6c-d66e2565b07e', pre_colonial_communal_custodianship).
narrative_ontology:cs_drift_state('8c50c902-2f61-40ac-bf6c-d66e2565b07e', contemporary_international_law, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8c50c902-2f61-40ac-bf6c-d66e2565b07e', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_practitioners).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, holding_institutions_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_practitioners).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_sovereignty).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, communal_property_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, reparative_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities whose cultural artifacts, sacred objects, and ancestral remains are held in museums and state collections worldwide. They experience the current legal regime as ongoing dispossession — artifacts were taken under colonial violence, and the law that legitimizes their retention is the same law that denies their authority. Their exit from this constraint is identity-locked: cultural continuity requires the return of these specific objects; no substitute or compensation suffices. They bear the cost of the constraint's suppression (legal barriers, institutional inertia, epistemic dismissal) while being the only parties with a legitimate claim under this reading.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary,
    powerless, generational, identity_locked, local).

% Elders, knowledge-keepers, language revitalizers, and ceremonial practitioners within indigenous communities who maintain living relationships with artifacts. They benefit from the reading's validation of their authority but pay the cost of navigating colonial legal systems, fundraising for repatriation, and performing the cultural labor that the reading treats as sovereign right. Their exit is constrained — they could disengage from the legal struggle, but doing so abandons the cultural continuity the reading protects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_practitioners, payer).

% Nation-states that inherited colonial-era collections and legal frameworks (e.g., UK, France, Germany, Belgium, Netherlands, USA). They currently hold legal title and de facto control over vast artifact holdings. Under this reading, they are extractors: they retain artifacts taken by force, profit from their display and study, and use state law to block return. They have arbitrage-grade exit — they could change domestic law, enter bilateral agreements, or support international instruments tomorrow — but do so only under political pressure. They also function as agenda_setters by controlling the legal and diplomatic channels through which claims are processed.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, payer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter).

% Major museums (British Museum, Louvre, Met, Smithsonian, Humboldt Forum, etc.) and university collections that physically hold artifacts. They extract value through prestige, tourism revenue, research access, and curatorial authority. Under this reading, their stewardship claim is illegitimate — they are possessors without title. Their exit is mobile: they have returned objects before (e.g., Benin Bronzes, Maori remains) when leadership or policy shifted, but institutional culture resists. They also function as agenda_setters by defining conservation standards, provenance research protocols, and the epistemic framework in which 'universal access' is weighed against return.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, holding_institutions_museums, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, holding_institutions_museums, agenda_setter).

% UNESCO officials, international lawyers, cosmopolitan curators, and scholars who argue artifacts belong to 'humanity' and should remain in encyclopedic museums. Under this reading, their position is not a competing legitimate claim but an ideological cover for extraction — the universal heritage frame naturalizes the colonial distribution of objects. They are excluded from the constraint's legitimate authority structure (which vests solely in indigenous communities) but are trapped in the current regime because their professional legitimacy depends on the very institutions and legal frameworks the reading delegitimizes.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_advocates, excluded,
    organized, civilizational, trapped, universal).

% Scholars of cultural property law, human rights advocates, and transitional justice practitioners who analyze the constraint from outside. They see the structural asymmetry: indigenous communities have the moral claim but no legal power; states and museums have the legal power but no moral claim under this reading. Their analytical seat has no extraction or benefit — they observe the contest and document the gap between law and legitimacy.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the return, protection, and ceremonial use of cultural artifacts by the communities whose cosmologies, histories, and identities they embody. Solves the problem of who has authority to decide an object's fate: the community that maintains living relationship with it, not the institution that possesses it.
% TRANSFER_FUNCTION: Moves legal title, physical custody, and decision-making authority over cultural artifacts from colonial successor states and holding institutions to indigenous communities. The transfer is reparative: it reverses the original extraction (colonial looting, forced sales, mission collections) and restores the authority that was never ceded.
% ABSENT_VOICES: Descendant communities that have been extinguished or assimilated — peoples whose artifacts remain in collections but who no longer exist as organized claimants. Their silence is structural: the constraint's logic requires a living community to hold authority; where continuity was severed, the reading has no mechanism to represent them. Also absent: the artifacts themselves, treated as property rather than as ancestors or beings with their own agency in many indigenous ontologies.
% DISAPPEARANCE_RATIONALE: If this reading's authority were recognized overnight, the global museum system would face cascading restitution claims. Legal title would shift from states/institutions to communities. Conservation paradigms would shift from 'preservation for universal access' to 'care for ceremonial use.' International law would need new instruments for communal (not state) title. The financial, epistemic, and diplomatic architecture of the heritage sector would reorganize around indigenous sovereignty.
% FOUNDING_PROBLEM: Colonial powers systematically stripped indigenous peoples of their cultural patrimony — sacred objects, ancestral remains, ceremonial regalia, knowledge-keeping materials — through conquest, missionization, and ethnographic extraction. The international legal order that emerged (including UNESCO 1970, UNIDROIT 1995, UNDRIP 2007) was built by and for states; it treats cultural property as state-owned or universally shared, never as communally owned by non-state peoples. The founding problem is the absence of a legal framework that recognizes indigenous communal property and cultural continuity as sovereign authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: UNDRIP Articles 11, 12, 31 (indigenous right to maintain, protect, and develop cultural heritage); UN Expert Mechanism on the Rights of Indigenous Peoples (EMRIP) studies on repatriation; the 2023 UN General Assembly resolution on 'Return or restitution of cultural property to countries of origin' which references indigenous peoples; and decades of indigenous testimony at the UN Permanent Forum on Indigenous Issues. No non-indigenous institutional actor corroborates the founding problem as live — states and museums uniformly treat it as resolved by existing conventions.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is near-maximum (0.88) because the constraint's operation transfers the full value of artifacts — cultural, spiritual, epistemic, economic — from communities to institutions/states without consent or compensation. Suppression is higher (0.91) because the regime's persistence depends on legal barriers (statutes of limitation, immunity from suit, state ownership doctrines), epistemic barriers (Western provenance standards, 'universal value' discourse), and institutional inertia. Theater ratio (0.42) reflects the growing gap between museums' public rhetoric (decolonization, collaboration, shared stewardship) and their actual retention of control — performative consultation without transfer of authority. Accessibility collapse (0.65): alternatives exist (repatriation, co-management, digital return) but are structurally blocked by the legal regime. Resistance (0.58): sustained but asymmetrical — communities resist through claims, protests, and ceremonial reclamation, but face overwhelming legal and resource asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the indigenous community seat, the constraint is a snare of maximal extraction and suppression — their ancestors' bones and sacred objects are held hostage by law. From the state/museum seat, the same constraint appears as a rope (coordination of preservation and access) or a mountain (the natural order of museums). The engine computes this divergence from the structural data: identity-locked exit + powerless + victim declaration → high χ; institutional power + mobile exit + beneficiary declaration → low/negative χ. The claimed_type (snare) is authored from the reading's own structural assessment, not from any single seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are full targets of the standing arrangement (d ≈ 1.0): they bear the extraction, have identity-locked exit, and hold the least structural power. Cultural continuity practitioners are similarly targeted but with slightly more agency (constrained exit, moderate power). Colonial successor states are beneficiaries of the standing arrangement (d ≈ 0.15) — they collect the legal title, diplomatic leverage, and nationalist prestige — but are also agenda_setters who administer the regime. Holding institutions are beneficiaries (d ≈ 0.2) collecting prestige, revenue, and epistemic authority, with mobile exit (they can and sometimes do return objects). Universal heritage advocates are excluded from legitimate authority under this reading but trapped in the regime that validates their professional identity. Observers see the full structure without extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate 'preserve cultural heritage for humanity' has atrophied into 'retain colonial collections indefinitely.' The coordination function (preservation) is real but has been captured by the extraction function (retention of looted objects). The reading exposes this mandatrophy: the arrangement no longer serves its declared purpose for the communities whose heritage it is. The founding problem (colonial dispossession without legal remedy) remains live — corroborated by UNDRIP, EMRIP, and indigenous testimony — while states and museums treat it as resolved. This mismatch (founding_problem_status=live, disappearance_verdict=world_rearranges) flags a zombie constraint: one that persists after its justification has been acknowledged as illegitimate by the affected parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinct_communities_representation,
    'How does this reading represent communities that have been extinguished or assimilated — whose artifacts remain in collections but who have no living claimants?',
    'Analyze whether the reading''s logic (authority vests in communities maintaining cultural continuity) produces a gap for extinct peoples, and whether successor indigenous nations, descendant diasporas, or a trusteeship model can fill it without reproducing state authority.',
    'If the reading cannot represent extinct communities, its universal claim to legitimate authority is structurally incomplete — the artifacts of extinct peoples would fall into a legitimacy vacuum, potentially captured by states or museums by default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinct_communities_representation, conceptual, 'Representation gap for extinguished peoples in a community-based authority model').

omega_variable(
    communal_vs_individual_authority,
    'Within indigenous communities, who holds the authority to decide an artifact''s fate — the collective, a designated lineage, a ceremonial society, or a recognized governance body?',
    'Ethnographic and legal analysis of specific communities'' internal authority structures for cultural property; compare with how UNDRIP and national repatriation laws (e.g., NAGPRA) operationalize ''community consent.''',
    'If authority is contested internally, the reading''s beneficiary declaration (''indigenous_communities'') masks intra-community power dynamics. The engine''s directionality derivation assumes a unified beneficiary; internal division would mean some community members are de facto payers or excluded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_vs_individual_authority, empirical, 'Intra-community authority structure for cultural property decisions').

omega_variable(
    reading_relations_kernel_structure,
    'Does the indigenous_stewardship_reading structurally foreclose, coexist with, or merely influence the sovereign_repatriation_reading and universal_heritage_reading within a single legal framework?',
    'Test whether a state could simultaneously recognize indigenous communal title (this reading) AND retain sovereign title for state-to-state repatriation (sovereign_repatriation_reading) — i.e., whether the two readings occupy different legal registers (communal vs. sovereign) that can layer, or whether they make contradictory claims to the same title.',
    'If forecloses: no legal system can adopt both; they are mutually exclusive frameworks. If coexists_with: different parties hold each reading simultaneously (current reality). If influences: this reading''s advancement shifts the legitimacy conditions for the others without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_kernel_structure, conceptual, 'Structural relationship between this reading and its siblings in the cultural_property_legal_corpus kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of indigenous authority structural (legal barriers, state power) or internalized (communities accepting museum authority as legitimate, epistemic capture)?',
    'Post-restitution suppression trajectory: if communities that have regained artifacts still defer to museum conservation paradigms or seek museum validation, internalized suppression persists after structural barriers are removed.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after formal exit (repatriation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in indigenous cultural property claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cult_tr_t2007, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(cult_tr_t2015, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1970, 0.95).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1985, 0.92).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(cult_be_t2007, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2007, 0.87).
narrative_ontology:measurement(cult_be_t2015, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2015, 0.86).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1970, 0.95).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1985, 0.92).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(cult_su_t2007, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2007, 0.88).
narrative_ontology:measurement(cult_su_t2015, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2015, 0.9).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.08).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_deaccessioning_standards).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, unesco_1970_convention_implementation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, undrip_implementation_mechanisms).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'cultural property' label into a distinct constraint with ε=0.88, beneficiary=indigenous_communities, victim=states/museums. The sovereign_repatriation_reading (ε≈0.45) and universal_heritage_reading (ε≈0.35) are separate constraints with different beneficiary/victim structures and different ε referents. All three share the kernel cultural_property_legal_corpus and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.15).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
