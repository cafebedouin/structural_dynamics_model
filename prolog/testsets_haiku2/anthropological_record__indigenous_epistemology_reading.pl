% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Anthropological Record via Indigenous Epistemology: Oral Tradition Authority Over Material Evidence
 *   domain: epistemology/anthropology/indigenous_rights
 *
 * SUMMARY:
 *   The anthropological record — skeletal remains, artifacts, settlement
 *   patterns — is a contested kernel that three readings inhabit: the
 *   creationist reading (divine origins compatible with scriptural timeline),
 *   the naturalist reading (materialist human evolution knowable via
 *   scientific method), and this reading, the indigenous epistemology reading
 *   (relational continuity with ancestors knowable via sustained oral
 *   tradition, with community authority over interpretation). This constraint
 *   story instantiates ONLY the indigenous epistemology reading. It asserts
 *   that oral tradition, maintained across generations within kinship and
 *   ceremonial structures, constitutes valid knowledge of human continuity
 *   with place and ancestors—valid enough to override or co-govern
 *   institutional interpretive authority. The constraint operates as a
 *   tangled rope: genuine coordination function (integrating descendant
 *   communities as authoritative interpreters) yoked to asymmetric extraction
 *   (institutional anthropology and museums lose unilateral control and must
 *   renegotiate access and authority). Enforcement is active: repatriation
 *   law, museum governance mandates, consultation requirements, and tribal
 *   sovereignty assertions enforce the subordination of credentialed
 *   authority to community knowledge claims.
 *
 * KEY AGENTS:
 *   - Indigenous knowledge keepers: Hold and transmit oral tradition; claim and exercise interpretive authority over the record.
 *   - Tribal sovereignty movements: Mobilize the reading as legal and political leverage for repatriation and institutional reform.
 *   - Academic anthropologists: Practice evidence-based analysis under credentialism; face subordination of their interpretive primacy to community claims.
 *   - Museum collection authorities: Custodians of accumulated remains and artifacts; required to recognize community override via repatriation and co-governance.
 *   - Descendant communities: Recognized as legitimate stewards and interpreters of ancestral remains and relational continuity.
 *   - State regulatory bodies: Enforce NAGPRA and heritage statutes that codify the constraint's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.58).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Anthropological Record via Indigenous Epistemology: Oral Tradition Authority Over Material Evidence").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/indigenous_rights").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '0771e0a1-fb84-4063-91a4-f4a1861fe275').
narrative_ontology:cs_kernel_codification('0771e0a1-fb84-4063-91a4-f4a1861fe275', distributed).
narrative_ontology:cs_authority_grounding('0771e0a1-fb84-4063-91a4-f4a1861fe275', extraction).
narrative_ontology:cs_reading_relation('0771e0a1-fb84-4063-91a4-f4a1861fe275', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0771e0a1-fb84-4063-91a4-f4a1861fe275', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('0771e0a1-fb84-4063-91a4-f4a1861fe275', foundational, oral_tradition_epistemically_valid).
narrative_ontology:cs_axiom_status(oral_tradition_epistemically_valid, holdable).
narrative_ontology:cs_axiom_grounding('0771e0a1-fb84-4063-91a4-f4a1861fe275', oral_tradition_epistemically_valid, conventional).
narrative_ontology:cs_axiom('0771e0a1-fb84-4063-91a4-f4a1861fe275', foundational, community_authority_over_ancestral_interpretation).
narrative_ontology:cs_axiom_status(community_authority_over_ancestral_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('0771e0a1-fb84-4063-91a4-f4a1861fe275', community_authority_over_ancestral_interpretation, deontological).
narrative_ontology:cs_reference_frame('0771e0a1-fb84-4063-91a4-f4a1861fe275', colonial_authority_institutional_curation).
narrative_ontology:cs_drift_state('0771e0a1-fb84-4063-91a4-f4a1861fe275', contemporary_repatriation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0771e0a1-fb84-4063-91a4-f4a1861fe275', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_knowledge_keepers).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, tribal_sovereignty_movements).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_anthropologists).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_collection_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, descendant_communities).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, international_academic_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit ancestral knowledge through sustained oral tradition within kinship and ceremonial structures. Claim authority over interpretation of the anthropological record based on continuous relational knowledge of place and lineage. Have increasingly asserted control over how ancestral remains and cultural materials are held, studied, and narrated. Their exit from this framework is identity-constituting: the claim to interpretive authority is inseparable from their role as knowledge carriers.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_knowledge_keepers, agenda_setter,
    organized, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_knowledge_keepers, beneficiary).

% Mobilize the indigenous epistemology reading as a basis for repatriation law, museum governance reform, and assertion of community control over cultural and skeletal materials. Benefit from the constraint's operation by gaining institutional standing and legal leverage to reclaim and re-interpret the record on their own terms. Face ongoing resistance from credentialed institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, tribal_sovereignty_movements, beneficiary,
    organized, generational, constrained, regional).

% Practice evidence-based analysis of human origins and continuity using skeletal morphology, isotope analysis, genetic sequencing, and comparative ethnography. The indigenous epistemology reading subordinates their credentialed interpretive authority to community knowledge claims, requiring renegotiation of research access, publication rights, and interpretive primacy. Exit is constrained: their careers depend on research access; institutional prestige depends on sustained authority over the record.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_anthropologists, payer,
    institutional, biographical, constrained, global).

% Curate and preserve anthropological collections (skeletal remains, artifacts, documentation) as universally accessible scientific and cultural patrimony. The indigenous epistemology reading requires them to recognize community override of collection authority: repatriation mandates, interpretive co-governance, and deaccessioning. They bear the cost of collection loss and collaborative governance overhead while losing unilateral curatorial control.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_collection_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, museum_collection_authorities, agenda_setter).

% Are recognized as the legitimate interpreters and stewards of ancestral remains and cultural knowledge. Oral tradition continuity with those remains is affirmed as epistemically valid and politically authoritative. The constraint provides institutional and legal standing to reclaim materials held in museums and academic collections. Their identity is constituted through relational continuity; exit is not a live option.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_communities, beneficiary,
    moderate, civilizational, identity_locked, local).

% Offer alternative frameworks (scriptural timeline, designed complexity) for interpreting human origins and continuity. The indigenous epistemology reading does not foreclose them but repositions them as one community knowledge system among others, rather than as a rival to science or indigenous authority. They are structurally excluded from the interpretation authority framework the constraint establishes.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_knowledge_systems, excluded,
    moderate, civilizational, constrained, regional).

% Enforce laws (NAGPRA, Heritage Acts, cultural property statutes) that codify community authority over ancestral remains and require consultation and repatriation. They mediate between institutional collections and indigenous claims, translate the indigenous epistemology reading into enforceable legal obligations.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, state_regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Conduct comparative research across populations and time periods, treating the anthropological record as globally accessible data. The indigenous epistemology reading restricts their access to certain materials and requires acknowledgment of community authority over interpretation, slowing research and introducing barriers to comparative work.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, international_academic_networks, payer,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_knowledge_keepers).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for recognizing sustained oral tradition as valid knowledge production about human continuity with place and ancestors; coordinates between institutional science, state regulatory authority, and indigenous knowledge systems in the governance of ancestral remains and cultural interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority from credentialed academic institutions to indigenous communities; transfers physical control of ancestral remains from museums to descendant groups; transfers the epistemic burden of proof for claims about ancestral continuity from scientific evidence to relational continuity demonstrated through oral tradition.
% ABSENT_VOICES: Deceased ancestors themselves (the constraint is ostensibly about their continuity and authority, yet they cannot speak in contemporary frameworks); non-affiliated indigenous individuals who may not identify with tribal governance structures but have ancestral claims; academic researchers from non-Western traditions who practice rigorous analysis but fall outside both the scientific credentialism and the indigenous knowledge-keeper categories.
% DISAPPEARANCE_RATIONALE: If the indigenous epistemology reading vanished — if community authority over ancestral interpretation were dissolved — skeletal materials in museums would remain under institutional control, repatriation mandates would dissolve, and the interpretive authority of academic anthropology would reassert its primacy. Millions of remains currently in community custody or repatriated would face competing claims for control and interpretation.
% FOUNDING_PROBLEM: Anthropological institutions accumulated skeletal remains and cultural materials from indigenous peoples under colonial authority, displaced them from their communities, and claimed exclusive right to interpret them through scientific frameworks that often denied or contradicted indigenous knowledge of ancestral continuity and place. Indigenous communities lost both the physical presence of their ancestors and the authority to narrate their own continuity.
% FOUNDING_PROBLEM_CORROBORATION: Museum inventories document millions of remains in institutional care without repatriation (Smithsonian, major universities, European museums). State regulatory bodies (US Interior Department, various heritage authorities) and international bodies (UNESCO, International Council of Museums) acknowledge the colonial history and ongoing dispossession. Independent human rights assessments document ongoing access restrictions and interpretive subordination. Descendant communities and sovereignty movements attest the problem persists in daily interactions with institutions.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 → 0.62) as the reading gains institutional codification and legal enforcement: NAGPRA (1990), subsequent repatriation law, museum policy changes, and increased tribal sovereignty assertion progressively subordinate institutional anthropology's authority. Theater rises moderately (0.22 → 0.41) as institutions adopt collaborative and consultative performances while maintaining substantial control over interpretation and access for materials not yet repatriated. Suppression remains steady (~0.58) because the constraint's persistence does not rely on coercive force against the knowledge keepers (they claim authority and exercise it) but rather on legal/regulatory coercion against institutional resistance — the suppression measure captures the institutional resistance to community override, which has plateaued in the legal-statutory domain even as practical repatriation work continues. Accessibility collapse is high (0.68) because once the indigenous epistemology reading is understood — once oral tradition is recognized as epistemically valid — alternatives that treat the record as exclusively a scientific object become less accessible to institutions. Resistance is high (0.74) because academic institutions, museum networks, and international comparative research communities actively resist the subordination of credentialed authority and experience material losses (collections, access, interpretation primacy). The measurements track institutional compliance and legal codification, not grassroots acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The knowledge-keeper and descendant-community seats experience this constraint as vindication and restoration of authority; their directionality is near full beneficiary. The institutional seats (academia, museums) experience it as asymmetric extraction: they coordinate the governance structure (accepting community input, repatriation, and collaborative interpretation) while bearing costs (collection loss, access restriction, authority subordination). Their directionality is near full target. State regulatory bodies experience it as implementation of law—directionality near symmetric. The engine computes these divergences from the structural beneficiary/victim declarations and exit-option asymmetries. The authored claim is tangled_rope; the metrics describe an arrangement that genuinely coordinates indigenous authority while extractively subordinating institutional authority — the structural definition of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous knowledge keepers have identity-locked exit (the claim to authority is constitutive of their identity; exit means abandoning the framework that grounds their role). This generates d near 0.0 (full beneficiary) despite their organized power: they benefit from the constraint and cannot credibly exit. Descendant communities are moderate-powered with identity-locked exit; they also sit near beneficiary (d ~ 0.15). Academic anthropologists are institutional-power with constrained exit (career dependence on research access and institutional prestige); they experience authority subordination and access restriction, placing them near target (d ~ 0.85). Museums are institutional-power with constrained exit (collections are sunk costs; institutional standing depends on maintaining curatorial authority); they also sit near target (d ~ 0.80). The constraint extracts from institutional seats by forcing renegotiation of governance and access on terms they do not set. No override needed; the structural derivation captures the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial dispossession of remains and interpretive authority) is live. The constraint coordinates a genuine function (integration of descendant communities as authoritative interpreters) that institutional science cannot solve alone. The tangled_rope classification holds because both elements are present: (1) beneficiaries (knowledge keepers, descendants) genuinely benefit from authority restoration; (2) payers (institutions) experience asymmetric cost (authority subordination, collection loss) while also participating in the coordination (collaborative governance). Without active enforcement (repatriation mandates, consultation requirements, tribal authority codification), institutions would revert to unilateral control; the constraint's persistence depends on legal/regulatory forcing. This distinguishes it from pure rope (where participants voluntarily coordinate because all benefit) and from snare (where the coordination story is cover for pure extraction). The coordination is real; the extraction is asymmetric but structurally tied to the coordination function itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_epistemology_boundary,
    'What counts as valid epistemology in oral tradition — what standards of accuracy, consistency, and falsifiability apply — and does the constraint require parity with scientific epistemology or merely recognition as a different-but-valid framework?',
    'Analysis of actual repatriation and governance negotiations: when institutions and communities disagree on interpretation, what epistemological standards are applied? Do communities claim parity with science or complementarity?',
    'If parity is required, the constraint is more extractive of institutional authority and more coordinative of shared truth-seeking. If complementarity is sufficient, the constraint permits incommensurable epistemologies to coexist with community override, which is more extractive of institutional primacy but permits less-contested governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_epistemology_boundary, conceptual, 'What epistemic standards the constraint applies to oral tradition relative to science').

omega_variable(
    community_authority_scope_ambiguity,
    'Does community authority extend only to remains with direct descent-line continuity, or does it extend to all remains from the region/cultural area that communities claim historical/ancestral connection to?',
    'Case law and policy implementation from repatriation disputes: how do courts and agencies resolve claims when multiple communities assert connection to a single set of remains, or when descent cannot be definitively established?',
    'Narrow scope (direct descent only) concentrates authority and reduces extractive cost to institutions. Broad scope (regional/cultural affiliation) amplifies community authority but increases ambiguity about who holds it and expands institutional compliance burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_authority_scope_ambiguity, empirical, 'The scope of community authority over anthropological materials').

omega_variable(
    institutional_resistance_identity,
    'Does institutional resistance to the indigenous epistemology reading stem from genuine belief that credentialed analysis is epistemically superior, or from identity-fusion of institutional prestige with interpretive authority, or both?',
    'Long-term observation of institutional behavior: institutions that accept collaborative governance while maintaining research programs show different patterns than those that maximize resistance. Post-transition research productivity and quality provide signals.',
    'If resistance is identity-fused (institutional identity constituted through interpretive authority), the constraint''s suppression is structural and will remain high even after legal compliance. If resistance is epistemically grounded, institutions may eventually accept the reading as valid, lowering suppression over decades.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_resistance_identity, empirical, 'Whether institutional resistance to the constraint is epistemically or identity-grounded').

omega_variable(
    kernel_reading_contestation_locus,
    'Is the dispute among the three readings about WHAT the record IS (what it shows about human origins and continuity), or about WHO HAS THE AUTHORITY to interpret what the record shows?',
    'Analysis of actual controversies: when creationist, naturalist, and indigenous epistemology readings conflict, are they disagreeing about facts (what evolution happened, when, how) or about authority (who gets to decide what the record means)?',
    'If the dispute is factual, the readings may be genuinely foreclosing (incompatible claims about what happened). If the dispute is about authority, the readings can coexist as parallel interpretive systems with different standards, making them coexisting frameworks. This omega documents the under-determination of reading relation choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_locus, conceptual, 'Whether kernel dispute is about fact or authority, determining reading-relation classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__indigenous_epistemology_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__indigenous_epistemology_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(anth_tr_t24, anthropological_record__indigenous_epistemology_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(anth_tr_t32, anthropological_record__indigenous_epistemology_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(anth_be_t8, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(anth_be_t16, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(anth_be_t24, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(anth_be_t32, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(anth_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(anth_su_t8, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(anth_su_t16, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(anth_su_t24, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(anth_su_t32, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(anth_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, repatriation_law_nagpra).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, museum_governance_reform).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, tribal_sovereignty_assertion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the anthropological_record kernel. The naturalist_reading and creationist_reading are sibling constraints instantiating alternative interpretations of the same material evidence. All three share a referent (the anthropological record) but differ in what knowledge system is authorized to interpret it and what epistemic standards apply. Treat the constraint family as linked via network.affects_constraints; each reading is a separate constraint story with its own beneficiaries, victims, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
