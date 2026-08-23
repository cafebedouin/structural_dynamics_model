% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Indigenous Oral Tradition as Epistemic Authority for Ancestral Continuity
 *   domain: epistemology/anthropology/indigenous_studies
 *
 * SUMMARY:
 *   This constraint story captures the indigenous epistemology reading of the
 *   anthropological record: the claim that relational continuity with
 *   ancestors and place is knowable through sustained oral tradition, and
 *   that this knowledge grounds community authority over ancestral remains
 *   and narratives. The reading operates as a living constraint in
 *   contemporary repatriation law (NAGPRA), museum policy, and archaeological
 *   ethics — it coordinates indigenous communities' relationship to ancestors
 *   while actively subordinating naturalist (scientific) and creationist
 *   (scriptural) interpretive frameworks. The claimed_type 'rope' reflects
 *   the reading's self-presentation as pure coordination of intergenerational
 *   knowledge transmission; the authored metrics reveal asymmetric extraction
 *   (epistemic authority transferred from external institutions to
 *   communities) and active enforcement (legal/administrative protocols
 *   requiring oral tradition consultation), structurally indicating
 *   tangled_rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.68).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Oral Tradition as Epistemic Authority for Ancestral Continuity").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/indigenous_studies").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '2ccb0aa4-000d-4969-b8a1-417fafd6507e').
narrative_ontology:cs_kernel_codification('2ccb0aa4-000d-4969-b8a1-417fafd6507e', distributed).
narrative_ontology:cs_authority_grounding('2ccb0aa4-000d-4969-b8a1-417fafd6507e', lineage).
narrative_ontology:cs_interpretation_layer_present('2ccb0aa4-000d-4969-b8a1-417fafd6507e').
narrative_ontology:cs_reading_relation('2ccb0aa4-000d-4969-b8a1-417fafd6507e', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ccb0aa4-000d-4969-b8a1-417fafd6507e', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('2ccb0aa4-000d-4969-b8a1-417fafd6507e', foundational, oral_tradition_epistemic_authority).
narrative_ontology:cs_axiom_status(oral_tradition_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('2ccb0aa4-000d-4969-b8a1-417fafd6507e', oral_tradition_epistemic_authority, deontological).
narrative_ontology:cs_axiom('2ccb0aa4-000d-4969-b8a1-417fafd6507e', foundational, community_sovereignty_over_ancestral_remains).
narrative_ontology:cs_axiom_status(community_sovereignty_over_ancestral_remains, holdable).
narrative_ontology:cs_axiom_grounding('2ccb0aa4-000d-4969-b8a1-417fafd6507e', community_sovereignty_over_ancestral_remains, deontological).
narrative_ontology:cs_reference_frame('2ccb0aa4-000d-4969-b8a1-417fafd6507e', oral_tradition_epistemic_sovereignty).
narrative_ontology:cs_drift_state('2ccb0aa4-000d-4969-b8a1-417fafd6507e', contemporary_nagpra_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2ccb0aa4-000d-4969-b8a1-417fafd6507e', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_elders_knowledge_keepers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, naturalist_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, creationist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, international_indigenous_networks).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_epistemic_validity).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, community_authority_over_ancestral_remains).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, relational_ontology_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold collective authority over ancestral remains and cultural narratives through tribal governance structures. Set consultation protocols, determine cultural affiliation, and decide disposition of remains. The constraint's enforcement machinery (NAGPRA, institutional policies) operationalizes their authority. Exit from this epistemological framework would mean surrendering relational continuity — identity-locked because the framework constitutes their collective self-understanding and duties to ancestors.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_communities, beneficiary).

% Embody and transmit the oral tradition that grounds the constraint's epistemic claims. Their testimony determines cultural affiliation and proper treatment of remains. They administer the constraint's interpretation layer — deciding what oral tradition says in specific cases. Identity-locked: their authority derives from their position within the tradition; exit would dissolve the role itself.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_elders_knowledge_keepers, agenda_setter,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_elders_knowledge_keepers, beneficiary).

% Seek access to ancestral remains for scientific analysis (DNA, isotopes, morphometrics) to reconstruct population history, migration, health. The constraint requires their research designs to incorporate oral tradition consultation and accept community veto over destructive analysis. They bear costs: restricted samples, modified methodologies, rejected proposals. Exit options constrained: they can shift to non-human subjects or compliant communities, but the constraint follows the remains they study. Some exit via international collaborations where NAGPRA doesn't apply.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, naturalist_researchers, payer,
    powerful, biographical, constrained, global).

% Advocate for scriptural timeline as framework for human origins. The constraint subordinates their framework alongside naturalist science — oral tradition is primary for ancestral continuity. They bear epistemic exclusion: their interpretive claims carry no weight in repatriation decisions. Identity-locked to scriptural framework; exit would require theological reformation. Unlike researchers, they lack institutional power to negotiate — their exclusion is more total.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_advocates, payer,
    moderate, generational, identity_locked, national).

% Administer NAGPRA compliance, manage collections, fund research. They occupy a dual position: constrained by the constraint (must follow consultation/repatriation) but also enforcing it. They gain legal clarity and reduced litigation risk (benefit) but bear administrative costs and loss of collection control (cost). Their analytical seat lets them see the full structure; they are not primary beneficiaries or payers but the institutional substrate through which the constraint operates.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, federal_agencies_museums, observer,
    institutional, generational, analytical, national).

% Transnational indigenous solidarity networks (UN Permanent Forum, UNESCO intangible heritage) that amplify the constraint's legitimacy. They benefit from the precedent this reading sets for epistemic sovereignty globally. Mobile exit: they can shift advocacy to other forums. Their beneficiary status is derivative — they collect normative gains, not material control over specific remains.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, international_indigenous_networks, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains relational continuity with ancestors and place through intergenerational oral transmission, coordinating community identity, land relationships, ceremonial obligations, and collective memory across centuries. Solves the problem of how a people know who they are and where they belong without written archives.
% TRANSFER_FUNCTION: Moves interpretive authority over ancestral remains and associated narratives from external institutions (museums, universities, state agencies) to indigenous communities grounded in oral tradition. Transfers decision-making power: who speaks for the ancestors, what research is permitted, where remains rest.
% ABSENT_VOICES: Naturalist researchers who view material evidence as primary and sufficient for ancestral affiliation; creationist groups who view scriptural timeline as primary; both are structurally subordinated by this reading's framework. Also absent: indigenous individuals who dissent from elders' interpretations or seek scientific collaboration — their voices are mediated through the constraint's internal hierarchy.
% DISAPPEARANCE_RATIONALE: If oral tradition's epistemic authority vanished overnight, NAGPRA's cultural affiliation standard would collapse, repatriation claims would lose their primary evidentiary basis, museums would reclaim decision-making authority over collections, and indigenous communities would lose the legal-administrative lever that currently protects ancestral remains. Community knowledge transmission would fracture without the institutional reinforcement the constraint provides.
% FOUNDING_PROBLEM: Colonial dispossession of ancestral remains and sacred objects; epistemic erasure of indigenous ways of knowing through scientific racism, salvage anthropology, and state policies that treated oral tradition as myth not history. The arrangement was built to protect relational continuity against objectification and to restore authority to the communities from whom knowledge and remains were extracted.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by NAGPRA legislative history (Senate Report 101-473 documenting 'civil and human rights' violations), UN Declaration on Rights of Indigenous Peoples (Articles 11, 12, 31), and testimony of indigenous elders outside direct beneficiary circles — e.g., Maori leaders supporting NAGPRA-style frameworks in Aotearoa, Sami parliament interventions on Nordic museum collections. No non-indigenous institution corroborates the founding problem as 'live'; they frame it as 'historically addressed.'
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects the constraint's operation in transferring decision-making authority over ancestral remains from museums/universities to indigenous communities — a real resource transfer (control of remains, research access, narrative authority). Suppression (0.68) captures the active exclusion of naturalist research protocols and creationist claims from decision-making where oral tradition is determinative. Theater ratio remains low (0.22) because the coordination function — maintaining relational continuity across generations — is genuine and actively practiced, not performative. Accessibility collapse (0.58) is moderate: naturalist methods remain valid for other questions (paleoenvironment, migration timing) but collapse for the specific question 'who are these ancestors to us?' Resistance (0.54) reflects ongoing contestation from scientific associations, some creationist groups, and property-rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the indigenous_communities seat (agenda_setter/beneficiary), the constraint is experienced as rope — a coordination mechanism restoring epistemic sovereignty after centuries of extraction. From the naturalist_researchers seat (payer), it operates as snare/tangled_rope — a constraint that suppresses their epistemic framework and extracts research access without compensation. The creationist_advocates seat (payer/excluded) experiences similar suppression but with different motivational structure. The engine computes this divergence from the structural data: same constraint, different directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities and elders are structural beneficiaries (d near 0.0) — the constraint subsidizes their epistemic authority and material control. Naturalist researchers are structural targets (d near 0.8) — they bear compliance costs, lose research access, and face epistemic subordination. Creationist advocates are similarly targeted but with lower power (moderate vs powerful) and more constrained exit (identity_locked to scriptural framework). Federal agencies sit near symmetric (d ~0.5) — they gain legal clarity from NAGPRA but bear administrative costs. The derivation chain: beneficiary/victim declarations + power levels + exit options (indigenous communities: organized/identity_locked; researchers: powerful/constrained; creationists: moderate/identity_locked).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial dispossession and epistemic erasure — remains live (founding_problem_status: live). The constraint has not atrophied into piton; its coordination function (intergenerational knowledge transmission, ceremonial continuity) is actively maintained, and its enforcement machinery (NAGPRA, consultation protocols) has strengthened over the interval. Mandatrophy is not resolved because the colonial arrangement it resists persists. The constraint's extraction is directional: it extracts from the former extractors, which the reading frames as restoration not predation. This prevents mislabeling coordination as pure extraction — the extraction is real but counter-directional to the historical flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_commensurability,
    'Can oral tradition and scientific method be commensurate on questions of ancestral continuity, or do they constitute incommensurable frameworks that cannot be reconciled within a single epistemic governance regime?',
    'Comparative analysis of co-management agreements (e.g., Kennewick Man/Ancient One resolution, Hawaiian iwi kupuna protocols) where both frameworks operated; track whether hybrid governance stabilizes or collapses into dominance of one framework.',
    'If incommensurable, the constraint''s suppression of naturalist frameworks is structural not contingent; if commensurable, the measured suppression reflects power imbalance not epistemic necessity. Changes classification from tangled_rope (structural incommensurability) toward rope (contingent power distortion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_commensurability, conceptual, 'Whether indigenous and naturalist epistemologies can share authority over the anthropological record').

omega_variable(
    colonial_extraction_quantification,
    'How to quantify the epistemic extraction of colonial science from indigenous communities — the centuries of removed remains, recorded ceremonies, and appropriated knowledge that the current constraint responds to?',
    'Historical accounting of museum/institutional holdings acquired without consent; comparative analysis of research outputs derived from non-consensual access vs. community-led research. Requires indigenous data sovereignty frameworks.',
    'If the standing arrangement''s extraction is quantified as severe, the indigenous epistemology constraint reads as restorative (low ε for this constraint, high ε for the arrangement it resists). If unquantifiable, the ε assessment remains contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_extraction_quantification, empirical, 'Measuring the historical epistemic extraction that the indigenous reading identifies as the referent arrangement').

omega_variable(
    internal_dissent_mechanism,
    'When indigenous community members disagree on oral tradition interpretation (e.g., competing descent claims, divergent ceremonial knowledge), does the constraint contain internal resolution mechanisms or does it suppress intra-community dissent?',
    'Ethnographic study of dispute resolution within communities practicing oral tradition epistemology; analysis of whether elders'' councils, ceremonial processes, or other mechanisms resolve epistemic disputes without external adjudication.',
    'If internal dissent is suppressed, the constraint extracts from community members as well as external frameworks — shifts toward snare/tangled_rope with additional victim class. If resolved internally, coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_dissent_mechanism, empirical, 'Whether the constraint''s coordination function extends to internal epistemic disputes or masks intra-community power dynamics').

omega_variable(
    kernel_reading_structure,
    'This constraint is the indigenous_epistemology_reading of the anthropological_record kernel. Sibling readings: naturalist_reading (materialist origins via scientific method), creationist_reading (divine creation via scriptural timeline). What structural relationship does this reading bear to each sibling?',
    'Map the logical space: does the claim ''material evidence insufficient without oral tradition'' logically foreclose naturalist_reading''s ''material evidence sufficient via scientific method''? Or do they occupy different domains of validity (coexists_with)? Does this reading''s institutional uptake (NAGPRA, UNDRIP) create downstream pressure on naturalist_reading''s operating conditions (influences)?',
    'Forecloses relation would mean no single governance framework could recognize both epistemic authorities — classification implications for pluralistic policy. Coexists_with permits parallel authority structures. Influences captures the realpolitik of NAGPRA reshaping archaeological practice without eliminating scientific epistemology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment-system framing: structural relations between this reading and sibling readings of the anthropological_record kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthro_indig_epist_tr_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(anthro_indig_epist_tr_t0, observed).
narrative_ontology:measurement(anthro_indig_epist_tr_t10, anthropological_record__indigenous_epistemology_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(anthro_indig_epist_tr_t10, observed).
narrative_ontology:measurement(anthro_indig_epist_tr_t20, anthropological_record__indigenous_epistemology_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(anthro_indig_epist_tr_t20, observed).
narrative_ontology:measurement(anthro_indig_epist_tr_t30, anthropological_record__indigenous_epistemology_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(anthro_indig_epist_tr_t30, observed).
narrative_ontology:measurement(anthro_indig_epist_tr_t40, anthropological_record__indigenous_epistemology_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(anthro_indig_epist_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anthro_indig_epist_be_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(anthro_indig_epist_be_t0, observed).
narrative_ontology:measurement(anthro_indig_epist_be_t10, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(anthro_indig_epist_be_t10, observed).
narrative_ontology:measurement(anthro_indig_epist_be_t20, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(anthro_indig_epist_be_t20, observed).
narrative_ontology:measurement(anthro_indig_epist_be_t30, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(anthro_indig_epist_be_t30, observed).
narrative_ontology:measurement(anthro_indig_epist_be_t40, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(anthro_indig_epist_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anthro_indig_epist_su_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(anthro_indig_epist_su_t0, observed).
narrative_ontology:measurement(anthro_indig_epist_su_t10, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(anthro_indig_epist_su_t10, observed).
narrative_ontology:measurement(anthro_indig_epist_su_t20, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(anthro_indig_epist_su_t20, observed).
narrative_ontology:measurement(anthro_indig_epist_su_t30, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(anthro_indig_epist_su_t30, observed).
narrative_ontology:measurement(anthro_indig_epist_su_t40, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(anthro_indig_epist_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, nagpra_implementation).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, museum_repatriation_policy).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, archaeological_ethics_code).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% Part of the anthropological_record constraint family. This reading (indigenous_epistemology) coordinates identity and relational continuity via oral tradition; naturalist_reading coordinates scientific knowledge production via material evidence; creationist_reading coordinates theological coherence via scriptural authority. The three readings share the kernel 'anthropological record' but instantiate different constraints with different ε, beneficiaries, and enforcement structures. ε differs substantially: indigenous reading assesses colonial arrangement as high-extraction; naturalist reading assesses indigenous constraint as suppressing science; creationist reading assesses both as suppressing scriptural authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, institutional, 0.45).
constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, powerful, 0.78).
constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
