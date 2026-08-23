% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   human_readable: Indigenous Epistemological Authority Over Ancestral Remains
 *   domain: epistemology/anthropology/philosophy_of_science
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous epistemology reading of the
 *   anthropological record kernel. It asserts that knowledge of ancestral
 *   continuity is held in sustained oral tradition, not primarily in material
 *   evidence, and that descendant communities hold sovereign authority over
 *   ancestral remains — an authority that subordinates both credentialed
 *   scientific frameworks and scriptural literalist claims. The constraint
 *   operates through legal mandates (NAGPRA, state laws, institutional
 *   policies) that require consultation and consent. It is a tangled rope:
 *   genuine coordination (protecting vulnerable communities from extraction)
 *   fused with asymmetric extraction (researchers and curators lose autonomy
 *   and control). The constraint did not exist in 1990; it was built through
 *   indigenous activism, legislative struggle, and institutional resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.18).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.05).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Indigenous Epistemological Authority Over Ancestral Remains").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/philosophy_of_science").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '45a9e2bc-7709-4157-b81c-e25e420e6b69').
narrative_ontology:cs_kernel_codification('45a9e2bc-7709-4157-b81c-e25e420e6b69', distributed).
narrative_ontology:cs_authority_grounding('45a9e2bc-7709-4157-b81c-e25e420e6b69', lineage).
narrative_ontology:cs_interpretation_layer_present('45a9e2bc-7709-4157-b81c-e25e420e6b69').
narrative_ontology:cs_reading_relation('45a9e2bc-7709-4157-b81c-e25e420e6b69', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('45a9e2bc-7709-4157-b81c-e25e420e6b69', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('45a9e2bc-7709-4157-b81c-e25e420e6b69', foundational, oral_tradition_epistemic_sovereignty).
narrative_ontology:cs_axiom_status(oral_tradition_epistemic_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('45a9e2bc-7709-4157-b81c-e25e420e6b69', oral_tradition_epistemic_sovereignty, deontological).
narrative_ontology:cs_axiom('45a9e2bc-7709-4157-b81c-e25e420e6b69', foundational, ancestral_remains_as_relational_subjects).
narrative_ontology:cs_axiom_status(ancestral_remains_as_relational_subjects, holdable).
narrative_ontology:cs_axiom_grounding('45a9e2bc-7709-4157-b81c-e25e420e6b69', ancestral_remains_as_relational_subjects, deontological).
narrative_ontology:cs_reference_frame('45a9e2bc-7709-4157-b81c-e25e420e6b69', pre_nagpra_institutional_control).
narrative_ontology:cs_drift_state('45a9e2bc-7709-4157-b81c-e25e420e6b69', contemporary_genomic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45a9e2bc-7709-4157-b81c-e25e420e6b69', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, descendant_community_authorities).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, academic_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, institutional_curators).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_epistemic_parity).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, community_sovereignty_over_ancestral_remains).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, relational_ontology_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold ancestral remains and associated knowledge as living relationships rather than scientific objects. Their authority derives from sustained oral tradition that encodes relational continuity with ancestors and place. They set protocols for access, research, and repatriation. Exit from this role means severing the identity-constituting relationship with ancestors — not a strategic choice but a dissolution of self. Gains include epistemic sovereignty and protection of sacred relationships from instrumental extraction.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, indigenous_communities, beneficiary).

% Formally recognized tribal or community bodies (e.g., THPOs, cultural committees) that operationalize indigenous authority within legal-administrative frameworks. They negotiate with institutions, review research proposals, and authorize repatriations. Their power is real but mediated through state recognition structures. They benefit from institutional leverage but bear the burden of translating relational ontology into bureaucratic procedure. Exit means abandoning the only mechanism that compels institutional compliance.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_community_authorities, beneficiary,
    moderate, biographical, constrained, local).

% Scientists (archaeologists, geneticists, bioanthropologists) whose research programs depend on access to ancestral remains. They lose autonomy over research questions, methods, and publication when community consent becomes a gate. Some adapt by building collaborative partnerships; others experience the constraint as career-threatening exclusion. Their exit option is constrained: they can shift to other materials or fields, but the constraint follows the most scientifically valuable remains. They pay in foregone publications, redirected grants, and epistemic subordination to non-credentialed authority.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, academic_researchers, payer,
    powerful, biographical, constrained, global).

% Museums, universities, and repositories that hold ancestral remains collections. They bear compliance costs (repatriation staff, consultation processes, collection deaccessioning) and lose control over holdings that define institutional prestige and research capacity. They are structurally positioned as custodians forced to become facilitators of return. Their exit is constrained by legal mandates (NAGPRA, etc.) and reputational risk — they cannot simply divest without process. They pay in resources, authority, and the epistemic framework that justified collection.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, institutional_curators, payer,
    institutional, generational, constrained, national).

% Groups whose origin narratives are grounded in scriptural authority (e.g., young-earth creationists, certain traditionalist faith communities). They would contest both scientific and indigenous authority over the deep past, but are excluded from the NAGPRA/consultation framework which recognizes only federally recognized tribes and scientific claimants. Their epistemic framework has no standing in the current constraint — they are not at the table.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, scriptural_literalist_communities, excluded,
    organized, generational, identity_locked, global).

% Analysts examining how epistemic authority is allocated across incommensurable knowledge traditions. They track whether the constraint produces genuine coordination (protection of vulnerable communities from extraction) or becomes a new form of epistemic gatekeeping. They neither collect nor pay; they map the structure.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting culturally affiliated communities from having their ancestors' remains treated as scientific property without consent. Before NAGPRA and related frameworks, institutions unilaterally controlled access, research, and display. The constraint creates a mandatory consultation gate that forces researchers and curators to recognize descendant community authority — a coordination achievement that prevents a race to the bottom in extraction.
% TRANSFER_FUNCTION: Moves decision-rights over ancestral remains — research access, destructive analysis, display, repatriation — from institutional curators and credentialed researchers to descendant community authorities. The transfer runs: institutional_control → community_consent_gate. What flows back is not money but epistemic deference: researchers must frame questions in terms the community recognizes as legitimate; curators must treat remains as relatives, not specimens.
% ABSENT_VOICES: Scriptural-literalist communities (excluded from the legal recognition framework), unaffiliated descendants of ancient populations with no recognized tribal continuity, and researchers working on remains too old for cultural affiliation determination (pre-contact, pre-agricultural). Also absent: the ancestors themselves, whose interests the constraint purports to represent but who cannot speak in any forum.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, institutions would revert to unilateral control over ancestral remains collections. Repatriation would become discretionary; destructive analysis would resume without consent; display of sacred objects would continue. Indigenous communities would lose the only legal lever that compels return. The world of practice would rearrange dramatically — not because the constraint is natural law, but because it is the sole structural barrier against default institutional extraction.
% FOUNDING_PROBLEM: Centuries of colonial collection treated indigenous ancestors as scientific specimens — excavated without consent, measured for racial typologies, displayed as curiosities, stored in boxes. Communities had no legal standing to stop excavation, demand return, or control research. The founding problem was the total epistemic and legal subordination of indigenous relational continuity to extractive scientific curiosity backed by state power.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous communities and NAGPRA legislative history attest the founding problem is live — new excavations, unprovenanced collections, and genomic research on ancient DNA continue the pattern. Academic professional societies (SAA, AAPA) now formally endorse the framework but some members argue the founding problem is substantially solved and the constraint now overreaches. Federal agencies (NPS) document ongoing non-compliance, corroborating that the problem persists. No single outside arbiter settles the dispute.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).
:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but non-zero (0.18): the constraint extracts decision-rights from researchers and curators, but the transfer is bounded and purpose-specific — it does not capture their general labor or income. Suppression is very low (0.05): the constraint operates through legal process, not coercion; alternatives (shifting research focus, collaborative work) remain open. Theater ratio is low (0.08): consultation is substantive, not performative, though some institutions go through motions. Accessibility collapse is moderate (0.35): the constraint closes the 'unilateral institutional control' alternative but opens the 'collaborative partnership' alternative. Resistance is moderate (0.45): some researchers resist epistemically (refusing to cede interpretive authority) and some institutions resist procedurally (delaying repatriation), but overt defiance is rare.
 *
 * PERSPECTIVAL GAP:
 *   From the indigenous community seat, the constraint is rope — it coordinates protection against a genuine threat (unilateral extraction) with minimal overhead. From the researcher seat, it reads as tangled rope — they experience real coordination (ethical clarity, community partnerships) but also real extraction (lost autonomy, redirected careers). From the institutional curator seat, it reads closer to snare — they bear diffuse costs without concentrated benefit, and the constraint's persistence depends on their compliance. The engine computes these divergences from the structural data: same constraint, different directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are agenda-setters and beneficiaries: they set the terms of engagement and gain epistemic sovereignty. Their exit is identity-locked — the relationship with ancestors constitutes their collective identity; leaving the role means ceasing to be that people. Descendant community authorities are beneficiaries with constrained exit: they hold the legal lever but are trapped in bureaucratic translation. Academic researchers are payers with constrained exit: they lose specific research autonomy but can redirect; their global scope and powerful resource base give them arbitration-grade exit from the *field* but not from the *constraint* on the most valuable remains. Institutional curators are payers with constrained exit: they bear compliance costs but cannot divest cleanly. Scriptural-literalist communities are excluded: their epistemic framework has no standing in the constraint's recognition logic. Observers are analytical with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial extraction of ancestors) remains live but has mutated: ancient DNA research, unprovenanced collections, and climate-driven site exposure create new extraction frontiers. The constraint has not atrophied — its mandate expands. However, a mandatrophy risk exists if the framework becomes a general epistemic veto rather than a specific protection: if community authority extends to blocking any research on any remains with any cultural affiliation claim, the coordination function (protection from extraction) could invert into a new extraction (community control over knowledge production itself). The constraint currently navigates this boundary case by case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_parity_vs_epistemic_veto,
    'Does community authority over ancestral remains function as epistemic parity (oral tradition counted alongside material evidence) or as epistemic veto (community consent required for any knowledge production)?',
    'Track research outcomes: are collaborative projects producing knowledge neither party could produce alone (parity), or are community refusals blocking research without alternative pathways (veto)?',
    'If parity, the constraint remains tangled rope with genuine coordination. If veto, it trends toward snare — extraction from knowledge production itself, not just from remains control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_parity_vs_epistemic_veto, empirical, 'Whether the constraint coordinates knowledge production or gates it.').

omega_variable(
    cultural_affiliation_boundary,
    'Where does the constraint''s cultural affiliation requirement draw the line — and does that line track indigenous self-identification or federal recognition?',
    'Analyze NAGPRA review committee decisions and federal acknowledgment cases: when unaffiliated descendants or non-federally-recognized groups claim authority, how is the boundary adjudicated?',
    'If the line tracks federal recognition, the constraint extracts from indigenous groups excluded by the state — a snare dynamic within the beneficiary class. If it tracks self-identification, the coordination function extends more faithfully.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_affiliation_boundary, conceptual, 'Whether the constraint''s beneficiary definition reproduces state exclusion.').

omega_variable(
    ancient_dna_frontier,
    'How does the constraint handle ancient DNA research on remains too old for cultural affiliation determination (pre-10kya, pre-agricultural)?',
    'Track publication patterns and institutional policies: are researchers treating ''culturally unidentifiable'' as a loophole, or are new frameworks emerging for deep-time indigenous authority?',
    'If ''culturally unidentifiable'' becomes an extraction zone, the constraint''s coordination function has a structural hole. If indigenous authority extends to deep time via relational ontology, the constraint''s scope expands but its epistemic claims intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_dna_frontier, empirical, 'Whether the constraint''s coordination function has a temporal boundary that becomes an extraction zone.').

omega_variable(
    kernel_reading_foreclosure,
    'Does this reading''s core premise (oral tradition as sovereign epistemic authority over ancestors) logically foreclose the naturalist reading''s core premise (material evidence as primary, scientific method as adjudicator) within a single framework?',
    'Examine whether any research program or policy framework successfully holds both: community consent as necessary AND scientific method as sufficient for knowledge claims about ancestors. If no such framework exists, foreclosure is structural.',
    'If forecloses, the kernel has a structural fracture — the readings cannot be reconciled in practice, only held by different parties. If coexists_with, the constraint family operates as a negotiated boundary. The reading_relations declaration below records the author''s judgment; this omega names the uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the indigenous epistemology reading forecloses the naturalist reading within a single epistemic framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1990, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(anth_tr_t2000, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(anth_tr_t2010, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2010, 0.06).
narrative_ontology:measurement(anth_tr_t2020, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2020, 0.07).
narrative_ontology:measurement(anth_tr_t2025, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(anth_be_t1990, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(anth_be_t2000, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(anth_be_t2010, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(anth_be_t2020, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2020, 0.16).
narrative_ontology:measurement(anth_be_t2025, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1990, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(anth_su_t2000, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(anth_su_t2010, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2010, 0.06).
narrative_ontology:measurement(anth_su_t2020, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2020, 0.05).
narrative_ontology:measurement(anth_su_t2025, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__indigenous_epistemology_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, nagpra_implementation).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, ancient_dna_ethics_framework).

% DUAL FORMULATION NOTE:
% Part of the anthropological_record constraint family. This reading (indigenous_epistemology_reading) claims epistemic sovereignty for oral tradition and community authority. The naturalist_reading claims scientific method as primary adjudicator. The creationist_reading claims scriptural authority. Their ε values differ substantially: this reading extracts from researchers (ε~0.18); naturalist_reading extracts from communities (ε~0.35 by denying authority); creationist_reading extracts from both (ε~0.45 by denying both material and oral evidence). The kernel label 'anthropological record' conflates three structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, institutional, 0.7).
constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, powerful, 0.65).
constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, organized, 0.15).
constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
