% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Vedic Corpus Codified as Administrative Hindu Law (Colonial Orientalist Reading)
 *   domain: legal/religious/colonial
 *
 * SUMMARY:
 *   Between 1770 and 1950, British colonial administrators and orientalist
 *   scholars undertook the project of codifying Vedic and Dharmashastra texts
 *   into a unified, written 'Hindu Law' system for administrative governance.
 *   This constraint describes one reading of how that texts-as-law
 *   relationship functioned: as a scaffold that crystallized fluid social
 *   practices into fixed legal categories, rendering colonized subjects
 *   legible for census, taxation, and adjudication. The reading frames the
 *   codification as serving colonial extraction and social control while
 *   being justified as neutral recovery of ancient law. THIS IS ONE READING
 *   of a contested kernel (the vedic_corpus_social_prescription kernel).
 *   Sibling readings (other constraints) include an orthodox reading that
 *   accepts the Vedic texts as literally prescribing varna hierarchy, and a
 *   reformist reading that reinterprets Vedic texts as spiritual rather than
 *   social. This story instantiates the colonial-orientalist reading—the one
 *   that treats codification as a tool of governance, not as uncovering
 *   pre-existing law.
 *
 * KEY AGENTS:
 *   - british_colonial_administration: The agenda-setter that controls the codification project and uses it to administer colonial India
 *   - orientalist_scholars_and_translators: Professional interpreters who establish canonical readings of texts and gain institutional authority through the project
 *   - upper_caste_collaborator_elites: Gain recognition and mobility within the colonial hierarchy by serving as informants and mediators
 *   - colonized_legal_subjects: Rendered governable by codification but lose interpretive autonomy and community-based adjudication
 *   - lower_caste_populations: Subject to legal crystallization of caste hierarchy that was previously more fluid in practice
 *   - reformist_intellectuals: Excluded voices arguing against literal prescriptivism and colonial instrumentalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.68).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.72).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Vedic Corpus Codified as Administrative Hindu Law (Colonial Orientalist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "legal/religious/colonial").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'ccc6fa66-1d04-4b4a-91fc-b33460b05091').
narrative_ontology:cs_kernel_codification('ccc6fa66-1d04-4b4a-91fc-b33460b05091', fixed_text).
narrative_ontology:cs_authority_grounding('ccc6fa66-1d04-4b4a-91fc-b33460b05091', extraction).
narrative_ontology:cs_interpretation_layer_present('ccc6fa66-1d04-4b4a-91fc-b33460b05091').
narrative_ontology:cs_reading_relation('ccc6fa66-1d04-4b4a-91fc-b33460b05091', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccc6fa66-1d04-4b4a-91fc-b33460b05091', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('ccc6fa66-1d04-4b4a-91fc-b33460b05091', foundational, vedic_texts_contain_prescriptive_legal_content).
narrative_ontology:cs_axiom_status(vedic_texts_contain_prescriptive_legal_content, holdable).
narrative_ontology:cs_axiom_grounding('ccc6fa66-1d04-4b4a-91fc-b33460b05091', vedic_texts_contain_prescriptive_legal_content, empirically_contingent).
narrative_ontology:cs_axiom('ccc6fa66-1d04-4b4a-91fc-b33460b05091', foundational, unified_written_codification_superior_to_plural_oral_traditions).
narrative_ontology:cs_axiom_status(unified_written_codification_superior_to_plural_oral_traditions, overridden).
narrative_ontology:cs_axiom_grounding('ccc6fa66-1d04-4b4a-91fc-b33460b05091', unified_written_codification_superior_to_plural_oral_traditions, conventional).
narrative_ontology:cs_reference_frame('ccc6fa66-1d04-4b4a-91fc-b33460b05091', vedic_texts_as_unified_prescriptive_law).
narrative_ontology:cs_drift_state('ccc6fa66-1d04-4b4a-91fc-b33460b05091', post_independence_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ccc6fa66-1d04-4b4a-91fc-b33460b05091', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, british_colonial_administration).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars_and_translators).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_collaborator_elites).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, brahminical_orthodox_interpreters).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_collaborator_elites).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_civilization_requires_written_legal_codification).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_texts_contain_unified_prescriptive_legal_system).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, social_stratification_is_natural_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes the codification project as the legitimate framework for Indian jurisprudence. Controls which texts are consulted, which scholars interpret them, and which interpretations become law. Justifies the codification as bringing rational governance and legal certainty to traditional systems. Uses the codified texts to render colonized populations legible for taxation, conscription, and adjudication.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, british_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce the authoritative translations and interpretations of Vedic and Dharmashastra texts. Frame themselves as neutral scholars recovering the 'true' meaning of ancient law. Gain professional prestige, institutional position, and influence over colonial legal policy through their interpretive authority. Their readings become canon—alternative interpretations are delegitimized as non-scholarly or politically motivated.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars_and_translators, agenda_setter,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars_and_translators, beneficiary).

% Participate in the codification process as informants and mediators. Gain institutional recognition under colonial law as custodians of 'Hindu law' and as intermediaries in the colonial administrative hierarchy. Their interpretations of caste status are legally crystallized, securing their position. They also bear some cost in loss of interpretive autonomy—their traditional practices are now measured against fixed written texts.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_collaborator_elites, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, upper_caste_collaborator_elites, payer).

% Subjected to adjudication under codified 'Hindu law' derived from Vedic/Dharmashastra texts. Their social practices and disputes are now measured against fixed, written legal codes they did not author and often cannot access. The codification removes flexibility that previously existed in oral jurisprudence and community-based dispute resolution. They face taxation, conscription, and legal penalties based on caste status as codified in the law.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, national).

% Encounter the Vedic/Dharmashastra texts codified as law precisely because those texts prescribe hierarchical caste duties. The codification makes ritual-status-based restrictions into administrative law. Their mobility, occupation, and legal standing are constrained by the very texts now weaponized as colonial legal instruments. Exit from lower-caste status through reinterpretation or community practice is foreclosed when the interpretation becomes fixed colonial law.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_populations, payer,
    powerless, biographical, identity_locked, national).

% Argue that Vedic texts encode spiritual rather than social prescriptions and that caste hierarchy is a later corruption rather than a Vedic mandate. Are largely excluded from the colonial codification project because their readings threaten the legal framework's legitimacy. Their alternative interpretations cannot compete with the orientalist canon's institutional backing.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indian_reformist_intellectuals, excluded,
    moderate, biographical, constrained, national).

% Some orthodox interpreters support the codification because it legalizes Vedic varna hierarchy. Others resist because codification displaces their interpretive authority—previously they determined meaning through oral commentary and ritual practice; now a fixed written text becomes the standard. Their position is mixed: the codification vindicates their cosmology but threatens their institutional role.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, brahminical_orthodox_interpreters, beneficiary,
    powerful, biographical, mobile, national).

% Courts and legal institutions that apply the codified texts in adjudication. Treat the codified law as authoritative because it is written and systematized. Their interpretive role is narrowed—they apply law rather than interpret it—which increases compliance with colonial governance objectives.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_legal_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, british_colonial_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to solve the problem of adjudicating disputes in Indian territories by creating a single, written legal framework that colonial courts and administrators can uniformly apply. Eliminates the prior state where regional, community, and oral jurisprudence produced variable outcomes. A centralized codification enables standardized governance across disparate populations.
% TRANSFER_FUNCTION: Moves interpretive authority from Indian communities and brahminical scholars to British-backed orientalists and colonial legal institutions. Transfers social legitimacy from previously respected local authorities to the written codified texts. Extracts compliance from lower-caste populations by embedding caste hierarchy into law itself. Transfers prestige and institutional position from Indian scholars to European translators.
% ABSENT_VOICES: Reform-minded Indian intellectuals who argue against literal Vedic prescriptivism are excluded. Local communities whose dispute-resolution practices are overridden by codified law have no seat. Lower-caste populations whose status is legally crystallized are not consulted. Alternative textual traditions outside the Vedic corpus are marginalized. These absent voices would argue that codification misrepresents the texts, that oral jurisprudence better served communities, and that the process serves colonial extraction rather than legitimate governance.
% DISAPPEARANCE_RATIONALE: If the codified Hindu Law framework vanished, colonial administration would lose a key tool for rendering populations legible and governable. Community dispute resolution would resume prior pluralistic forms. Lower-caste populations would regain flexibility in social positioning. The colonial state would have to govern through military force alone rather than through law, or negotiate with actual Indian authority structures. The entire administrative infrastructure of colonial India depends on this codified framework.
% FOUNDING_PROBLEM: Colonial administrators arrived in India with no transparent mechanism for adjudicating disputes among Indian subjects according to Indian principles. Local courts operated opaquely, by caste and region, creating unpredictability for trade and tax collection. The founding problem was how to govern a vast population through law rather than force alone.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrators themselves attested the founding problem in correspondence and policy documents. However, by the late 19th century, Indian reformists and legal historians attest that the original problem (lack of consistent adjudication) was substantially solved by codification, and the framework persists as a mechanism of social control rather than neutral governance. Modern scholarship, particularly postcolonial historians outside the colonial establishment, attests that the codification problem was solved but the constraint persists as rent collection and racialized hierarchy.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1770 to 1850 (0.15 → 0.52) as the codification project moves from idea to institutional reality. It plateaus 1850–1950 (0.52 → 0.68) as the framework becomes entrenched. Theater rises similarly but more gradually (0.08 → 0.41), reflecting the growing weight of legitimating narrative ('recovery of ancient law', 'rational governance') relative to functional reality (legal control and caste crystallization). Suppression requirement tracks extractiveness closely (0.25 → 0.72), indicating that maintaining the codified framework requires active enforcement: resistance from communities practicing alternatives, resistance from Indian interpreters dispossessed of authority, resistance from lower castes subjected to legal hierarchy. The founding problem (lack of transparent adjudication) is substantially solved by 1850, yet extractiveness continues rising—the constraint persists not to solve the founding problem but to sustain colonial governance and caste hierarchy. All measurements on one shared time grid; basis marked 'projected' for estimated pre-codification states (1770–1810) and 'observed' for documented colonial records (1850+).
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administration's seat, this is genuine coordination solving a governance problem through transparent law. From the colonized and lower-caste seats, the same structure is enforced extraction: their practices crystallized into fixed legal hierarchy, enforced through courts. From the reformist intellectual seat (excluded), the structure is suppressive capture of interpretive authority. The engine computes these divergent types; the authored claim is independent of the computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration sits at the beneficiary end (d near 0.0): it collects governance capacity and social control from the codification. Colonized legal subjects sit at the target end (d near 1.0): they are rendered governable and subject to fixed legal categories they did not author. Lower-caste populations sit even further at the target end (d ≈ 0.95) because the texts codified are ones that prescribe their subordination. Orientalist scholars occupy a complex middle position—they benefit from institutional authority (d ≈ 0.3) but are not simple extractors; they are intermediaries whose professional interest aligns with justifying the codification. Upper-caste collaborators occupy the most ambiguous position (d ≈ 0.35): they benefit from legal crystallization of their status but lose interpretive autonomy. Reformist intellectuals, excluded from authority, would occupy the target end if seated (d ≈ 0.85) because they bear suppression (their interpretations are delegitimized) without gaining administrative position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for consistent adjudication) is attested as solved by the 1880s—colonial administrators report legal certainty is achieved, Indian reformists acknowledge the system works administratively. Yet extractiveness continues rising (0.52 → 0.68 between 1850 and 1950) and theater rises sharply (0.28 → 0.41 in the same interval), while suppression requirement intensifies (0.58 → 0.72). This pattern is diagnostic of mandatrophy: the constraint's original mandate (provide transparent legal framework for governance) is complete, but the constraint persists and becomes more extractive because it now serves secondary functions (caste hierarchy, administrative control, preventing alternative legal systems). The rising theater ratio indicates that much activity after 1890 is preserving the codification's legitimacy rather than solving governance problems. The constraint is a dead mandate sustained by inertia and institutional interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Vedic/Dharmashastra corpus inherently prescriptive about social hierarchy, or is that prescription a reading imposed by the codification process itself?',
    'Historical philology comparing pre-codification interpretive traditions (oral commentary, ritual practice, community adjudication) with post-codification written law to detect what changed when texts became fixed. Ethnographic study of how communities interpreted the texts pre-codification.',
    'If the corpus is inherently prescriptive, then codification merely crystallized pre-existing law, and the constraint is a rope. If prescription is imposed by the codification process, then the constraint is a snare that uses religious authority as cover for social engineering. This omega determines whether the constraint reconstructs or constructs hierarchy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the texts themselves dictate hierarchy or the codification process imposes that reading.').

omega_variable(
    temporal_sunset_clause_reality,
    'Did the colonial administration genuinely intend the codification as transitional governance (sunset clause authoring suggests ''until India self-governs''), or was the sunset clause a post-hoc narrative?',
    'Colonial policy documents and correspondence from the period of codification design (1770–1820) to determine whether administrators explicitly framed the project as transitional or whether that framing emerged later as India demanded independence.',
    'If the sunset clause was genuine, the constraint is a true scaffold and the classification holds. If it was post-hoc, then the constraint is a tangled rope (coordination+extraction) or snare (pure extraction) sustained through the sunset narrative. This omega determines whether the constraint is structurally justified as temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_sunset_clause_reality, empirical, 'Whether the codification was genuinely intended as transitional governance.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression measured (0.72 at interval end) structural (enforced by courts, police, bureaucracy) or internalized (colonized subjects and lower-caste populations internalized the legal hierarchy and enforce it through self-regulation)?',
    'Post-independence case studies of how quickly caste-legal structures were formally abolished versus how quickly they were abandoned in practice. Community testimony about whether legal codification changed identity claims or whether identity claims drove legal codes.',
    'If suppression is structural, the constraint remains actively extractive and control-dependent even after colonial administration withdraws. If internalized, the extraction persists in subjectivity even after institutional enforcement is removed. Either case supports the constraint''s persistence beyond its founding justification, but internalization would explain higher-than-expected compliance in post-independence India.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative legal systems is external or internalized.').

omega_variable(
    beneficiary_boundary_ambiguity,
    'Do upper-caste collaborator elites benefit from the constraint sufficiently to be classified as beneficiaries, or do they pay costs (loss of interpretive autonomy) that make them payers with secondary benefits?',
    'Institutional history analysis: tracing whether upper-caste scholars and intermediaries gained more power and security under codified law than they held before, and whether their losses (authority over interpretation) exceeded their gains (administrative recognition).',
    'If upper castes are net beneficiaries, they are co-agents of extraction alongside colonial administration. If they are net payers despite some benefit, the constraint becomes more asymmetric (colonial administration extracting from everyone). This affects the breadth of the payer set and the solidarity conditions for resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_boundary_ambiguity, empirical, 'Whether upper-caste elites are net beneficiaries or net payers under codification.').

omega_variable(
    identity_lock_mechanism_for_lower_castes,
    'Is the identity_locked exit option for lower-caste populations a function of codified legal status, or is it anterior to codification and merely reinforced by it?',
    'Comparative history: pre-codification accounts of caste mobility and reinterpretation of status versus post-codification accounts. Did codification close mobility paths that were open before, or did it merely make previously-closing paths legally explicit?',
    'If codification creates the identity lock (texts become law, law becomes identity, identity cannot be exited), the constraint is more extractive than measuring suppression alone suggests—it doesn''t merely constrain action but constitutes self-concept. If codification only formalizes a pre-existing lock, the constraint is still extractive but the source of extraction is older than the constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_lower_castes, empirical, 'Whether codification creates or formalizes the caste-identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1770, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1770, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1770, 0.08).
narrative_ontology:measurement_basis(vedi_tr_t1770, projected).
narrative_ontology:measurement(vedi_tr_t1810, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1810, 0.15).
narrative_ontology:measurement_basis(vedi_tr_t1810, projected).
narrative_ontology:measurement(vedi_tr_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1850, 0.28).
narrative_ontology:measurement_basis(vedi_tr_t1850, observed).
narrative_ontology:measurement(vedi_tr_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1890, 0.38).
narrative_ontology:measurement_basis(vedi_tr_t1890, observed).
narrative_ontology:measurement(vedi_tr_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1920, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t1920, observed).
narrative_ontology:measurement(vedi_tr_t1950, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1950, 0.41).
narrative_ontology:measurement_basis(vedi_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1770, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1770, 0.15).
narrative_ontology:measurement_basis(vedi_be_t1770, projected).
narrative_ontology:measurement(vedi_be_t1810, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1810, 0.34).
narrative_ontology:measurement_basis(vedi_be_t1810, projected).
narrative_ontology:measurement(vedi_be_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1850, 0.52).
narrative_ontology:measurement_basis(vedi_be_t1850, observed).
narrative_ontology:measurement(vedi_be_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1890, 0.64).
narrative_ontology:measurement_basis(vedi_be_t1890, observed).
narrative_ontology:measurement(vedi_be_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1920, 0.68).
narrative_ontology:measurement_basis(vedi_be_t1920, observed).
narrative_ontology:measurement(vedi_be_t1950, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement_basis(vedi_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1770, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1770, 0.25).
narrative_ontology:measurement_basis(vedi_su_t1770, projected).
narrative_ontology:measurement(vedi_su_t1810, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1810, 0.45).
narrative_ontology:measurement_basis(vedi_su_t1810, projected).
narrative_ontology:measurement(vedi_su_t1850, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1850, 0.58).
narrative_ontology:measurement_basis(vedi_su_t1850, observed).
narrative_ontology:measurement(vedi_su_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement_basis(vedi_su_t1890, observed).
narrative_ontology:measurement(vedi_su_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1920, 0.72).
narrative_ontology:measurement_basis(vedi_su_t1920, observed).
narrative_ontology:measurement(vedi_su_t1950, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement_basis(vedi_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.18).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_caste_categorization).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, brahminical_authority_displacement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vedic_corpus_social_prescription kernel. The colonial-orientalist reading treats codification as a process of converting fluid texts into fixed administrative law, thereby enabling colonial governance and legalizing caste hierarchy. Sibling readings (separate constraints): the orthodox_varna_reading treats Vedic texts as literally mandating cosmic hierarchy (beneficiary is brahminical authority); the reformist_spiritual_reading denies social prescription entirely and treats hierarchy as post-Vedic corruption (beneficiary is modernizing Indian society). The three constraints share a kernel but have different ε values, different beneficiary/victim structures, and different temporal trajectories. All three affect the constraint on colonial_census_caste_categorization, which operationalized the legal categories into demographic facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
