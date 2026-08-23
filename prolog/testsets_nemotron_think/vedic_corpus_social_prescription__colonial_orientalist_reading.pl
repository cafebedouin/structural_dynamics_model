% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Anglo-Hindu Law: Vedic Corpus as Unified Legal Code for Governance
 *   domain: religious_studies/social_stratification/hermeneutics/colonial_law
 *
 * SUMMARY:
 *   The colonial orientalist reading constructs Vedic and Dharmashastra texts
 *   as a unified, timeless 'Hindu law' code — a single coherent legal system
 *   derivable from Sanskrit texts — to serve British administrative needs.
 *   Beginning with Warren Hastings' 1772 Plan and culminating in the High
 *   Courts Act 1861 and subsequent legislation, colonial courts, advised by
 *   selected Brahmin pandits, extracted and fixed rules on caste, marriage,
 *   inheritance, adoption, and religious endowments from texts that were
 *   originally diverse, contested, and context-dependent. This scaffold was
 *   presented as 'discovering' authentic Hindu law but functionally created
 *   it. The constraint persists post-1947 in India's personal law system,
 *   scheduled castes lists, and Hindu Marriage Act — the colonial fix became
 *   the post-colonial structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.72).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Anglo-Hindu Law: Vedic Corpus as Unified Legal Code for Governance").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics/colonial_law").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '97f828dc-a887-4740-b549-f0d73e40b57b').
narrative_ontology:cs_kernel_codification('97f828dc-a887-4740-b549-f0d73e40b57b', formalized).
narrative_ontology:cs_authority_grounding('97f828dc-a887-4740-b549-f0d73e40b57b', extraction).
narrative_ontology:cs_interpretation_layer_present('97f828dc-a887-4740-b549-f0d73e40b57b').
narrative_ontology:cs_reading_relation('97f828dc-a887-4740-b549-f0d73e40b57b', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('97f828dc-a887-4740-b549-f0d73e40b57b', vedic_corpus_social_prescription__reformist_spiritual_reading, influences).
narrative_ontology:cs_axiom('97f828dc-a887-4740-b549-f0d73e40b57b', foundational, vedic_texts_constitute_unified_legal_code).
narrative_ontology:cs_axiom_status(vedic_texts_constitute_unified_legal_code, holdable).
narrative_ontology:cs_axiom_grounding('97f828dc-a887-4740-b549-f0d73e40b57b', vedic_texts_constitute_unified_legal_code, conventional).
narrative_ontology:cs_axiom('97f828dc-a887-4740-b549-f0d73e40b57b', foundational, colonial_state_legitimate_codifier).
narrative_ontology:cs_axiom_status(colonial_state_legitimate_codifier, overridden).
narrative_ontology:cs_axiom_grounding('97f828dc-a887-4740-b549-f0d73e40b57b', colonial_state_legitimate_codifier, conventional).
narrative_ontology:cs_reference_frame('97f828dc-a887-4740-b549-f0d73e40b57b', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('97f828dc-a887-4740-b549-f0d73e40b57b', postcolonial_constitutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97f828dc-a887-4740-b549-f0d73e40b57b', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_interpreters).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, non_brahmin_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, women_subject_to_personal_law).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, legal_centralization_enables_governance).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, textual_authority_legitimates_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% British colonial state (East India Company then Crown) establishes courts, codifies 'Hindu law' from Dharmashastra texts for revenue, property, and personal law adjudication. Gains administrative legibility, tax collection efficiency, and legitimacy through 'rule of law' rhetoric. Controls the interpretive process through judicial appointments and legislative enactments.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Brahmin scholars employed as court pandits to interpret Sanskrit texts for colonial judges. Gain official recognition, stipends, and authority to define 'authentic' Hindu law. Their interpretations become binding precedent. Dependent on colonial patronage; exit means loss of institutional position but retain community authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_interpreters, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandit_interpreters, agenda_setter).

% Ordinary Indian subjects (across castes, regions, genders) subjected to rigidified caste categories in courts, census, and revenue systems. Fluid local customs (jati, regional practice) replaced by textual varna categories. No meaningful exit from colonial legal system; resistance met with state coercion. Bear costs of fixed identities for marriage, inheritance, adoption, religious endowments.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, local).

% Communities whose diverse practices (matriliny, non-varna hierarchies, tribal customs) are erased or forced into brahminical textual categories. Lose autonomy in dispute resolution; colonial courts privilege brahminical interpretation. Some adopt Sanskritization to claim higher varna status; others organize anti-caste movements. Exit constrained by colonial legal monopoly.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, non_brahmin_communities, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, non_brahmin_communities, excluded).

% Women's rights in marriage, property, widowhood, adoption codified through selective reading of Dharmashastra texts that restrict female agency. Pre-colonial customary rights (stridhan, regional widow remarriage) suppressed. No exit from personal law system; reform efforts require colonial legislative action which is slow and partial.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, women_subject_to_personal_law, payer,
    powerless, biographical, trapped, local).

% Indian reformers (Rammohan Roy, Phule, Ambedkar, etc.) who contest colonial codification as either distortion of tradition or entrenchment of brahminical patriarchy. Excluded from official law-making until late colonial period. Their voices enter through petitions, press, and later legislative councils. Exit constrained by colonial political structure.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_nationalists, excluded,
    moderate, generational, constrained, national).

% Christian missionaries who critique Hindu law as idolatrous and oppressive, advocating for uniform civil code based on British law. Compete with colonial administration for moral authority. Not formally part of Anglo-Hindu law system but influence public opinion and legislation (e.g., Sati abolition, Age of Consent). Can exit by shifting focus to evangelism or education.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, missionary_authorities, excluded,
    organized, generational, mobile, global).

% Scholars (Derrett, Davis, Rocher, Lariviere, etc.) analyzing Anglo-Hindu law as colonial construction. See the constraint as produced through translation, selection, and judicial precedent. No material stake; analytical exit is trivial. Provide the retrospective classification this story instantiates.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, modern_legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a centralized, textually anchored legal system replacing diverse, fluid, practice-based norms — enabling colonial census enumeration, revenue settlement, and uniform adjudication across heterogeneous Indian territories.
% TRANSFER_FUNCTION: Moved interpretive authority from local councils, family elders, and diverse textual traditions to colonial courts staffed by British judges advised by selected Brahmin pandits; fixed negotiated social identities (jati, regional custom) into rigid varna categories legible to state bureaucracy.
% ABSENT_VOICES: Women, lower-caste and Dalit communities, tribal groups, non-brahmin regional traditions, and Muslim subjects governed by parallel Anglo-Muhammadan law — all excluded from the pandit-judge interpretive loop. Their customary practices were either erased or forced into brahminical categories without consultation.
% DISAPPEARANCE_RATIONALE: If colonial codification vanished overnight, post-colonial India would not revert to pre-colonial fluidity — the codified categories (varna, scheduled castes, Hindu marriage law, coparcenary property) became the scaffolding for the Constitution, reservations, and personal law system. The world rearranged permanently around the colonial fix.
% FOUNDING_PROBLEM: Colonial administration needed a unified, predictable legal framework for governing diverse Indian populations — replacing Mughal fatwa-based courts and myriad local customs with a single 'Hindu law' system administrable by British judges.
% FOUNDING_PROBLEM_CORROBORATION: Colonial records (Warren Hastings' 1772 Plan, Cornwallis Codes, Law Commission reports) explicitly state the administrative motive. Post-colonial scholars (Derrett 1968, Davis 2010, Rocher 2012) corroborate from outside the beneficiary set. No serious historian maintains the founding problem (colonial governance need) is live.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) is moderate: the constraint transfers interpretive power and fixes identities for state legibility, but also provides genuine coordination (dispute resolution, property transfer) — hence scaffold not snare. Suppression (0.72) is high: alternative customs, regional practices, and non-brahminical interpretations were actively excluded from courts and census. Theater (0.41) is significant: the 'ancient law' rhetoric masks colonial construction; pandit opinions were selected, translated, and overruled by judges. Accessibility collapse (0.68) reflects how colonial categories became the only legally recognized reality. Resistance (0.55) is substantial: reform movements, non-brahmin politics, and women's petitions contested the fix throughout.
 *
 * PERSPECTIVAL GAP:
 *   The colonial seat experiences this as coordination (creating order from chaos); the colonized subject seat experiences it as extraction (imposed fixity replacing negotiated fluidity). The pandit seat experiences it as both: gained authority but lost interpretive autonomy to judicial review. The engine computes this divergence from the structural power/exit asymmetry — the same constraint is scaffold from the administration's view (transitional fix for governance) and snare from the subject's view (permanent identity capture).
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration is primary beneficiary (d ~ 0.1): gains legibility, revenue, legitimacy. Brahmin pandits are secondary beneficiaries (d ~ 0.3): gain official authority but within colonial frame. Colonized subjects are primary payers (d ~ 0.9): trapped in rigid categories, no exit. Non-brahmin communities and women are acute payers (d ~ 0.85-0.95): their diverse practices erased. Reformists and missionaries are excluded (d undefined): outside the constraint's operation but affected by its outputs. Modern historians are analytical observers (d = 0.5): symmetric analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial governance need) is dead — British rule ended in 1947. Yet the constraint persists in post-colonial personal law, reservations, and caste certificates. The scaffold's sunset clause was independence itself, but the structure was adopted by the successor state. This is mandatrophy: a transitional arrangement whose mandate expired but whose machinery was captured and repurposed. The classification prevents mislabeling: it is not pure coordination (rope) because extraction is real and suppression active; not pure extraction (snare) because genuine legal coordination occurred; not mountain because it is demonstrably constructed. Scaffold with expired sunset captures the historical trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_construction_vs_indigenous_unity,
    'Did pre-colonial Indian traditions recognize a unified ''Hindu law'' system derivable from Vedic/Dharmashastra texts, or is this unity a colonial epistemological imposition?',
    'Comparative analysis of pre-colonial legal practice (inscriptional evidence, commentarial traditions, regional customary law) versus colonial judicial selections. If pre-colonial practice shows systematic pluralism and the ''unified code'' appears only in colonial records, the unity is constructed.',
    'If constructed, the constraint''s claimed_type (scaffold) is validated — a transitional administrative fix masquerading as ancient law. If indigenous unity existed, the constraint might be a rope (coordination of pre-existing system) with extraction layered on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_construction_vs_indigenous_unity, empirical, 'Whether the unified legal code is an indigenous or colonial epistemological object.').

omega_variable(
    pandit_agency_in_codification,
    'To what extent did Brahmin pandits actively shape Anglo-Hindu law versus passively serving colonial extraction?',
    'Analysis of pandit opinion records (vivadas), their disputes with judges, and their independent textual activities. Did pandits resist, strategically select, or enthusiastically co-produce the fixed categories?',
    'If pandits were active co-producers, the beneficiary structure includes them as genuine agenda-setters (not just colonial instruments). If passive, they are colonial instruments and the beneficiary is solely the administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pandit_agency_in_codification, empirical, 'Degree of Brahmin pandit agency in producing the codified law.').

omega_variable(
    scaffold_sunset_intent,
    'Did colonial administrators intend the codified Hindu law as temporary (until a uniform civil code) or as permanent governance infrastructure?',
    'Examine Law Commission debates (1830s-1860s), legislative records on uniform civil code proposals, and administrative correspondence on the ''permanence'' of personal law.',
    'If temporary intent existed, has_sunset_clause = true is structurally accurate. If permanent intent, the scaffold classification is generous — the constraint was a snare from inception with scaffold rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_intent, conceptual, 'Whether the scaffold''s transitional framing was genuine or rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1772, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1772, 0.25).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.3).
narrative_ontology:measurement(vedi_tr_t1828, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1828, 0.35).
narrative_ontology:measurement(vedi_tr_t1861, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1861, 0.38).
narrative_ontology:measurement(vedi_tr_t1898, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1898, 0.4).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.41).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1772, 0.35).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement(vedi_be_t1828, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1828, 0.48).
narrative_ontology:measurement(vedi_be_t1861, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1861, 0.55).
narrative_ontology:measurement(vedi_be_t1898, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1898, 0.58).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1772, 0.55).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(vedi_su_t1828, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1828, 0.65).
narrative_ontology:measurement(vedi_su_t1861, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1861, 0.7).
narrative_ontology:measurement(vedi_su_t1898, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1898, 0.72).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.12).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_muhammadan_law_parallel_codification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, postcolonial_hindu_personal_law).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, scheduled_castes_legal_category).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, uniform_civil_code_debate).

% DUAL FORMULATION NOTE:
% Part of vedic_corpus_social_prescription kernel family. This reading (colonial_orientalist_reading) treats the corpus as administrative instrument; orthodox_varna_reading treats it as revealed cosmic order; reformist_spiritual_reading treats it as metaphorical spirituality. The colonial reading's codification created the fixed categories that the other readings now contest or defend within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, organized, 0.25).
constraint_indexing:directionality_override(vedic_corpus_social_prescription__colonial_orientalist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
