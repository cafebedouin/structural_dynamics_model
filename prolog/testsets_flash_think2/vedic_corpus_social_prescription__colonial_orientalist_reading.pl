% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Colonial Orientalist Reading of Vedic/Dharmashastra as Unified 'Hindu Law'
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint describes the colonial-orientalist reading of diverse
 *   Vedic and Dharmashastra texts as constituting a unified, timeless 'Hindu
 *   law' system, which was then codified for administrative governance. This
 *   reading served the colonial project by creating legible legal subjects
 *   and a predictable administrative framework, often at the expense of
 *   existing fluid social practices and indigenous legal systems. It is
 *   instantiated as a Scaffold because its justification was transitional
 *   (administrative legibility for colonial rule), and its 'sunset' was the
 *   end of colonial power, though its structural legacy persists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.55).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.75).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Reading of Vedic/Dharmashastra as Unified 'Hindu Law'").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '17b20291-f471-4b63-86a1-7640b975b486').
narrative_ontology:cs_kernel_codification('17b20291-f471-4b63-86a1-7640b975b486', formalized).
narrative_ontology:cs_authority_grounding('17b20291-f471-4b63-86a1-7640b975b486', extraction).
narrative_ontology:cs_interpretation_layer_present('17b20291-f471-4b63-86a1-7640b975b486').
narrative_ontology:cs_reading_relation('17b20291-f471-4b63-86a1-7640b975b486', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b20291-f471-4b63-86a1-7640b975b486', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_axiom('17b20291-f471-4b63-86a1-7640b975b486', foundational, vedic_texts_contain_unified_law).
narrative_ontology:cs_axiom_status(vedic_texts_contain_unified_law, holdable).
narrative_ontology:cs_axiom_grounding('17b20291-f471-4b63-86a1-7640b975b486', vedic_texts_contain_unified_law, conventional).
narrative_ontology:cs_axiom('17b20291-f471-4b63-86a1-7640b975b486', foundational, law_must_be_codified_for_governance).
narrative_ontology:cs_axiom_status(law_must_be_codified_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('17b20291-f471-4b63-86a1-7640b975b486', law_must_be_codified_for_governance, instrumental).
narrative_ontology:cs_reference_frame('17b20291-f471-4b63-86a1-7640b975b486', administrative_legibility_framework).
narrative_ontology:cs_drift_state('17b20291-f471-4b63-86a1-7640b975b486', post_independence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17b20291-f471-4b63-86a1-7640b975b486', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_systems).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, local_customary_authorities).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_legal_theory).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, administrative_legibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer, which codified diverse Vedic and Dharmashastra texts into a unified 'Hindu law' system. This provided administrative legibility, facilitated governance, taxation, and dispute resolution, and solidified colonial control over legal subjects.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Gained academic authority, funding, and influence by 'discovering,' translating, and interpreting ancient Indian texts to construct a coherent legal system for the colonial state. Their work legitimized the colonial legal project and established new academic disciplines.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, beneficiary,
    organized, biographical, mobile, global).

% Subjected to a rigid, often alien, legal system that replaced fluid customary laws and social practices. This codification frequently led to the ossification of social hierarchies (e.g., caste), loss of local autonomy, and limited recourse against colonial authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, generational, trapped, national).

% The diverse, fluid, and often localized customary legal systems that existed prior to colonial intervention. Their authority was systematically undermined, suppressed, and replaced by the codified 'Hindu law,' leading to their marginalization and eventual decline.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_systems, excluded,
    powerless, generational, trapped, local).

% Traditional leaders, religious figures, and community elders who previously held significant authority in adjudicating disputes and maintaining social order. They lost much of their legal standing and were forced to operate within or subordinate to the colonial legal framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, local_customary_authorities, payer,
    moderate, biographical, constrained, local).

% Critiqued the colonial codification for its misrepresentation of indigenous traditions, its rigidity, and its role in perpetuating social inequalities. They advocated for alternative, more equitable legal and social reforms, often drawing on different interpretations of the same source texts.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_indian_intellectuals, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified, legible, and administratively manageable legal framework for the colonial state to govern diverse populations, collect taxes, and adjudicate disputes across vast territories, replacing a fragmented landscape of customary laws.
% TRANSFER_FUNCTION: Transferred legal authority, social control, and administrative legibility from diverse, fluid indigenous legal systems and local authorities to the centralized colonial state. It also transferred academic prestige and influence to orientalist scholars.
% ABSENT_VOICES: Indigenous legal practitioners, local community leaders, and those whose social status was negatively fixed by the new codes were largely excluded from the codification process. They would have argued for the validity, flexibility, and local specificity of existing customary laws and against the imposition of a rigid, foreign-derived system.
% DISAPPEARANCE_RATIONALE: If the colonial codification of 'Hindu law' vanished overnight, the legal landscape in post-colonial nations would be fundamentally altered. Existing legal structures, which still bear its legacy, would collapse, requiring a complete re-evaluation of legal authority, personal law, and social organization, likely leading to a resurgence of diverse customary practices and significant legal uncertainty.
% FOUNDING_PROBLEM: The colonial administration faced significant challenges in governing a vast, diverse territory with myriad fluid customary laws, making administration, taxation, and consistent dispute resolution complex and inefficient. There was a perceived need for a uniform, predictable legal system.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrative records and reports attest to the problem of administrative legibility and the desire for a unified legal system. Post-colonial historians and legal scholars, from outside the colonial administration, corroborate that the problem of administrative legibility was largely 'solved' by codification, but that the resulting system created new problems and injustices, indicating the original problem, as framed by the colonial power, is now dead or superseded by its consequences.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.55) reflects the imposition of a rigid legal system that extracted administrative control and revenue, while also causing social stratification. Suppression (0.75) was high due to the active enforcement by the colonial state, which systematically undermined and replaced indigenous legal authority. The theater ratio (0.30) is moderate; while there was a genuine administrative need for legibility, the narrative of 'discovering' timeless, unified law had a performative aspect that masked the imposition. Accessibility collapse (0.80) was severe as alternative legal avenues were largely shut down. Resistance (0.50) was present but often overwhelmed by colonial power. The 'has_sunset_clause: true' reflects the transitional nature of the colonial project itself, which eventually ended with independence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the colonial administration and orientalist scholars, this was a necessary and beneficial act of bringing order and 'modernity' to a complex society. From the perspective of colonized legal subjects and indigenous authorities, it was an imposition that disrupted social fabric, ossified hierarchies, and served foreign interests. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration and orientalist scholars are clear beneficiaries, gaining control, legitimacy, and academic authority. Colonized legal subjects, indigenous legal systems, and local customary authorities are victims, bearing the costs of imposed rigidity, loss of autonomy, and suppression of their traditions. Reformist Indian intellectuals act as observers, analyzing and critiquing the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of this constraint was to provide administrative legibility for colonial governance. While this function was achieved, the constraint persisted beyond its initial justification, becoming a tool for maintaining colonial power and extracting resources. The 'sunset' of this scaffold was the end of colonial rule, but the codified structures left a lasting, often problematic, legacy in post-colonial legal systems, indicating a form of mandatrophy where the structure outlived its original, albeit extractive, purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_discovery_vs_colonial_construct,
    'To what extent was the ''Hindu law'' system a genuine discovery of an existing, unified legal tradition, versus a colonial construct imposed for administrative convenience?',
    'Comparative historical-legal analysis of pre-colonial legal practices across diverse regions, examining the degree of uniformity and the mechanisms of legal change prior to colonial intervention.',
    'If primarily a colonial construct, the constraint''s extractiveness and suppression are higher, as it represents a foreign imposition. If a genuine discovery, the constraint''s coordination function is more prominent, albeit still within an extractive colonial framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_discovery_vs_colonial_construct, conceptual, 'Ambiguity regarding the ontological status of ''Hindu law'' as discovered or constructed.').

omega_variable(
    impact_on_social_mobility,
    'Did the codification of ''Hindu law'' genuinely fix and rigidify social hierarchies (e.g., caste), or did it merely provide a new framework for existing, more fluid social stratification?',
    'Sociological and anthropological studies comparing pre-colonial and colonial-era social mobility patterns, focusing on the legal enforceability of caste and other social distinctions.',
    'If codification significantly rigidified hierarchies, the constraint''s extractiveness and suppression are amplified for lower-status groups. If fluidity persisted, the constraint''s impact on social mobility is less direct, though still significant for administrative purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_social_mobility, empirical, 'The actual effect of codified law on social mobility and stratification.').

omega_variable(
    post_colonial_legal_legacy,
    'To what extent do contemporary legal systems in post-colonial nations (e.g., India) still reflect and perpetuate this colonial-orientalist reading of ''Hindu law''?',
    'Analysis of post-independence legal reforms, judicial precedents, and ongoing debates regarding personal law, examining the continuity or rupture with colonial-era codifications.',
    'If the legacy is substantial, the constraint''s effects (extraction, suppression) continue to manifest in contemporary society, indicating a long-term, intergenerational impact. If largely repudiated, the constraint''s historical impact is contained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_legal_legacy, empirical, 'The enduring influence of colonial legal codification on modern legal systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1800, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(vedi_tr_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1830, 0.25).
narrative_ontology:measurement(vedi_tr_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement(vedi_tr_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(vedi_tr_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.45).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(vedi_be_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1830, 0.45).
narrative_ontology:measurement(vedi_be_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1860, 0.5).
narrative_ontology:measurement(vedi_be_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(vedi_be_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(vedi_su_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1830, 0.65).
narrative_ontology:measurement(vedi_su_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement(vedi_su_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(vedi_su_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_classification).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, land_revenue_systems).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, post_colonial_legal_systems).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_corpus_social_prescription' kernel, focusing on the colonial-orientalist interpretation. Sibling readings (orthodox_varna_reading, reformist_spiritual_reading) offer alternative interpretations of the same source texts, leading to different structural constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
