% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Corpus
 *   domain: religious/legal/administrative
 *
 * SUMMARY:
 *   This story instantiates the colonial-orientalist reading of the Vedic
 *   corpus kernel: the claim, developed and operationalized by British
 *   colonial administrators (with William Jones, Henry Colebrooke, and later
 *   Warren Hastings's Digest project as key episodes) and their Orientalist
 *   scholarly apparatus, that the Vedic/Dharmashastra texts constitute a
 *   single, coherent, ancient, and essentially unchanging 'Hindu law'
 *   suitable for direct codification into an administrable legal system. This
 *   reading is structurally distinct from the orthodox_varna_reading (which
 *   reads the same corpus as literally prescribing a divinely mandated cosmic
 *   hierarchy, a live theological claim independent of administrative use)
 *   and from the reformist_spiritual_reading (which denies the corpus has
 *   prescriptive social content at all, reading it as metaphorical
 *   cosmology). The colonial-orientalist reading's ε is set by its
 *   administrative function: it is a moderate-extraction scaffold that
 *   converts plural, regionally negotiated, often oral and non-Brahminical
 *   legal practice into a fixed, citable, court-enforceable code, with
 *   predictable winners (colonial administration, the scholarly apparatus
 *   that supplies its legitimacy, and the Brahmin pandit class whose textual
 *   tradition is elevated) and losers (lower-caste litigants, women, and
 *   customary-law communities whose actual practice is overridden).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.62).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Corpus").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious/legal/administrative").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '98c72e5b-bfe4-41da-b345-6bb4f10debaa').
narrative_ontology:cs_kernel_codification('98c72e5b-bfe4-41da-b345-6bb4f10debaa', formalized).
narrative_ontology:cs_authority_grounding('98c72e5b-bfe4-41da-b345-6bb4f10debaa', extraction).
narrative_ontology:cs_interpretation_layer_present('98c72e5b-bfe4-41da-b345-6bb4f10debaa').
narrative_ontology:cs_reading_relation('98c72e5b-bfe4-41da-b345-6bb4f10debaa', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('98c72e5b-bfe4-41da-b345-6bb4f10debaa', vedic_corpus_social_prescription__reformist_spiritual_reading, coexists_with).
narrative_ontology:cs_axiom('98c72e5b-bfe4-41da-b345-6bb4f10debaa', foundational, textual_corpus_admits_singular_administrable_codification).
narrative_ontology:cs_axiom_status(textual_corpus_admits_singular_administrable_codification, holdable).
narrative_ontology:cs_axiom_grounding('98c72e5b-bfe4-41da-b345-6bb4f10debaa', textual_corpus_admits_singular_administrable_codification, conventional).
narrative_ontology:cs_axiom('98c72e5b-bfe4-41da-b345-6bb4f10debaa', secondary, administrative_legibility_justifies_selective_textual_canonization).
narrative_ontology:cs_axiom_status(administrative_legibility_justifies_selective_textual_canonization, holdable).
narrative_ontology:cs_axiom_grounding('98c72e5b-bfe4-41da-b345-6bb4f10debaa', administrative_legibility_justifies_selective_textual_canonization, instrumental).
narrative_ontology:cs_reference_frame('98c72e5b-bfe4-41da-b345-6bb4f10debaa', precolonial_plural_customary_practice).
narrative_ontology:cs_drift_state('98c72e5b-bfe4-41da-b345-6bb4f10debaa', post_independence_statutory_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('98c72e5b-bfe4-41da-b345-6bb4f10debaa', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandits_advising_courts).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_litigants).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, regional_customary_law_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandits_advising_courts).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, textual_primacy_over_lived_practice).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, administrative_legibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commissions translation and digest of Dharmashastra texts (e.g. Manusmriti) to produce a single administrable code, because governing through a legible, textually-fixed 'Hindu law' is cheaper than adjudicating each dispute against fluid local custom. Sets which texts count as authoritative, appoints pandits to interpret them, and enforces the resulting code through colonial courts. Can revise or discard the scheme when it no longer serves administrative convenience.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, beneficiary).

% Produce the scholarly apparatus (translations, commentaries, comparative frameworks) that supplies the 'timeless unified system' claim academic and institutional legitimacy. Careers, publications, and appointments depend on the corpus being treatable as a coherent legal system rather than a heterogeneous, contested, regionally variable textual tradition.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, agenda_setter).

% Serve as court-appointed interpreters of the codified texts, gaining official standing and income from being the recognized authority on 'Hindu law.' Benefit from having their own textual tradition privileged over customary and regional practice, but are themselves constrained to the colonial court's chosen texts and cannot introduce interpretations outside the sanctioned corpus.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandits_advising_courts, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, brahmin_pandits_advising_courts, payer).

% Are classified as Hindu subjects for legal purposes and adjudicated under the codified system regardless of whether their actual community practice matches the Sanskrit textual prescriptions selected by colonial courts. Have no route to contest the classification itself; only the application of it within the sanctioned framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, continental).

% Find that the codified, Brahmin-authored textual hierarchy is now backed by state enforcement in disputes over land, marriage, and inheritance where local custom had previously been more favorable or more flexible. Codification converts what was contestable social practice into binding precedent citable against them in court.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_litigants, payer,
    powerless, biographical, trapped, regional).

% Have inheritance, marriage, and widow-remarriage rights fixed according to the most restrictive textual readings selected during codification (often more restrictive than actual regional practice), with colonial courts now enforcing these as 'traditional Hindu law' rather than the contested, evolving norms they previously were.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, women_under_codified_personal_law, payer,
    powerless, generational, trapped, continental).

% Practiced localized, orally-transmitted, often non-Brahminical legal customs that varied by caste, region, and community. These practices are not consulted in the codification process and are subsequently treated by colonial courts as deviations from the 'true' textual law rather than as valid legal traditions in their own right.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, regional_customary_law_communities, excluded,
    powerless, generational, trapped, regional).

% Study the codification process retrospectively, documenting how colonial administrators selected specific Sanskrit texts (often via Brahmin informants with their own interests) and treated them as if they represented a singular, ancient, unchanging legal system, when the pre-colonial reality was plural, regionally negotiated, and non-textual in large part.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, historians_of_south_asian_law, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides colonial administrators with a single, citable, textually-fixed legal code for adjudicating disputes among a vast and religiously heterogeneous colonized population, reducing the administrative cost of case-by-case engagement with regional custom.
% TRANSFER_FUNCTION: Moves interpretive authority away from local, often non-Brahminical customary practice and concentrates it in colonial courts and their appointed Brahmin pandit-interpreters, while moving legal certainty and administrative efficiency to the colonial state; the costs of rigidification and loss of customary flexibility fall on lower-caste litigants, women, and regional communities whose actual practices are overridden by the selected textual reading.
% ABSENT_VOICES: Regional customary law communities, non-Brahmin legal specialists, and women whose lived marriage/inheritance practices diverged from the textual ideal were not consulted in the selection of authoritative texts or the framing of the corpus as unified and timeless; their objections surface only later, in reformist and nationalist critiques of colonial-era case law.
% DISAPPEARANCE_RATIONALE: If the codified 'Hindu law' scaffold vanished, colonial (and post-independence) courts would lose their citable textual basis for personal-law adjudication; litigation would have to re-engage with plural regional customary practices directly, and precedents built on the codified corpus (property, inheritance, marriage rulings) would lose their textual anchor — a substantial rearrangement of a body of case law still operative in South Asian personal law today.
% FOUNDING_PROBLEM: Colonial administrators needed an efficient, legible basis for adjudicating civil disputes among the colonized population without deploying costly case-by-case ethnographic investigation of local custom in every dispute.
% FOUNDING_PROBLEM_CORROBORATION: The administrative-efficiency problem that motivated codification (need for a low-cost adjudication basis under colonial rule) ended with decolonization; the codified corpus persists as inherited case law and doctrinal precedent. Independent historians of South Asian law (e.g. scholarship on the Anglo-Hindu law tradition) attest, from outside both the colonial administration and the Brahmin-pandit interpretive class, that the 'unified timeless system' claim was a colonial-era administrative construction rather than a discovery of a pre-existing legal unity — no beneficiary party makes this admission voluntarily.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.35 to 0.58) as the codified corpus is progressively cited, layered with case law, and hardened into precedent across roughly a century and a half of colonial and immediate post-colonial adjudication (T=0 to T=190 spans early codification efforts through consolidation into statutory Anglo-Hindu law). Theater ratio also rises (0.20 to 0.40) as the 'timeless unified system' framing increasingly functions as legitimating rhetoric for what is, on inspection, a selective 19th-century textual assemblage rather than a genuine ancient legal unity. Suppression tracks the same rise (0.45 to 0.62) as courts increasingly treat deviation from the codified texts as illegitimate rather than as evidence of the corpus's actual plurality. Accessibility collapse is moderate (0.5) rather than mountain-grade, because customary practice persisted in unofficial spaces even as it lost formal legal standing; resistance is moderate-high (0.55) reflecting ongoing contestation from reformist movements, lower-caste litigants, and later postcolonial legal scholarship.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration and orientalist scholars sit near the full-beneficiary end: they set the terms of codification, select which texts count, and collect the administrative and academic value of a legible system, with arbitrage-grade or mobile exit from the arrangement's costs. Brahmin pandits are a partial beneficiary — elevated interpretive status — but are also constrained to work within colonially-sanctioned texts, giving them a dual role. Colonized legal subjects broadly, and especially lower-caste litigants, women under codified personal law, and customary-law communities, sit near the full-target end: trapped exit, no voice in text selection, and legal costs (loss of favorable customary provisions, hardening of restrictive textual readings into binding precedent) imposed on them through the same structure that colonial administration finds convenient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding administrative problem (need for a low-cost, legible adjudicative basis under colonial rule) is dead — colonial administration itself has ended — yet the codified corpus persists as inherited statutory and case law in post-independence personal-law systems. This is exactly the mismatch the R5 genealogy interview is designed to surface: founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals that the scaffold's stated sunset condition (administrative transition period) never actually triggered a wind-down; the structure was inherited wholesale rather than retired, which is the diagnostic signature of scaffold-to-piton or scaffold-to-tangled-rope drift the framework is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unity_as_discovery_vs_construction,
    'Was the ''unified Hindu law'' the colonial administration codified an accurate discovery of pre-existing legal coherence in the Dharmashastra tradition, or a construction imposed by selecting particular texts (chiefly Manusmriti and a narrow set of commentaries) and treating regional/customary variation as deviation from them?',
    'Comparative historical analysis of pre-colonial legal practice across regions of the subcontinent (drawing on inscriptional evidence, temple records, and customary law compilations predating British codification) against the texts selected for the colonial digests, to establish whether the selected corpus actually governed practice uniformly beforehand.',
    'If discovery, the scaffold''s administrative-legibility function has a stronger claim to genuine coordination value; if construction, the coordination story is substantially a cover for imposing a particular (Brahminical, textualist) legal tradition at the expense of others, pushing the classification further toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unity_as_discovery_vs_construction, empirical, 'Whether codified Hindu law reflects genuine pre-colonial unity or colonial-era textual selection.').

omega_variable(
    kernel_reading_multiplicity,
    'Given that the same Vedic/Dharmashastra corpus supports at least three structurally distinct readings (colonial-administrative codification, orthodox theological prescription, reformist spiritual metaphor), is there a fact of the matter about which reading the texts themselves warrant, or is the corpus genuinely underdetermined with respect to social prescription?',
    'This is a genealogical/hermeneutic question that cannot be settled empirically in the same way as the discovery-vs-construction omega above; it depends on contested premises about textual interpretation, the authority of commentarial tradition, and whether ''what a text prescribes'' is separable from the interpretive community that reads it.',
    'If the corpus is genuinely underdetermined, all three sibling readings are legitimately coexisting rather than one being simply correct and the others errors — which supports the framework''s decision to model them as separate constraints rather than resolve them into one. If a fact of the matter exists and favors one reading, the others may deserve downstream reclassification as historically contingent misreadings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Whether the kernel''s multiple readings reflect genuine textual underdetermination or a resolvable interpretive question.').

omega_variable(
    pandit_informant_selection_bias,
    'How much of the colonial codification''s specific content was shaped by the social position and interests of the particular Brahmin pandits colonial administrators consulted, versus being a neutral scholarly reconstruction of the textual tradition?',
    'Archival analysis of the correspondence and consultation records between colonial administrators (e.g. Hastings, Jones, Colebrooke) and their pandit informants, cross-referenced against the range of interpretive positions actually present in pre-colonial Dharmashastra commentarial literature.',
    'Strong evidence of informant selection bias toward Brahminical and textually maximalist positions would sharpen the beneficiary/victim asymmetry already authored (Brahmin pandits as partial beneficiaries) and support a higher effective extraction reading for the lower-caste and customary-law victim groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pandit_informant_selection_bias, empirical, 'Extent to which pandit informant selection shaped the codified corpus''s specific hierarchical content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 190).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedi_tr_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(vedi_tr_t100, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 100, 0.37).
narrative_ontology:measurement(vedi_tr_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 150, 0.39).
narrative_ontology:measurement(vedi_tr_t190, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 190, 0.4).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vedi_be_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(vedi_be_t100, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(vedi_be_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 150, 0.57).
narrative_ontology:measurement(vedi_be_t190, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 190, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vedi_su_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(vedi_su_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(vedi_su_t100, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(vedi_su_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 150, 0.61).
narrative_ontology:measurement(vedi_su_t190, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 190, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.1).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% This constraint, vedic_corpus_social_prescription__orthodox_varna_reading, and vedic_corpus_social_prescription__reformist_spiritual_reading are three readings of one contested kernel (vedic_corpus_social_prescription). Each has its own ε: this colonial-administrative reading is moderate (0.58, scaffold-with-drift toward tangled_rope), reflecting a specific 19th-century administrative-legal function with identifiable colonial-era beneficiaries and colonized-subject victims; the orthodox theological reading and reformist spiritual reading address different, non-administrative claims about the same underlying texts and are expected to carry different ε values and different beneficiary/victim structures. The three are linked here rather than merged, per the ε-invariance principle: measuring 'the Vedic corpus' as administrative law versus as theology versus as spiritual metaphor yields different extraction profiles because they are different constraints, not different observables of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
