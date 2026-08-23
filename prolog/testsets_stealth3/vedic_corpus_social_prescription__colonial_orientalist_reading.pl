% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Orientalist Codification of 'Hindu Law' (Vedic Corpus Social Prescription — Colonial Administrative Reading)
 *   domain: religious/hermeneutic/political
 *
 * SUMMARY:
 *   In 1772 Warren Hastings's judicial plan required that inheritance,
 *   marriage, caste, and religious-usage suits be decided by 'the laws of the
 *   Shaster' — launching a century-long project in which Company
 *   scholar-administrators translated Sanskrit legal texts, fixed selected
 *   digests as judicial canon, and administered a codified 'Hindu law'
 *   through colonial courts staffed by salaried pandit law officers. The
 *   arrangement solved a real problem (a tiny administration governing a
 *   vast, legally plural population) while crystallizing fluid social
 *   practice into fixed legal categories legible for census, taxation, and
 *   adjudication. KEY AGENTS (by structural relationship):
 *   colonial_fiscal_administration — agenda-setter and principal collector
 *   (institutional/arbitrage); anglo_hindu_legal_profession and
 *   orientalist_translator_scholars — secondary beneficiaries
 *   (organized/mobile); salaried_pandit_assessors — dual-positioned
 *   (moderate/constrained); hindu_litigant_subjects, fluid_jati_communities,
 *   lower_caste_subjects, hindu_women_under_codified_law — targets
 *   (powerless-to-moderate/trapped); customary_panchayat_arbitrators —
 *   excluded voice; historians_of_colonial_law — analytical observer.
 *   Interval mapping: time_point = year minus 1772, so t=0 is 1772 and t=184
 *   is 1956 (the Hindu Code Bills' completion). This file instantiates ONE
 *   reading of the vedic_corpus_social_prescription kernel; the sibling
 *   readings are separate stories with separate epsilons, per the
 *   epsilon-invariance decomposition documented in
 *   network.dual_formulation_note. The claimed type (scaffold) and the
 *   metrics are authored independently: the metrics describe the
 *   arrangement's actual operation, and the engine computes per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Codification of 'Hindu Law' (Vedic Corpus Social Prescription — Colonial Administrative Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious/hermeneutic/political").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'f68cf178-9df0-42f2-8127-7310d44b9b42').
narrative_ontology:cs_kernel_codification('f68cf178-9df0-42f2-8127-7310d44b9b42', formalized).
narrative_ontology:cs_authority_grounding('f68cf178-9df0-42f2-8127-7310d44b9b42', extraction).
narrative_ontology:cs_interpretation_layer_present('f68cf178-9df0-42f2-8127-7310d44b9b42').
narrative_ontology:cs_reading_relation('f68cf178-9df0-42f2-8127-7310d44b9b42', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('f68cf178-9df0-42f2-8127-7310d44b9b42', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('f68cf178-9df0-42f2-8127-7310d44b9b42', foundational, corpus_constitutes_unified_positive_law).
narrative_ontology:cs_axiom_status(corpus_constitutes_unified_positive_law, holdable).
narrative_ontology:cs_axiom_grounding('f68cf178-9df0-42f2-8127-7310d44b9b42', corpus_constitutes_unified_positive_law, empirically_contingent).
narrative_ontology:cs_axiom('f68cf178-9df0-42f2-8127-7310d44b9b42', secondary, codification_perfects_textual_authority).
narrative_ontology:cs_axiom_status(codification_perfects_textual_authority, holdable).
narrative_ontology:cs_axiom_grounding('f68cf178-9df0-42f2-8127-7310d44b9b42', codification_perfects_textual_authority, instrumental).
narrative_ontology:cs_reference_frame('f68cf178-9df0-42f2-8127-7310d44b9b42', timeless_unified_scriptural_law_code).
narrative_ontology:cs_drift_state('f68cf178-9df0-42f2-8127-7310d44b9b42', post_independence_hindu_code_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f68cf178-9df0-42f2-8127-7310d44b9b42', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_fiscal_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_hindu_legal_profession).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_translator_scholars).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, salaried_pandit_assessors).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_litigant_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, fluid_jati_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_under_codified_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, salaried_pandit_assessors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the presidencies' revenue and judicial machinery with a small European officer corps over a vast population. Commissions translations of Sanskrit legal texts, fixes which digests count as authoritative, establishes the courts, appoints and pays the law officers, and legislates replacements whenever the textual system inconveniences it. Gains legible taxpayers, predictable inheritance rules, and a legitimating account of ruling through native law. Can reshape or wind down the arrangement by regulation at any time.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_fiscal_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Barristers, pleaders, and munsifs who build careers arguing Anglo-Hindu law before the new courts. Fees and salaries flow from a case load the codified system generates; their livelihood depends on the system's continuation. Exit means retraining into other colonial practice areas at moderate cost.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, anglo_hindu_legal_profession, beneficiary,
    organized, biographical, mobile, continental).

% Company scholar-administrators and philologists whose translations and digests become the judicial canon. They gain scholarly fame, learned-society standing, pensioned posts, and a European readership. Once received by the courts, their translations outrank the living interpretive traditions they sampled. Exit: return to Europe or pivot to other orientalist fields.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_translator_scholars, beneficiary,
    organized, biographical, mobile, global).

% Sanskrit scholars hired as law officers to supply textual rulings on demand. They receive steady salaries and state recognition unavailable through traditional patronage, but lose independent authority: they answer to judges, cite approved digests, and are dismissed wholesale in 1864 when the government decides printed translations make them redundant. Exit is retirement to traditional teaching at a loss of income and public standing.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, salaried_pandit_assessors, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__colonial_orientalist_reading, salaried_pandit_assessors, payer).

% Families whose marriage, inheritance, adoption, and caste disputes are now decided by colonial courts applying translated textual rules. They pay court fees, travel, and delay, and bear the risk that a fixed textual rule overrides a settlement their community would have negotiated. They cannot opt out: disputes touching the enumerated topics fall under the new courts' jurisdiction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_litigant_subjects, payer,
    powerless, biographical, trapped, national).

% Caste and village communities whose working arrangements — flexible rank, negotiable custom, local arbitration — are frozen into fixed legal categories backed by precedent. Custom must now be pleaded and proven as an exception to textual rules, with the burden and cost of proof on the community. Internally organized but with little leverage over the legislative process that defines their categories.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, fluid_jati_communities, payer,
    moderate, generational, trapped, regional).

% Subordinated castes whose disabilities, previously mediated by local negotiation and occasional defiance, are written into enforceable law and policed by state courts. They bear the sharpest version of the freeze: what was contingent hardens into entitlement for others and obligation for them. They had no seat in the codification process, and exit is blocked by economic dependence on upper-caste landholders and employers.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, lower_caste_subjects, payer,
    powerless, generational, trapped, national).

% Widows, daughters, and wives whose property, maintenance, and guardianship claims are narrowed when colonial judges apply scriptural rules literally — in documented cases more strictly than the settlements families had worked out locally. Remarriage, inheritance shares, and widow's rights are fixed by precedent they had no hand in making. Exit is blocked by kinship dependence; challenges reach the courts only through male relatives litigating on their behalf.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, hindu_women_under_codified_law, payer,
    powerless, generational, trapped, national).

% Village elders and caste councils who settled most disputes before the courts arrived. Their settlements cost nothing, moved quickly, and adjusted to circumstance; the new system takes their jurisdiction, labels their methods irregular, and leaves them the residue. They would testify that living negotiated practice, not translated text, was what communities actually used. They were not consulted in the codification.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, customary_panchayat_arbitrators, excluded,
    moderate, biographical, trapped, local).

% Later philologists, legal historians, and area-studies scholars who reconstruct what the corpus said, what the translators selected, and what the courts did with the result. They hold the archives and languages, bear no costs under the arrangement, and describe rather than decide.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, historians_of_colonial_law, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_fiscal_administration).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__colonial_orientalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes a vast, legally plural population administrable by a small foreign officer corps: uniform text-grounded rules for inheritance, marriage, adoption, and caste disputes; trained interpreters; legible subjects for revenue rolls and courts. The problem addressed is real — adjudication and taxation cannot proceed without fixed, teachable rules.
% TRANSFER_FUNCTION: Moves adjudicative authority from dispersed community institutions (panchayats, pandits, household settlement) to centralized colonial courts; moves fees, salaries, and scholarly capital to the administration and its professional dependents; moves decision-making power over family and caste life from the governed to the state.
% ABSENT_VOICES: Customary panchayat arbitrators and the communities whose negotiated settlements were displaced; women whose property rules were fixed without any woman consulted; lower-caste communities bearing codified disabilities with no seat in the codification process, which consulted texts and pandit lineages but not the governed.
% DISAPPEARANCE_RATIONALE: While it operated, courts, careers, revenue assessments, and legal categories were organized around the codified corpus; overnight removal would have returned family and caste adjudication to community institutions, stranded the Anglo-Hindu profession, and left the administration without a teachable rule source. After 1947 the rearrangement happened anyway — by deliberate legislative replacement.
% FOUNDING_PROBLEM: How can a small foreign administration govern a vast population without knowledge of its languages, customs, or law, and without provoking revolt? Hastings's 1772 answer: discover and administer the natives' own law from their authoritative texts.
% FOUNDING_PROBLEM_CORROBORATION: No colonial beneficiary attests obsolescence. Corroboration comes from outside the beneficiary set: the Constituent Assembly debates and the Hindu Code Bill sponsorship under Ambedkar's law ministry treat the arrangement as colonial administrative machinery to be replaced; nationalist legal reformers from Rammohan Roy onward attacked specific codified rules; modern legal historiography documents the administrative origin of the 'classical' system. The founding problem — colonial governance — dissolved with the Raj.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends at 0.58 (moderate): the arrangement transferred real jurisdiction, court costs, and fixed legal identities onto the governed, but bounded by genuine service provision (uniform adjudication, some standardization gains) and by eventual supersession. Suppression ends at 0.45: the enforcement machinery (courts, law officers, police-backed judgments, the demotion of custom to a provable exception) was built up through the nineteenth century and then dismantled as codes replaced textual adjudication; the scalar is the end-state snapshot per this story's convention, and the series carries the build-up/decay arc. Theater_ratio ends at 0.46 and rises monotonically: after the 1864 abolition of the pandit assessors, shastric consultation became largely ceremonial, and the label 'ancient law' covered rules that were substantially novel colonial legislation — classic Goodhart drift of performative fidelity to the texts over functional use of them. Accessibility_collapse 0.60: within the official legal domain alternatives collapsed substantially (custom survived only as a costly, specially-proven exception; pandit plurality reduced to approved digests), while social and customary life persisted outside the courts. Resistance 0.40: the nativizing strategy muted resistance by presenting imposition as restoration; what remained was strategic forum-shopping, occasional agitation, and late nationalist critique. All three series run on one shared nine-point grid (t = 0, 23, 46, 69, 92, 115, 138, 161, 184) so every metric is authored at every examined time point. Coordination type enforcement_mechanism (floor default retained, no override): the arrangement is legal/governance infrastructure. Fixing_cost is prohibitive: replacing the textual apparatus earlier would have required a trained secular judiciary, codification capacity, and abandonment of the legitimation narrative — costs the administration judged too high for a century, which is why the declared sunset stretched from the 1830s to the 1950s.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as its own working solution: it commissioned the translations, controls the canon, collects the legibility, and can legislate replacements — from that seat the structure looks like manageable infrastructure. The payer seats experience the same structure as fixed categories and court costs arriving without consent: litigants face rules they cannot renegotiate, jati communities must prove their own customs as exceptions, lower castes inherit hardened disabilities, and women confront precedents set without them. The pandit assessor seat straddles the divide — salary and recognition on one side, subordination and eventual dismissal on the other — which is why it carries dual roles. The excluded panchayat seat experiences pure displacement. Same-power differentiation: the anglo_hindu_legal_profession (organized, mobile) and the fluid_jati_communities (moderate, trapped) are nominal subjects of the same legal order with opposite relationships to it, differentiated entirely by who writes and receives the rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the administration, the profession, and the translator-scholars; victim declarations drive high directionality for the four payer groups. Trapped exit pushes the subject seats toward the full-target end of the scale; the administration's arbitrage-grade exit (it wrote the rules and can rewrite them) sits nearest the beneficiary end. The pandit assessors' dual declaration (beneficiary with payer secondary role) yields an intermediate position. Spatial scope is continental: verification of what the 'law' actually required was hard across the presidencies, which amplifies effective extraction for the scaled seats. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing a colony through textual nativization — died with decolonization, and the arrangement was formally superseded (assessors abolished 1864, secular codes through the late nineteenth century, Hindu Code Bills 1955-56): the mandate outlived its function and was resolved by replacement rather than atrophy, hence mandatrophy_resolved with founding_problem_status dead. The classification guards against both mislabelings: a pure-extraction reading ignores the real coordination achieved (uniform adjudication and legibility that successor administrations, including independent India's, inherited and used); a pure-coordination reading ignores the asymmetric freeze imposed on populations with no seat in the codification. The scaffold claim captures the transitional self-justification and the declared terminal condition — while the theater series documents how the transition rhetoric stretched across a century, and the sunset_sincerity omega marks the open question of whether the sunset was commitment or cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_vcsp,
    'This constraint instantiates the colonial_orientalist_reading of kernel vedic_corpus_social_prescription; how would the sibling readings (orthodox_varna_reading, reformist_spiritual_reading) change the structural picture?',
    'Author and classify the sibling stories separately, then compare: the orthodox reading relocates authority to divine mandate and shifts the victim set to those subordinated by that claim; the reformist reading removes prescriptive social content entirely, collapsing the legal arrangement toward zero epsilon.',
    'Cross-reading comparison isolates which structural features belong to the corpus itself versus to the colonial administrative appropriation; this story''s classification must not be read as a verdict on the corpus or on the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_vcsp, conceptual, 'Committer-frame routing: this is one reading of a three-reading kernel; sibling deltas are structural, not evaluative.').

omega_variable(
    corpus_unification_artifact,
    'Did the Vedic/Dharmashastra corpus ever constitute a unified legal system, or is the ''unity'' a product of colonial selection, translation, and digest-making?',
    'Philological comparison of pre-colonial commentarial diversity (regional schools such as Mitakshara and Dayabhaga lineages, smriti recensions, situational commentary) against the homogenized output of the colonial digests and judicial canon.',
    'If unity is an artifact, the arrangement codifies a constructed object and its extraction rides on fabrication, strengthening snare-side readings of the mature phase; if the unity is substantially genuine, part of the measured cost is the price of recovering real law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corpus_unification_artifact, empirical, 'Whether the codified ''Hindu law'' corresponds to a real unified corpus or a translation artifact.').

omega_variable(
    customary_fluidity_freeze_magnitude,
    'How much legal rigidity did codification add to previously fluid jati and customary practice?',
    'Compare pre-colonial customary variation (district records, panchayat practice, administrator accounts) with post-codification case-law outcomes for the same communities and dispute types.',
    'Determines how much of the measured extraction is newly imposed by the arrangement versus ratification of pre-existing hierarchy — changing victim-set attribution and the effective extraction computed for the lower-caste and women''s seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_fluidity_freeze_magnitude, empirical, 'Magnitude of the freeze the codified categories imposed on negotiable custom.').

omega_variable(
    sunset_sincerity,
    'Was the declared transition — administer textual native law until natives are educated enough for modern codes — a genuine sunset commitment or indefinitely deferrable rhetoric?',
    'Track the gap between declared transition milestones (Macaulay''s 1835 minute, announced code programs, the 1864 abolition of the law officers) and actual retention of the textual apparatus; test whether retention tracked the declared condition or administrative convenience.',
    'A sincere sunset supports the scaffold claim; an insincere sunset reclassifies the mature arrangement toward snare, with the coordination story functioning as cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_sincerity, conceptual, 'Sincerity of the arrangement''s declared terminal condition.').

omega_variable(
    subject_net_benefit_contest,
    'Did colonized legal subjects derive net benefit from codified textual law (standardized procedure, some protections, suppression of extreme practices) or net loss (frozen hierarchy, displaced custom, court costs)?',
    'Distributional analysis of court outcomes by class, gender, and caste before and after codification, weighting procedural gains against substantive freezes.',
    'Net benefit would damp effective extraction for the subject seats and strengthen the coordination half of the structure; net loss confirms the asymmetry the victim declarations assert.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subject_net_benefit_contest, preference, 'Contested net-benefit accounting for the governed population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcsp_cor_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vcsp_cor_tr_t23, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 23, 0.16).
narrative_ontology:measurement(vcsp_cor_tr_t46, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 46, 0.21).
narrative_ontology:measurement(vcsp_cor_tr_t69, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 69, 0.28).
narrative_ontology:measurement(vcsp_cor_tr_t92, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 92, 0.36).
narrative_ontology:measurement(vcsp_cor_tr_t115, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 115, 0.41).
narrative_ontology:measurement(vcsp_cor_tr_t138, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 138, 0.43).
narrative_ontology:measurement(vcsp_cor_tr_t161, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 161, 0.45).
narrative_ontology:measurement(vcsp_cor_tr_t184, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 184, 0.46).

% Extraction over time
narrative_ontology:measurement(vcsp_cor_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vcsp_cor_be_t23, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 23, 0.46).
narrative_ontology:measurement(vcsp_cor_be_t46, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 46, 0.52).
narrative_ontology:measurement(vcsp_cor_be_t69, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 69, 0.58).
narrative_ontology:measurement(vcsp_cor_be_t92, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 92, 0.63).
narrative_ontology:measurement(vcsp_cor_be_t115, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 115, 0.64).
narrative_ontology:measurement(vcsp_cor_be_t138, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 138, 0.62).
narrative_ontology:measurement(vcsp_cor_be_t161, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 161, 0.6).
narrative_ontology:measurement(vcsp_cor_be_t184, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 184, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vcsp_cor_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(vcsp_cor_su_t23, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 23, 0.42).
narrative_ontology:measurement(vcsp_cor_su_t46, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 46, 0.53).
narrative_ontology:measurement(vcsp_cor_su_t69, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 69, 0.61).
narrative_ontology:measurement(vcsp_cor_su_t92, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 92, 0.66).
narrative_ontology:measurement(vcsp_cor_su_t115, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 115, 0.63).
narrative_ontology:measurement(vcsp_cor_su_t138, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 138, 0.58).
narrative_ontology:measurement(vcsp_cor_su_t161, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 161, 0.52).
narrative_ontology:measurement(vcsp_cor_su_t184, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 184, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_census_caste_enumeration).

% DUAL FORMULATION NOTE:
% The colloquial label 'Hindu law' decomposes under the epsilon-invariance principle into three readings of kernel vedic_corpus_social_prescription, each a separate story with its own epsilon, beneficiaries, and victims: this colonial_orientalist_reading (arrangement: colonial codification for administration; moderate epsilon), orthodox_varna_reading (arrangement: varna hierarchy as divine mandate; different victim set), and reformist_spiritual_reading (no prescriptive arrangement; epsilon near zero). This story links both siblings and the downstream colonial_census_caste_enumeration constraint, which inherited the codified categories after 1871.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
