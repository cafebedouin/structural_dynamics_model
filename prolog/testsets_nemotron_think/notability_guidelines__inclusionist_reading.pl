% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Structural Gatekeeping (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's Notability guideline (WP:N) requires that topics have
 *   'significant coverage in reliable sources independent of the subject' to
 *   merit an article. The inclusionist reading holds that this rule, whatever
 *   its original intent, now functions as a structural gatekeeping apparatus
 *   that systematically excludes knowledge from marginalized communities —
 *   indigenous peoples, oral traditions, Global South scholarship, grassroots
 *   movements — because their epistemic practices do not produce the
 *   Western-style secondary sources WP:N demands. The constraint is enforced
 *   through Articles for Deletion (AfD), speedy deletion criteria, and a
 *   sourcing doctrine that treats institutional publication as the only
 *   legitimate form of verification. Beneficiaries are the institutional
 *   knowledge producers (academia, publishing, mainstream media) whose
 *   epistemic authority is reinforced; victims are the communities whose
 *   knowledge is rendered invisible. The claimed type is snare: the
 *   coordination story (quality control) is cover for extraction (epistemic
 *   rent).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.82).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines as Structural Gatekeeping (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'c03eb2a4-d3ce-40f9-90eb-dd88e42b3912').
narrative_ontology:cs_kernel_codification('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', formalized).
narrative_ontology:cs_authority_grounding('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', practice).
narrative_ontology:cs_interpretation_layer_present('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912').
narrative_ontology:cs_reading_relation('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', foundational, institutional_verifiability_excludes_marginalized_epistemologies).
narrative_ontology:cs_axiom_status(institutional_verifiability_excludes_marginalized_epistemologies, holdable).
narrative_ontology:cs_axiom_grounding('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', institutional_verifiability_excludes_marginalized_epistemologies, empirically_contingent).
narrative_ontology:cs_axiom('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', foundational, notability_as_structural_gatekeeping_not_quality_filter).
narrative_ontology:cs_axiom_status(notability_as_structural_gatekeeping_not_quality_filter, holdable).
narrative_ontology:cs_axiom_grounding('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', notability_as_structural_gatekeeping_not_quality_filter, deontological).
narrative_ontology:cs_reference_frame('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', verifiability_quality_control).
narrative_ontology:cs_drift_state('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', contemporary_inclusionist_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c03eb2a4-d3ce-40f9-90eb-dd88e42b3912', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, academic_publishing_industry).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, mainstream_media_institutions).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, western_academic_establishment).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, oral_tradition_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_scholars).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, grassroots_activists).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, community_historians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, deletionist_editors).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, institutional_verifiability_as_sole_epistemic_legitimacy).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, notability_as_proxy_for_reliability).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, western_citation_practices_as_universal_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, research institutes, and think tanks whose published output becomes the primary 'reliable source' pool. They gain epistemic authority and citation traffic from WP:N's requirement that notability be established through secondary coverage in such sources. They do not administer the guideline but structurally benefit from its citation economy.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Commercial and society publishers whose paywalled journals constitute the dominant 'reliable source' corpus. WP:N drives demand for publication in indexed venues and reinforces the citation metrics that sustain subscription and APC revenue models. They capture value from the constraint without participating in its enforcement.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, academic_publishing_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Established newspapers, magazines, and broadcast outlets whose reporting becomes the 'significant coverage' threshold for notability. They gain traffic, authority, and a de facto gatekeeping role over public recognition. The constraint reinforces their epistemic monopoly without their direct involvement in Wikipedia governance.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, mainstream_media_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% The collective apparatus of Western academia — hiring committees, grant agencies, tenure systems — that sets the standards for what counts as legitimate scholarship. Wikipedia's reliance on 'reliable sources' imports these standards wholesale. Members of this establishment often participate directly in Wikipedia as editors and administrators, giving them dual structural position.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, western_academic_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, western_academic_establishment, agenda_setter).

% Communities whose knowledge, histories, and achievements exist primarily in oral tradition, community archives, non-English sources, or grassroots documentation. They cannot meet WP:N's 'significant coverage in reliable sources' threshold because the sources themselves exclude them. They bear the cost of invisibility on the world's most consulted reference work; exit means accepting erasure.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, global).

% Elders, knowledge keepers, and communities whose epistemologies are relational, place-based, and transmitted orally. WP:N treats oral testimony as inherently unreliable unless mediated through institutional anthropology or journalism. Their knowledge is excluded at the epistemological level; exit would require abandoning their own knowledge traditions to conform to Western citation norms.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders, payer,
    powerless, generational, identity_locked, global).

% Communities worldwide whose historical memory, genealogies, and cultural knowledge are maintained through structured oral practice rather than written records. WP:N's 'verifiability' requirement structurally disqualifies these traditions. They pay with epistemic exclusion; there is no exit that preserves their epistemic integrity.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, oral_tradition_communities, payer,
    powerless, generational, trapped, global).

% Researchers from the Global South publishing in regional journals, non-English languages, or formats not indexed by Western databases. Their work is systematically underrepresented in the 'reliable source' pool. They can sometimes meet the threshold through extraordinary effort (publishing in Western venues), but the structural burden is asymmetrically high compared to Northern counterparts.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_scholars, payer,
    moderate, biographical, constrained, global).

% Organizers and movements whose impact is documented in community media, social media, government reports, and NGO publications — sources routinely dismissed as 'not reliable' or 'not significant coverage' by Wikipedia's sourcing standards. They pay with invisibility of their achievements; exit means accepting that their work will not be recognized in the global knowledge commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, grassroots_activists, payer,
    moderate, biographical, constrained, global).

% Independent researchers, local historians, and amateur archivists who document marginalized histories using primary sources, interviews, and community records. Their work is disqualified by WP:N's preference for secondary sources. They bear the labor of documentation without the epistemic recognition; exit means publishing in venues that Wikipedia will not cite.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, community_historians, payer,
    moderate, biographical, constrained, global).

% The legal and operational steward of Wikipedia. Sets the policy framework, employs the Trust & Safety team, and ultimately bears legal liability. Has the authority to change WP:N but is structurally incentivized to maintain the 'reliable sources' doctrine as a liability shield and quality signal to donors and partners.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation, agenda_setter,
    institutional, generational, arbitrage, global).

% The elected enforcement corps who close AfD discussions, apply speedy deletion criteria, and interpret WP:N in practice. Their authority derives from community trust and technical tools. Many are identity-locked: their Wikipedia identity, reputation, and social network are bound to the enforcement regime. They administer the constraint and are also subject to its norms.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_administrators, agenda_setter,
    organized, biographical, identity_locked, global).

% Editors who view WP:N as a quality shield and actively enforce it through AfD nominations, PROD tags, and policy advocacy. They benefit from the constraint by gaining status, authority, and a defensible epistemic position within the community. Their identity is fused with the 'gatekeeper' role; exit would mean abandoning their core Wikipedia self-concept.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletionist_editors, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, deletionist_editors, beneficiary).

% Editors who argue for broader notability standards, better coverage of marginalized topics, and recognition of non-Western sources. They are structurally excluded from policy-setting: the 'reliable sources' doctrine is treated as settled, and challenges are dismissed as 'activism.' They can edit articles but cannot change the structural rule that excludes their subjects.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, excluded,
    organized, biographical, constrained, global).

% Scholars, activists, and organizations (e.g., Whose Knowledge?, Wiki Education, academic researchers) who study and campaign against epistemic exclusion on Wikipedia. They observe the constraint from outside the enforcement structure, producing evidence of its differential impact. They have analytical exit but no structural leverage to change the constraint.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_justice_advocates, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global volunteer editing workforce around a single verifiability standard, preventing the encyclopedia from fragmenting into competing epistemic fiefdoms or becoming a platform for unverifiable claims, hoaxes, and promotional content.
% TRANSFER_FUNCTION: Moves epistemic authority, visibility, and citation capital from marginalized knowledge holders (who cannot produce 'reliable sources' on their own terms) to institutional knowledge producers (whose output defines the 'reliable source' corpus). Extracts unpaid labor from marginalized communities to document themselves in forms legible to Western institutions.
% ABSENT_VOICES: Oral tradition holders, indigenous elders, community historians, grassroots organizers, and Global South knowledge producers whose epistemologies exist outside institutional publication channels. They are structurally absent from Wikipedia's policy discussions because participation requires fluency in the very citation norms that exclude them.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, marginalized communities could contribute knowledge directly from primary sources, community archives, and oral testimony. Institutional producers would lose their structural citation monopoly. The encyclopedia's topic coverage would shift dramatically toward Global South, indigenous, and grassroots subjects. The volunteer editor corps would face a coordination crisis over new inclusion standards.
% FOUNDING_PROBLEM: Early Wikipedia (2001-2006) faced rampant vandalism, hoaxes, promotional articles, and unverifiable claims. Notability emerged as a pragmatic filter: 'significant coverage in reliable sources' was a proxy for 'this topic can be verified by strangers without original research.' It solved a real coordination problem for a nascent project with no expert review mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by early Wikipedia administrators (e.g., Jimbo Wales' 2004-2006 mailing list posts on notability as anti-vandalism) and the 2006-2007 policy debates that formalized WP:N. Contemporary inclusionist scholars (e.g., Heather Ford, Jackie Koerner, Whose Knowledge? collective) and marginalized knowledge advocates attest that the vandalism/verifiability problem is substantially solved by other mechanisms (patrolling, bots, sourcediting norms) but the gatekeeping function persists and has expanded.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82) because the constraint transfers epistemic authority from excluded communities to included institutions without reciprocity. Suppression is high (0.78) because the constraint's persistence depends on actively deleting articles, blocking sources, and policing the 'reliable sources' boundary — not on participant preference. Theater ratio is moderate (0.38): the quality-control rhetoric is real but an increasing share of enforcement activity defends the institutional citation monopoly rather than preventing hoaxes. Accessibility collapse (0.72) is high because once a topic is deemed 'non-notable,' the alternatives (draft space, other wikis, external sites) have negligible visibility compared to a Wikipedia article. Resistance (0.63) is substantial: inclusionist editors, knowledge justice advocates, and marginalized communities actively contest the constraint, but their resistance is structurally disadvantaged by the constraint's own rules.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seats (Foundation, administrators), the constraint appears as necessary coordination — a quality filter that makes the project viable. From the payer seats (marginalized communities), the same structure appears as enforced epistemic extraction — a rule that demands they reproduce their knowledge in alien forms to be recognized. The engine computes this divergence from the structural data; the inclusionist reading names the extraction that the deletionist reading calls quality.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers, academic publishing, and mainstream media are structural beneficiaries (d near 0): they collect epistemic rents without enforcement costs. Wikimedia Foundation and administrators are agenda_setters with mixed directionality: they bear enforcement costs but gain institutional legitimacy. Deletionist editors are identity-locked agenda_setters who benefit from the gatekeeper role. Marginalized communities, indigenous knowledge holders, and oral tradition communities are trapped/identity-locked payers (d near 1): they bear the full cost of exclusion with no exit that preserves their epistemic integrity. Global South scholars and grassroots activists are constrained payers: they can sometimes meet the threshold but at asymmetric cost. Inclusionist editors are excluded: they would change the rule but are structurally locked out of policy-setting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (vandalism/verifiability in a nascent project) is dead — solved by other mechanisms (patrolling, bots, sourcediting norms, mature community). The constraint persists because it now serves a different function: protecting the epistemic monopoly of institutional sources. This is classic mandatrophy: the arrangement outlives its founding problem and is maintained by the beneficiaries of its new function. The mandate has not been resolved; it has been captured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is WP:N a single constraint with multiple interpretations, or are the deletionist, deliberative, and inclusionist readings structurally distinct constraints sharing a label?',
    'Apply the ε-invariance test: if measuring WP:N from the deletionist frame yields low ε (coordination) but from the inclusionist frame yields high ε (extraction), they are distinct constraints. The kernel_id/reading_id decomposition in this corpus treats them as distinct constraints linked by network.affects_constraints.',
    'If distinct constraints, each gets its own ε, stakeholders, and classification. The deletionist reading''s low extraction does not reduce this reading''s high extraction. The engine classifies each reading independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel label ''WP:N'' covers one constraint or a constraint family.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (deletion policies, source requirements, admin tools) or internalized (marginalized contributors self-censoring, believing their knowledge is ''not notable'')?',
    'Post-exclusion trajectory analysis: if contributors from marginalized communities stop attempting Wikipedia contribution after AfD deletions, and the suppression persists even when they have institutional affiliations, the mechanism is partially internalized.',
    'If internalized, effective suppression is higher than the structural measure — the constraint operates beyond its formal enforcement boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in epistemic exclusion.').

omega_variable(
    coordination_extraction_boundary,
    'Is the ''reliable sources'' verification function genuinely inseparable from the institutional citation monopoly, or could Wikipedia implement alternative verification (e.g., community review, primary source assessment, oral history protocols) that would preserve coordination without extraction?',
    'Natural experiment from Wikipedia''s own medical and scientific topic areas (where primary literature is cited directly) and from fork projects (e.g., Wikidata''s reference model, specialized wikis with different sourcing policies). If coordination holds without institutional secondary sources, the extraction is separable.',
    'If separable, the constraint is a Snare with a thin coordination veneer; if inseparable, part of the measured extraction is the genuine cost of coordination at scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    identity_lock_mechanism_admins,
    'What specific identity-fusion mechanism binds Wikipedia administrators to the enforcement regime — professional identity (career path dependence), relational identity (social network within Wikipedia), ideological identity (belief in the ''reliable sources'' doctrine), or institutional identity (the admin corps has ''become'' its function)?',
    'Longitudinal study of admin retention, burnout, and policy dissent: if admins who question WP:N leave or are marginalized, the mechanism is institutional/ideological; if they stay but comply, it is relational/professional.',
    'If identity-locked, their directionality is amplified toward target despite agenda_setter role — they enforce a constraint they cannot exit without identity loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_admins, empirical, 'Identity-lock dynamics for Wikipedia''s enforcement corps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 2006, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2006, notability_guidelines__inclusionist_reading, theater_ratio, 2006, 0.12).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2009, notability_guidelines__inclusionist_reading, theater_ratio, 2009, 0.18).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2012, notability_guidelines__inclusionist_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2015, notability_guidelines__inclusionist_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2018, notability_guidelines__inclusionist_reading, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2021, notability_guidelines__inclusionist_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_tr_t2024, notability_guidelines__inclusionist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2006, notability_guidelines__inclusionist_reading, base_extractiveness, 2006, 0.35).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2009, notability_guidelines__inclusionist_reading, base_extractiveness, 2009, 0.45).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2012, notability_guidelines__inclusionist_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2015, notability_guidelines__inclusionist_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2018, notability_guidelines__inclusionist_reading, base_extractiveness, 2018, 0.73).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2021, notability_guidelines__inclusionist_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_be_t2024, notability_guidelines__inclusionist_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2006, notability_guidelines__inclusionist_reading, suppression_requirement, 2006, 0.45).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2009, notability_guidelines__inclusionist_reading, suppression_requirement, 2009, 0.52).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2012, notability_guidelines__inclusionist_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2015, notability_guidelines__inclusionist_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2018, notability_guidelines__inclusionist_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2021, notability_guidelines__inclusionist_reading, suppression_requirement, 2021, 0.74).
narrative_ontology:measurement(notability_guidelines__inclusionist_reading_su_t2024, notability_guidelines__inclusionist_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.02).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_deletion_process).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikimedia_governance).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, reliable_sources_guideline).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, verifiability_policy).

% DUAL FORMULATION NOTE:
% This constraint (inclusionist_reading) and its siblings (deletionist_reading, deliberative_reading) form a constraint family around the kernel 'notability_guidelines'. The deletionist reading claims low ε (coordination quality filter); this reading claims high ε (epistemic extraction). The deliberative reading claims moderate ε with coordination function. The ε values differ because each reading evaluates the standing arrangement under contest from its own structural frame. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, organized, 0.35).
constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
