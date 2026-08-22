% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Exclusionary Gatekeeping (Inclusionist Reading)
 *   domain: digital_commons/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's Notability guidelines (WP:N) establish epistemic standards
 *   for article inclusion: subjects must be documented in 'reliable sources'
 *   — typically academic journals, mainstream media, and established
 *   publishers. This inclusionist reading treats Notability as a snare: a
 *   constraint that presents itself as quality control (founded problem:
 *   preventing degradation through spam/vandalism) but functions structurally
 *   as gatekeeping apparatus excluding marginalized communities whose
 *   knowledge does not exist in institutional publication infrastructure. The
 *   founding problem is contested — some evidence suggests alternative
 *   platforms operate with looser notability criteria without documented
 *   degradation — and the constraint's enforcement has intensified over time
 *   as deletion backlogs grew and policy interpretation tightened.
 *   Marginalized communities and their knowledge traditions are
 *   systematically victimized because they lack access to the publication
 *   infrastructure that defines 'reliable sources.'
 *
 * KEY AGENTS:
 *   - Wikipedia administration: enforces Notability standards, interprets reliable sourcing, adjudicates deletions
 *   - Institutional knowledge producers (academics, publishers, mainstream media): benefit from having their knowledge pre-validated by existing publication infrastructure
 *   - Marginalized communities (Indigenous peoples, non-Western traditions, diaspora groups, emerging movements): victimized through erasure because their knowledge lacks institutional publication
 *   - Wikipedia deletionists: faction enforcing Notability most stringently, maintaining epistemic hierarchy
 *   - Wikipedia inclusionists: faction resisting Notability, advocating expanded sourcing criteria
 *   - Commercial publishers and academic journals: benefit from Wikipedia's requirement to source through paywall-protected journals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.81).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.87).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines as Exclusionary Gatekeeping (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'e8d0d1bb-e617-4aff-b1d2-a3603c764464').
narrative_ontology:cs_kernel_codification('e8d0d1bb-e617-4aff-b1d2-a3603c764464', formalized).
narrative_ontology:cs_authority_grounding('e8d0d1bb-e617-4aff-b1d2-a3603c764464', extraction).
narrative_ontology:cs_interpretation_layer_present('e8d0d1bb-e617-4aff-b1d2-a3603c764464').
narrative_ontology:cs_reading_relation('e8d0d1bb-e617-4aff-b1d2-a3603c764464', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8d0d1bb-e617-4aff-b1d2-a3603c764464', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('e8d0d1bb-e617-4aff-b1d2-a3603c764464', foundational, notability_as_epistemic_exclusion).
narrative_ontology:cs_axiom_status(notability_as_epistemic_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('e8d0d1bb-e617-4aff-b1d2-a3603c764464', notability_as_epistemic_exclusion, empirically_contingent).
narrative_ontology:cs_axiom('e8d0d1bb-e617-4aff-b1d2-a3603c764464', foundational, institutional_publication_privilege).
narrative_ontology:cs_axiom_status(institutional_publication_privilege, holdable).
narrative_ontology:cs_axiom_grounding('e8d0d1bb-e617-4aff-b1d2-a3603c764464', institutional_publication_privilege, empirically_contingent).
narrative_ontology:cs_axiom('e8d0d1bb-e617-4aff-b1d2-a3603c764464', secondary, marginalized_knowledge_systematic_erasure).
narrative_ontology:cs_axiom_status(marginalized_knowledge_systematic_erasure, holdable).
narrative_ontology:cs_axiom_grounding('e8d0d1bb-e617-4aff-b1d2-a3603c764464', marginalized_knowledge_systematic_erasure, deontological).
narrative_ontology:cs_reference_frame('e8d0d1bb-e617-4aff-b1d2-a3603c764464', inclusive_epistemic_commons).
narrative_ontology:cs_drift_state('e8d0d1bb-e617-4aff-b1d2-a3603c764464', contemporary_deletion_intensification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8d0d1bb-e617-4aff-b1d2-a3603c764464', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_publishing_infrastructure).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, academic_disciplinary_canons).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, non_western_knowledge_traditions).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, localized_and_experiential_knowledges).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, emerging_social_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_deletionists).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, commercial_publishing).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_readers_from_marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, wikipedia_inclusionists).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, wikipedia_readers_from_marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces Notability guidelines through deletion review processes, policy interpretation, and community standards-setting. Adjudicates what counts as 'reliable source,' what subject domains merit coverage, and when marginal topics warrant article space. Sets the epistemic bars that determine encyclopedic inclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic disciplines, established publishers, mainstream media organizations, and institutional actors whose knowledge already meets the 'reliable sources' standard. Their subjects reliably pass Notability gates because their institutional authority generates the publications that define reliable sourcing. They benefit from Wikipedia's role reinforcing their epistemic authority as the measure of what deserves to exist in public knowledge.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    powerful, generational, arbitrage, global).

% Indigenous peoples, non-Western traditions, diaspora communities, economically precarious groups whose knowledge exists in oral transmission, community practice, or non-English-language publication. Their subjects fail Notability gates not because they lack genuine significance, but because the knowledge-production infrastructure that generates 'reliable sources' is structurally inaccessible to them. They pay through systematic erasure: their histories, movements, knowledge systems are not admitted to the global commons because they cannot afford institutional publication or academic citation.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, global).

% Epistemically distinct ways of knowing (Indigenous science, traditional medicine, oral histories, place-based ecological knowledge) that are systematically deprioritized by Notability's reliance on peer review, journal publication, and Western academic citation patterns. Exit from this constraint would require wholesale abandonment of their own knowledge transmission traditions to adopt Western academic publication practices — identity_locked because the alternative is epistemic colonization.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, non_western_knowledge_traditions, payer,
    powerless, civilizational, identity_locked, global).

% Grassroots activism, mutual aid networks, horizontally organized resistance, newly formed community organizations. They lack access to traditional reliable-source pathways (academic journals, mainstream media) during their formation phase. Their knowledge — tactical innovation, locally adapted solutions, participant testimony — gets deleted from Wikipedia because it has not yet been processed through institutional gatekeepers.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, emerging_social_movements, payer,
    moderate, biographical, constrained, global).

% Established academic fields whose topics, methodologies, and terminology are already embedded in journal publication, conference structures, and funding infrastructure. Their canonical subjects are routinely above the Notability threshold because the constraint was designed by and for the academic publishing ecosystem. They collect epistemic rent: their frame becomes the measure of what deserves to exist.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, academic_disciplinary_canons, beneficiary,
    powerful, generational, arbitrage, global).

% The community faction and administrator set that enforces Notability rules most stringently. They advocate for higher evidentiary bars, stricter source evaluation, more aggressive deletion of fringe or local topics. They benefit from the maintenance of epistemic hierarchy because their role as quality-control gatekeepers depends on sustaining the scarcity they police.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_deletionists, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikipedia_deletionists, beneficiary).

% The editor community faction advocating for looser Notability criteria, broader inclusion, and more generous interpretation of what constitutes reliable sourcing. They resist the constraint from within, restoring deleted articles and arguing for expansion of epistemic authority recognition. Their constrained exit: leaving Wikipedia governance means abandoning the fight; staying means losing article restoration votes to the deletion enforcement majority.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_inclusionists, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikipedia_inclusionists, observer).

% Academic publishers, journal platforms, and trade publishers who control access to the 'reliable sources' that Notability gates require. Wikipedia's reliance on peer review and published sources funnels epistemic authority through paywall-protected journals, directing prestige and readership flow toward commercial gatekeepers.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, commercial_publishing, beneficiary,
    institutional, generational, arbitrage, global).

% Users seeking knowledge about their own communities, histories, and traditions. They experience the constraint as omission: they cannot find their own stories, leaders, movements, or knowledge in the encyclopedia. They benefit from the parts of Wikipedia that do exist, but pay through systematic erasure of their own epistemic worlds.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_readers_from_marginalized_communities, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikipedia_readers_from_marginalized_communities, beneficiary).

% Wikibase projects, community wikis, non-hierarchical knowledge platforms that would admit marginalized knowledge if not for Wikipedia's hegemonic position as the reference standard. Their alternative inclusion criteria and epistemic frameworks are marginalized by Wikipedia's dominance in search engines, citation conventions, and user expectations.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, alternative_knowledge_platforms, excluded,
    moderate, biographical, trapped, global).

% Governments, UNESCO, cultural heritage organizations, and knowledge equity advocates. They observe the constraint's operation and can impose remedies (mandates for inclusive sourcing, quotas for marginalized knowledge, funding for alternative platforms) that would alter enforcement or exception-handling.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_commons_governance_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, wikipedia_administration).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemic standard for a global encyclopedia by requiring sourcing through a defined publication infrastructure (peer review, academic journals, mainstream media). Solves the coordination problem of 'what counts as knowledge worth preserving collectively' by anchoring to an existing institutional hierarchy.
% TRANSFER_FUNCTION: Moves epistemic authority and visibility from marginalized knowledge traditions to institutional producers by requiring mediation through 'reliable sources' — a requirement that structurally advantages knowledge already produced by academic/publishing institutions and structurally disadvantages knowledge produced through oral transmission, community practice, or non-English publication.
% ABSENT_VOICES: Marginalized communities themselves — the people whose knowledge is being judged by Notability standards they had no role in setting. Alternative epistemologies (Indigenous science, traditional medicine, oral histories) are excluded from the policy-setting conversation entirely; their objections to the framework never reach the deliberative process. Non-English-speaking communities have no proportional voice in policy interpretation.
% DISAPPEARANCE_RATIONALE: If Notability guidelines disappeared, Wikipedia would flood with topics currently deleted as non-notable: marginalized histories, local movements, non-Western knowledge traditions, emerging social experiments. The encyclopedia would become radically more representative. The authority structure that currently validates which knowledge 'counts' would have to be renegotiated. Search engines and academic citation would fragment across multiple knowledge platforms instead of funneling through Wikipedia as the reference standard. The constraint's removal would reorganize the global knowledge commons around different epistemic principles.
% FOUNDING_PROBLEM: Early Wikipedia faced vandalism and spam; unlimited article expansion without quality control would degrade the encyclopedia. Notability guidelines were established to maintain epistemic coherence and prevent the commons from collapsing into noise.
% FOUNDING_PROBLEM_CORROBORATION: Wikipedia administrators and deletionists attest the founding problem (spam, vandalism, degradation risk) remains live. Inclusionists and knowledge equity advocates contest this, citing evidence from non-English Wikipedia projects and alternative knowledge platforms that operate with looser Notability criteria without collapsing into documented degradation — their alternative interpretation comes from outside the benefiting institutional establishment and is grounded in comparative platform performance data.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs from 0.68 to 0.81 over the interval as deletion practices tighten and sourcing standards narrow (observed drift documented in AfD trend analysis). Suppression is high (0.87 at interval end) because the constraint depends on continuous enforcement: administrator-led deletions, community deletion reviews, source-evaluation policing. Without active enforcement, marginalized editors would restore deleted articles; the suppression is structural coercion, not normative consensus. Theater is moderate-high (0.62) because Notability enforcement is partially theatrical: quality maintenance is a real function, but a significant portion of active enforcement defends institutional epistemic hierarchy rather than preventing genuine degradation. The measurement series establish one shared time grid so every metric is jointly observable at each time point. The escalating theater_ratio reflects policy tightening that defends exclusionary sourcing standards in the name of 'reliability' — the cover story intensifies while the exclusionary function persists.
 *
 * PERSPECTIVAL GAP:
 *   The administration and deletionist seats experience this as quality control protecting the commons. The beneficiary institutional seats experience this as validation of their epistemic authority. The victim seats experience this as systematic erasure. These are not mere disagreements about a shared fact — they are structural differences in how the constraint operates relative to different seats' positions. The inclusionist reading centralizes the victim perspective: from that seat, the 'quality control' framing is cover story, and the operative function is exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   From the Wikipedia administration seat: Notability is coordination (shared epistemic standard for a global commons, founded to solve a real problem). From the institutional beneficiary seats (academic publishers, established disciplines): the constraint validates their epistemic authority and funnels visibility toward their institutions. From the marginalized victim seats: the constraint operates as enforced exclusion — they cannot access the 'reliable sources' infrastructure the constraint requires, so they are systematically erased. The engine computes these divergent directionalities from the structural data: the beneficiaries have high d (near 1.0 = target position for extraction flowing their direction), victims have high d (near 1.0 = target position bearing extraction), admin has moderate d (agenda-setter position straddling coordination and enforcement). The schema-enforced stakeholder coverage ensures every named agent appears in the computational surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (spam/vandalism degradation) was live at Wikipedia's formation. There is some evidence the problem is now contested: non-English Wikipedia projects and alternative platforms operate with looser Notability criteria and report no documented degradation. This raises mandatrophy questions: if the founding problem's status has shifted from 'live' to 'contested' or even 'dead,' then Notability's persistence despite loosened problem urgency suggests it persists as an extraction mechanism, not as a solution to its founding problem. The theater_ratio drift supports this: as deletion practice tightens (theater increases), the original problem-solving function does not appear to demand intensification — the tightening serves to maintain institutional epistemic hierarchy rather than prevent fresh waves of degradation. The inclusionist reading treats this as a snare that began as scaffolding (temporary quality mechanism) and has calcified into permanent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_platform_degradation_paradox,
    'Do non-English Wikipedia projects and alternative knowledge platforms operating with looser Notability criteria actually show measurable degradation compared to Wikipedia''s core English encyclopedia, or does the absence of documented degradation suggest the founding problem has been substantially solved or was always overstated?',
    'Comparative empirical analysis: audit non-English Wikipedia coverage quality, measure reader satisfaction, document spam/vandalism rates in projects with genuinely looser Notability enforcement; compare against English Wikipedia''s metrics over equivalent time periods.',
    'If alternative platforms show no degradation: the founding problem''s status drops from ''live'' to ''dead,'' strongly supporting mandatrophy reading and snare classification. If degradation is documented: Notability''s persistence is more defensible as problem-solving rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_degradation_paradox, empirical, 'Whether evidence from alternative platforms challenges the necessity of strict Notability enforcement.').

omega_variable(
    institutional_publication_infrastructure_contingency,
    'Is the ''reliable sources'' infrastructure (peer review, academic journals, mainstream media) a contingent artefact of 20th-century knowledge production, or is it a necessary feature of any high-fidelity knowledge commons?',
    'Historical analysis of pre-institutional knowledge systems (oral traditions, craft guilds, scientific societies) and their epistemic quality mechanisms; comparative analysis of contemporary non-Western knowledge systems that maintain fidelity through alternative mechanisms (community validation, practitioner expertise, intergenerational transmission).',
    'If infrastructure is contingent: then Notability''s requirement to source through institutional publication is functionally a requirement to source through one particular cultural form, making it structurally exclusionary to non-Western traditions. If necessary: then Notability requirements track genuine epistemic necessity rather than cultural hegemony.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_publication_infrastructure_contingency, conceptual, 'Whether institutional publication is a necessary condition for reliable knowledge or one possible instantiation among many.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of marginalized knowledge primarily structural (external deletion enforcement, source accessibility barriers, policy gatekeeping) or internalized (marginalized editors have internalized the epistemic hierarchy and self-censor, producing the ''reliable sources'' as unavailable to them)?',
    'Post-removal suppression trajectory: if Notability enforcement ceased but marginalized knowledge remained excluded because communities had internalized the hierarchy, reclassify suppression as partially internalized. Ethnographic study of marginalized editor decision-making: do they avoid article creation because enforcement is visible, or because they have accepted the epistemic frame?',
    'If suppression is primarily structural: removing enforcement could catalyze rapid content expansion. If internalized: relaxing policy would not alone restore marginalized knowledge; communities would need explicit legitimation and capacity-building to re-author their own histories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether marginalized communities'' knowledge absence is externally coerced or internalized.').

omega_variable(
    reliable_sources_as_knowledge_production_gatekeeping,
    'Does the requirement to source through ''reliable sources'' constitute one legitimate epistemic standard among many, or does it function as systematic exclusion of non-Western and non-institutionalized knowledge because those knowledge systems do not typically produce outputs in the publication format that counts as ''reliable''?',
    'Comparative analysis of sourcing patterns: catalog which knowledge domains reliably produce ''reliable sources'' (academic disciplines, mainstream media coverage) versus which do not (local histories, oral traditions, emerging movements); map those patterns against Notability deletion rates by domain and identify whether absence of ''reliable sources'' correlates with deletion independent of article quality/notability.',
    'If correlation is high and systematic: Notability functions as exclusion of non-Western and non-institutionalized knowledge as a structural class. If correlation is weak: sourcing patterns reflect other factors and Notability may not be systematically exclusionary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_as_knowledge_production_gatekeeping, empirical, 'Whether ''reliable sources'' requirement structurally excludes non-Western and non-institutionalized knowledge.').

omega_variable(
    kernel_reading_contest_structure,
    'Is the contest between deletionist, deliberative, and inclusionist readings of Notability genuinely underdetermined by the policy text itself, or does one reading''s core premise logically foreclose the others such that policy choice between them is illusory?',
    'Formal analysis of the policy kernel: examine whether the founding rationale (preventing degradation through quality standards) logically entails one reading''s core premise over another. If deletionist reading''s premise (Notability is necessary quality control) is foundational, can inclusionist reading (Notability is extractive gatekeeping) hold within the same policy framework?',
    'If readings are genuinely underdetermined: policy choice between them is a values question legitimately contestable. If one reading''s premise logically forecloses others: the kernel is more constrained than it appears, and apparent ''deliberation'' masks structural foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether the Notability policy kernel logically entails one reading or genuinely admits multiple interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(nota_tr_t0, observed).
narrative_ontology:measurement(nota_tr_t3, notability_guidelines__inclusionist_reading, theater_ratio, 3, 0.51).
narrative_ontology:measurement_basis(nota_tr_t3, observed).
narrative_ontology:measurement(nota_tr_t6, notability_guidelines__inclusionist_reading, theater_ratio, 6, 0.54).
narrative_ontology:measurement_basis(nota_tr_t6, observed).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.59).
narrative_ontology:measurement_basis(nota_tr_t12, observed).
narrative_ontology:measurement(nota_tr_t18, notability_guidelines__inclusionist_reading, theater_ratio, 18, 0.61).
narrative_ontology:measurement_basis(nota_tr_t18, observed).
narrative_ontology:measurement(nota_tr_t25, notability_guidelines__inclusionist_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(nota_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(nota_be_t0, observed).
narrative_ontology:measurement(nota_be_t3, notability_guidelines__inclusionist_reading, base_extractiveness, 3, 0.71).
narrative_ontology:measurement_basis(nota_be_t3, observed).
narrative_ontology:measurement(nota_be_t6, notability_guidelines__inclusionist_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(nota_be_t6, observed).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement_basis(nota_be_t12, observed).
narrative_ontology:measurement(nota_be_t18, notability_guidelines__inclusionist_reading, base_extractiveness, 18, 0.81).
narrative_ontology:measurement_basis(nota_be_t18, observed).
narrative_ontology:measurement(nota_be_t25, notability_guidelines__inclusionist_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(nota_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.79).
narrative_ontology:measurement_basis(nota_su_t0, observed).
narrative_ontology:measurement(nota_su_t3, notability_guidelines__inclusionist_reading, suppression_requirement, 3, 0.81).
narrative_ontology:measurement_basis(nota_su_t3, observed).
narrative_ontology:measurement(nota_su_t6, notability_guidelines__inclusionist_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement_basis(nota_su_t6, observed).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement_basis(nota_su_t12, observed).
narrative_ontology:measurement(nota_su_t18, notability_guidelines__inclusionist_reading, suppression_requirement, 18, 0.86).
narrative_ontology:measurement_basis(nota_su_t18, observed).
narrative_ontology:measurement(nota_su_t25, notability_guidelines__inclusionist_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(nota_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.12).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_source_reliability_standard).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, academic_publishing_authority_gating).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel admits three structurally distinct readings with different ε values and different beneficiary/victim structures. The deletionist_reading treats Notability as a necessary quality filter (low extraction, mountain candidate). The deliberative_reading treats it as a contestable negotiation space (moderate extraction, rope/tangled_rope candidate). The inclusionist_reading (this constraint) treats it as a structural gatekeeping apparatus serving institutional interests (high extraction, snare). These are not three measurement perspectives on one constraint — they are three different constraints instantiated by three different readings of the same kernel. Each has its own extracted beneficiary set, its own victim set, its own operational logic. They are linked via network.affects_constraints because policy change in one reading affects operating conditions for the others: if deletionist principles tighten, inclusionist editors lose restoration arguments; if deliberative procedures open, deletionist gatekeeping loses institutional shelter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
