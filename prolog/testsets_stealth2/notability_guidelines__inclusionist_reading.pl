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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Notability Regime as Structural Gatekeeping Apparatus (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the inclusionist_reading of the
 *   notability_guidelines kernel: it models Wikipedia's notability regime —
 *   the General Notability Guideline and its topic-specific supplements,
 *   enforced through Articles for Deletion, speedy deletion, and new-page
 *   review — as a structural gatekeeping apparatus. On this reading the
 *   operative rule is that a subject exists encyclopedically only if parties
 *   controlling institutional publication have already chronicled it in
 *   'reliable sources'; communities whose knowledge is transmitted orally,
 *   regionally, or outside commercial and academic publishing therefore
 *   cannot buy, earn, or argue their way in, and volunteers who attempt to
 *   document them donate labor that is subsequently deleted. KEY AGENTS (by
 *   structural relationship): - veteran_afd_administrators: agenda-setting
 *   enforcement cadre (organized/identity_locked) — operates the deletion
 *   workflow and accrues standing from it; - institutional_source_producers:
 *   primary beneficiary (institutional/arbitrage) — its imprimatur is the
 *   entry ticket; - marginalized_knowledge_communities: primary target
 *   (powerless/trapped) — bears the exclusion; -
 *   diaspora_and_first_time_contributors: secondary target
 *   (moderate/constrained) — donates labor and loses it; -
 *   regional_journal_publishers: secondary target (moderate/constrained) —
 *   publishes rigorously outside the indexing umbrellas; -
 *   general_encyclopedia_readers: diffuse beneficiary-with-costs
 *   (moderate/mobile); - oral_history_and_indigenous_archives: excluded
 *   holder of disqualified evidence (moderate/trapped); -
 *   knowledge_equity_researchers: analytical observer quantifying the
 *   exclusion. EPSILON REFERENT: the standing arrangement — WP:N as written
 *   and operated — assessed by this reading's own lights, never the
 *   inclusive-sourcing regime this reading endorses. CLAIM/METRIC
 *   INDEPENDENCE: claimed_type (snare) is authored from structural belief
 *   under this reading; the metrics are authored as descriptively true; where
 *   the engine's per-seat computations diverge from the claim, that
 *   divergence is the datum. ASSUMPTIONS: interval units are years, t=0
 *   approximately 2005 (guideline-essay era, pre-professionalized deletion
 *   workflow) and t=20 approximately 2025; the sibling readings are separate
 *   files in this kernel family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - veteran_afd_administrators: agenda_setter (organized/identity_locked/global/generational) — runs AfD closures, writes supplemental guidelines, collects standing and procedural control from the workflow
 *   - institutional_source_producers: beneficiary (institutional/arbitrage/global/generational) — universities, major newsrooms, journals, presses whose output is the entry ticket; pays nothing, receives citations
 *   - marginalized_knowledge_communities: payer (powerless/trapped/regional/generational) — indigenous nations, oral-tradition keepers, locally significant figures absent from metropolitan chronicles
 *   - diaspora_and_first_time_contributors: payer (moderate/constrained/global/biographical) — volunteers documenting home communities whose drafts die at AfD
 *   - regional_journal_publishers: payer (moderate/constrained/continental/biographical) — rigorous outlets outside Scopus/Web of Science ruled unreliable at deletion debates
 *   - general_encyclopedia_readers: beneficiary + secondary payer (moderate/mobile/global/biographical) — receives the filtered reference and inherits its blind spots unknowingly
 *   - oral_history_and_indigenous_archives: excluded (moderate/trapped/regional/generational) — holds documented records of exactly the disputed subjects; barred as unreliable, no seat in the debates
 *   - knowledge_equity_researchers: observer (analytical/analytical/global/generational) — publishes coverage-disparity studies; sees the full pipeline, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.76).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.8).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Notability Regime as Structural Gatekeeping Apparatus (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'c7888486-0543-4fa1-84e9-fbfdef50a014').
narrative_ontology:cs_kernel_codification('c7888486-0543-4fa1-84e9-fbfdef50a014', formalized).
narrative_ontology:cs_authority_grounding('c7888486-0543-4fa1-84e9-fbfdef50a014', practice).
narrative_ontology:cs_interpretation_layer_present('c7888486-0543-4fa1-84e9-fbfdef50a014').
narrative_ontology:cs_reading_relation('c7888486-0543-4fa1-84e9-fbfdef50a014', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7888486-0543-4fa1-84e9-fbfdef50a014', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('c7888486-0543-4fa1-84e9-fbfdef50a014', foundational, significance_precedes_publication).
narrative_ontology:cs_axiom_status(significance_precedes_publication, holdable).
narrative_ontology:cs_axiom_grounding('c7888486-0543-4fa1-84e9-fbfdef50a014', significance_precedes_publication, deontological).
narrative_ontology:cs_axiom('c7888486-0543-4fa1-84e9-fbfdef50a014', foundational, coverage_disparity_tracks_publisher_power).
narrative_ontology:cs_axiom_status(coverage_disparity_tracks_publisher_power, holdable).
narrative_ontology:cs_axiom_grounding('c7888486-0543-4fa1-84e9-fbfdef50a014', coverage_disparity_tracks_publisher_power, empirically_contingent).
narrative_ontology:cs_reference_frame('c7888486-0543-4fa1-84e9-fbfdef50a014', sum_of_all_human_knowledge_ideal).
narrative_ontology:cs_drift_state('c7888486-0543-4fa1-84e9-fbfdef50a014', contemporary_knowledge_equity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c7888486-0543-4fa1-84e9-fbfdef50a014', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_source_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, veteran_afd_administrators).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_knowledge_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, diaspora_and_first_time_contributors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, regional_journal_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, general_encyclopedia_readers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, general_encyclopedia_readers).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, institutional_mediation_of_epistemic_worth).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, verifiability_through_published_secondary_sources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Long-tenured editors and administrators who run Articles for Deletion, draft and interpret the supplemental notability guidelines, and close deletion debates. Their standing, permissions, awards, and social networks were earned through fluency in these procedures; stepping away would forfeit a reputation that exists nowhere outside the project. Many donate ten or more unpaid hours weekly to sustaining the deletion workflow, and their judgment calls define which subjects survive.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, veteran_afd_administrators, agenda_setter,
    organized, generational, identity_locked, global).

% University presses, major newsrooms, academic journals, and large publishing houses whose output constitutes the published reliable sources the guideline requires. Nothing is demanded of them: their imprint alone qualifies a subject for an article, and inbound citations from the encyclopedia raise the discoverability of their catalogs. They can disregard the project entirely at no cost to themselves.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_source_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Indigenous nations maintaining oral records, local historians of under-documented regions, practitioners of non-Western knowledge systems, and living figures who are significant inside their own spheres but were never chronicled by metropolitan press. The entry condition — significant coverage in recognized publications — is a credential their communities had no hand in issuing and cannot retroactively obtain. Leaving is not an available remedy: the loss is the absence of their record from the world's default reference work, which follows them wherever they go.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_knowledge_communities, payer,
    powerless, generational, trapped, regional).

% Volunteers, often from immigrant or minority backgrounds, who arrive intending to document their home towns, languages, artists, or elders; they draft articles and watch them tagged and deleted for insufficient reliable coverage. Labor is donated and then removed. Some persist by acquiring the sourcing conventions; many simply stop editing. Walking away from the project is easy, but the subjects they came to represent remain unrepresented wherever they go.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, diaspora_and_first_time_contributors, payer,
    moderate, biographical, constrained, global).

% Editors of scholarly journals, newspapers, and magazines outside the major indexing umbrellas — no DOI, not in Scopus or Web of Science, not syndicated internationally. Their rigorously edited publications are routinely judged unreliable or insufficient at deletion debates regardless of content. Joining the recognized tier requires indexing subscriptions and English-language visibility they often cannot afford; abandoning their regional readerships to chase accreditation is not a viable trade.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, regional_journal_publishers, payer,
    moderate, biographical, constrained, continental).

% Readers worldwide who use the encyclopedia as first-stop orientation. They receive a consistent, spam-resistant reference work; they also inherit its blind spots as a picture of the world, usually without knowing that whole regions, movements, and people are missing from it. Substituting other references is trivially easy; noticing what is absent is not.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, general_encyclopedia_readers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, general_encyclopedia_readers, payer).

% Community archives, tribal libraries, oral-history projects, and indigenous-language broadcasters that hold curated, documented records of precisely the subjects deletion debates find unproven. Their materials are treated as unusable at AfD, they have no standing in the discussions that classify their collections, and no pathway exists by which their holdings could come to count as reliable sources.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, oral_history_and_indigenous_archives, excluded,
    moderate, generational, trapped, regional).

% Academic and movement researchers who measure the encyclopedia's coverage gaps — biographies of women, Global South topics, minority-language subjects — and publish disparity studies. They observe the full pipeline from source availability through deletion outcomes and can quantify whom the sourcing bar screens out, but hold no vote in the guideline's administration and no standing in individual deletion debates.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_equity_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, veteran_afd_administrators).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the attention-allocation and trust problems of a radically open wiki: thousands of uncoordinated volunteers need one shared, administrable test for which subjects merit articles, so that effort converges on a bounded, spam-resistant encyclopedia instead of fragmenting into unlimited self-description and promotion. Prior significant coverage in recognized channels serves as the cheap, delegable proxy test.
% TRANSFER_FUNCTION: Moves definitional authority — whose existence, work, and memory count as encyclopedically real — from undocumented communities and uncredentialed contributors toward the operators of the reliable-source ecosystem and the editors fluent in its conventions. Incidentally moves contributor labor, much of which is deleted after donation, and moves citational traffic toward incumbent publishers.
% ABSENT_VOICES: The subjects of unwritten articles and the keepers of oral and unindexed records are absent from deletion debates: the people whose recognition is at issue hold no standing at AfD, and the archives holding their disqualified evidence (oral_history_and_indigenous_archives, role excluded) are never consulted. Debates occur among editors already fluent in the source hierarchy, so apparent unanimity about what counts as evidence arises partly because the dissenting epistemic seats were never in the room.
% DISAPPEARANCE_RATIONALE: Overnight repeal would flood the encyclopedia with promotional, vanity, and unverifiable content within days — the founding problem reasserts itself immediately — while simultaneously opening coverage to currently excluded subjects. The project's character, staffing, and public standing would rearrange around whatever equilibrium the community built next; neither the spam regime nor the exclusion regime sustains itself without this rule.
% FOUNDING_PROBLEM: Between 2001 and 2005 the open wiki attracted self-promotion, vanity biography, advertising, and unverifiable claims faster than any reviewer could judge individual cases on their merits; editors needed a bright-line, delegable test for worth-an-article and adopted prior significant coverage in reliable sources as a cheap, corruption-resistant proxy.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: Wikimedia Foundation anti-abuse and platform-integrity reporting documents continuing promotional-content pressure at scale, and paid-editing litigation and disclosure cases corroborate it adversarially. Long-standing guideline critics — equity campaigners, inclusionist editors, external scholars of Wikipedia's coverage gaps — attest from the opposite side that the original spam problem was real but that the proxy now excludes far more than spam. No seat disputes that the founding problem existed or persists; the dispute is whether the current apparatus answers it.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.76: the arrangement converts donated documentation labor into deletion statistics and converts communal memory into an evidentiary void; the spam-control benefit purchased is real, but its incidence is asymmetric — the seats that pay (subjects without institutional chronicles, their would-be documentarians, non-indexed publishers) are not the seats that set the terms. Suppression 0.80, authored raw and UNSCALED per the framework rule (only extractiveness is scaled by directionality and scope): structural mechanisms dominate — policy language framing exclusion as neutrality ('notability is not a matter of opinion'), speedy-tagging that deters novices before any debate occurs, sanctions for 'advocacy editing' that recode community self-documentation as misconduct, and citation conventions operable mainly by the source-fluent; whether an internalized chill adds on top is routed to the suppression_structural_vs_internalized omega rather than baked into the scalar. Theater_ratio 0.48: roughly half of visible process activity — boilerplate votes, policy-essay elaboration, ritual debate closures, backlog statistics — maintains the appearance of case-by-case neutrality while outcomes track source-channel availability closely; genuine removal of promotional and hoax content is the functional remainder, so this is substantive process with heavy ceremonial accretion, not inert performance. Accessibility_collapse 0.55, honest to type: partial — once the sourcing bar is understood, alternatives remain available (other wikis, direct publication, community archives) but none delivers the specific good at stake, presence in the world's default reference work, so the understood alternative set collapses incompletely. Resistance 0.62: sustained and recurring — gender-gap and equity campaigns, thematic edit-a-thons, repeated notability-reform RfCs, critical scholarship, press scrutiny — repeatedly mounted and repeatedly absorbed into procedure (see coalition_absorption_risk). MEASUREMENTS: one shared grid (t=0,4,8,12,16,20) for all three tracked metrics as the alignment rule requires; the trajectories are a monotonic ratchet, not a cycle — codification of supplemental guidelines, professionalization of new-page review, and maturation of automated enforcement raised the enforcement requirement steadily, which is why suppression_requirement is authored as a rising series tracking enforcement-capacity change rather than left to the static scalar. COORDINATION TYPE: identity_coordination, because the dominant function whose failure WP:N guards against is dissolution of the project's epistemic identity (what this encyclopedia is); the FNL gaming risk is acknowledged — 'this is who we are as an encyclopedia' is precisely the identity cover story the framework warns about, and the type's modest offset accommodates genuine boundary-maintenance complexity without excusing the asymmetry the metrics record. RECEIPT SURFACE: gains demonstrably accrue to the enforcement cadre — standing, procedural control, and social capital convertible directly from the arrangement's operation — so gain_flow names that seat rather than asserting diffuseness; fixing_cost is prohibitive for whoever could fix it: repeated notability-reform RfCs have failed against entrenched consensus, arbitration precedent, and the coordination cost of rewriting core policy, measured against a benefit half the community does not concede exists.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the enforcement cadre's position the same structure is custodial diligence: thousands of unpaid hours protecting an artifact the cadre identifies with — the identity lock binds them TO the arrangement, and their computed classification should diverge sharply from the payer seats'. Institutional source producers sit near the subsidy pole: the rule spends their imprimatur and pays them in citational centrality, a transaction so favorable it is invisible from inside. Marginalized communities and their documentarians meet a wall whose texture — polite, procedural, evidence-shaped — is imperceptible from inside the room where everyone already shares one definition of evidence. Same-nominal-level divergence among the moderate-power seats turns on exit, not wealth: readers are mobile (other references are one click away), diaspora contributors are constrained (leaving abandons the represented subjects, not just the hobby), and regional journals are constrained by indexing capital they cannot quickly acquire. Inter-institutionally, the enforcement cadre, the publishing ecosystem, and the Foundation's equity programs experience one guideline as three different objects: a charter of authority, a subsidy, and a reform target respectively. The engine computes per-seat classifications from this structural data; the authored snare claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive derivation. institutional_source_producers (beneficiary, arbitrage-grade exit) derive nearest the full-beneficiary pole — the arrangement subsidizes them unconditionally. marginalized_knowledge_communities (victim, trapped) derive nearest the full-target pole; trapped exit amplifies their effective extraction beyond what a mobile victim would bear. diaspora_and_first_time_contributors and regional_journal_publishers (victims, constrained) sit high but below the trapped pole. general_encyclopedia_readers (declared beneficiary with a payer secondary role, mobile exit) derive near-symmetric-low, matching genuine benefit plus diffuse, unnoticed coverage-gap cost. ONE OVERRIDE, on the organized atom: it maps uniquely to veteran_afd_administrators, and without it their identity_locked exit option would drag the derived d target-ward — backwards, since their lock binds them to the arrangement rather than against it. The override sets d=0.12: near the beneficiary pole, because standing, procedural control, and social capital accrue to them from the arrangement's operation, net of the substantial unpaid labor they donate to sustain it. No override is declared for the moderate atom despite heterogeneous seats there: their differing beneficiary/victim positions and exit options differentiate them correctly without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an open wiki flooded with vanity pages, self-promotion, and unverifiable claims — remains live, attested from outside the benefiting parties (Foundation anti-abuse telemetry, adversarial paid-editing disclosures, and the guideline's own critics agree the spam pressure is real), so mandatrophy is NOT resolved and no sunset logic applies. The classification work cuts both ways. It prevents the deletionist move — reading the live spam problem as proof that the whole apparatus is proportionate pure coordination — and equally prevents the piton move — dismissing the arrangement as inert ceremony; the exclusion is real, ongoing, and attached to a functioning filter, which is what distinguishes this verdict from decay. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds them consistent: no zombie flag fires. What the reading contributes is the second clause of the persistence explanation — the arrangement persists because its founding problem persists AND because the seats that hold the pen are the seats the arrangement advantages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the inclusionist_reading of the notability_guidelines kernel; what constraint would the deletionist_reading or the deliberative_reading instantiate from the same guideline pages?',
    'Author the two sibling stories and compare their structural surfaces: the deletionist instantiation declares readers-and-editors-at-large as beneficiaries with low epsilon; the deliberative instantiation declares a perpetual-review process without closure; classification deltas across the family localize what the kernel contest actually changes.',
    'The snare verdict is reading-indexed, not topic-indexed: under the deletionist instantiation the same pages compute nearer a working-filter profile, under the deliberative instantiation nearer a transitional-process profile. Cross-reading comparison is the only way to attribute classification to the reading versus the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Reading-indexed classification over a shared kernel: this file is one of three sibling constraints.').

omega_variable(
    sibling_epsilon_divergence_location,
    'Where in the structure do the three readings disagree — is the contest located in the beneficiary/victim declarations (who counts as a knowledge-holder entitled to recognition), in the metric values, or in endorsement of the source hierarchy itself?',
    'Side-by-side audit of the three sibling stories'' beneficiaries/victims arrays and epsilon values: agreement on metrics with opposed victim sets would locate the dispute in structural declarations rather than measurement.',
    'If the victim sets are the sole divergence, cross-reading reconciliation reduces to a recognition question (whose chronicles count as sources) rather than an empirical one, and metric harmonization across the family would be a category error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_epsilon_divergence_location, conceptual, 'Locates the kernel disagreement within the structural data rather than the metric surface.').

omega_variable(
    criterion_vs_corps_locus,
    'Does systematic exclusion originate in the notability criterion itself (significant coverage in reliable sources) or in the composition and incentives of the editor corps applying it?',
    'Natural experiments comparing jurisdictions, periods, and sister projects with identical or translated criteria but differing contributor demographics — post-edit-a-thon cohorts, non-English wikis with adapted sourcing norms — measuring deletion rates for demographically matched subjects.',
    'If the criterion carries the exclusion, textual reform fails and the arrangement survives translation intact; if the corps carries it, diversification reforms outcomes without touching the text, and the constraint''s persistence is a personnel fact rather than a doctrinal one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_vs_corps_locus, empirical, 'Whether the gatekeeping force sits in the rule text or in the enforcing population.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (policy text, deletion outcomes, sanctions for advocacy editing) or internalized (would-be contributors who have learned that their subjects and communities are ''not notable'' and self-exclude before ever drafting)?',
    'Post-exit trajectory: survey lapsed contributors from underrepresented cohorts after policy-relaxation experiments or supervised sourcing waivers; if self-censorship and pre-emptive abandonment persist once the barrier is lifted, the internalized share is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure and textual reform under-delivers; remediation would need to target chilling effects carried by contributors rather than the rulebook alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the contributor population.').

omega_variable(
    coalition_absorption_risk,
    'Can the powerless payer seats coalition effectively (GLAM partnerships, thematic edit-a-thons, sourcing-reform requests for comment), or does effective participation route through mastering the very source hierarchy that excludes them, converting resistance into unpaid staffing of the gate?',
    'Track a decade of equity-campaign outcomes: did deletion attrition for targeted subject classes (women biographies, Global South topics) fall durably after campaign investment, or did campaign-produced drafts face unchanged deletion rates?',
    'If absorption dominates, the resistance measurement overstates durable reform capacity and the arrangement''s stability exceeds what coalition theory predicts for powerless victims; if coalitions have moved outcomes, the victim seats carry latent class power the power atom understates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_absorption_risk, empirical, 'Whether victim-class collective action escapes co-optation by the sourcing conventions.').

omega_variable(
    genre_vs_implementation,
    'Is systematic exclusion intrinsic to any tertiary reference work that gates inclusion on prior institutional publication, or contingent to this implementation''s specific criteria, culture, and enforcement tooling?',
    'Cross-platform comparison of coverage-gap profiles across independently governed reference projects with different admission rules at matched scale and age.',
    'If genre-intrinsic, the verdict indicts the verifiability-gated-reference genre generally and reform space is design-space rather than governance-space; if contingent, this instance''s classification must not be generalized to sibling projects with different admission regimes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genre_vs_implementation, conceptual, 'Whether the exclusionary pattern is a property of the genre or of this implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpn_incl_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(wpn_incl_tr_t0, observed).
narrative_ontology:measurement(wpn_incl_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(wpn_incl_tr_t4, observed).
narrative_ontology:measurement(wpn_incl_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(wpn_incl_tr_t8, observed).
narrative_ontology:measurement(wpn_incl_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(wpn_incl_tr_t12, observed).
narrative_ontology:measurement(wpn_incl_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(wpn_incl_tr_t16, observed).
narrative_ontology:measurement(wpn_incl_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(wpn_incl_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(wpn_incl_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(wpn_incl_be_t0, observed).
narrative_ontology:measurement(wpn_incl_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(wpn_incl_be_t4, observed).
narrative_ontology:measurement(wpn_incl_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(wpn_incl_be_t8, observed).
narrative_ontology:measurement(wpn_incl_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(wpn_incl_be_t12, observed).
narrative_ontology:measurement(wpn_incl_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement_basis(wpn_incl_be_t16, observed).
narrative_ontology:measurement(wpn_incl_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(wpn_incl_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(wpn_incl_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(wpn_incl_su_t0, observed).
narrative_ontology:measurement(wpn_incl_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement_basis(wpn_incl_su_t4, observed).
narrative_ontology:measurement(wpn_incl_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(wpn_incl_su_t8, observed).
narrative_ontology:measurement(wpn_incl_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(wpn_incl_su_t12, observed).
narrative_ontology:measurement(wpn_incl_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(wpn_incl_su_t16, observed).
narrative_ontology:measurement(wpn_incl_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(wpn_incl_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, verifiability_policy).

% DUAL FORMULATION NOTE:
% Colloquially 'WP:N' names one thing; per the epsilon-invariance principle it decomposes into at least three structurally distinct constraints sharing one kernel. This file is the inclusionist_reading: high epsilon, declared victims, snare-claimed. The siblings — notability_guidelines__deletionist_reading (low-epsilon quality filter) and notability_guidelines__deliberative_reading (process scaffold) — author the same pages with different beneficiary/victim structures. The upstream member is the shared codified text together with the verifiability policy it operationalizes; each reading cites that text as warrant, so edges run from this reading to its siblings and to verifiability_policy for contamination propagation across the family. Sibling files reciprocate the links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
