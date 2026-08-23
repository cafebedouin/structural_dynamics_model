% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N Notability Boundary as Deliberative Negotiation Process
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   On the deliberative reading, the English Wikipedia notability arrangement
 *   is not a fixed admission threshold but a standing negotiation: the
 *   guideline pages (WP:N, the general notability guideline, the
 *   subject-specific notability guidelines) supply vocabulary and
 *   burden-allocation for Articles-for-Deletion discussions, and the boundary
 *   itself is the running output of thousands of contested cases, closure
 *   precedents, deletion-review reversals, and periodic requests-for-comment
 *   that revise the text. The guideline's legitimacy under this reading rests
 *   on its revisability — each formulation is a provisional summary of where
 *   the community currently stands, expected to be superseded. Costs are real
 *   and unevenly borne: first-time creators lose uncompensated work at the
 *   exact moment of entry, and editors covering domains whose sources lag
 *   behind discoverability endure repeated create-delete cycles. The
 *   claim/metric split is deliberate: the claimed type is scaffold (the
 *   structural delta this reading asserts — transitional text, persistent
 *   negotiation), while the metrics describe the arrangement's actual
 *   operation including its ritualization and enforcement hardening; the
 *   engine measures the divergence. KEY AGENTS (by structural relationship):
 *   - first_time_article_creators: Primary payer (powerless/constrained) —
 *   bears deletion of uncompensated work at entry -
 *   editors_of_lagging_source_domains: Secondary payer (moderate/constrained)
 *   — bears repeated create-delete cycles -
 *   experienced_deliberation_participants: Primary beneficiary
 *   (organized/constrained) — steers outcomes, accumulates standing -
 *   administrators_closing_discussions: Agenda-setter
 *   (institutional/constrained) — closes, executes, salts; records consensus
 *   - wiki_project_coordinators: Dual-positioned beneficiary/payer
 *   (organized/constrained) — hold SNG jurisdiction, lose nurtured articles -
 *   readers_of_the_encyclopedia: Diffuse beneficiary (powerless/mobile) —
 *   receive curated coverage, absent from the process -
 *   offline_knowledge_communities: Excluded voice (powerless/trapped) —
 *   adjudicated without a seat - peer_production_researchers: Analytical
 *   observer (analytical/analytical) — documents the process from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.45).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.38).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N Notability Boundary as Deliberative Negotiation Process").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '50c46748-b707-41bb-8d47-6d1294bbff34').
narrative_ontology:cs_kernel_codification('50c46748-b707-41bb-8d47-6d1294bbff34', formalized).
narrative_ontology:cs_authority_grounding('50c46748-b707-41bb-8d47-6d1294bbff34', practice).
narrative_ontology:cs_interpretation_layer_present('50c46748-b707-41bb-8d47-6d1294bbff34').
narrative_ontology:cs_reading_relation('50c46748-b707-41bb-8d47-6d1294bbff34', notability_guidelines__deletionist_reading, influences).
narrative_ontology:cs_reading_relation('50c46748-b707-41bb-8d47-6d1294bbff34', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('50c46748-b707-41bb-8d47-6d1294bbff34', foundational, notability_is_constituted_by_deliberation).
narrative_ontology:cs_axiom_status(notability_is_constituted_by_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('50c46748-b707-41bb-8d47-6d1294bbff34', notability_is_constituted_by_deliberation, conventional).
narrative_ontology:cs_axiom('50c46748-b707-41bb-8d47-6d1294bbff34', foundational, guideline_text_is_provisional_summary).
narrative_ontology:cs_axiom_status(guideline_text_is_provisional_summary, holdable).
narrative_ontology:cs_axiom_grounding('50c46748-b707-41bb-8d47-6d1294bbff34', guideline_text_is_provisional_summary, conventional).
narrative_ontology:cs_axiom('50c46748-b707-41bb-8d47-6d1294bbff34', secondary, contested_cases_drive_boundary_revision).
narrative_ontology:cs_axiom_status(contested_cases_drive_boundary_revision, holdable).
narrative_ontology:cs_axiom_grounding('50c46748-b707-41bb-8d47-6d1294bbff34', contested_cases_drive_boundary_revision, instrumental).
narrative_ontology:cs_reference_frame('50c46748-b707-41bb-8d47-6d1294bbff34', consensus_negotiated_boundary).
narrative_ontology:cs_drift_state('50c46748-b707-41bb-8d47-6d1294bbff34', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50c46748-b707-41bb-8d47-6d1294bbff34', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, experienced_deliberation_participants).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, administrators_closing_discussions).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wiki_project_coordinators).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, readers_of_the_encyclopedia).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, first_time_article_creators).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, editors_of_lagging_source_domains).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, offline_knowledge_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, wiki_project_coordinators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Close deletion discussions after weighing arguments against the guideline text and prior closures, execute the outcome (delete, keep, redirect), salt pages against recreation, and defend closures at deletion review. Their authority is formally derivative — they record consensus rather than impose judgment — but closure discretion shapes which arguments count. They are unpaid volunteers; stepping away costs them standing in the only community where that standing exists.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, administrators_closing_discussions, agenda_setter,
    institutional, biographical, constrained, global).

% Veteran editors fluent in guideline jurisprudence whose nominations, defenses, and policy citations steer most outcomes. They accumulate reputation and informal authority through participation, and their arguments become the precedent material the boundary evolves from. The same participation consumes large amounts of unpaid evaluative labor; leaving would abandon their primary collaborative community and accumulated status.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, experienced_deliberation_participants, beneficiary,
    organized, biographical, constrained, global).

% Maintain the subject-specific notability guidelines for their domains, giving them recognized jurisdiction over how their field's boundary is argued. They simultaneously lose articles they have nurtured when general-notability arguments override their subject-specific standards, so they both operate the boundary machinery and absorb its failures inside their topic areas.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wiki_project_coordinators, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, wiki_project_coordinators, payer).

% Receive topical coverage curated by the boundary without ever seeing the deliberation that produces it. They benefit incidentally from the maintenance of a shared relevance standard and bear none of its costs. Substituting another information source costs them nothing, so their position exerts no pressure on the arrangement in either direction.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, readers_of_the_encyclopedia, beneficiary,
    powerless, immediate, mobile, global).

% Arrive through article creation and commonly encounter a deletion discussion within days or weeks. They lose uncompensated work at the moment of maximum investment, face argument norms they have not learned, and frequently leave the project permanently afterward. Their objections occasionally flip an outcome when well-sourced, but they have no standing in the revisions that determine what the boundary will demand next.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, first_time_article_creators, payer,
    powerless, immediate, constrained, global).

% Cover emerging scholarship, non-Anglophone regions, and local history where significant sources exist but sit outside the indexed books, journals, and news archives the general guideline rewards. They endure repeated create-delete cycles while waiting for discoverability to catch up with significance, and some maintain parallel off-wiki drafts against the next deletion round.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, editors_of_lagging_source_domains, payer,
    moderate, generational, constrained, global).

% Communities whose histories, languages, and prominent figures are adjudicated in discussions none of their members attend. They typically learn of deletions after closure, have no channel to contest the boundary itself (only article-level arguments via sympathetic proxies), and cannot opt out of being governed by a standard they had no part in setting.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, offline_knowledge_communities, excluded,
    powerless, generational, trapped, global).

% Study deletion outcomes, newcomer retention, and guideline evolution; publish findings the community sometimes cites in its own reform debates. They hold no stake in any particular outcome and can see the full structure — including the gap between the deliberative ideal and ritualized practice — from outside the incentive system.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, peer_production_researchers, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable procedure for resolving membership disputes — which topics belong in the encyclopedia — such that thousands of simultaneous content conflicts resolve into outcomes participants will treat as legitimate without any central authority; each resolved case doubles as precedent material that updates the boundary.
% TRANSFER_FUNCTION: Moves deletion decisions and boundary-defining authority upward toward experienced participants and closing administrators; moves the labor of source evaluation from casual contributors toward the veteran cohort; moves the destroyed work of deleted articles from their creators into the commons' discard stream, where it functions as boundary information rather than retained content.
% ABSENT_VOICES: Offline knowledge communities and the subjects of deleted biographies have no seat at all — they are governed by the boundary without access to it. Field experts comment when they find a discussion, but their input is routinely discounted as conflicted. The largest absent constituency is the population of prospective contributors who encountered an early deletion discussion, left, and never registered their objection anywhere.
% DISAPPEARANCE_RATIONALE: If the notability arrangement and its deliberation machinery vanished overnight, the project would have no channel for the hundreds of daily belong-or-not disputes: mass-creation and mass-deletion campaigns would collide with no legitimate arbiter, and the community would reconstruct a boundary mechanism within months — as it historically did, building WP:N out of the escalation of the pre-2006 deletion wars. The arrangement is load-bearing for the project's basic content decisions.
% FOUNDING_PROBLEM: An openly editable encyclopedia accumulating content without a relevance standard was becoming an indiscriminate repository; between roughly 2003 and 2005 the project experienced escalating deletion disputes with no shared criteria, threatening both content coherence and contributor peace, and the notability guideline was built to operationalize the existing 'not an indiscriminate collection of information' policy into a decidable standard.
% FOUNDING_PROBLEM_CORROBORATION: Historical mailing-list archives, early Signpost coverage, and the surviving text of the pre-AfD deletion debates document the founding crisis from outside any current faction; peer-production research literature corroborates both the crisis and the subsequent institutionalization. Notably, the deletionist and inclusionist factions — opposed on everything else — both attest that the founding problem existed and was real, disputing only whether the current arrangement answers it.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.45): the arrangement destroys real uncompensated labor and concentrates that destruction on newcomers and lagging-domain editors, but the deliberative conversion of losses into boundary information and the availability of reversal paths (deletion review, later recreation when sources improve) keep it well below pure-extraction levels. Suppression (0.38) reflects binding closures, salting, and creation protection rather than participant preference — the process is voluntary to join but not voluntary to lose. Theater ratio (0.35) is below the proxy-replacement threshold but rising: guideline citation increasingly functions as ritual ('fails GNG', !vote stacking) alongside genuine source evaluation, and the rising series tracks that ritualization. Accessibility collapse is low-moderate (0.40): alternatives persist (other platforms, off-wiki drafts, delayed recreation), so understanding the constraint does not foreclose options. Resistance (0.55) is high and structurally absorbed — relisting, deletion-review appeals, and boundary-testing mass creations are inputs the negotiation metabolizes rather than threats it must crush; that absorption is precisely what distinguishes this reading's arrangement from one requiring suppression of dissent. The temporal series run on one shared grid (t=0..20, mapping approximately 2006-2026, the AfD era); the suppression_requirement series is authored deliberately to trace enforcement-capacity maturation — closure norms, salting practice, and protection regimes hardened over the interval — which is the sanctioned use of that series, not a restatement of the static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute differently. From the first-time creator's position the arrangement is an abrupt confiscation: weeks of unpaid work vanish days after entry, decided by strangers citing texts the creator has never read. From the closing administrator's position the same event is the system working: a contested case resolved by recorded consensus, feeding precedent into an evolving boundary. Long-tenured participants exhibit institutional identity fusion — 'Wikipedian' as constitutive self-concept — which suppresses exit independently of satisfaction; if that identity frame broke at scale, the deliberative quorum would thin and the negotiation's legitimacy would erode faster than any rule change could cause. The engine computes these per-seat classifications from the power/exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: experienced deliberation participants collect influence and standing; administrators collect closure authority and project standing; WikiProject coordinators collect jurisdiction over their domains' boundaries; readers receive curated coverage incidentally and diffusely. Victims: first-time creators bear confiscated labor; lagging-domain editors bear repeated destruction cycles; offline knowledge communities bear adjudication without representation. Derived directionalities place payers near the full-target end (powerless or moderate power, constrained exit) and readers near the full-beneficiary end (mobile exit, no participation cost). Two overrides correct the derivation where it would err: administrators are declared beneficiaries but capture little personal rent while donating heavy labor, so their d sits near-symmetric (0.35) rather than near the beneficiary pole; experienced participants and WikiProject coordinators (both organized-power seats) similarly pay large unpaid-labor costs against their influence gains, placing them slightly beneficiary-of-center (0.30) rather than at the derived near-zero. The extraction's product — boundary clarity — accrues to no named seat, which is why gain_flow is authored as diffuse: the checked claim is that no stakeholder captures the extracted value; it is consumed by the process itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbounded inclusion pressure on an openly editable repository, crystallized in the 2003-2005 deletion wars — remains live, so no mandate decay is declared and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The scaffold classification does preventive work here: it blocks the two symmetric mislabels. Reading the current guideline text as the permanent constraint invites treating the arrangement as a durable coordination mechanism whose criteria are settled (rope) or as rent-collection behind a quality cover (snare); reading the ritualized citation culture as the whole arrangement invites the piton verdict. The theater ratio (0.35, below the 0.5 proxy-replacement line) and the continuing production of real boundary movement (reversed closures, revised criteria, retired guidelines) are the authored evidence that the negotiation remains functional rather than performative. Mandatrophy resolution is correctly withheld: the arrangement has not outlived its function; it has changed function — from crisis response to standing constitutional process — which is the transition the scaffold claim names.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the deliberative_reading of kernel notability_guidelines: WP:N as a perpetual negotiation process whose boundary is the output of AfD deliberation rather than an input measured against fixed criteria. What structurally changes under the sibling readings?',
    'Cross-reading comparison of the sibling constraint files: the deletionist_reading authors epsilon for a fixed epistemic filter (lower resistance, different beneficiary set centered on content-quality maintenance); the inclusionist_reading authors epsilon for a structural gatekeeping apparatus (higher suppression, victim set shifted to marginalized knowledge domains). Each sibling is a separate file with its own epsilon, victims, and classification.',
    'Under the deletionist_reading the same arrangement computes as a durable coordination filter with the guideline text as the operative constraint; under the inclusionist_reading it computes as enforced exclusion with the deliberation as legitimating cover. This file''s scaffold classification holds only under the deliberative premise that the negotiation, not the text, is the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this story is one reading of the notability_guidelines kernel; sibling readings instantiate different constraints.').

omega_variable(
    operative_constraint_location,
    'Where do the three readings locate the operative constraint — in the guideline text (deletionist), in the deliberative process (deliberative), or in the exclusionary effect (inclusionist) — and which location does the observable record actually support?',
    'Test whether AfD outcomes track the written criteria, the argument quality of participants, or the demographic/source profile of the topic. If outcomes track argumentation and shift with participant composition, the process-location reading is supported; if outcomes are stable across argument variation, a text- or effect-location reading gains ground.',
    'Process-location supports the scaffold claim (transitional text, persistent negotiation). Text-location would push toward a rope/tangled_rope classification of the criteria themselves; effect-location would push the arrangement toward snare-flavored computation with the deliberation as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_constraint_location, conceptual, 'The disagreement between readings is located in what counts as the constraint: text, process, or effect.').

omega_variable(
    procedural_sunset_semantics,
    'Does the deliberative reading''s sunset — every codified formulation of WP:N is designed to be superseded through the same machinery that applies it — satisfy the scaffold sunset-clause requirement, or is the absence of any calendrical expiry evidence that the arrangement is a steady-state coordination mechanism mislabeled as transitional?',
    'Compare revision cadence against deliberative drift: if major formulations of the guideline persist substantially unchanged past the point where AfD practice and RfC outcomes have moved past them, the ''procedural sunset'' is nominal and the text functions as a durable fixture.',
    'If the procedural sunset is real, the scaffold classification stands with the negotiation as the persistent element and each text-version as transitional support. If formulations are effectively frozen, the constraint reclassifies toward rope or tangled_rope with the text as the durable object.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_sunset_semantics, conceptual, 'Whether the sunset clause is authentically procedural (revision-by-design) or a post-hoc label on a durable rule.').

omega_variable(
    newcomer_attrition_share,
    'What share of AfD-deleted articles are first-time creators'' work, and what share of those creators leave the project permanently after deletion?',
    'Edit-tracking datasets linking article creator tenure to post-AfD retention; published newcomer-survival studies already cover part of this.',
    'High newcomer concentration would raise effective extraction on the payer seat well above the story-level scalar and strengthen the reading that the negotiation runs partly on uncompensated destruction of newcomer labor; low concentration would support treating deletion costs as broadly distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(newcomer_attrition_share, empirical, 'Distribution of deletion costs across editor tenure cohorts.').

omega_variable(
    source_discoverability_lag,
    'Does the deliberative process systematically lag source availability for emerging scholarship, non-Anglophone regions, and local history — sources that exist but sit outside the indexed channels the general notability guideline rewards — such that outcomes are biased independently of participant intent?',
    'Sample AfD outcomes reversed after indexing improvements (digitization drives, regional database coverage, search-index expansion) and measure the lag between topic significance and source discoverability.',
    'Common lag-driven reversals would indicate the negotiation extracts structurally from lagging domains regardless of good faith, pushing the arrangement toward a hybrid coordination/extraction profile and raising the payer-seat extraction above the authored scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_discoverability_lag, empirical, 'Whether boundary outcomes track source discoverability rather than topic significance.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (deletion execution, page salting, creation protection, closure discretion) versus internalized (veteran and newcomer editors preemptively avoiding ''unnotable'' topics, chilling topic selection before any enforcement occurs)?',
    'Compare attempted-creation rates against successful-publication rates by topic class; survey editors on self-censored topic choices; measure whether chilling persists in editors who have never experienced an adverse AfD outcome directly.',
    'If internalized suppression dominates, the constraint''s effective suppressive force exceeds the structural measure and persists even where enforcement machinery relaxes; if structural mechanisms dominate, enforcement reform translates directly into reduced suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_deliberative_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ng_deliberative_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ng_deliberative_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(ng_deliberative_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(ng_deliberative_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(ng_deliberative_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(ng_deliberative_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(ng_deliberative_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(ng_deliberative_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ng_deliberative_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(ng_deliberative_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(ng_deliberative_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ng_deliberative_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ng_deliberative_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(ng_deliberative_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(ng_deliberative_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(ng_deliberative_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(ng_deliberative_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, inclusionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'WP:N' decomposes into three structurally distinct constraints — one per reading of the notability_guidelines kernel. This file instantiates the deliberative_reading (boundary as output of negotiation; scaffold). The deletionist_reading instantiates the filter claim (necessary epistemic quality control; the text is the operative constraint). The inclusionist_reading instantiates the gatekeeping claim (systematic exclusionary apparatus; the effect is the operative constraint). The epsilon values differ because the referent arrangements differ in what each reading holds constant: filter-legitimacy versus process-legitimacy versus effect-accountability. This reading sits upstream of the deletionist_reading (its precedents and RfC outcomes continuously revise the criteria the filter reading treats as fixed) and coexists with the inclusionist_reading (live opposing faction, no resolution path). Per the epsilon-invariance principle, no single story averages across these; each sibling is authored separately and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__deliberative_reading, institutional, 0.35).
constraint_indexing:directionality_override(notability_guidelines__deliberative_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
