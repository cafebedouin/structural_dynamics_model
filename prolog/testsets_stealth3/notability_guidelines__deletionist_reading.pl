% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Gate — Deletionist Reading (Necessary Epistemic Quality Filter)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   English Wikipedia's notability guideline is the admission threshold
 *   governing which topics warrant standalone articles in the largest
 *   collaboratively written reference work. This file instantiates the
 *   deletionist reading of the contested kernel: the guideline as a NECESSARY
 *   epistemic quality filter that prevents commons degradation — spam
 *   flooding, vanity promotion, and unverifiable claims — by pre-committing
 *   the community to an evidentiary test (significant coverage in independent
 *   reliable sources) applied before inclusion. Per the claim/metric
 *   independence rule, the claimed type (rope) states this reading's
 *   structural thesis, while the metrics are authored separately as this
 *   reading's assessment of the standing arrangement's actual operation. The
 *   epsilon referent is the standing WP:N arrangement as applied, assessed by
 *   this reading's own lights: the burden falling on rejected contributors is
 *   counted as filtration cost rather than extraction, yielding low authored
 *   epsilon. The sibling readings (inclusionist_reading,
 *   deliberative_reading) author different epsilon over the same referent in
 *   their own files; nothing about the contest is folded into this
 *   constraint. KEY AGENTS (by structural relationship): see key_agents; the
 *   beneficiary set and the absence of a victim set are deliberate structural
 *   declarations encoding the reading's just-exclusion claim.
 *
 * KEY AGENTS:
 *   - encyclopedia_readership: primary beneficiary (moderate/mobile) — consumes the filtered commons; experiences the gate only as its absence
 *   - volunteer_editor_community: beneficiary with cost-bearing second seat (organized/constrained) — gains collaborative stability, supplies the filter's operating labor
 *   - english_wikipedia_administrators: agenda_setter (institutional/identity_locked) — administers daily application; their authority is non-portable outside the project
 *   - new_topic_article_creators: cost-bearing contributors (powerless/mobile) — absorb discarded effort; deliberately NOT designated victims under this reading
 *   - spam_and_vanity_publishers: excluded defector class (organized/arbitrage) — the filtration's target population; loses one channel among many
 *   - wikimedia_foundation: institutional beneficiary (generational horizon) — receives liability insulation and brand protection as byproducts
 *   - subjects_of_deleted_articles: excluded bystanders (powerless/mobile) — learn of removals post hoc with no practical standing
 *   - knowledge_quality_researchers: analytical observer — sees the full structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.22).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.25).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Gate — Deletionist Reading (Necessary Epistemic Quality Filter)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '312c2c37-1b31-475d-ad1b-8ad0a4351be9').
narrative_ontology:cs_kernel_codification('312c2c37-1b31-475d-ad1b-8ad0a4351be9', formalized).
narrative_ontology:cs_authority_grounding('312c2c37-1b31-475d-ad1b-8ad0a4351be9', practice).
narrative_ontology:cs_interpretation_layer_present('312c2c37-1b31-475d-ad1b-8ad0a4351be9').
narrative_ontology:cs_reading_relation('312c2c37-1b31-475d-ad1b-8ad0a4351be9', notability_guidelines__inclusionist_reading, forecloses).
narrative_ontology:cs_reading_relation('312c2c37-1b31-475d-ad1b-8ad0a4351be9', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('312c2c37-1b31-475d-ad1b-8ad0a4351be9', foundational, notability_is_discovered_evidentiary_property).
narrative_ontology:cs_axiom_status(notability_is_discovered_evidentiary_property, holdable).
narrative_ontology:cs_axiom_grounding('312c2c37-1b31-475d-ad1b-8ad0a4351be9', notability_is_discovered_evidentiary_property, empirically_contingent).
narrative_ontology:cs_axiom('312c2c37-1b31-475d-ad1b-8ad0a4351be9', foundational, quality_requires_precommitment_filtration).
narrative_ontology:cs_axiom_status(quality_requires_precommitment_filtration, holdable).
narrative_ontology:cs_axiom_grounding('312c2c37-1b31-475d-ad1b-8ad0a4351be9', quality_requires_precommitment_filtration, instrumental).
narrative_ontology:cs_reference_frame('312c2c37-1b31-475d-ad1b-8ad0a4351be9', gng_evidentiary_threshold).
narrative_ontology:cs_drift_state('312c2c37-1b31-475d-ad1b-8ad0a4351be9', contemporary_sng_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('312c2c37-1b31-475d-ad1b-8ad0a4351be9', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, volunteer_editor_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikimedia_foundation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, volunteer_editor_community).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, new_topic_article_creators).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, spam_and_vanity_publishers).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, general_notability_guideline_evidential_sufficiency).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, precommitment_filtration_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consume the encyclopedia's article set as a finished public good. They never see the gate operate: promotional stubs, vanity biographies, and unverifiable claims are removed before or shortly after they would surface. Their stake is the signal-to-noise ratio of what they read and the trust that makes the site usable as a first reference. Dissatisfied readers switch to other references at near-zero cost, so their leverage is aggregate (traffic, donations) rather than organizational.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, encyclopedia_readership, beneficiary,
    moderate, generational, mobile, global).

% Hundreds of thousands of registered editors who write articles, patrol new pages, and staff the deletion processes. A shared admission criterion is what lets them collaborate without relitigating taste on every article: a contributor can predict what belongs before investing effort, and a reviewer can cite a common standard instead of personal preference. The same community supplies the labor the criterion consumes — patrolling, debating, closing — so its members gain the collaborative stability the standard provides while carrying its operating costs.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editor_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, volunteer_editor_community, payer).

% Experienced editors trusted with closing deletion debates, executing speedy deletions, and interpreting the guideline at its edges. Their authority exists entirely inside the project: leaving means forfeiting accumulated standing, user rights, and social position with no portable equivalent elsewhere. They do not originate the criterion — community-wide consensus revisions do — but they administer its daily application, and their close summaries effectively fix the criterion's meaning case by case.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, english_wikipedia_administrators, agenda_setter,
    institutional, biographical, identity_locked, global).

% Contributors — often newcomers, sometimes the article's subject or an admirer of it — who research and write an article on a topic that later fails the significance test. Typical outcomes: speedy deletion within minutes of creation, or a deletion debate ending in removal. The invested effort is unrecoverable. Available responses: improve sourcing and recreate, publish on other platforms, or stop contributing. Most arrive unaware any threshold exists until their first article is removed.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, new_topic_article_creators, payer,
    powerless, immediate, mobile, global).

% Organized operators — search-ranking farms, public-relations placements, paid-editing outfits, vanity-press networks — that treat article space as free reach. They adapt continuously to enforcement through sockpuppets and covert paid editing, but they have never shaped the criterion itself; each pathway they exploit is eventually closed. Exclusion costs them one distribution channel among many they operate, replaced elsewhere at will.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_and_vanity_publishers, payer,
    organized, immediate, arbitrage, global).

% The nonprofit that hosts the platform. A defensible, consistently applied admission threshold insulates it from defamation and hosting-liability exposure, protects the brand on which donation revenue depends, and keeps paid-manipulation scandals containable. It rarely intervenes in day-to-day application but retains override power through legal office actions, exercised sparingly and publicly.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikimedia_foundation, beneficiary,
    institutional, generational, mobile, global).

% People and organizations who discover, usually via search engines or news coverage, that an article about them existed and was deleted. They receive no notification at decision time, have no practical channel into the process (discussion and review pages are open in principle but effectively discoverable only to insiders), and typically learn the outcome after the fact. Their objections, where any exist, are voiced outside the project.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, subjects_of_deleted_articles, excluded,
    powerless, immediate, mobile, global).

% Academic and institutional analysts of open-collaboration governance who study deletion logs, debate outcomes, and quality metrics. They observe the full structure from outside, publish on filter efficacy and its side effects, and hold no stake in the guideline's persistence.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, knowledge_quality_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared, pre-committed admission threshold for a commons written by hundreds of thousands of anonymous volunteers under unlimited write access. Significant coverage in independent reliable sources gives contributors a checkable prediction of what belongs before they invest effort, gives reviewers a common citable standard in place of taste disputes, and gives the community a way to settle inclusion conflicts by evidence rather than by escalating personal disagreement. It converts an unbounded 'what deserves an article?' argument into a bounded evidentiary lookup.
% TRANSFER_FUNCTION: Redirects editorial attention and article space from self-referential and promotional content toward independently documented topics; discards the unpaid labor invested in articles that fail the threshold rather than transferring it to any seat; and concentrates decision labor (patrolling, debating, closing) on the experienced-editor subset. Money does not move. This reading classifies the discarded-labor flow as filtration cost, not as gain to anyone.
% ABSENT_VOICES: Subjects and authors of deleted articles are notified late or never and lack a practical channel into the process at decision time; knowledge traditions documented outside indexable published sources enter only through the gate's narrow exceptions. Within this reading these are accepted boundary costs of maintaining a quality threshold rather than suppressed dissent — participation in deletion debates is formally open to anyone — but the reading concedes the costs fall on people who never agreed to bear them.
% DISAPPEARANCE_RATIONALE: Without the shared threshold the encyclopedia reverts to unbounded inclusion: promotional and vanity content floods in faster than post-hoc cleanup can remove it, reader trust and donation revenue decline, and experienced editors either exit or reconstruct a replacement criterion within months. Every adjacent arrangement — new-page patrol, draft incubation, search visibility, the Foundation's liability posture — presumes the threshold's existence.
% FOUNDING_PROBLEM: Mid-2000s Wikipedia's open-edit model attracted accelerating volumes of vanity biographies, advertising, hoax articles, and fan content, degrading signal-to-noise and threatening the project's credibility as a reference work. The community formalized a notability threshold (a general evidentiary guideline plus topic-specific supplements) to define, in advance, which topics warrant standalone articles.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: platform-governance research on open-collaboration systems documents quality decay under unfiltered contribution; the spam-filtering and trust-and-safety literature records promotional flooding as the default failure mode of open platforms; comparable projects adopted analogous closure criteria independently; and former contributors who left over strictness — opponents of this reading's remedy — nonetheless attest the underlying flood problem is real. No attesting source depends on the guideline's persistence for funding or standing.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.22: the gate's costs are concentrated in discarded contributor labor and debate hours; this reading counts those as the price of admission to a curated commons (analogous to peer-review rejection), leaving only a thin residue — borderline deletions of later-notable topics, opaque speedy outcomes — as genuine loss. Suppression 0.25: no coercion of persons and abundant off-platform alternatives for any excluded content; what remains is the closed on-wiki hosting option plus enforcement friction. Theater 0.20: the function is largely real (spam demonstrably intercepted), with a mild and rising performative component — policy-citation ritual at deletion debates where outcomes track editor coalitions more than criterion application. Accessibility_collapse 0.50: once understood, the on-wiki alternative ('post it here anyway') collapses completely, while off-wiki publication venues remain fully open — hence moderate, not mountain-grade. Resistance 0.55: persistent and organized — deletion-review overturns, essay cultures challenging strictness, recurrent community-wide requests for comment on threshold reform — yet never regime-threatening. Coordination type information_standard: the primary function is a content-admission standard whose failure mode (arbitrary or unstable criteria) most directly causes the commons problem; the type's low floor (0.02) leaves the small measured excess visible rather than excused. Temporal grid: one shared grid, t=0 corresponds to 2006 (guideline crystallization) through t=20 (2026); all three tracked series are authored at all six points and each terminates at its base_properties scalar. The suppression_requirement series rises (0.10 to 0.25) tracing enforcement industrialization — manual deletion debates, then new-page patrol and curation tooling, then assisted patrolling and draft/incubation pipelines — while the suppression scalar stays low because coercion per actor never rose; machinery intensity and per-person coercion are different quantities. No cyclical dynamics are modeled: debate volume fluctuates seasonally but the tracked metrics show monotone drift, so a flat-to-rising grid is the honest shape.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the creator seat the gate is abrupt and opaque: minutes from creation to removal, rationale delivered as citations to policy pages, effort unrecoverable. From the reader seat the gate is invisible and experienced as simple quality — curation indistinguishable from a well-edited library. From the administrator seat it is procedural due process: every case argued, every close documented. From the Foundation seat it is an aggregate risk ledger. The engine derives per-seat classifications from power, exit, and directionality; the authored rope claim is this reading's stance and does not adjudicate among the seats — divergence there is measurement, not error.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (readership, editor community, Foundation) derive low directionality — subsidized by the arrangement. New topic creators bear real, bounded costs but this reading declines to designate them victims: they are cost-bearers within a fair system, not targets of extraction, and the empty victims array encodes exactly that judgment. Spam and vanity operators hold payer roles but arbitrage-grade exit, which the derivation correctly reads as damping their effective burden — matching the reading's claim that exclusion costs them little. No directionality_overrides are authored: the structural declarations (roles, exits, power atoms) suffice, and adding overrides here would launder the reading's conclusions past the derivation chain. Receipt surface: the extracted quantity is destroyed effort, not transferred value — no named seat accrues it; quality surplus distributes diffusely across readership, and the Foundation's gains are insurance-like byproducts of maintained quality rather than captured extraction, so 'diffuse' is authored as an affirmative checked verdict, not a default. Fixing cost is 'cheap': the guideline is an ordinary editable community page; relaxation and replacement attempts recur routinely, and the binding cost of removal is rebuilding consensus, not access to the lever.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (promotional flooding of an open commons) is live and adaptive, the arrangement retains its primary function, and no sunset applies. The relevant mandatrophy-adjacent risk runs the opposite direction from the usual case: not a dead mandate kept alive theatrically, but a LIVE mandate absorbing ever-more discretionary scope — supplementary guidelines proliferating past the original flood-control purpose — which the rising theater_ratio series tracks. The composition and application-variance omegas are the guards against the specific misclassification danger here: a coordination reading laundering genuine extraction as filtration. The classification prevents the inverse error as well: because victims are structurally undeclared and exits are real, the engine is not invited to read targeted extraction into what this reading holds to be quality maintenance; if the sibling readings' structural data support a harsher verdict over the same referent, the divergence surfaces at the family level, not inside this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates one reading (deletionist) of the contested kernel notability_guidelines: is the standing arrangement a necessary quality filter whose costs are just filtration, or does that characterization depend on adopting this reading''s ontology of the notability boundary?',
    'Cross-reading comparison of the three constraint stories sharing kernel_id notability_guidelines — deletionist (this file), inclusionist_reading, deliberative_reading — assessing which ontology of the boundary the observable record (deleted-page composition, debate outcome distributions) supports.',
    'Classification is rope under this reading; the inclusionist sibling authors a victim set and far higher epsilon over the same referent, computing as snare-or-tangled-rope. Adopting a different reading flips the type with no change in the underlying arrangement — the classification is a property of the reading, routed here rather than hidden in the metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level contest: reading choice, not further evidence about the arrangement alone, selects among incompatible classifications.').

omega_variable(
    deleted_content_composition,
    'Is the deleted-page population actually dominated by spam, vanity, and unverifiable content (justly excluded, as this reading claims), or by sincere marginal-topic articles whose removal imposes uncompensated losses on real contributors?',
    'Stratified sampling of deletion logs with blind classification of deleted content by type and author intent; comparison against the just-exclusion profile this reading assumes.',
    'If sincere losses dominate, the no-victim-set declaration weakens, effective extraction rises above what a filtration-floor account tolerates, and the rope claim loses its coordination-only footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deleted_content_composition, empirical, 'Whether the excluded population matches the justly-excluded profile this reading assumes.').

omega_variable(
    gate_necessity_counterfactual,
    'Does the threshold actually preserve commons quality at its current strictness — would a materially looser criterion degrade reader trust and contributor retention (the necessity premise), or would quality persist under lighter filtering?',
    'Natural experiments: quality and retention metrics across language editions with laxer thresholds, draft-incubation pilots, and historical windows of relaxed enforcement.',
    'If quality survives looser gating, the necessity premise fails, the coordination justification shrinks toward preference, and the residual burden on rejected contributors becomes harder to excuse as filtration cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gate_necessity_counterfactual, empirical, 'Empirical necessity of current strictness to the claimed quality outcome.').

omega_variable(
    enforcement_application_variance,
    'Is the threshold applied uniformly across subject domains, or do deletion outcomes vary systematically by topic area (supplementary guidelines differing sharply in leniency), such that just exclusion describes some domains and not others?',
    'Regression of deletion-debate outcomes and speedy-deletion rates on subject domain, controlling for source availability and article quality.',
    'High domain variance undermines the uniform-fairness premise even within this reading; the just-exclusion claim would hold only domain-locally, complicating the no-victim-set declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_application_variance, empirical, 'Uniformity of gate application across domains as a precondition of the just-exclusion claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_dr_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ng_dr_tr_t0, observed).
narrative_ontology:measurement(ng_dr_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(ng_dr_tr_t4, observed).
narrative_ontology:measurement(ng_dr_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(ng_dr_tr_t8, observed).
narrative_ontology:measurement(ng_dr_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(ng_dr_tr_t12, observed).
narrative_ontology:measurement(ng_dr_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(ng_dr_tr_t16, observed).
narrative_ontology:measurement(ng_dr_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(ng_dr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ng_dr_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ng_dr_be_t0, observed).
narrative_ontology:measurement(ng_dr_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(ng_dr_be_t4, observed).
narrative_ontology:measurement(ng_dr_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.18).
narrative_ontology:measurement_basis(ng_dr_be_t8, observed).
narrative_ontology:measurement(ng_dr_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement_basis(ng_dr_be_t12, observed).
narrative_ontology:measurement(ng_dr_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement_basis(ng_dr_be_t16, observed).
narrative_ontology:measurement(ng_dr_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(ng_dr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ng_dr_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(ng_dr_su_t0, observed).
narrative_ontology:measurement(ng_dr_su_t4, notability_guidelines__deletionist_reading, suppression_requirement, 4, 0.14).
narrative_ontology:measurement_basis(ng_dr_su_t4, observed).
narrative_ontology:measurement(ng_dr_su_t8, notability_guidelines__deletionist_reading, suppression_requirement, 8, 0.17).
narrative_ontology:measurement_basis(ng_dr_su_t8, observed).
narrative_ontology:measurement(ng_dr_su_t12, notability_guidelines__deletionist_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(ng_dr_su_t12, observed).
narrative_ontology:measurement(ng_dr_su_t16, notability_guidelines__deletionist_reading, suppression_requirement, 16, 0.23).
narrative_ontology:measurement_basis(ng_dr_su_t16, observed).
narrative_ontology:measurement(ng_dr_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement_basis(ng_dr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, deliberative_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'WP:N' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories sharing kernel notability_guidelines. This file instantiates deletionist_reading: the gate as necessary quality filter, low epsilon, beneficiary set, no victim set. The sibling inclusionist_reading authors a victim set and substantially higher epsilon over the SAME referent (the standing arrangement), computing as snare-or-tangled-rope; deliberative_reading authors a process-defined classification centered on perpetual AfD negotiation. The epsilon values differ because each reading assesses the shared referent by its own lights (OQ-26); the referent does not move between files. Edges here implement the family linkage; neither sibling file is described inside this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
