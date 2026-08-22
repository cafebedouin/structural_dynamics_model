% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: WP:N Notability Gate — Deletionist Reading (Epistemic Quality Filter)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the DELETIONIST READING of the WP:N notability
 *   kernel: the standard as a necessary epistemic quality filter that
 *   prevents commons degradation. On this reading, requiring significant
 *   coverage in reliable, independent secondary sources before a topic merits
 *   an article is a pre-committed admission rule that solves a genuine
 *   collective-action problem — open editing otherwise drowns the
 *   encyclopedia in promotional, vanity, and unverifiable content. The
 *   arrangement operates through Articles for Deletion (AfD), speedy-deletion
 *   criteria, proposed deletion, draftspace, and salting of repeatedly
 *   recreated titles. Assumptions stated: the interval maps T0 to roughly
 *   2005 (WP:N's consolidation as an enforced guideline) and T20 to roughly
 *   2025; the metric series run on one shared six-point grid. Per the
 *   committer-frame rules, only this reading is authored here — the
 *   inclusionist and deliberative readings are separate constraint files
 *   linked via network.affects_constraints, and the contest between readings
 *   is carried entirely in omega variables. KEY AGENTS (by structural
 *   relationship): - readership: Primary beneficiary (powerless/mobile) —
 *   consumes the filtered commons, holds no governance seat -
 *   volunteer_editor_community: Beneficiary/payer (organized/identity_locked)
 *   — performs the filtering labor, net gainer but heavily invested -
 *   afd_administrators: Agenda setter (institutional/constrained) — applies
 *   and enforces the standard, collects no rents -
 *   marginal_topic_contributors: Primary cost-bearer (moderate/constrained) —
 *   good-faith contributors whose topics fail the coverage test -
 *   spam_and_vanity_promoters: Excluded party (powerless/mobile) — the
 *   population the filter exists to exclude; abundant external alternatives -
 *   wikimedia_foundation: Platform steward-observer (institutional/arbitrage)
 *   — hosts infrastructure, abstains from content adjudication -
 *   media_commons_researchers: Analytical observer (analytical/analytical) —
 *   audits deletion outcomes and coverage gaps
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.16).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.2).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N Notability Gate — Deletionist Reading (Epistemic Quality Filter)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'c5eb8188-0071-4296-bd49-5a2bc235aeed').
narrative_ontology:cs_kernel_codification('c5eb8188-0071-4296-bd49-5a2bc235aeed', formalized).
narrative_ontology:cs_authority_grounding('c5eb8188-0071-4296-bd49-5a2bc235aeed', practice).
narrative_ontology:cs_interpretation_layer_present('c5eb8188-0071-4296-bd49-5a2bc235aeed').
narrative_ontology:cs_reading_relation('c5eb8188-0071-4296-bd49-5a2bc235aeed', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5eb8188-0071-4296-bd49-5a2bc235aeed', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('c5eb8188-0071-4296-bd49-5a2bc235aeed', foundational, significant_independent_coverage_required).
narrative_ontology:cs_axiom_status(significant_independent_coverage_required, holdable).
narrative_ontology:cs_axiom_grounding('c5eb8188-0071-4296-bd49-5a2bc235aeed', significant_independent_coverage_required, instrumental).
narrative_ontology:cs_axiom('c5eb8188-0071-4296-bd49-5a2bc235aeed', secondary, collective_signal_priority_over_inclusion_interests).
narrative_ontology:cs_axiom_status(collective_signal_priority_over_inclusion_interests, holdable).
narrative_ontology:cs_axiom_grounding('c5eb8188-0071-4296-bd49-5a2bc235aeed', collective_signal_priority_over_inclusion_interests, deontological).
narrative_ontology:cs_reference_frame('c5eb8188-0071-4296-bd49-5a2bc235aeed', independent_coverage_admission_standard).
narrative_ontology:cs_drift_state('c5eb8188-0071-4296-bd49-5a2bc235aeed', post_generative_ai_content_surge, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5eb8188-0071-4296-bd49-5a2bc235aeed', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, volunteer_editor_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, volunteer_editor_community).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, marginal_topic_contributors).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, independent_secondary_source_principle).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, encyclopedic_signal_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the encyclopedia and relies on its articles being about things that matter and can be checked. Receives a predictable signal-to-noise ratio without performing any filtering work. Has no vote in deletion discussions and no formal seat in policy-making; if dissatisfied, switches to another reference source in seconds.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, readership, beneficiary,
    powerless, biographical, mobile, global).

% Writes, sources, and polices articles; nominates failing topics for deletion and defends passing ones. Gains a workspace where effort spent improving an article is not drowned by promotional noise. Pays with thousands of unpaid hours, much of it consumed by deletion review. Long-tenured members describe editing as part of who they are; leaving would mean abandoning a community and a body of work bound up with their sense of self.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editor_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, volunteer_editor_community, payer).

% Close deletion debates, apply the notability standard and its subject-specific offshoots, delete or keep pages accordingly, and block re-creation of repeatedly deleted topics. Collect no payment; their standing rests on being seen to apply the written standard faithfully. Stepping back means surrendering advanced permissions and a role in daily governance.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, afd_administrators, agenda_setter,
    institutional, biographical, constrained, global).

% Create articles on local musicians, minor academics, neighborhood institutions, and niche hobbies. Discover at deletion review that their topic lacks the required independent written coverage, and see the work removed. Can republish on fan wikis or personal sites but loses the audience, permanence, and search visibility the encyclopedia provides.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, marginal_topic_contributors, payer,
    moderate, immediate, constrained, regional).

% Seek free promotion for bands, businesses, resumes, and causes. The admission standard bars exactly what they submit, and automated filters and rapid-deletion routes remove it within minutes. They face no penalty beyond removal and can promote anywhere else on the web immediately.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_and_vanity_promoters, excluded,
    powerless, immediate, mobile, global).

% Hosts the platform and funds research but deliberately leaves content admission to volunteer governance. Its strategy documents call for broadening coverage of underserved regions and topics, which sits in tension with a stricter admission standard; it nonetheless refrains from overriding community policy.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikimedia_foundation, observer,
    institutional, generational, arbitrage, global).

% Study deletion logs, citation patterns, and demographic gaps in coverage. Publish findings on which topics get removed and who writes them. Neither gains nor loses from any particular admission decision; their analyses feed both stricter and looser reform proposals.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, media_commons_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commons-quality collective-action problem of open editing: without a shared admission standard, promotional, vanity, and unverifiable content floods in and degrades the signal for everyone. WP:N pre-commits the community to one criterion — significant coverage in reliable, independent secondary sources — so individual editors do not relitigate every inclusion fight from scratch, and scarce editorial attention concentrates on topics with independent evidentiary grounding.
% TRANSFER_FUNCTION: Moves editorial attention and page space toward topics with independent secondary coverage, and away from self-promotional and unverifiable submissions. Costs fall on contributors of marginal topics (lost work and reach) and on the volunteers who staff deletion review; benefits flow to the readership and to editors relying on the quality baseline. Promotional actors are denied free access to the platform's reach.
% ABSENT_VOICES: Spam and vanity promoters are deliberately excluded — their exclusion is the standard's designed function, and their objection carries no standing here. Readers number in the hundreds of millions yet hold no formal seat in deletion processes. Subjects of deleted topics participate only reactively, after nomination. Keepers of oral-culture and Global South knowledge whose topics lack written independent coverage were absent from the standard's design conversations and remain structurally unheard; they sit outside the talk pages, in the citation gap.
% DISAPPEARANCE_RATIONALE: If the standard and its enforcement vanished overnight, promotional and synthetic content would flood the article space within days, watchlists would overflow, search engines would demote the site, and burned-out editors would either build ad-hoc private filters or leave — the commons would rearrange around whatever replacement admission norm emerged, at large transitional cost.
% FOUNDING_PROBLEM: Early Wikipedia (roughly 2001-2005) was accumulating vanity pages, band advertisements, resume entries, and unverifiable claims faster than volunteers could remove them; the community needed a shared, pre-committed line between 'encyclopedic' and 'everything editable' so that quality did not depend on endless case-by-case combat.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: platform-governance and spam-filtering research literatures document the promotional-content pressure on open wikis; search-quality analyses treat Wikipedia's reliability as conditional on admission screening; and — decisively — the inclusionist opposition itself concedes the spam and vanity problem is real, disputing the remedy's incidence rather than the problem's existence. Adversarial corroboration of this strength is unusual and weights the finding heavily.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.16) because the standard's costs fall overwhelmingly on would-be promoters whose exclusion is the designed function, and on volunteers who pay voluntarily; the residual cost — good-faith contributors of marginally-covered topics losing work — is real but buffered by alternatives-to-deletion (userfication, draftspace, movement to fan wikis). Suppression is low-moderate (0.20): within-project enforcement has hardened (speedy-deletion criteria, salting, title locks), but external publishing venues remain fully open, so no one is compelled to remain inside the arrangement. Theater is low (0.20): AfD carries ritual (badge norms, vote-counting idioms) but closures predominantly track source analysis. Accessibility collapse is moderate (0.40): once WP:N is understood, the option 'host marginal content here' closes completely, but parallel venues remain untouched — the partial-collapse profile of a working standard rather than a natural limit. Resistance is substantial (0.60): inclusionist counter-mobilization, mass keep-voting, essay literature, and recurring proposals to weaken or bypass the standard are organized and continuous. Claim/metric independence is preserved: 'rope' is authored from this reading's structure (genuine coordination function, net beneficiaries, unsuppressed alternatives); the metrics describe observed operation; any divergence between claim and computed type is the engine's measurement, not an error to reconcile. Note on scaling: suppression is authored as a raw structural property and is not scaled; extractiveness is scaled by the engine from directionality and the global spatial scope (larger scope modestly amplifies effective extraction through verification difficulty). The temporal series show mild monotonic drift — enforcement maturation, not ratcheting extraction — with no oscillation requiring cyclical treatment.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience this arrangement differently despite sharing one community. The readership seat computes near-full subsidy: maximal benefit, zero participation burden, instant exit. The editor seat is genuinely dual: net beneficiary of the quality baseline, yet identity-locked — the engine weighs lock toward the target side even for willing participants, because locked agents cannot price their own contribution accurately. The marginal-contributor seat bears the arrangement's sharpest concentrated cost with constrained exit, computing the highest effective burden. The administrator seat sits mid-structure: process stewardship without rent. Same-level lateral dynamics matter here: deletionist and inclusionist editors hold nominally identical standing, and the constraint differentiates them by exit profile and identity investment rather than by formal rank — the faction that experiences the standard as protective tends to be the faction whose identity is fused with the project.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (readership, volunteer_editor_community) derive low directionality — the arrangement subsidizes them. No victim set is declared, per this reading's structure: the excluded populations are judged justly excluded, not extracted-from. Marginal_topic_contributors and spam_and_vanity_promoters nevertheless derive high directionality from their cost-bearing positions; the crucial asymmetry is exit: spam promoters hold arbitrage-grade exit (instant relocation to the open web), placing them nearest the beneficiary end among cost-bearers, while marginal contributors are constrained (lost reach and permanence), holding them nearer the full-target end. Identity lock pulls the editor community slightly toward the target end despite net benefit — flagged here rather than overridden, because the derivation captures the structural fact that locked agents bear costs they cannot exit. No directionality overrides are authored: the beneficiary/victim-plus-exit derivation produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and strengthening: promotional pressure predates the standard and the generative-AI content surge has multiplied it. Mandatrophy is therefore NOT resolved, and the classification guards both failure directions. Against snare mislabeling: costs are diffuse and largely voluntary, the enforcing population collects no rents, and the founding problem's existence is corroborated by the arrangement's own adversaries — a captured extraction apparatus rarely enjoys adversarial corroboration. Against mountain mislabeling: the standard is constructed, written, revisable through consensus, and meets organized resistance — nothing natural-law-like attaches to it. The drift risk this reading actually faces is inertial: if AfD volume collapsed into rubber-stamp ritual while spam pressure persisted, the arrangement would persist nominally while the filter stopped functioning — the theater_ratio series is tracked precisely to date any such transition, and the SNG-proliferation omega watches the procedural-burden pathway that would precede it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (deletionist_reading) of the notability_guidelines kernel. Do the sibling readings (inclusionist_reading, deliberative_reading) instantiate structurally different constraints, and where exactly does the disagreement bite?',
    'Corpus-level comparison of the sibling stories'' epsilon values, beneficiary/victim sets, and computed types; adversarial collaboration between deletionist and inclusionist editor cohorts on shared deletion-outcome data.',
    'Adopting the inclusionist reading would raise epsilon sharply, add victim sets (marginalized knowledge communities), and shift classification toward enforced extraction; adopting the deliberative reading would lower suppression, reframe enforcement as process-legitimate, and dissolve the fixed-standard framing. The three readings are different constraints, not one constraint viewed from angles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the WP:N kernel is instantiated determines the constraint''s entire beneficiary/victim structure and classification.').

omega_variable(
    epsilon_referent_standing_arrangement,
    'Is epsilon authored over the standing arrangement under contest — WP:N as actually enforced through AfD, speedy deletion, and salting — rather than over the deletionist reading''s endorsed ideal filter?',
    'Re-audit of the epsilon referent against the OQ-26/OQ-258 rulings: values must describe the existing arrangement as this reading assesses it, never the reading''s preferred alternative.',
    'If epsilon were authored over the ideal filter it would collapse toward zero and the story would measure nothing; the authored 0.16 prices the real enforcement arrangement''s residual costs (good-faith marginal deletions, consumed volunteer labor) by deletionist lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_standing_arrangement, conceptual, 'Epsilon referent discipline: the arrangement under contest, not the endorsed alternative.').

omega_variable(
    just_exclusion_completeness,
    'The deletionist reading declares no victim set. Is that declaration complete, or do good-faith contributors of marginally-covered topics constitute a persistently harmed class that this reading''s own lights discount?',
    'Longitudinal tracking of deleted-article authors: return rates, destination venues, and stated grievance, separated from promoter-authored deletions to distinguish just exclusion from collateral cost-bearing.',
    'If a distinct harmed class of good-faith contributors is confirmed, the structure acquires asymmetric cost-bearing alongside its coordination function and classification migrates toward the hybrid coordination/extraction type; if deletions concentrate on promotional and vanity submissions, the no-victim declaration stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_exclusion_completeness, empirical, 'Whether the reading''s no-victim structure survives contact with author-level deletion outcome data.').

omega_variable(
    sng_proliferation_drift,
    'Are the subject-specific notability offshoots (SNGs) drifting from the core independent-coverage standard into interest-group gatekeeping that raises procedural burden without raising quality?',
    'Compare deletion rates and source-analysis depth for AfDs closed on SNG grounds versus core-policy grounds across the interval; measure overlap between SNG authorship and affected WikiProject constituencies.',
    'Confirmed drift would raise theater_ratio and the effective burden on niche-topic contributors, dating a transition away from the flat-to-mildly-rising trajectories authored here; refutation supports them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sng_proliferation_drift, empirical, 'Interpretive-layer health check: whether the sub-guidelines absorb drift or manufacture it.').

omega_variable(
    ai_surge_enforcement_capacity,
    'Does the post-generative-AI surge in promotional and synthetic content permanently strengthen the case for the strict admission standard, or overwhelm volunteer enforcement capacity until the filter fails operationally?',
    'Track AfD backlog, deletion latency, and the share of machine-generated submissions surviving initial review from 2023 onward.',
    'A strengthened case consolidates this reading''s reference frame (revival confirmed); overwhelmed capacity produces enforcement decay, rising theater, and an arrangement that persists nominally while the filter stops functioning — the inertial endpoint this reading most fears.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_surge_enforcement_capacity, empirical, 'External shock trajectory: whether the filter''s founding problem strengthens or outruns its enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(nota_tr_t0, observed).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement_basis(nota_tr_t4, observed).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement_basis(nota_tr_t8, observed).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(nota_tr_t12, observed).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(nota_tr_t16, observed).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(nota_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(nota_be_t0, observed).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.12).
narrative_ontology:measurement_basis(nota_be_t4, observed).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(nota_be_t8, observed).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement_basis(nota_be_t12, observed).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement_basis(nota_be_t16, observed).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement_basis(nota_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(nota_su_t0, observed).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deletionist_reading, suppression_requirement, 4, 0.09).
narrative_ontology:measurement_basis(nota_su_t4, observed).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deletionist_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement_basis(nota_su_t8, observed).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deletionist_reading, suppression_requirement, 12, 0.17).
narrative_ontology:measurement_basis(nota_su_t12, observed).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deletionist_reading, suppression_requirement, 16, 0.19).
narrative_ontology:measurement_basis(nota_su_t16, observed).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(nota_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, reliable_sources_guideline).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'WP:N' covers at least three structurally distinct claims that cannot share one story, because measuring the arrangement by different observables yields different epsilon values. This file authors the deletionist_reading (quality filter; low epsilon; readership as beneficiary; no victim set). inclusionist_reading authors the same text as a gatekeeping apparatus (high epsilon; marginalized knowledge communities as victims). deliberative_reading authors it as a negotiation process (epsilon indexed to process legitimacy). The deletionist reading is upstream historically: its enforcement practice generated the deletion record that the inclusionist reading cites as evidence and that the deliberative reading treats as the point of the arrangement. All family members are linked via network.affects_constraints; the reliable_sources_guideline edge records the dependency whereby this standard's operative force borrows the definition of 'reliable, independent source'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
