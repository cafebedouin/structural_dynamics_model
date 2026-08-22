% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: WP:N Notability Threshold (Deletionist Reading): Necessary Epistemic Quality Filter
 *   domain: digital_commons/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's notability guideline (WP:N) requires that a subject have
 *   significant coverage in independent, reliable, secondary sources before
 *   it merits a standalone article; the deletion machinery (speedy deletion,
 *   proposed deletion, articles-for-deletion) enforces the requirement by
 *   removing non-conforming content. This story instantiates the DELETIONIST
 *   READING of that arrangement: the threshold as a necessary epistemic
 *   quality filter that prevents commons degradation, whose exclusions are
 *   the filter working rather than anyone being wronged. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   notability regime as this reading assesses it — low, near coordination
 *   cost — not for the regime as a rival reading would assess it. The sibling
 *   readings (inclusionist, deliberative) are separate constraints linked
 *   through the network and through cs_structure.reading_relations; the
 *   contest between readings is routed to omega variables, not folded into
 *   this file. KEY AGENTS (by structural relationship): -
 *   encyclopedia_readership: Primary beneficiary (moderate/mobile) — receives
 *   the quality baseline, pays nothing directly - volunteer_editor_community:
 *   Dual-positioned beneficiary/payer (organized/constrained) — gains a
 *   shared decision standard, loses deleted labor -
 *   afd_administrators_and_closers: Agenda setter (institutional/constrained)
 *   — administers the standard and could revise it -
 *   rejected_topic_contributors: Cost-bearing contributors (powerless/mobile)
 *   — lose submitted work, exit to open platforms -
 *   marginalized_domain_scholars: Excluded voice (moderate/trapped) — objects
 *   from outside the deletion process - independent_content_platforms:
 *   Incidental beneficiary (powerful/arbitrage) — absorbs excluded topics and
 *   traffic - wiki_governance_researchers: Analytical observer
 *   (analytical/analytical) — measures the filter's effects
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.18).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.3).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N Notability Threshold (Deletionist Reading): Necessary Epistemic Quality Filter").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '8a8df198-82fa-4461-b863-ed90bbf2dfcc').
narrative_ontology:cs_kernel_codification('8a8df198-82fa-4461-b863-ed90bbf2dfcc', formalized).
narrative_ontology:cs_authority_grounding('8a8df198-82fa-4461-b863-ed90bbf2dfcc', practice).
narrative_ontology:cs_interpretation_layer_present('8a8df198-82fa-4461-b863-ed90bbf2dfcc').
narrative_ontology:cs_reading_relation('8a8df198-82fa-4461-b863-ed90bbf2dfcc', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a8df198-82fa-4461-b863-ed90bbf2dfcc', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('8a8df198-82fa-4461-b863-ed90bbf2dfcc', foundational, notability_threshold_necessary_for_commons_integrity).
narrative_ontology:cs_axiom_status(notability_threshold_necessary_for_commons_integrity, holdable).
narrative_ontology:cs_axiom_grounding('8a8df198-82fa-4461-b863-ed90bbf2dfcc', notability_threshold_necessary_for_commons_integrity, instrumental).
narrative_ontology:cs_axiom('8a8df198-82fa-4461-b863-ed90bbf2dfcc', foundational, insufficient_independent_coverage_warrants_exclusion).
narrative_ontology:cs_axiom_status(insufficient_independent_coverage_warrants_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('8a8df198-82fa-4461-b863-ed90bbf2dfcc', insufficient_independent_coverage_warrants_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('8a8df198-82fa-4461-b863-ed90bbf2dfcc', source_grounded_inclusion_threshold).
narrative_ontology:cs_drift_state('8a8df198-82fa-4461-b863-ed90bbf2dfcc', contemporary_ai_content_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8a8df198-82fa-4461-b863-ed90bbf2dfcc', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, volunteer_editor_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, independent_content_platforms).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, volunteer_editor_community).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, rejected_topic_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read the encyclopedia as a daily reference, relying on its promise that included subjects meet a shared evidentiary bar. They receive the quality baseline the standard maintains and pay nothing directly; if dissatisfied they can consult other reference works, though habit and search-engine placement make switching rare.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, encyclopedia_readership, beneficiary,
    moderate, biographical, mobile, global).

% Create, expand, and patrol articles under the written standard. The criterion gives them a shared basis for inclusion decisions that precedes personal discretion disputes, and long-term contributors often describe the project's mission in identity-forming terms. When an article they built is nominated and deleted, their unpaid labor is lost; some respond by targeting safer topics, others by reducing activity or leaving.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editor_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, volunteer_editor_community, payer).

% Experienced editors with administrative tools who run deletion discussions, judge consensus, and operate the speedy-deletion and proposed-deletion routes alongside full discussions. They maintain the guideline pages themselves and could initiate revision through the community's normal editing process. What flows to them is standing within the project — closure records and respect — not money; what flows from them is the day-to-day operating force of the standard.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, afd_administrators_and_closers, agenda_setter,
    institutional, generational, constrained, global).

% Arrive wanting an article about a business, hobby project, band, or acquaintance, learn at a deletion discussion that independent reliable coverage is required, and lose the work they invested. Most publish instead on blogs, fan wikis, or social platforms, where no equivalent bar applies; few return to test the standard a second time.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, rejected_topic_contributors, payer,
    powerless, immediate, mobile, global).

% Study or document fields — pre-digital women scientists, Global South history, oral traditions — where the press coverage and published sources the standard demands were never produced because of upstream biases in publishing. They cannot conjure sources into existence, their subjects remain unrepresented in the world's default reference work, and their objections appear mostly in academic literature rather than in the deletion discussions themselves.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, marginalized_domain_scholars, excluded,
    moderate, generational, trapped, continental).

% Operate fan wikis, niche encyclopedias, and long-form publishing venues that receive topics and traffic the encyclopedia turns away. They benefit from the standard's exclusions without administering them, and their continued existence is the standing demonstration that rejected material has somewhere else to go.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, independent_content_platforms, beneficiary,
    powerful, generational, arbitrage, global).

% Analyze deletion logs, discussion archives, and editor attrition to measure what the standard keeps out and what it protects. They publish findings that both camps cite, hold no vote in deletion discussions, and can abandon the research question at will.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wiki_governance_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining signal-to-noise in a reference work that anyone may edit anonymously: a shared, source-grounded criterion for which subjects merit communal editorial investment, applied before effort is sunk into content the community cannot verify or maintain.
% TRANSFER_FUNCTION: Moves editorial attention and the project's credibility budget away from unverifiable, self-interested, or ephemeral content toward verifiable subjects; incidentally moves the labor of rejected contributors out of the commons entirely, since deleted work is destroyed rather than redirected.
% ABSENT_VOICES: Scholars and community historians of poorly covered domains rarely participate in deletion discussions; their objection — that the standard imports Anglophone commercial-media bias into what counts as real — is voiced mainly off-platform in research literature. First-time contributors whose articles are deleted usually leave without articulating any grievance at all.
% DISAPPEARANCE_RATIONALE: Without a notability criterion, inclusion decisions revert to raw editor preference and whoever edits longest. Predictable waves of vanity pages, promotional content, and machine-generated filler would enter faster than patrol capacity absorbs them; the quality signal that readers and search engines rely on dilutes; and the community would either reconstruct a threshold within months or fragment into separately governed projects.
% FOUNDING_PROBLEM: Early in the project's life the wiki accumulated indiscriminate content — vanity biographies, advertising, hobbyist cruft — that threatened the goal of being a credible encyclopedia. The notability guideline was codified around 2005-2006 to define which subjects merit articles at all, complementing the verifiability policy which governs what may be said about them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: platform-governance researchers documenting the industrialization of search-engine-optimization spam, analyses of machine-generated content floods in open repositories, and archivists' accounts of junk-content burdens in ungated collections. The inclusionist wing of the academic literature disputes the problem's current salience and the standard's design, but attests that the spam-and-quality problem the guideline was built for was real.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.18) because this reading holds the standard's costs sit near coordination cost: the criterion is cheap to apply, its decisions are procedurally reviewable, and the residue of over-deletion (borderline topics, newcomer attrition) is the reading's admitted imperfection, priced into the value rather than denied. Suppression (0.30) is authored as a raw structural property, unscaled by power or scope: enforcement is real (work is destroyed against its maker's wishes) but alternatives are abundant and unsuppressed — rejected material flows to open platforms, which is precisely why this reading denies a victim set. Theater_ratio (0.15) is low because the process performs substantive filtering work; the slow growth in the series tracks boilerplate !voting and policy-citation ritual accreting around a functional core. Accessibility_collapse (0.35) is moderate-low: understanding the standard does not collapse alternatives, it channels contribution toward viable topics and leaves external publishing untouched. Resistance (0.40) is real but mostly internal — inclusionist editors, recurring reform proposals, and newcomer anger at first-article deletion — rather than organized external opposition; rejected contributors are diffuse and mobile, so coalition formation among them is weak, and the resistance that exists comes largely from inside the community. The suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: ad hoc early deletions gave way to a proceduralized apparatus (AfD, PROD, CSD refinement) whose suppressive force rose as it matured and then stabilized in the backlog era. All three series share one time grid, and each metric's terminal value equals its base_properties scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the closer's chair the arrangement is stewardship: applying a shared standard to protect a common work. From the rejected contributor's chair the same discussion is the destruction of their labor, however procedurally fair. From the readership's chair it is invisible infrastructure behind a trustworthy reference. The engine computes per-seat classifications from power, exit, and role; this file's rope claim is the reading's own assessment, and any divergence between the computed payer-seat type and that claim is measurement, not error. Same-level differentiation is also present: two moderate-power contributors with equal standing meet opposite fates depending solely on their subject's relationship to the criterion — the constraint-specific factor, not global power, drives their divergent experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the readership receives the quality baseline at zero direct cost, and the editor community's net position is favorable (the standard reduces discretion disputes it would otherwise fight constantly), though its dual payer role tempers that. The closer seat sits near symmetric: it expends real labor operating the standard and collects standing rather than material rent. Rejected contributors derive elevated directionality — they bear the arrangement's concentrated costs — but their mobile exit dampens effective extraction, which is exactly the structural fact this reading leans on when it denies a victim set. No victims[] array is declared because this reading asserts no unjustly burdened set; the cost-bearing parties are nonetheless named honestly as payer-role stakeholders so the engine can compute whatever their position implies. Marginalized-domain scholars are authored as excluded, commentary-grade only: their objection is recorded, not adjudicated here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — indiscriminate content degrading an open commons — is live and corroborated from outside the benefiting parties, so no mandatrophy is declared and the constraint is not drifting toward inertial performance. The classification discipline cuts both ways here. Against the snare reading: a snare requires suppressed victims, and the structural record shows unsuppressed exits (open platforms absorb rejected material) and no seat capturing the extraction — gains accrue diffusely to the readership as a public good, which is why gain_flow is authored as an affirmatively checked 'diffuse'. Against the piton reading: the filter demonstrably performs its function (active deletion logs, measurable quality effects), so theatrical maintenance is not the story. The residual risk this reading must price is over-deletion error (see the overdeletion_error_rate omega): if the filter's false-positive rate proved high and sustained, the no-victim-set assertion would erode and the classification would migrate toward tangled_rope even under this reading's own lights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'This constraint is one reading of the notability_guidelines kernel — the deletionist_reading. Would instantiating the inclusionist_reading or deliberative_reading instead change the constraint''s structural identity outright?',
    'Compile the sibling stories and compare: victim-set presence, epsilon, enforcement basis, and the engine''s per-seat classifications across readings. The cross-reading classification spread is itself the resolution data.',
    'The inclusionist instantiation declares marginalized-knowledge victims and raises epsilon substantially (snare or tangled_rope territory); the deliberative instantiation relocates persistence in ongoing negotiation and may soften the enforcement profile. This file''s rope classification holds only under the deletionist framing — it is not a claim about the kernel as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings are separate constraints, not internal hedges.').

omega_variable(
    coverage_value_correlation,
    'Does significant coverage in independent reliable sources actually correlate with encyclopedic value — the empirical premise on which this reading''s necessity claim rests?',
    'Longitudinal audits comparing deletion outcomes against later scholarly recognition, citation uptake, and post-hoc notability of deleted subjects, controlling for topic domain and coverage era.',
    'A weak correlation would show the filter routinely discards valuable knowledge, raising effective extraction above this reading''s estimate and destabilizing the rope claim toward tangled_rope; a strong correlation would confirm the filter as low-cost coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_value_correlation, empirical, 'Whether the source-coverage proxy tracks the value it claims to select for.').

omega_variable(
    necessity_natural_or_constructed,
    'Is the notability threshold a structural necessity of any large open collaborative commons (approaching natural-law status for this domain), or a constructed policy choice that a differently constituted community could replace?',
    'Comparative analysis of alternative commons-governance regimes — specialist wikis, federated moderation experiments, reputation-weighted inclusion systems — that sustain quality without a notability-style threshold.',
    'If constructed and replaceable, the constraint persists by community choice and sits firmly in rope/scaffold territory; if structurally necessary for open commons at scale, it approaches mountain-like standing within this domain, changing how its resistance and accessibility_collapse values should be read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_natural_or_constructed, conceptual, 'Whether the ''necessary filter'' claim describes natural structure or an enacted policy.').

omega_variable(
    overdeletion_error_rate,
    'What fraction of deleted subjects later satisfy the very threshold used to delete them?',
    'Resurrection-rate study: sample deleted articles across eras, reapply the current guideline with hindsight-available sourcing, and measure the pass rate.',
    'Sustained resurrection rates above a modest band would quantify this reading''s admitted over-filtering cost, force epsilon upward, and complicate the no-victim-set assertion — the people whose later-notable work was destroyed begin to look burdened rather than justly filtered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdeletion_error_rate, empirical, 'The error rate of the filter as applied, measurable from deletion logs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_deletionist_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ng_deletionist_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(ng_deletionist_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(ng_deletionist_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(ng_deletionist_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(ng_deletionist_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(ng_deletionist_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(ng_deletionist_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(ng_deletionist_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(ng_deletionist_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(ng_deletionist_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.17).
narrative_ontology:measurement(ng_deletionist_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(ng_deletionist_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ng_deletionist_su_t4, notability_guidelines__deletionist_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(ng_deletionist_su_t8, notability_guidelines__deletionist_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(ng_deletionist_su_t12, notability_guidelines__deletionist_reading, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(ng_deletionist_su_t16, notability_guidelines__deletionist_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(ng_deletionist_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, verifiability_policy).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, reliable_sources_guideline).

% DUAL FORMULATION NOTE:
% This story is one member of a constraint family decomposing the colloquial label 'WP:N' per the epsilon-invariance principle. The label covers at least three structurally distinct claims: (1) this deletionist reading — the threshold as necessary epistemic quality filter, low epsilon, no unjustly burdened set; (2) the inclusionist reading — the same standing arrangement assessed as a gatekeeping apparatus with identifiable excluded victims and substantially higher epsilon; (3) the deliberative reading — the arrangement as a perpetual negotiation process whose persistence rests on ongoing AfD deliberation rather than on the criterion's content. The referent of epsilon is identical across the family (the standing notability regime); the values diverge because each reading assesses that referent by its own lights. Each reading is a separate file with its own claimed_type, metrics, and stakeholders; this file links to its siblings via affects_constraints and via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
