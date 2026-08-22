% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__censorship_mechanism_reading, []).

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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: Article 17 Erasure Right - Censorship Mechanism Reading
 *   domain: technological/legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Article 17 erasure right as enacted (codified in 2016, applicable
 *   from T4=2018, preceded by the T0=2014 search-deindexing judgment that
 *   established the practice). THIS file generates the
 *   censorship_mechanism_reading: the claim that the erasure pipeline
 *   operates as a privatized prior-restraint substitute, in which unilateral
 *   requests, a one-month compliance clock, and catastrophic fine asymmetry
 *   induce controllers to remove lawful public-interest content without
 *   notice to or hearing for its authors. The sibling readings
 *   (privacy_fundamental_reading, competitive_moat_reading) are OTHER
 *   constraints in OTHER files with their own epsilon values, victim sets,
 *   and classifications; per epsilon-invariance they are not averaged into
 *   this story. The referent of epsilon here is the standing erasure
 *   arrangement as this reading assesses it, never the safeguarded regime
 *   this reading would prefer. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (genuine privacy coordination
 *   retaining an operative suppression function) while the authored metrics
 *   describe heavily extractive, actively enforced operation; the engine
 *   measures that divergence rather than the author reconciling it. KEY
 *   AGENTS (by structural relationship): strategic_erasure_requesters:
 *   primary beneficiary (organized/mobile) - subsidized seat, files
 *   unilaterally at near-zero cost; reputation_management_firms: secondary
 *   beneficiary (organized/mobile) - industrializes the request stream;
 *   large_platform_controllers: administering intermediary with cost-bearing
 *   side (institutional/constrained) - executes removals under fine threat;
 *   supervisory_authorities: agenda setter (institutional/analytical) - sets
 *   enforcement posture, hears only requester-side complaints;
 *   news_publishers: primary target (organized/constrained) - archive reach
 *   collapses under deindexing; independent_journalists: primary target
 *   (moderate/identity_locked) - byline-bound, no monitoring capacity;
 *   digital_archivists: secondary target (moderate/constrained) -
 *   preservation fragmented by jurisdiction; public_information_seekers:
 *   diffuse target (moderate/constrained) - filtered results with no
 *   visibility into filtering; erased_speakers: excluded voice
 *   (moderate/trapped) - no procedural seat, no notice; cjeu_courts:
 *   analytical observer (institutional/analytical) - redraws exception
 *   boundaries without touching the clock or the fine asymmetry.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.72).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.78).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Erasure Right - Censorship Mechanism Reading").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technological/legal/political").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '0ef4ddc4-fc52-40dc-86ef-73cd132f99c6').
narrative_ontology:cs_kernel_codification('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', fixed_text).
narrative_ontology:cs_authority_grounding('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', lineage).
narrative_ontology:cs_interpretation_layer_present('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6').
narrative_ontology:cs_reading_relation('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', foundational, speech_suppression_is_operative_function).
narrative_ontology:cs_axiom_status(speech_suppression_is_operative_function, holdable).
narrative_ontology:cs_axiom_grounding('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', speech_suppression_is_operative_function, empirically_contingent).
narrative_ontology:cs_axiom('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', foundational, erasures_constitute_prior_restraint).
narrative_ontology:cs_axiom_status(erasures_constitute_prior_restraint, holdable).
narrative_ontology:cs_axiom_grounding('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', erasures_constitute_prior_restraint, deontological).
narrative_ontology:cs_reference_frame('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', narrow_adjudicated_erasure).
narrative_ontology:cs_drift_state('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', contemporary_strategic_request_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ef4ddc4-fc52-40dc-86ef-73cd132f99c6', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, news_publishers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, independent_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_information_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, large_platform_controllers).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, informational_self_determination_doctrine).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, data_minimization_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National data protection authorities and the European board issue binding guidelines, decide complaints, and can fine controllers up to four percent of worldwide turnover. Their enforcement posture sets the practical price of refusing a removal request. Complaints reach them from requesters; the people whose published work gets removed have no route into their dockets.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, supervisory_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Search engines and social platforms run the removal pipeline: log the request, weigh the exceptions, act within one month. Fine exposure is catastrophic relative to the cost of removing a link, so borderline calls tilt toward removal. Deindexing is applied to European service versions first; original publishers are not told. Compliance staffing and legal reserves are real annual costs.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, large_platform_controllers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, large_platform_controllers, payer).

% Individuals, officeholders, and companies file deletion and deindexing requests aimed at lawful coverage of their conduct: old convictions, misconduct reporting, critical commentary. Filing is free and unilateral, and the regulation reaches any service visible to people in the Union, so the seat travels globally. Success means search results and platform posts about them disappear.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters, beneficiary,
    organized, biographical, mobile, global).

% Agencies sell erasure as a service: they identify damaging links, batch requests across every covered controller, escalate refusals to authorities, and charge recurring fees. Their volume teaches controllers that resistance is expensive. They profit from the spread between a cheap filing and the value of a vanished result.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, mobile, global).

% Newsrooms maintain decades of investigative archives whose reach runs through search and platform distribution. When results are deindexed, traffic to the reporting collapses even though the article remains published. Remedies run through courts on timelines measured in years, against a removal clock measured in weeks.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, news_publishers, payer,
    organized, generational, constrained, continental).

% A reporter's byline and accumulated body of work live in the index; when coverage of a powerful subject is removed, commissions dry up and editors grow wary of the beat. Watching for silent removals is unpaid labor, and moving to pseudonymous publishing would forfeit the named accountability that makes the work commissionable in the first place.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, independent_journalists, payer,
    moderate, biographical, identity_locked, continental).

% Libraries and web archives preserve pages that erasure campaigns target. Preservation duties collide with deletion duties across jurisdictions, so archives either fragment their collections along national lines or accept legal risk. The historical record becomes a patchwork that depends on where a copy physically sits.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    moderate, civilizational, constrained, global).

% People searching for information about candidates, officials, or companies receive results already filtered by private removal decisions. Nothing marks the absence; a searcher cannot know what a query used to return. There is no channel to contest a filtering they never see.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_information_seekers, payer,
    moderate, biographical, constrained, continental).

% Authors and sources of removed material learn of deletions indirectly, if at all, usually through traffic analytics after the fact. The balancing between privacy and publication happens among requester, platform, and regulator; the speaker holds no procedural seat and receives no notice before the removal takes effect.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, erased_speakers, excluded,
    moderate, biographical, trapped, global).

% The Union's highest court decides the boundary cases: whether delisting must be global, how far journalistic exceptions reach, what counts as an overriding public interest. Its rulings redraw the exception map periodically but leave the one-month clock and the fine asymmetry untouched.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, cjeu_courts, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, strategic_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives individuals an enforceable claim to have inaccurate, excessive, or harmful personal data deleted or deindexed by services processing that data, addressing a real collective problem of permanent searchable records, doxxing, and non-consensual imagery that individuals cannot fix bilaterally.
% TRANSFER_FUNCTION: Moves visibility and reach, the currency of search and platform distribution, from published speech to requesting subjects, via controller removal decisions made under fine-exposure asymmetry; compliance costs and legal risk move onto controllers as a side flow.
% ABSENT_VOICES: The speakers and publishers whose content is erased hold no standing in the erasure proceeding, receive no notice, and enter the record only after removal, if at all. The public's interest in open inquiry is likewise represented by no seat in the balancing.
% DISAPPEARANCE_RATIONALE: If the erasure right vanished overnight, deindexed journalism resurfaces in indexes within weeks, the reputation-erasure industry loses its product line, and controllers dismantle removal queues. Simultaneously, people harmed by doxxing, non-consensual imagery, and stale inaccurate records lose their only working remedy. Both camps expect major rearrangement, in opposite directions, which is itself evidence the arrangement currently carries two opposed functions.
% FOUNDING_PROBLEM: Permanent, searchable digital records meant individuals could not escape past data: embarrassing, inaccurate, or harmful personal information persisted indefinitely and spread beyond its original context. Union legislators built a deletion right to restore individual control over personal-data lifecycles.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection bodies, organizations documenting image-based abuse and doxxing, and academic data-protection scholarship attest the founding problem is live, and none of them collects from erasure commerce. Press-freedom organizations corroborate the problem's liveness while contesting the current instrument's proportionality. No attesting source outside the benefiting parties claims the problem is dead.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__censorship_mechanism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__censorship_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72 because the transfer runs from published speech to requesting subjects through removal decisions no adjudicator reviewed: the requester pays nothing, the speaker is compensated nothing, and the controller's incentive gradient points at deletion. Suppression is authored at 0.78 as a RAW structural property, unscaled by power or scope: persistence depends on the fine ratchet (up to four percent of worldwide turnover against a marginal removal cost near zero) and on exit suppression for targets (no speaker notice, no adversarial hearing, extraterritorial reach closing jurisdictional escape). Theater_ratio is moderate-low at 0.28 because the pipeline genuinely removes harmful material alongside the strategic filings; the performative share is boilerplate compliance correspondence and box-ticking audits, growing but not dominant. Accessibility_collapse is 0.58: alternatives for targets partially survive (non-EU-facing hosts, print, direct sharing) but collapse substantially once one understands that every index serving Union users is covered. Resistance is 0.62: sustained press-freedom litigation, controller pushback on manifestly unfounded requests, and scholarly critique are real and ongoing, which is what keeps this a hybrid rather than a settled capture. The temporal series run on ONE shared grid (T=0,2,4,6,8,11) so every metric is authored at every examined point. The suppression_requirement series is included deliberately because the story traces an enforcement-capacity ratchet: soft-law practice before T4, then the statutory clock and fine ceiling activate at T4, then matured authority practice hardens the over-compliance default. Coalition note: the payer seats are individually weak but have coalition potential, and the resistance metric partially reflects exercised coalition power through press-freedom organizations; the analysis treats coalition formation as live, not hypothetical.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats should compute differently, and the engine derives that divergence from the structural data. From the supervisory authorities' seat the arrangement is a functioning rights-protection scheme with abuse margins; from the newsroom and archive seats the same pipeline is a suppression channel they cannot see into and cannot afford to litigate against; from the controller seat it is a compliance regime whose rational strategy is over-removal; from the requester seat it is a free lever. Same nominal legal order, four different experienced arrangements. The excluded seat (erased_speakers) is the sharpest marker of the gap: the arrangement's balancing procedure literally lacks the seat that bears the speech-side cost, so unanimity in the proceeding is manufactured by absence rather than by agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for strategic_erasure_requesters and reputation_management_firms, amplified toward the subsidy end by their mobile exit: they can file, win or lose, and walk away, bearing only opportunity cost. Victim declarations drive high directionality for news_publishers, independent_journalists, digital_archivists, and public_information_seekers, pushed toward the full-target end by constrained or identity-locked exit: the index is the distribution layer for their work, and there is no substitute channel that restores reach. Identity-lock dynamics bind the journalist seat specifically: professional identity is fused with named, searchable publication, so the available exit (pseudonymity) dissolves the accountability function that constitutes the role; if that identity frame broke, the seat's effective extraction would drop toward the constrained-publisher profile. The controller seat is genuinely dual-positioned: it administers the pipeline (agenda-setting side) while absorbing compliance cost and legal risk (cost-bearing side), with partial offsetting relief from geofenced, Europe-only deindexing; its net position sits near symmetric, slightly cost-bearing. Supervisory authorities and courts sit near the administrative midpoint: they neither collect the transferred visibility nor bear the suppressed speech.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permanent records, doxxing, non-consensual imagery) is live and independently corroborated, so this is NOT a resolved mandate: the arrangement persists because its original justification still obtains, with an emergent second function layered on top. Classification discipline cuts both ways here. Labeling the arrangement a pure coordination mechanism (rope) would erase the identifiable victims and launder strategic filings as privacy protection; labeling it pure extraction (snare) would erase the genuine remedies the pipeline delivers for abuse victims and mispredict the politics of reform. The tangled_rope claim registers both: coordination function present, asymmetric extraction present, active enforcement required to hold the structure. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag, which is the correct outcome: this is a living hybrid, not a dead mandate kept alive by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This story instantiates the censorship_mechanism_reading of the article17_erasure_right kernel; do the sibling readings (privacy_fundamental_reading, competitive_moat_reading) or this reading best characterize the arrangement''s dominant structural function?',
    'Cross-reading corpus comparison: compile all three sibling stories and compare computed per-seat classifications, epsilon profiles, and victim sets against shared operational data on request composition and removal outcomes.',
    'If the privacy_fundamental_reading dominates operationally, this story''s epsilon is overstated and the arrangement computes closer to rope; if the censorship mechanism dominates, this reading''s profile stands and the siblings understate victim presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which reading of the erasure-right kernel captures the arrangement''s dominant function.').

omega_variable(
    strategic_request_share,
    'What fraction of erasure and deindexing requests target genuinely harmful personal data versus lawful public-interest speech about the requester?',
    'Controller transparency reports cross-tabulated with independent newsworthiness assessment of removed URLs; supervisory-authority decision audits sampling upheld versus refused requests and the grounds invoked.',
    'Determines whether the coordination function or the suppression function dominates operationally; a high strategic share pushes effective extraction upward and the arrangement toward the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_request_share, empirical, 'Operational split between privacy-genuine and speech-suppressive request volume.').

omega_variable(
    overcompliance_chilling_rate,
    'How much removal exceeds what a fully adjudicated balancing would order, given the fine-asymmetry incentive to delete borderline content?',
    'Independent panel reassessment of a random sample of complied-with removals against the statutory exceptions and the journalistic-derogation case law; comparison of pre-clock and post-clock removal rates for equivalent request classes.',
    'Quantifies the prior-restraint-substitute effect: a high over-removal rate confirms the enforcement asymmetry, not the statute''s text, drives outcomes, supporting escalation toward the pure-extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overcompliance_chilling_rate, empirical, 'Magnitude of chilling over-removal induced by the compliance clock and fine exposure.').

omega_variable(
    counter_notice_restoration,
    'Would mandatory notice-and-counter-notice procedure (speaker informed before or promptly after removal, with expedited review) restore adversarial balance to the pipeline?',
    'Natural experiment from procedures that already require notice or from pilot regimes with counter-notice mechanics: measure reversal rates, appeal volumes, and public-interest restoration compared with the silent-removal baseline.',
    'If notice restores balance, the arrangement is a reformable hybrid and the extraction component is procedural rather than structural; if suppression persists under notice, the structure itself is the mechanism and the classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_notice_restoration, empirical, 'Whether procedural safeguards would dissolve the suppression function or merely decorate it.').

omega_variable(
    geofencing_blunt_or_export,
    'Does Europe-only geofenced deindexing blunt the suppression effect (content remains visible from the rest of the world) or extend it (the compliance model exports as a global norm)?',
    'Longitudinal comparison of non-EU jurisdictions adopting comparable delisting instruments, and measurement of cross-border traffic to deindexed material from non-EU service versions.',
    'If geofencing blunts, the arrangement''s spatial amplification of extraction is contained and victims retain external exits; if it exports, the effective scope approaches global and target exit options degrade further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geofencing_blunt_or_export, conceptual, 'Whether territorial limitation contains or propagates the suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t2, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2, observed).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(arti_tr_t6, observed).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t11, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 11, 0.28).
narrative_ontology:measurement_basis(arti_tr_t11, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t2, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement_basis(arti_be_t2, observed).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement_basis(arti_be_t6, observed).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t11, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 11, 0.72).
narrative_ontology:measurement_basis(arti_be_t11, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t2, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement_basis(arti_su_t2, observed).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement_basis(arti_su_t6, observed).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t11, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 11, 0.78).
narrative_ontology:measurement_basis(arti_su_t11, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Article 17 right to erasure' conflates three structurally distinct claims. The privacy_fundamental_reading authors epsilon for the data-sovereignty arrangement (victim set: data subjects facing corporate retention; low-to-moderate extraction). The competitive_moat_reading authors epsilon for the compliance-cost arrangement (victim set: small controllers and entrants; extraction via cost asymmetry favoring incumbents). THIS story authors epsilon for the speech-suppression arrangement (victim set: publishers, journalists, archivists, the searching public; high extraction via enforcement asymmetry). The privacy_fundamental_reading is the upstream legitimating claim: its doctrinal success is what supplies the strategic requester's cover, which is why this story links to it as a dependent. Each file carries its own stable epsilon; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
