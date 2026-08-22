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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: WP:N Deletionist Reading: Notability as Quality Filter
 *   domain: epistemic/knowledge_infrastructure/platform_governance
 *
 * SUMMARY:
 *   This story instantiates the DELETIONIST READING of WP:N (Wikipedia's
 *   Notability Guidelines). Under this reading, WP:N is a necessary epistemic
 *   quality filter that solves a real coordination problem: maintaining an
 *   encyclopedic commons against degradation by promotional, spam, and
 *   non-notable content. The deletionist reading frames the standard as a
 *   beneficiary-symmetric Rope—readers benefit from a curated resource;
 *   editors benefit from a shared governance framework that avoids constant
 *   spam reversal. The constraint is coordination, not extraction: the
 *   enforcement serves quality preservation, not rent collection. The
 *   standard is enforced through Articles for Deletion consensus processes,
 *   where articles failing the notability bar are removed by community
 *   decision. This reading coexists with two sibling readings held by
 *   different editor factions: the INCLUSIONIST reading views WP:N as
 *   structural gatekeeping that excludes marginalized knowledge, and the
 *   DELIBERATIVE reading views it as a perpetual negotiation process where
 *   boundaries evolve through AfD deliberation. The three readings share the
 *   kernel (the WP:N policy text and institutional practice) but draw it into
 *   different constraints with different ε values, beneficiary/victim
 *   structures, and types. This story is ONE reading only—the ε value,
 *   metrics, and beneficiary structure describe the standing arrangement as
 *   the deletionist reading understands it, not as the inclusionist reading
 *   would measure it.
 *
 * KEY AGENTS:
 *   - wikipedia_readership: primary beneficiary (organized, mobile exit) — benefits from quality-filtered resource
 *   - article_deletionists: agenda-setter (organized, mobile exit) — enforce the standard through AfD
 *   - article_inclusionists: excluded observer (moderate power, constrained exit) — would dispute the boundary
 *   - wikipedia_editors (multi-reading faction): observer across the ecosystem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.18).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.12).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N Deletionist Reading: Notability as Quality Filter").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "epistemic/knowledge_infrastructure/platform_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'd63ca385-121d-444a-a29b-0d86d6f3e65f').
narrative_ontology:cs_kernel_codification('d63ca385-121d-444a-a29b-0d86d6f3e65f', formalized).
narrative_ontology:cs_authority_grounding('d63ca385-121d-444a-a29b-0d86d6f3e65f', practice).
narrative_ontology:cs_interpretation_layer_present('d63ca385-121d-444a-a29b-0d86d6f3e65f').
narrative_ontology:cs_reading_relation('d63ca385-121d-444a-a29b-0d86d6f3e65f', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d63ca385-121d-444a-a29b-0d86d6f3e65f', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('d63ca385-121d-444a-a29b-0d86d6f3e65f', foundational, notability_via_documented_coverage).
narrative_ontology:cs_axiom_status(notability_via_documented_coverage, holdable).
narrative_ontology:cs_axiom_grounding('d63ca385-121d-444a-a29b-0d86d6f3e65f', notability_via_documented_coverage, deontological).
narrative_ontology:cs_axiom('d63ca385-121d-444a-a29b-0d86d6f3e65f', foundational, mainstream_media_as_legitimacy_arbiter).
narrative_ontology:cs_axiom_status(mainstream_media_as_legitimacy_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('d63ca385-121d-444a-a29b-0d86d6f3e65f', mainstream_media_as_legitimacy_arbiter, conventional).
narrative_ontology:cs_reference_frame('d63ca385-121d-444a-a29b-0d86d6f3e65f', quality_preservation_via_shared_standard).
narrative_ontology:cs_drift_state('d63ca385-121d-444a-a29b-0d86d6f3e65f', contemporary_marginalized_knowledge_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d63ca385-121d-444a-a29b-0d86d6f3e65f', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, wikipedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, knowledge_quality_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Encounters a curated encyclopedia where articles meet a consistent notability standard: verifiable coverage in independent reliable sources, not original research, not promotional content. The reader benefits from reduced signal noise and confidence that an article's presence signals documented significance, not vanity or spam.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_readership, beneficiary,
    organized, biographical, mobile, global).

% Enforce the deletionist reading of WP:N through Articles for Deletion (AfD) processes: articles failing the notability filter are nominated and removed. They argue this maintains encyclopedia quality by excluding topics that cannot meet the standard of third-party documentation in established media. They administer the enforcement through consensus discussions and deletion decisions.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, article_deletionists, agenda_setter,
    organized, biographical, mobile, global).

% Believe WP:N criteria are too rigid and exclude notable topics that lack mainstream media coverage—local history, emerging subjects, marginalized communities' documented achievements. They would object that the deletionist reading privileges established media as the sole arbiter of notability, but their voice is structurally secondary in the deletionist framing.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, article_inclusionists, excluded,
    moderate, biographical, constrained, global).

% Non-agent entity representing promotional spam, original research, and vanity articles that the constraint excludes. The deletionist reading treats exclusion of this content as just enforcement, not extraction—spam has no legitimate claim to remain.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, vandal_spam_content, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(notability_guidelines__deletionist_reading, vandal_spam_content).

% Volunteer editors across the spectrum (deletionists, inclusionists, deliberativists) who conduct the actual AfD discussions, vote on deletion, and implement removal decisions. They are seated as observers here because they hold multiple readings of WP:N—this story instantiates only the deletionist reading held by one editor faction.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_editors, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining an encyclopedic commons against degradation: without a notability standard, the repository would accumulate low-value, promotional, and non-notable content, reducing utility for readers seeking authoritative information. A shared standard for what merits inclusion coordinates contributors and readers around a common quality expectation.
% TRANSFER_FUNCTION: Transfers curation labor and editorial authority to the deletionist editor faction and to the standard-setters (WMF Notability Policy working groups, high-reputation editors). In exchange, readers receive a quality-filtered resource. The transfer is of governance legitimacy, not economic rent—the deletionist reading does not model editors as capturing extraction value.
% ABSENT_VOICES: Inclusionists argue that marginalized communities, emerging fields, and topics lacking mainstream media coverage are systematically excluded by a standard that privileges established media as the arbiter of notability. They would demand a more expansive or locally-responsive standard. Alternative knowledge systems (oral history traditions, community documentation, non-English-language scholarship) are structurally disfavored by the deletionist frame. These voices are organized but secondary in the deletionist reading.
% DISAPPEARANCE_RATIONALE: If WP:N deletionist enforcement vanished overnight, Wikipedia would accumulate promotional articles, vanity biographies, and non-notable content within weeks. Signal-to-noise would degrade substantially, reducing reader trust and utility. The constraint's persistence is necessary to maintain the commons's epistemic function.
% FOUNDING_PROBLEM: Wikipedia's early years saw rapid accumulation of low-quality, non-notable, and promotional articles. The commons required a shared standard to distinguish documentary significance from personal promotion, spam, and original research. WP:N codified that standard.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing spam and promotional edit attempts, persistent editor discussions of notability borderlines, and reader complaints about low-quality articles all attest the founding problem remains live. Academic studies of Wikipedia quality correlate stricter notability enforcement with higher reader satisfaction (independent corroboration outside the deletionist faction). Inclusionists dispute that the problem justifies the current standard's scope—they attest the problem is real but the solution is too broad.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.18 at interval end) because the deletionist reading finds no victim set—spam and vanity exclusion is just enforcement against non-legitimate content, not extraction from legitimate stakeholders. Readers get a public good (quality commons); editors get a shared standard reducing coordination friction. The low theater ratio (0.08) reflects the reading's judgment that the constraint's function is mostly real: removal is executed because articles fail the notability bar, not because the bar is maintained performatively. The suppression requirement is also low (0.12) because the reading assumes agreement on the standard's legitimacy—strong spam filters, automated-removal tools, and community consensus make enforcement relatively low-friction within the deletionist frame. Accessibility collapse is HIGH (0.72) because once the notability standard is understood, alternatives collapse: an article either meets third-party-coverage criteria or it does not; there is limited space for borderline negotiation in the deletionist reading. Resistance is MODERATE (0.31) because inclusionists organize counter-proposals and the boundary is genuinely contested—but the deletionist reading treats this as healthy policy debate, not structural resistance to an extractive constraint. The measurement series shows extractiveness, theater, and suppression drifting slightly upward over the interval (from 0.08→0.18, 0.05→0.08, 0.08→0.12), suggesting mild creep as edge cases accumulate and the standard's enforcement tightens—but all remain low, consistent with a Rope classification even under the deletionist reading.
 *
 * PERSPECTIVAL GAP:
 *   The engine's per-seat computation will show divergence: from the deletionist seat, the constraint is low-extractiveness Rope (quality coordination); from the inclusionist seat (not authored in this story, but present in a sibling story), the same institutional practices measure as higher-extractiveness Snare or Tangled Rope (gatekeeping excluding marginalized knowledge). That divergence is exactly the kernel-reading contestation the framework models. This story declares only the deletionist measurements; the sibling stories will declare the inclusionist and deliberative measurements. The contested boundary is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seat (readership) has HIGH accessibility collapse and LOW directionality (near 0.0) because the deletion of spam simply ends non-legitimate content—the reader's alternatives do not collapse by this constraint, the constraint opens alternatives by keeping noise out. The agenda-setter (deletionists) has MOBILE exit and ORGANIZED power—they can leave Wikipedia, but the constraint aligns with their values, so directionality is low (they are net beneficiaries of their own enforcement). Inclusionists are EXCLUDED not PAYER because the deletionist reading does not model them as bearing extraction costs—they are modeled as holding an alternative standard, not as suffering from the enforcement of this one. The non-agent (spam/vanity) is labeled as payer only for bookkeeping; the deletionist reading does not extend victim status to content deemed non-notable by legitimate standard.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (decay of mandate) is low in the deletionist reading because the founding problem (spam/vanity degradation) remains live—ongoing spam attempts, persistent low-quality submissions, and reader complaints all attest the problem WP:N was built to solve is still active. If the founding problem status shifted to 'dead' (somehow all low-quality content vanished, rendering the filter unnecessary), mandatrophy would activate and the constraint would reclassify toward Piton (maintained by theater, not by function). The deletionist reading does not face this reclassification risk under current conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mainstream_media_bias_in_notability,
    'Does the deletionist reading''s reliance on independent reliable sources—typically mainstream media—systematically bias notability toward established institutions and against marginalized communities?',
    'Empirical audit of deleted articles by topic domain: if marginalized-community topics and non-Western scholarship correlate with higher deletion rates relative to comparable mainstream topics, the bias hypothesis is supported. Expert testimony from inclusionist editors and scholars of knowledge infrastructure.',
    'If bias is demonstrated, the constraint reclassifies from deletionist Rope (justified quality filter) toward inclusionist Snare or Tangled Rope (unjust gatekeeping). The ε value would rise substantially in the inclusionist reading, while remaining low in the deletionist reading—the readings'' divergence would become empirically grounded in documented bias rather than mere theoretical contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainstream_media_bias_in_notability, empirical, 'Whether notability criteria systematically exclude marginalized knowledge due to mainstream-media bias.').

omega_variable(
    reading_stability_over_time,
    'Is the deletionist reading stable over Wikipedia''s evolution, or does the boundary between quality-filtering Rope and gatekeeping Snare shift as Wikipedia''s user base and knowledge domain evolve?',
    'Longitudinal analysis of AfD outcomes and community consensus over 5+ year periods. Tracking of whether deletion rates and reasons remain consistent or shift toward gatekeeping patterns.',
    'If the reading drifts from quality-filtering toward gatekeeping, the constraint would transition from stable Rope toward Tangled Rope or Snare, and the three readings'' ε values would converge (deletionist ε would rise, inclusionist ε would remain high, deliberative ε would rise toward the others). A stable reading supports the deletionist Rope classification; a drifting reading suggests the kernel''s interpretation is shifting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_over_time, conceptual, 'Whether the deletionist reading remains coherent as Wikipedia''s knowledge domain and user base evolve.').

omega_variable(
    consensus_legitimacy_vs_structural_power,
    'Does the AfD consensus process genuinely reflect distributed community judgment, or does deletionist editor power concentrate such that consensus is manufactured rather than emergent?',
    'Social network analysis of AfD discussions: identify whether outcomes correlate with editor-faction power rather than article quality, and whether turnout and participation patterns shift with topic domain or editor background. Qualitative analysis of consensus-formation mechanisms.',
    'If consensus is manufactured by structural power, the suppression requirement rises (consensus becomes performative), theater ratio rises (manufactured agreement masks force), and the constraint reclassifies toward Tangled Rope or Snare even in the deletionist reading. If consensus is genuinely emergent, the low-suppression Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_legitimacy_vs_structural_power, empirical, 'Whether AfD consensus reflects distributed community judgment or concentrated editor power.').

omega_variable(
    kernel_reading_committer_contestation,
    'Is the notability_guidelines kernel a single constraint viewed from different perspectives, or do the three readings (deletionist, inclusionist, deliberative) actually instantiate three different constraints with different ε values because they measure different referents?',
    'Operator analysis per ε-invariance principle (DP-001): if changing the observable (deletionist quality-filter frame vs. inclusionist gatekeeping frame) changes ε by a wide margin—say from 0.18 to 0.65—then the readings are measuring different constraints, not the same constraint from different seats. If ε stays stable across readings when corrected for directionality, the readings are the same constraint; if ε diverges, they are separate constraints linked by the kernel.',
    'If readings are separate constraints (divergent ε), the corpus should author three separate JSON files with distinct constraint_ids and link them via network.affects_constraints. If readings are same constraint (stable ε corrected for directionality), this single story suffices. Current authoring assumes separate constraints; if operator analysis contradicts that, refactor to one ε-stable story with per-seat computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_contestation, conceptual, 'Whether the three readings of notability_guidelines are one constraint with multiple seats or three separate constraints linked by a kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deletionist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(nota_tr_t25, notability_guidelines__deletionist_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deletionist_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(nota_be_t25, notability_guidelines__deletionist_reading, base_extractiveness, 25, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deletionist_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(nota_su_t25, notability_guidelines__deletionist_reading, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.05).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This story instantiates the DELETIONIST reading of the notability_guidelines kernel. Sibling readings (inclusionist and deliberative) are separate constraint stories with different ε values, beneficiary/victim structures, and claimed types. All three stories share the kernel (WP:N policy text and AfD process) but draw it into different constraints. The three-story family models the kernel contestation: each reading measures a different property of the same institutional arrangement, producing divergent classifications. Link: notability_guidelines__inclusionist_reading (coexists_with, higher ε, Snare/Tangled Rope); notability_guidelines__deliberative_reading (coexists_with, moderate ε, Tangled Rope). The deletionist reading privileges coordination and quality preservation; the inclusionist reading emphasizes gatekeeping and exclusion; the deliberative reading models notability as emergent from ongoing negotiation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
