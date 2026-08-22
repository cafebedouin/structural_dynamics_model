% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: WP:N as Perpetual Boundary-Negotiation Process (Deliberative Reading)
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   Wikipedia's notability guideline (WP:N) governs whether a topic merits a
 *   standalone article. Rather than treating notability as a checklist
 *   mechanically applied, this reading holds that the guideline's meaning is
 *   continuously produced through thousands of individual
 *   Articles-for-Deletion (AfD) discussions, each of which cites past
 *   precedent while remaining free to extend, narrow, or reverse it. The
 *   written policy text lags and summarizes the deliberation rather than
 *   controlling it in advance. On this reading, WP:N most resembles a
 *   governance scaffold: a standing structure whose purpose is to manage an
 *   ongoing transition — the continuous re-fitting of boundary cases into an
 *   evolving, never-finalized standard — rather than a settled steady-state
 *   rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.32).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N as Perpetual Boundary-Negotiation Process (Deliberative Reading)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'd371cbf2-1d40-46e5-948a-5d2da0a3a965').
narrative_ontology:cs_kernel_codification('d371cbf2-1d40-46e5-948a-5d2da0a3a965', distributed).
narrative_ontology:cs_authority_grounding('d371cbf2-1d40-46e5-948a-5d2da0a3a965', practice).
narrative_ontology:cs_interpretation_layer_present('d371cbf2-1d40-46e5-948a-5d2da0a3a965').
narrative_ontology:cs_reading_relation('d371cbf2-1d40-46e5-948a-5d2da0a3a965', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d371cbf2-1d40-46e5-948a-5d2da0a3a965', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('d371cbf2-1d40-46e5-948a-5d2da0a3a965', foundational, notability_is_process_output_not_fixed_criterion).
narrative_ontology:cs_axiom_status(notability_is_process_output_not_fixed_criterion, holdable).
narrative_ontology:cs_axiom_grounding('d371cbf2-1d40-46e5-948a-5d2da0a3a965', notability_is_process_output_not_fixed_criterion, conventional).
narrative_ontology:cs_axiom('d371cbf2-1d40-46e5-948a-5d2da0a3a965', foundational, boundary_revision_through_deliberation_is_legitimate_governance).
narrative_ontology:cs_axiom_status(boundary_revision_through_deliberation_is_legitimate_governance, holdable).
narrative_ontology:cs_axiom_grounding('d371cbf2-1d40-46e5-948a-5d2da0a3a965', boundary_revision_through_deliberation_is_legitimate_governance, instrumental).
narrative_ontology:cs_reference_frame('d371cbf2-1d40-46e5-948a-5d2da0a3a965', consensus_based_precedent_accretion).
narrative_ontology:cs_drift_state('d371cbf2-1d40-46e5-948a-5d2da0a3a965', contemporary_afd_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d371cbf2-1d40-46e5-948a-5d2da0a3a965', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_editor_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, article_subjects_with_contested_notability).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, readers_seeking_reliable_coverage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, article_subjects_with_contested_notability).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, consensus_based_epistemic_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer editors who argue, vote, and set precedent in individual Articles-for-Deletion discussions. Each discussion re-derives what 'notable' means for a marginal case, citing prior precedent while also being free to argue precedent should shift. They can walk away from any single discussion, participate in another, or stop editing altogether; their collective output is the boundary itself, which is never finalized.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, afd_participating_editors, agenda_setter,
    organized, biographical, mobile, global).

% The broader volunteer base benefits from having a living, adjustable standard rather than a frozen rule: it can absorb new domains (webcomics, YouTube creators, non-Western historical figures) as consensus develops, without requiring a wholesale rewrite of policy. The standard's flexibility is itself the coordination good they collect.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editor_community, beneficiary,
    organized, generational, mobile, global).

% Subjects (people, organizations, works) whose coverage sits near the boundary. They benefit when deliberation eventually recognizes borderline cases previously excluded, and they pay when an AfD closes against them — but the same subject can be recreated and re-argued later as sourcing changes or consensus shifts. They have no vote in the discussion and no formal standing, only the ability to wait for the boundary to move or to seek independent coverage that strengthens a future case.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, article_subjects_with_contested_notability, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, article_subjects_with_contested_notability, payer).

% General readers who rely on notability deliberation to keep the encyclopedia's coverage anchored to independently verifiable sourcing rather than promotional or unverifiable content. They do not participate in AfD but benefit from its output; if they disagree with a given article's fate they can raise it in future discussions or seek information elsewhere.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, readers_seeking_reliable_coverage, beneficiary,
    moderate, immediate, mobile, global).

% Editors with administrator privileges who assess AfD discussions for rough consensus and execute the outcome (keep, delete, merge, redirect). They do not vote but interpret the deliberation's result and can be challenged at deletion review; their discretion is itself subject to renegotiation over time as norms about what counts as 'rough consensus' shift.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, closing_administrators, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, closing_administrators, observer).

% Accounts created specifically to argue for retention of a promotional or self-interested article. Their arguments are systematically discounted in AfD closes regardless of content, on the premise that motivated participation is unreliable. They would object that the process treats their standing as suspect a priori, but they have no channel to contest that discounting rule itself — only the individual article outcome.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, single_purpose_and_promotional_accounts, excluded,
    powerless, immediate, trapped, global).

% The written guideline text (WP:N and its subject-specific supplements) is continuously amended by talk-page consensus and RfCs that follow patterns established in accumulated AfD precedent. It is not a fixed input to deliberation but a running summary of what deliberation has already decided, updated by the same community that applies it.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, notability_guideline_page_itself, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(notability_guidelines__deliberative_reading, notability_guideline_page_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, revisable procedure for deciding what belongs in a general-purpose encyclopedia when reasonable editors disagree about a borderline subject, without requiring unanimous agreement on a fixed definition of 'notable' in advance.
% TRANSFER_FUNCTION: Moves editorial attention and inclusion/exclusion outcomes through repeated case-by-case argumentation rather than a static rule; what is transferred is not primarily resources but standing — which subjects get a stable article and which get deleted, pending renegotiation.
% ABSENT_VOICES: Article subjects themselves have no direct standing in AfD; single-purpose accounts are present but structurally discounted; readers who never engage with deletion discussions have no voice at all in shaping precedent even though the outcome shapes what they can find.
% DISAPPEARANCE_RATIONALE: If AfD deliberation vanished overnight and notability reverted to a fixed checklist applied mechanically (or to no filter at all), the encyclopedia's coverage boundary would either freeze in its current, already-contested state or expand without any community mechanism for contesting individual cases — either way the accumulated precedent-generating machinery that currently absorbs disputes would disappear and disputes would have to be resolved some other way (edit wars, unilateral admin action, or external arbitration).
% FOUNDING_PROBLEM: Early Wikipedia had no consistent way to decide whether a borderline topic deserved a standalone article, leading to ad hoc, editor-by-editor judgment calls that produced inconsistent results and recurring disputes over the same categories of subject.
% FOUNDING_PROBLEM_CORROBORATION: Academic studies of Wikipedia governance (e.g. research on WikiProject and AfD deliberation patterns by outside social-science researchers, not Wikipedia editors) document that disputes over borderline notability recur continuously across subject domains, corroborating that the coordination problem the process addresses has not been resolved once and for all; deletionist and inclusionist editors within the project agree the underlying disagreement is ongoing even as they dispute whether the process resolves it fairly.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.28) because the process, on this reading, is not primarily transferring value from a victim class to a beneficiary class; it is producing a genuinely contested but broadly shared good (a workable, adjustable inclusion boundary) through participation that remains open to any editor. Suppression is moderate (0.32) because the process does discount certain classes of participation (single-purpose accounts) and does exclude article subjects from direct standing, but it does not rely on coercion to persist — participation is voluntary and precedent is explicitly revisable. Theater ratio is kept low-moderate (0.22, rising slightly) reflecting that some AfD activity is genuinely deliberative while a growing minority is ritualized citation of shorthand essays (WP:GNG boilerplate) rather than fresh argument — a mild but real drift toward procedural theater. Accessibility collapse is moderate (0.35): alternative approaches to inclusion decisions (fixed checklists, algorithmic notability scores) have been proposed and rejected repeatedly, but they remain conceivable and are periodically re-litigated, so collapse is far from total. Resistance is moderately high (0.55) because deletionist and inclusionist factions each actively contest the deliberative frame itself, arguing respectively that it is too indeterminate or too exclusionary — the deliberative reading's central claim (that boundary movement is itself legitimate governance) is exactly what both siblings resist.
 *
 * DIRECTIONALITY LOGIC:
 *   AfD participating editors and closing administrators are agenda-setters: they run the machinery and are also its primary beneficiaries in the sense that they retain control over how the boundary moves. Article subjects with contested notability are dual-positioned: they benefit when the process eventually recognizes them and pay when it does not, but on this reading that outcome is provisional rather than final, which is why their directionality is not pushed to the full-target end the way it would be under the inclusionist reading. Single-purpose/promotional accounts are excluded by design — their discounting is a structural feature the deliberative reading treats as reasonable epistemic hygiene rather than as extraction, which is precisely the point of disagreement with the inclusionist sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inconsistent, ad hoc notability judgments) remains live by this reading's own account, corroborated by outside academic observation of continuing AfD disputes across subject domains — this blocks a mandatrophy verdict. The disappearance_verdict of world_rearranges combined with founding_problem_status of live means this reading does not exhibit the zombie-mandate pattern (status=dead + verdict=world_rearranges) that would flag capture; the process is read as an active, still-functioning scaffold rather than an inertial holdover. This is the central structural claim that distinguishes the deliberative reading from a piton reading of the same kernel, which no sibling in this set has claimed but which is a latent fourth possibility this story explicitly does not instantiate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_vs_deletionist_vs_inclusionist_framing,
    'Is WP:N structurally a scaffold whose boundary-movement is the coordination good (this reading), a settled quality filter whose enforcement protects the commons (deletionist reading), or a gatekeeping apparatus whose deliberative form disguises systematic exclusion of marginalized subjects (inclusionist reading)?',
    'Longitudinal analysis of AfD outcome patterns by subject-demographic category, cross-referenced with whether precedent genuinely moves in response to sourcing-landscape changes (supporting deliberative/inclusionist readings) versus remains stable around a consistent quality bar (supporting deletionist reading); qualitative process-tracing of whether excluded subject categories systematically overlap with historically marginalized groups (supporting inclusionist) or are evenly distributed (supporting deliberative/deletionist).',
    'If outcome patterns show systematic exclusion correlated with marginalization independent of sourcing availability, the inclusionist reading''s higher ε and snare/tangled_rope-leaning classification would be the more structurally accurate account and this reading''s low ε would be shown to understate real extraction. If outcomes track sourcing availability closely with no demographic skew, the deletionist reading''s quality-filter account gains support and this reading''s scaffold framing would be the more conservative middle path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_vs_deletionist_vs_inclusionist_framing, conceptual, 'Which of the three sibling readings of the notability_guidelines kernel best captures the structural reality of AfD deliberation.').

omega_variable(
    precedent_convergence_vs_perpetual_drift,
    'Does AfD precedent converge toward a stable standard over time (making the ''perpetual negotiation'' framing a transitional description of a process that is actually settling), or does it remain in genuine perpetual flux (supporting the scaffold/deliberative reading indefinitely)?',
    'Track citation patterns and reversal rates of AfD precedent across subject-matter guideline pages (WP:NCORP, WP:NPROF, etc.) over a multi-year window; convergence would show declining reversal rates and increasingly stable precedent citation.',
    'If precedent has converged, the scaffold classification (predicated on the process being genuinely transitional rather than steady-state) weakens and the constraint would more closely resemble a rope with a settled coordination function; if precedent remains in perpetual flux, the scaffold/deliberative reading is reinforced and the sunset-clause framing (that this is a transition, not a permanent steady state) remains structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_convergence_vs_perpetual_drift, empirical, 'Whether AfD deliberation is converging toward stability or remains in genuine ongoing negotiation.').

omega_variable(
    single_purpose_account_discounting_legitimacy,
    'Is the systematic discounting of single-purpose and promotional account arguments in AfD closes a legitimate epistemic-hygiene practice (as this reading assumes) or itself a form of structural exclusion that happens to correlate with legitimate promotional filtering?',
    'Compare AfD outcomes where SPA arguments are discounted against cases where the same arguments, sourced identically, come from established editors — if outcomes diverge based on account status alone holding argument quality constant, the discounting functions as status-based exclusion rather than pure content filtering.',
    'If discounting tracks account status independent of argument quality, this reading''s treatment of SPA exclusion as reasonable hygiene understates a real suppression mechanism, and suppression should be authored higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_purpose_account_discounting_legitimacy, empirical, 'Whether discounting single-purpose accounts is legitimate filtering or status-based suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deliberative_reading, 0.1).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% This story is the deliberative reading of the notability_guidelines kernel. Two sibling constraints instantiate the same kernel from different structural premises: notability_guidelines__deletionist_reading (WP:N as necessary epistemic quality filter; low ε, rope/mountain-leaning) and notability_guidelines__inclusionist_reading (WP:N as structural gatekeeping apparatus; high ε, snare/tangled_rope-leaning). Per the ε-invariance principle, these are three separate constraints sharing a kernel, not one constraint measured three ways. Each carries its own ε, its own beneficiary/victim structure, and its own claimed_type, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
