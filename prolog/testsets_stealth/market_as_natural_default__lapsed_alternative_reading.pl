% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market-as-Natural-Default: Lapsed-Alternative Reading
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   Market arrangements dominate contemporary economic life and are treated
 *   as the natural default — the background condition of economic reasoning
 *   rather than one arrangement among others. This story authors that
 *   naturalization as a historical artifact: the alternatives to market
 *   arrangements — cooperative ownership, commons governance, guild and
 *   syndicalist forms, market-socialist proposals — were live, documented
 *   options within living memory and lapsed from curricula, policy discourse,
 *   and public memory as the twentieth century's systemic debate closed. No
 *   agent maintains the naturalization; no identifiable class collects from
 *   it; nothing enforces it. What remains is an inert default: the menu of
 *   conceivable economic arrangements narrowed by forgetting, not by closure,
 *   and the archive of alternatives survives in the historical record,
 *   recoverable by research. The costs are real but diffuse and low-intensity
 *   — policy options that never appear on the menu, careers spent studying
 *   arrangements the default treats as antiquarian — and they are borne by
 *   seats with little power to restore what was lapsed.
 *
 * KEY AGENTS:
 *   - economics_discipline: agenda-setting seat (institutional/constrained) — reproduces the default through curricula and canonical texts without defending it; could restore the history of alternatives only at prohibitive curricular cost
 *   - alternative_economists: primary payer seat (moderate/constrained) — heterodox scholars whose objects of study are the lapsed alternatives; bear the lapse as professional marginality
 *   - general_public: diffuse payer seat (powerless/constrained) — inherits the narrowed menu as common sense; bears small unconsidered-option costs
 *   - economic_historians: analytical observer seat (analytical/analytical) — hold the archive of lapsed alternatives; the recovery mechanism the arrangement's low epsilon depends on
 *   - cooperative_movements: excluded seat (organized/constrained) — living operators of functioning alternative arrangements, absent from the conversation where economic common sense is reproduced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.13).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.13).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, piton).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market-as-Natural-Default: Lapsed-Alternative Reading").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '0a8e6523-37eb-4c72-a440-69865c40810c').
narrative_ontology:cs_kernel_codification('0a8e6523-37eb-4c72-a440-69865c40810c', distributed).
narrative_ontology:cs_authority_grounding('0a8e6523-37eb-4c72-a440-69865c40810c', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0a8e6523-37eb-4c72-a440-69865c40810c', market_as_natural_default__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('0a8e6523-37eb-4c72-a440-69865c40810c', market_as_natural_default__hybrid_amnesia_reading, forecloses).
narrative_ontology:cs_axiom('0a8e6523-37eb-4c72-a440-69865c40810c', foundational, lapsed_memory_sufficient_for_naturalization).
narrative_ontology:cs_axiom_status(lapsed_memory_sufficient_for_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('0a8e6523-37eb-4c72-a440-69865c40810c', lapsed_memory_sufficient_for_naturalization, empirically_contingent).
narrative_ontology:cs_axiom('0a8e6523-37eb-4c72-a440-69865c40810c', foundational, alternatives_archival_not_destroyed).
narrative_ontology:cs_axiom_status(alternatives_archival_not_destroyed, holdable).
narrative_ontology:cs_axiom_grounding('0a8e6523-37eb-4c72-a440-69865c40810c', alternatives_archival_not_destroyed, empirically_contingent).
narrative_ontology:cs_reference_frame('0a8e6523-37eb-4c72-a440-69865c40810c', contested_menu_of_economic_arrangements).
narrative_ontology:cs_drift_state('0a8e6523-37eb-4c72-a440-69865c40810c', post_1989_systemic_debate_closure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a8e6523-37eb-4c72-a440-69865c40810c', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, alternative_economists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains the profession and sets what counts as economic knowledge: standard curricula, canonical textbooks, journal gatekeeping, hiring norms. Its textbooks open from exchange, scarcity, and preference rather than from the historical menu of ways humans have organized production and provision, and the history of alternatives appears, if at all, as a museum wing. Nobody in the discipline campaigns for this framing; each cohort teaches the frame it was trained in. Restoring the lapsed history would mean rewriting curricula, retraining faculty, and treating the field's starting point as one contingent outcome among several — a cost the discipline has no occasion to pay, since nothing forces the question.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economics_discipline, agenda_setter,
    institutional, generational, constrained, global).

% Work on the lapsed alternatives — cooperative production, commons governance, guild and syndicalist lineages, market-socialist proposals — from positions at the margins of the profession: marginal journals, marginal departments, thin hiring pipelines. Their objects of study are treated as historical curiosities rather than candidate arrangements, so their research reads as antiquarian no matter how practical its findings. They cannot move into the mainstream without abandoning their objects, and leaving the profession altogether forfeits the only platform the work has. A secondary living is available as the default's designated critics, which ties part of their livelihood to the very frame they dispute.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, alternative_economists, payer,
    moderate, biographical, constrained, global).

% Inherits the default as common sense. The menu of economic arrangements they ever consider was narrowed before they arrived: cooperative ownership, commons governance, and similar forms surface, if at all, as local curiosities rather than documented options with track records. They pay a small, diffuse cost in foregone options and receive a small offsetting convenience in not having to adjudicate first principles. There is no exit from the discourse they reason in, and no agent to petition, since nobody chose the narrowing and nobody defends it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, biographical, constrained, global).

% Maintain the archive of the lapsed alternatives: the cooperative movements' records, the guild-socialist debates, the commons regimes, the market-socialist design literature. They document that these were live, working arrangements within living memory and trace how they dropped from view. They neither administer the default nor campaign against it; their archive is what any revival of the alternatives would draw on.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% Operate functioning alternatives at meaningful scale — worker cooperatives, consumer cooperatives, mutuals, commons-governance bodies — under legal and financial machinery built around the default form. They object that their arrangements are treated as anomalies rather than documented options, and that finance, law, and education offer no on-ramps for their form. They are excluded not by a barrier anyone enforces but by absence: the conversation where economic common sense is set has no seat for them, and their demonstrated track record never enters the menu.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, cooperative_movements, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared default frame economizes on deliberation: because everyone treats market arrangements as the background condition, discourse, education, and policy proceed without relitigating how economic life should be organized. Under this reading that discourse economy is the arrangement's only remaining function — a residue of a once-contested menu, carried by habit rather than maintained by anyone.
% TRANSFER_FUNCTION: Almost nothing transfers by design. The arrangement moves consideration and attention away from the lapsed alternatives and toward the market default — a reallocation of the conceivable, not a payment. No seat receives what the arrangement's costs displace; the foregone options are simply unavailable to everyone, which is why no recipient of the arrangement's operation can be named.
% ABSENT_VOICES: The living constituencies of the lapsed alternatives — cooperative and commons movements, heterodox economists — would object that their arrangements are documented, functioning, and absent from the menu the default discourse presents. Their objection has no venue: the rooms where economic common sense is reproduced (curriculum committees, textbook markets, editorial pages) are not rooms anyone is barred from; they are rooms where the alternatives stopped being discussed, so there is nowhere for the objection to land.
% DISAPPEARANCE_RATIONALE: If the naturalization vanished overnight — if every participant in economic life woke remembering that cooperative, commons, and syndicalist arrangements were once live, documented, working options — curricula would need history sections they lack, policy menus would widen, and markets would have to be justified on merits rather than assumed. The rearrangement would be large precisely because so much currently rests on the assumption passing unexamined; the discourse economy the default provides would be replaced by explicit justification costs everywhere.
% FOUNDING_PROBLEM: None in the constructive sense: under this reading the naturalization was never built to solve a problem. It accreted as the twentieth century's systemic debate closed and the alternatives dropped out of curricula and public memory through generational turnover. There is no founding charter, no founding act, and no problem it was designed to solve — which is why its persistence needs no maintainer.
% FOUNDING_PROBLEM_CORROBORATION: No one attests a founding problem, and under this reading none exists to attest — the absence of any founding charter, founding organization, or founding act is itself the finding, and that absence is stated here explicitly rather than left implicit. The nearest outside attestation for the reading's genealogy comes from historians of economic thought and of the cooperative movements, whose archival work — conducted from no seat inside the arrangement — documents alternatives that were live within living memory and lapsed without being abolished.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.13, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).
:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.13) because the arrangement's costs are foregone options, not transfers: nothing is taken from anyone and delivered to anyone, and the reading's structural claim is that no class collects. The series rises from 0.04 (1950, alternatives still live in curricula and politics) to a 2010 peak (0.14) as the post-war systemic debate closed and the menu narrowed, then recedes slightly by 2025 as archival and heterodox scholarship begins recovering the alternatives. Suppression is authored as a raw structural property, unscaled by power or scope, and carries no temporal series: the reading's defining claim is that nothing enforces the default — no enforcement machinery exists to track, so the static scalar (0.10) carries the whole picture. Theater_ratio (0.28, endpoint) reflects ritualized naturalization rhetoric — textbooks that present the default as a starting point, commentary that treats market outcomes as verdicts of nature — which is a symptom of the lapse rather than its mechanism; the series peaks in the end-of-history years and recedes as the frame is questioned again. Accessibility_collapse is 0.55: the alternatives have collapsed out of live consideration but are documented and recoverable — well short of natural-law closure. Resistance is 0.15: the arrangement meets almost no organized resistance because it is invisible to the seats it costs; one cannot resist what one does not perceive as an arrangement. No beneficiaries are declared — the absence of an identifiable beneficiary class is this reading's core structural commitment, and declaring one would instantiate a different reading of the same kernel; the piton profile (no meaningful profiteer, inertia-borne persistence, prohibitive fix against diffuse gains) is the shape the structural data take. All temporal series share one grid (1950-2025 at fifteen-year steps) and every tracked metric is authored at every point; the trajectory is rise-plateau-slight-recession rather than cyclical, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge less in interest than in visibility. From the discipline seat, the naturalization is indistinguishable from ordinary practice — teaching exchange first is simply what economics is — so no arrangement is perceived at all, and that seat would compute its position from a state of no felt pressure. From the heterodox seat, the same structure is a lived exclusion: careers shaped by what the curriculum never covered. From the public seat, the arrangement is invisible by definition — the lapse's mechanism is that the missing options are not experienced as missing. From the historian seat, the whole arc is legible. The engine computes per-seat classifications from power, exit, and directionality; the structural point this story contributes is that the divergence here runs through perception, not coercion — no seat is forced, and the seats differ chiefly in whether they can see the arrangement at all.
 *
 * DIRECTIONALITY LOGIC:
 *   No seat sits at the beneficiary pole: the naturalization produces no receipt. The costs are deadweight — foregone options benefit no one — and the default's residual discourse economy is diffuse rather than captured, so gain_flow is authored as an affirmative 'diffuse' after checking every named seat (the discipline reproduces without collecting; the historians observe; the payers pay; the excluded are outside). The payer seats: alternative_economists bear the lapse directly, with directionality held below the full-target end because a secondary living as the default's designated critics gives them a partial stake in the frame they dispute; general_public bear a mild diffuse cost, modestly above symmetric since the discourse economy offsets part of the foregone-option cost. The discipline seat sits near symmetric: it pays in curricular narrowness and gains in discourse economy, with neither side dominant. Directionality overrides are authored for all three non-analytical power atoms because the derivation chain has almost no beneficiary/victim structure to read (one victim group declared) and would otherwise fall back to power-atom priors that encode administrator-and-beneficiary assumptions — importing the sibling readings' structure into a story whose claim is that no such structure exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The naturalization is mandatrophy without a mandate: it never had a founding problem to outlive, and its only function — organizing a systemic debate that has since closed — ended decades before the arrangement did. The R5 interview records the zombie profile directly: founding_problem_status dead against disappearance_verdict world_rearranges, the mismatch cell that flags a constraint persisting past its purpose, cross-checked here against the piton path (no capturing seat, prohibitive fix, diffuse gains). The classification guards against the two available misreadings: as enforced extraction, which would require the beneficiary class this reading denies (that misreading instantiates the sibling readings of the same kernel), and as natural law, which would deny the recoverability of the alternatives and the historical, artifact character of the arrangement. Mandatrophy is resolved: what persists is residue, and the honest description of residue is inertial rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading (lapsed_alternative_reading) of the kernel market_as_natural_default; the same standing arrangement carries two sibling readings — beneficiary_maintained_reading (naturalization actively defended post-hoc by incumbent beneficiaries) and hybrid_amnesia_reading (the lapse creates the conditions for beneficiary capture). Which sustaining mechanism does the arrangement actually contain?',
    'Systematic study of maintenance activity over the standing arrangement: curriculum politics, incumbent- and think-tank-funded defense of market framing, lobbying for the default''s policy machinery. Organized post-hoc defense collapses this reading toward the beneficiary-maintained sibling; documented lapse-then-capture collapses it toward the hybrid sibling.',
    'If a defending or capturing class is found, the constraint gains a beneficiary class, epsilon rises sharply, requires_active_enforcement flips true, and the classification leaves the inertial pole for an enforced structure; if the no-maintainer finding holds, this story''s low-epsilon inertial classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, empirical, 'Reading commitment for kernel market_as_natural_default: whether the naturalization is sustained by lapsed memory alone (this reading), active incumbent defense, or lapse-enabled capture.').

omega_variable(
    maintenance_activity_falsifier,
    'Is the naturalization of market arrangements genuinely unmaintained, or does maintenance activity exist that this reading''s lens does not register — funded popularization, curriculum defense, editorial alignment — at intensities too diffuse to look like enforcement?',
    'Audit of funding flows and the editorial record: trace who pays for the production and defense of naturalization rhetoric (business-school sponsorship, think-tank publications, op-ed placement) and whether defense activity spikes when the default is actually contested.',
    'Diffuse but real maintenance would raise effective epsilon above this reading''s ceiling, imply an implicit beneficiary class, and move the classification from inertial residue toward a defended structure; a null finding strengthens the no-maintainer claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_activity_falsifier, empirical, 'Whether the unmaintained status survives an audit of diffuse defense activity.').

omega_variable(
    alternatives_recoverability,
    'Are the lapsed alternatives recoverable as live options — archival closure — or has their material basis (legal forms, financing institutions, production know-how) decayed so far that revival would be reconstruction rather than recollection?',
    'Comparative study of revival attempts against the historical record: whether cooperative and commons revivals track archival recovery or require rebuilding infrastructure the record no longer contains.',
    'If the material basis has decayed, accessibility_collapse is understated and the arrangement is harder to reverse than the reading allows; if recoverable, the low-epsilon inertial classification holds and historical research remains a genuine route out of the default.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_recoverability, empirical, 'Whether the lapsed alternatives are archived-and-recoverable or materially decayed beyond revival.').

omega_variable(
    lapse_vs_frozen_choice,
    'Is the lapse agentless — generational turnover and archival attrition — or the residue of discrete past curricular and institutional choices that froze an earlier closure in place and left it unexamined?',
    'Intellectual history of the mid-century economics curriculum: whether the removal of alternative arrangements from canonical texts traces to diffuse turnover or to identifiable decisions whose effects were never revisited.',
    'Identifiable past choices would weaken the no-active-closure claim — an unexamined completed closure sits closer to the hybrid reading''s territory than to a pure lapse; an agentless lapse leaves this reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_vs_frozen_choice, conceptual, 'Whether the forgetting was agentless attrition or the residue of past choices left unexamined.').

omega_variable(
    authority_framing_underdetermination,
    'Is the naturalization''s authority structure genuinely diffuse — no adjudicating interpreter, the kernel held by epistemic habit alone — or does the discipline''s textbook-and-curriculum practice function as a de facto adjudicating interpreter of what the default means?',
    'Test whether disputes over the default''s content are adjudicated anywhere: if textbook markets, field journals, or hiring norms resolve what the frame includes, the practice-authority framing fits; if no seat resolves such disputes, the diffuse framing fits.',
    'Under the practice-authority framing the discipline seat becomes an interpretive authority with maintenance obligations and the no-maintainer claim weakens; under the diffuse framing this story''s classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Framing under-determination: diffuse epistemic authority versus de facto practice authority over the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement_basis(mark_tr_t1950, observed).
narrative_ontology:measurement(mark_tr_t1965, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement_basis(mark_tr_t1965, observed).
narrative_ontology:measurement(mark_tr_t1980, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(mark_tr_t1980, observed).
narrative_ontology:measurement(mark_tr_t1995, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(mark_tr_t1995, observed).
narrative_ontology:measurement(mark_tr_t2010, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement_basis(mark_tr_t2010, observed).
narrative_ontology:measurement(mark_tr_t2025, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(mark_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement_basis(mark_be_t1950, observed).
narrative_ontology:measurement(mark_be_t1965, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement_basis(mark_be_t1965, observed).
narrative_ontology:measurement(mark_be_t1980, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1980, 0.09).
narrative_ontology:measurement_basis(mark_be_t1980, observed).
narrative_ontology:measurement(mark_be_t1995, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 1995, 0.12).
narrative_ontology:measurement_basis(mark_be_t1995, observed).
narrative_ontology:measurement(mark_be_t2010, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement_basis(mark_be_t2010, observed).
narrative_ontology:measurement(mark_be_t2025, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 2025, 0.13).
narrative_ontology:measurement_basis(mark_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the market as natural default' conflates three structurally distinct constraints over one standing arrangement, decomposed per the epsilon-invariance principle into a kernel family. This story (lapsed_alternative_reading) authors the arrangement as an unmaintained artifact of lapsed memory: no beneficiary class, epsilon 0.13, inertial persistence. The sibling beneficiary_maintained_reading authors the same arrangement as actively defended post-hoc by incumbents — a beneficiary class, active enforcement, sharply higher epsilon. The sibling hybrid_amnesia_reading authors the lapse as stage one of a capture process — amnesia enabling extraction, moderate epsilon. Each story carries its own epsilon, beneficiaries, victims, and classification; the family is linked so contamination and drift analysis can track which sustaining mechanism the evidence favors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, institutional, 0.45).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, moderate, 0.8).
constraint_indexing:directionality_override(market_as_natural_default__lapsed_alternative_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
