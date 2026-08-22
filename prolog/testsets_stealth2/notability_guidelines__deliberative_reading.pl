% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N Notability Boundary as Deliberative Negotiation Process (Deliberative Reading)
 *   domain: digital commons governance / knowledge infrastructure / platform constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's notability guideline (WP:N), as operated through the Articles
 *   for Deletion process, is authored here under the deliberative reading of
 *   the notability_guidelines kernel: notability is not a fixed property the
 *   text states but an evolving output of structured negotiation, and the
 *   guideline text functions as the provisional running summary of where the
 *   community's inclusion boundary currently sits. The referent of every
 *   structural value in this file is the standing arrangement — the guideline
 *   text together with the discussion machinery that applies, tests, and
 *   rewrites it — assessed by this reading's own lights. Under this reading
 *   the text is constitutionally subordinate to live consensus (the project's
 *   own norms hold that consensus can override any guideline, and
 *   ignore-all-rules invocations are legitimate), each community-wide
 *   revision retires the prior formulation, and the arrangement's
 *   justification is the transition it carries: moving the boundary from
 *   whatever it was toward whatever the evidence next supports. Deletion is
 *   the arrangement's enforcement act, but under this reading each deletion
 *   is also a data point that feeds boundary calibration through appeal,
 *   renomination, and eventual guideline revision. The sibling readings of
 *   the same kernel are separate constraints, not components of this one; see
 *   the kernel_reading_contest omega and the network decomposition note.
 *
 * KEY AGENTS:
 *   - wikipedia_editing_community: collective beneficiary and standing agenda-setter (organized/constrained) — staffs the process, owns the text, receives the calibrated standard it outputs; cannot exit its own governance short of forking
 *   - afd_closers_and_guideline_editors: process administrators (organized/mobile) — close discussions and revise the text; unpaid, holding delegated authority and peer standing rather than any collected share
 *   - marginal_topic_article_authors: primary cost-bearers (moderate/constrained) — bear deletion risk and lost labor wherever the current boundary sits against their topics
 *   - encyclopedia_readers: diffuse beneficiary, structurally voiceless (powerless/mobile) — receive the admitted coverage, never the excluded; no standing in the discussions that draw the line
 *   - deleted_article_subjects: excluded parties (powerless/trapped) — adjudicated by discussions they are barred from entering by conflict-of-interest norms
 *   - wikimedia_foundation: platform-level beneficiary (institutional/arbitrage) — hosts the process, collects legitimacy and governance-cost relief, governs no content
 *   - wiki_governance_researchers: analytical observer (analytical/analytical) — measures outcomes, participation demographics, and boundary drift; feeds results back into guideline revision debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.26).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N Notability Boundary as Deliberative Negotiation Process (Deliberative Reading)").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital commons governance / knowledge infrastructure / platform constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, 'e5b431b6-d603-46d5-b103-e4ef567166f4').
narrative_ontology:cs_kernel_codification('e5b431b6-d603-46d5-b103-e4ef567166f4', formalized).
narrative_ontology:cs_authority_grounding('e5b431b6-d603-46d5-b103-e4ef567166f4', practice).
narrative_ontology:cs_interpretation_layer_present('e5b431b6-d603-46d5-b103-e4ef567166f4').
narrative_ontology:cs_reading_relation('e5b431b6-d603-46d5-b103-e4ef567166f4', notability_guidelines__deletionist_reading, influences).
narrative_ontology:cs_reading_relation('e5b431b6-d603-46d5-b103-e4ef567166f4', notability_guidelines__inclusionist_reading, influences).
narrative_ontology:cs_axiom('e5b431b6-d603-46d5-b103-e4ef567166f4', foundational, notability_is_process_output).
narrative_ontology:cs_axiom_status(notability_is_process_output, holdable).
narrative_ontology:cs_axiom_grounding('e5b431b6-d603-46d5-b103-e4ef567166f4', notability_is_process_output, conventional).
narrative_ontology:cs_axiom('e5b431b6-d603-46d5-b103-e4ef567166f4', foundational, consensus_overrides_guideline_text).
narrative_ontology:cs_axiom_status(consensus_overrides_guideline_text, holdable).
narrative_ontology:cs_axiom_grounding('e5b431b6-d603-46d5-b103-e4ef567166f4', consensus_overrides_guideline_text, conventional).
narrative_ontology:cs_reference_frame('e5b431b6-d603-46d5-b103-e4ef567166f4', deliberative_process_primacy).
narrative_ontology:cs_drift_state('e5b431b6-d603-46d5-b103-e4ef567166f4', mature_sng_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e5b431b6-d603-46d5-b103-e4ef567166f4', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_editing_community).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, encyclopedia_readers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikimedia_foundation).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, marginal_topic_article_authors).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, consensus_primacy_doctrine).
narrative_ontology:constraint_vindicates(notability_guidelines__deliberative_reading, descriptive_guideline_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A volunteer collective that writes, argues over, and maintains the encyclopedia. It staffs the deletion discussions, revises the notability guideline text through community-wide requests for comment, and lives inside the boundary decisions the process produces. Its members supply the labor the process runs on and receive the shared, revisable standard the process outputs; no member is paid, and the collective cannot leave its own governance — exit means forking the project, which has happened rarely and at high cost.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_editing_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, wikipedia_editing_community, agenda_setter).

% Experienced editors and administrators who close deletion discussions, weigh arguments against the guideline text and prior closures, and draft guideline revisions. They hold process authority delegated by the community, serve unpaid, and can step back from discussion work at any time without losing their place in the project; what they accumulate is standing among peers, not revenue. When their closures are contested they face review at deletion review, where their reasoning is examined line by line.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, afd_closers_and_guideline_editors, agenda_setter,
    organized, biographical, mobile, global).

% Editors — often newer or single-topic contributors — whose articles on niche subjects are nominated for deletion when the current boundary sits against them. They lose the labor invested in the article and sometimes the will to keep editing; the article text survives in page history and can be restored if better sources emerge or consensus shifts. Their remedies run through the same process that removed the article: arguing at the discussion, appealing at deletion review, renominating later, or proposing guideline changes. Many simply stop contributing to the affected topic area or leave the project.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginal_topic_article_authors, payer,
    moderate, biographical, constrained, global).

% The people the encyclopedia is written for. They receive whatever coverage the current boundary admits and never see what it excludes. They have no vote or standing in deletion discussions; their preferences enter only indirectly, when editors cite pageview statistics or argue about what readers want. They can read anything anywhere else at no cost, so nothing binds them — but nothing consults them either.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, encyclopedia_readers, beneficiary,
    powerless, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, encyclopedia_readers, excluded).

% People and organizations whose lives, works, or activities are adjudicated in deletion discussions about articles concerning them. Conflict-of-interest norms bar them from advocating for their own articles, and the encyclopedia's status as the record of record means a deletion decision about them follows them regardless of their preference. They cannot exit the question — it is about them — and their only channel is third parties who happen to care enough to argue.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deleted_article_subjects, excluded,
    powerless, biographical, trapped, global).

% The nonprofit that hosts the servers, holds the trademarks, and carries the legal exposure for the project. By long-standing policy it does not govern content, including notability; the community's self-run boundary process keeps content decisions and their controversies out of the Foundation's hands while the encyclopedia gets built at near-zero governance cost to it. It could restructure community governance but has historically declined to, and it operates many sister projects it could reallocate attention across.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikimedia_foundation, beneficiary,
    institutional, generational, arbitrage, global).

% Academic and independent researchers who study deletion rates, discussion outcomes, participation demographics, and guideline revision history. They publish analyses that circulate back into community debates and guideline requests for comment, and they hold no stake in any particular boundary outcome.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wiki_governance_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deliberative_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deliberative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets and revises the encyclopedia's inclusion boundary without a central editor: deletion discussions aggregate case-level arguments, evidence, and precedent into per-topic decisions, and community-wide requests for comment fold accumulated case-level drift back into revised guideline text. It solves the problem of how a leaderless volunteer community draws a line at scale while keeping the line answerable to changing evidence.
% TRANSFER_FUNCTION: Moves editorial labor and attention from article creation into boundary deliberation; moves deletion risk onto authors of topics the current boundary excludes; and moves case-by-case decision authority to whichever participants show up with the most policy-grounded arguments — while moving the accumulated standard itself back into common ownership through guideline revision.
% ABSENT_VOICES: Readers have no standing in deletion discussions — their preferences surface only as pageview statistics cited by editors. Article subjects are barred by conflict-of-interest norms from advocating for their own articles. Authors who lose a discussion and disengage take their objection out of the room permanently. And the not-yet-notable — topics whose significant coverage has not yet been published — are argued about by editors who will never overlap with the future editors who would defend them. All of them are outside the discussion space: readers in article space, subjects outside the project entirely, lost authors gone from the project, future defenders not yet arrived.
% DISAPPEARANCE_RATIONALE: If the guideline and its discussion machinery vanished overnight, inclusion decisions would not stop — every encyclopedia draws a line — but they would be made through ad hoc administrator fiat, mass revert wars, or uncritical mass-inclusion, and the community would have to rebuild a structured boundary mechanism within months. The calibration of coverage to evidence, the appeal paths, and the revisable text are all arrangements the project currently depends on.
% FOUNDING_PROBLEM: Early Wikipedia had no answer to unbounded growth: with anyone able to create an article, vanity pages, local businesses, school bands, and self-published authors flooded in, and there was no shared, non-hierarchical way to decide what belonged. The founding problem was how a volunteer community with no editor-in-chief draws an inclusion line at scale — and how to do it without installing a hierarchy that would end the project's openness.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: published Wikipedia-governance research (deletion-rate and AfD-outcome studies) documents the boundary question recurring across every new content class — podcasts, esports players, webcomics, AI-generated works; Wikimedia movement-strategy documents acknowledge unresolved growth-versus-quality tensions; and the behavioral record of perennial notability RfCs, each triggered by a novel topic type, attests that no stable solution has been reached. What no one outside the process attests is any particular boundary line — the corroboration covers the problem's liveness, not any answer's correctness.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.28: by this reading's lights the arrangement's costs are real — deliberation labor, deletion losses borne by marginal-topic authors — but the outputs are collectively held, every deletion is reversible in principle, and no seat captures the product, so net extraction sits modestly above the coordination floor and well below capture levels. Suppression is authored at 0.26 as a raw structural property — it is not scaled by power or scope in the engine's computation; only extractiveness is scaled, by directionality and scope. Deletion coerces, but it is appealable, unpenalized, and reversible, and alternatives persist (draft space, other wikis, renomination when sources emerge), so structural coercion is low-moderate. Theater is 0.20: the core of the process is functional argument, with a slowly accumulating ritual layer of policy-badge citation and vote-counting that the measurement series tracks. Accessibility collapse is 0.35: the arrangement channels alternatives rather than foreclosing them. Resistance is 0.40: perennial proposals to abolish notability, deletion-review appeals, and inclusionist counter-mobilization meet the arrangement continuously — much of which this reading counts as the mechanism operating rather than as opposition to it. Claim and metrics are independent authored facts: claimed_type scaffold is authored from the reading's structure (transitional text, perpetual negotiation, operative text-level sunset at every revision cycle — consensus can retire any formulation at any time, and periodically does), while the metrics are authored from observed operation; neither was tuned toward the other. The measurement series share one time grid (annual units, T0 at the guideline's early formalization era, T18 the present) and tell one story: enforcement softened (suppression_requirement 0.45 to 0.26) as alternative-to-deletion and draftification norms matured, extraction eased (0.38 to 0.28), and ritual accumulated slowly (0.12 to 0.20).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. The marginal-topic author seat experiences the arrangement as a loss-imposing filter whose current line happens to sit against them — their computed seat type should sit toward the extractive end. The closer and community seats experience the same structure as self-governance they staff, argue in, and revise — closer to coordination. The excluded seats experience it as a decision rendered about them without them: readers receive the admitted coverage and never the excluded; article subjects are adjudicated by discussions that conflict-of-interest norms bar them from entering. Same text, same process, four different constraints from four seats; the engine computes the per-seat divergence from the structural data, and this story does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (editing community, readers, Foundation) derive directionality near the subsidized end; the declared victim group (marginal-topic article authors) derives near the full-target end, amplified by their constrained exit. The closer seat carries no beneficiary or victim declaration — closers administer at personal labor cost for peer standing, roughly symmetric, and fall to the derivation's default for their power atom. Readers' structural voicelessness does not change their material directionality (they pay nothing into the arrangement) but it shapes the absent-voices record. Global scope applies the engine's verification-difficulty amplification to effective extraction; with base extraction at 0.28 the amplified value remains moderate. No directionality overrides are used: the derivation from declarations plus exit options produces the right relationships, and the override mechanism — keyed by power atom rather than by agent — is too coarse here to separate the two organized seats (the net-beneficiary community from the roughly symmetric closers) without distorting one of them. On the receipt surface: gain_flow is authored as diffuse as an affirmative claim, not a default — each named seat was checked and none captures the extraction: closers collect standing, not rents; the community receives a commons product its own members labor to produce; readers receive coverage they paid nothing into; the Foundation receives legitimacy and governance-cost relief, not the process outputs; deletion losers pay. fixing_cost is authored prohibitive on its own evidence: removing the text is cheap (revision authority is fully distributed and real), but the fix the arrangement awaits — a settled terminal boundary that would let the scaffold retire — does not exist at any cost, because every fixed line has failed against the next content class; the cost of an effective fix is therefore prohibitive relative to its unavailable benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim does the mandatrophy work in both directions. Against the deletionist temptation — reading the current text as a finished coordination standard — the sunset structure is operative and frequent: each revision cycle formally retires the prior formulation, and consensus overrides the text at individual discussions daily, so the arrangement has not quietly congealed into a rope. Against the inclusionist temptation — reading the process as entrenched extraction — the receipt surface shows no capturing seat (unpaid closers, common-owned text, reversible deletions), so it has not congealed into a snare either. The founding problem is live — new topic classes keep invalidating any fixed line — so the mandate has not outlived its function, and the R5 interview records that liveness with corroboration from outside the benefiting parties. The risk this reading actually carries is Goodhart drift: the theater series rises slowly as policy citation substitutes for argument, and the monitoring threshold is theater_ratio near 0.5, at which point the piton question would open. The deeper question — whether a transition with no terminus is still a transition, or whether transitional is doing permanent load-bearing work — is carried by the scaffold_terminus_existence omega rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the deliberative_reading of the notability_guidelines kernel. Would instantiating the deletionist_reading or the inclusionist_reading of the same arrangement produce a structurally different constraint, and where exactly is the disagreement located?',
    'The three readings locate the operative constraint differently: the deletionist reading treats the guideline text as the constraint (notability as input; epsilon re-authored higher, with coverage-quality losses as the victim structure); the inclusionist reading treats the enforcement pattern as the constraint (notability as gatekeeping; epsilon re-authored sharply higher, with marginalized knowledge producers as victims); this reading treats the deliberative process as the constraint (notability as output). Cross-reading tests against the same AfD outcome record — whether closures track the text''s criteria, participant demographics, or argument quality — resolve which structure the arrangement actually instantiates.',
    'If closures track the text, this story''s epsilon is under-authored and the deletionist constraint is the right object of classification; if closures track participant demographics, the inclusionist constraint is; if they track argument quality, this reading stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this is one of three live readings of the WP:N kernel; the siblings re-author epsilon and the victim structure over the same arrangement.').

omega_variable(
    deliberation_vs_elite_deference,
    'Does AfD deliberation function as the negotiation this reading posits — aggregating independent judgments into boundary outputs — or does participant seniority systematically determine closures, making perpetual negotiation a deference structure to an experienced-editor core?',
    'Regression of closure outcomes on argument features (source quality, policy-grounded reasoning) versus participant features (account age, edit count, administrator status); natural experiments from participation shocks such as mass-attention controversies and drive-by nomination waves that temporarily change who is in the room.',
    'If seniority dominates outcomes, the arrangement is closer to extraction-by-deference: epsilon rises, the no-capture receipt claim weakens, and the inclusionist sibling reading gains structural force. If argument features dominate, the low-extraction authoring and the diffuse gain-flow claim stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_vs_elite_deference, empirical, 'Whether the negotiation aggregates judgments or deference to senior participants decides them.').

omega_variable(
    scaffold_terminus_existence,
    'Is there a stable terminal notability boundary the negotiation could converge to, or is the boundary necessarily perpetual because each new content class invalidates any fixed line — in which case, is a transition with no terminus still a transition?',
    'Longitudinal analysis of the guideline''s revision history: successive RfC deltas that shrink indicate convergence toward a terminus the scaffold could sunset into; deltas that oscillate or track external novelty waves (new media forms, new topic types) indicate no terminus exists.',
    'If a terminus exists, the constraint is mis-typed as perpetual scaffold and should complete into a fixed coordination standard; if none exists, the scaffold is load-bearing indefinitely, its transitional justification is permanent, and fixing it means something other than retiring it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_terminus_existence, empirical, 'Whether the deliberative scaffold''s transition has a completable terminus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nota_tr_t3, notability_guidelines__deliberative_reading, theater_ratio, 3, 0.14).
narrative_ontology:measurement(nota_tr_t6, notability_guidelines__deliberative_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(nota_tr_t9, notability_guidelines__deliberative_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deliberative_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(nota_tr_t18, notability_guidelines__deliberative_reading, theater_ratio, 18, 0.2).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nota_be_t3, notability_guidelines__deliberative_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(nota_be_t6, notability_guidelines__deliberative_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(nota_be_t9, notability_guidelines__deliberative_reading, base_extractiveness, 9, 0.31).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deliberative_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(nota_be_t18, notability_guidelines__deliberative_reading, base_extractiveness, 18, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nota_su_t3, notability_guidelines__deliberative_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(nota_su_t6, notability_guidelines__deliberative_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(nota_su_t9, notability_guidelines__deliberative_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deliberative_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(nota_su_t18, notability_guidelines__deliberative_reading, suppression_requirement, 18, 0.26).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, inclusionist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label WP:N covers three structurally distinct claims about the same arrangement, decomposed per the epsilon-invariance principle into a three-story kernel family: deletionist_reading (the text as necessary epistemic quality filter), deliberative_reading (this file — the deliberative process as the operative constraint, the text as its provisional summary), and inclusionist_reading (the enforcement pattern as systematic gatekeeping). The readings differ in epsilon, victim structure, and claimed type, and cannot be merged without making epsilon observer-relative. The deletionist reading is the upstream, historically established framing; this reading sits midstream — the deliberative process is the venue where the deletionist text is continuously tested and the inclusionist capture claim is continuously evidenced or refuted, so this reading's operation structurally re-prices both siblings without foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
