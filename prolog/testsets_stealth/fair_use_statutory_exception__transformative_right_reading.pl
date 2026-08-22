% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative User Right (Section 107 as Administered)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   Fair use (17 U.S.C. 107 and its case law) permits unlicensed reuse of
 *   copyrighted work for transformative purposes. This story instantiates the
 *   transformative_right_reading of that statutory kernel: fair use exists to
 *   enable transformative reuse and cultural production, and courts are
 *   obligated to facilitate it. The epsilon referent is the standing
 *   arrangement under contest, fair use as actually administered (burden on
 *   the defendant, market-effect factor heavily weighted, defense costs borne
 *   by the use's proponent), assessed by this reading's own lights. On those
 *   lights the arrangement has a genuine, irreplaceable coordination function
 *   AND a live extraction mechanism: the same uncertainty that lets
 *   transformative uses proceed lets rights holders monetize the threat of
 *   suit, and the gains from that uncertainty accrue to rights holders as
 *   settlement leverage while the costs fall on the creators least able to
 *   bear them. The sibling readings (narrow_defense_reading,
 *   market_licensing_reading) are separate constraints with their own epsilon
 *   values over the same arrangement; they are not averaged or hedged here.
 *   KEY AGENTS (by structural relationship): federal_appellate_courts:
 *   agenda-setter (institutional/analytical) — administers the doctrine
 *   through precedent; rights_holder_litigants: enforcement seat and receipt
 *   of settlement value (institutional/mobile);
 *   large_scale_transformative_platforms: primary protected beneficiary
 *   (powerful/mobile); research_libraries_and_archives: protected beneficiary
 *   under institutional caution (organized/constrained);
 *   independent_documentary_filmmakers: primary target (powerless/trapped);
 *   small_scale_remix_creators: primary target (powerless/trapped);
 *   unlitigated_chilled_creators: excluded — the selection-effect absence;
 *   legal_academia: analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.58).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.55).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative User Right (Section 107 as Administered)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '4cb4c426-1b1f-4d83-b6bc-aed6717c0a59').
narrative_ontology:cs_kernel_codification('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', fixed_text).
narrative_ontology:cs_authority_grounding('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', lineage).
narrative_ontology:cs_interpretation_layer_present('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59').
narrative_ontology:cs_reading_relation('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', foundational, transformative_use_presumptive_right).
narrative_ontology:cs_axiom_status(transformative_use_presumptive_right, holdable).
narrative_ontology:cs_axiom_grounding('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', transformative_use_presumptive_right, deontological).
narrative_ontology:cs_axiom('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', foundational, licensing_markets_not_dispositive).
narrative_ontology:cs_axiom_status(licensing_markets_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', licensing_markets_not_dispositive, instrumental).
narrative_ontology:cs_axiom('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', secondary, shared_burden_allocation).
narrative_ontology:cs_axiom_status(shared_burden_allocation, holdable).
narrative_ontology:cs_axiom_grounding('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', shared_burden_allocation, conventional).
narrative_ontology:cs_reference_frame('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', transformative_user_right_framework).
narrative_ontology:cs_drift_state('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', contemporary_ai_training_litigation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4cb4c426-1b1f-4d83-b6bc-aed6717c0a59', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, large_scale_transformative_platforms).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, research_libraries_and_archives).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, rights_holder_litigants).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, independent_documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, small_scale_remix_creators).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, copyright_first_amendment_safety_valve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate fair use claims and author the precedents that define how the four statutory factors are weighed. They hear the cases that well-resourced parties choose to bring, so the doctrine's content is the residue of that selection. They cannot decline the docket and must decide within the statutory text; their precedents bind the circuits until the Supreme Court or Congress revises them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, federal_appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Publishers, record labels, studios, photo agencies, and estates that sue or threaten suit over unlicensed uses of their catalogs. They rarely need to win: a credible infringement claim against a transformative work forces settlement, clearance fees, or abandonment before any court rules. When they do win, the market-effect factor is re-weighted in their favor. They choose which cases to bring, which gives them agenda control over how the doctrine develops.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, rights_holder_litigants, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, rights_holder_litigants, beneficiary).

% Search engines, mass-digitization projects, and AI developers whose products depend on reusing copyrighted text and images at scale. They can absorb litigation costs, and when they win, the precedent shields an entire product category. They can structure products around litigated boundaries, delay suits until the technology is entrenched, and restructure operations across jurisdictions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, large_scale_transformative_platforms, beneficiary,
    powerful, generational, mobile, global).

% Institutions that digitize, preserve, and provide scholarly access to collections. They rely on the exception for mass digitization and preservation but operate under trustee, donor, and insurer constraints that push counsel toward the cautious side of unsettled questions. They cannot relocate their collections or their missions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, research_libraries_and_archives, beneficiary,
    organized, generational, constrained, national).

% Filmmakers using archival footage, music, or news clips in critical and historical work. Errors-and-omissions insurers routinely require them to license or cut material even where the use would likely qualify as fair, because defending a claim costs more than any film's budget. The work is finished when the claim arrives; the practical choice is pay or cut.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, independent_documentary_filmmakers, payer,
    powerless, biographical, trapped, national).

% Video essayists, mashup artists, and fan creators distributing transformative work through platforms with automated takedown systems. They face takedowns, channel strikes, and demonetization on mere accusation, and almost none can fund a fair use defense. Their recourse is to re-edit or stop publishing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, small_scale_remix_creators, payer,
    powerless, biographical, trapped, global).

% The population of transformative users who never reach a courtroom: they abandon projects, self-censor, or clear rights they might not legally need, so their claims never enter the case law that defines the doctrine. There is no record of the works that were never made, and no seat in any proceeding speaks for them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, unlitigated_chilled_creators, excluded,
    powerless, biographical, trapped, global).

% Scholars who map the doctrine's drift, run empirical studies of fair use outcomes, and argue readings of the statute. They author the theoretical case for treating fair use as a user right and the critiques of market-factor dominance. They bear no costs from the arrangement and hold no enforcement power.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_academia, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, rights_holder_litigants).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fair use solves a transaction-cost impossibility at the heart of copyright: transformative, non-substitutive uses (criticism, parody, search indexing, preservation, scholarship) require reuse of in-copyright work, but clearing rights for them is often impossible because owners are unidentifiable, numerous, or refuse as a matter of policy, and the act of asking can itself draw an infringement claim. The doctrine lets these uses proceed without prior license while leaving ordinary substitutive uses in the licensing market.
% TRANSFER_FUNCTION: The arrangement moves doctrinal protection to transformative users who can afford to litigate, taken from rights holders' exclusive control; and through the same machinery it moves settlement payments, clearance fees, and abandoned-project value from small transformative users to rights holders. Litigation risk is the currency: rights holders monetize doctrinal uncertainty, and well-resourced users purchase certainty with precedent.
% ABSENT_VOICES: The unlitigated chilled creators have no seat: the doctrine's content is built from litigated cases, which over-represent well-resourced parties on both sides, so the record flatters the arrangement by showing fair use working for everyone who reached court. Audiences and future users of suppressed works are also absent, as are the creators whose projects died in clearance before becoming cases.
% DISAPPEARANCE_RATIONALE: If the statutory exception and its case law vanished overnight, mass digitization, search indexing, documentary archival practice, and AI training would face immediate mass clearance problems or shutdown; licensing markets would expand to uses they cannot practically serve; and criticism, parody, and scholarship would contract to what rights holders consent to. The cultural-production economy is arranged around the exception.
% FOUNDING_PROBLEM: A literal exclusive-rights regime chokes the very activities copyright exists to serve: Folsom v. Marsh (1841) and the 1976 codification responded to the problem that criticism, commentary, and scholarship necessarily reuse copyrighted text, and treating every reuse as infringement would defeat copyright's constitutional purpose of promoting the progress of knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside this reading's beneficiary set: rights holders themselves rely on the exception internally for news and criticism about their own industries; appellate courts across ideological camps reaffirm the doctrine's necessity; comparative law shows other jurisdictions adopting fair use or expanded fair dealing; and the 1976 legislative history states explicitly that the exception preserves First Amendment-adjacent activity. No serious party contends that licensing markets alone solve the founding problem.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the doctrine's protection is real (the parody and mass-digitization lines of cases) but its exercise is taxed — the proponent bears the burden, defense costs run to seven figures before any ruling, and insurers force clearance regardless of merit. Suppression 0.55 is structural, not coercive: no one is forbidden to make transformative work; the arrangement makes asserting the right economically irrational for most of its intended beneficiaries. Theater 0.25: adjudication is genuine, but a growing share of market-harm evidence is econometric performance over hypothetical licensing markets that no one shows would actually exist. Accessibility_collapse 0.35: licensing, open licenses, and public-domain material are workable alternatives for some uses and no alternative at all for criticism of in-copyright work. Resistance 0.6: the arrangement is contested continuously in litigation, proposed legislation, and scholarship. The claim (tangled_rope) and the metrics are authored independently: the reading sees a coordination function it is committed to defending and an extraction mechanism it is committed to naming, and neither judgment was tuned to the other. The measurement series share one time grid (T0-T30, years since the 1994 parody decision) so every tracked metric is authored at every examined point; the rising trajectories model the reading's honest assessment that its ascendant moment is eroding as market-licensing logic returns through case law and as strategic enforcement intensifies with AI-training stakes. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change: the buildout of insurance-driven clearance regimes, platform takedown automation, and strategic litigation against transformative uses.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/receipt seat should compute differently. From the rights holders' seat the arrangement is a property regime with a safety valve they administer through selective enforcement; from the independent filmmaker's seat the same arrangement is a clearance regime they cannot afford to test; from the large platform's seat it is a litigated boundary they can afford to push and benefit from when it moves. The divergence between the powerless/trapped payer seats and the institutional/mobile receipt seat is the measurement this story exists to take; the engine computes it from the structural data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: large-scale transformative platforms and research libraries hold low d (the doctrine subsidizes them with protection they do not pay per-use for). Rights holders are declared beneficiaries on the extraction side: doctrinal uncertainty subsidizes them with settlement leverage and licensing pressure, which is this reading's central structural claim about the standing arrangement — the parties nominally constrained by the exception are the ones the uncertainty pays. Victims: independent documentary filmmakers and small remix creators hold high d — they bear the arrangement's costs (litigation risk, takedowns, forced clearance) while belonging to the class the doctrine nominally protects; the extraction lands on the least resourced members of the protected class, which is the tangled structure. Courts carry no benefit or cost declaration and take the institutional fallback. No directionality overrides are authored: the derivation from declarations and exit options captures the structure, and no override is authored for the excluded chilled-creator seat because an authored absence must not drive classification overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the standing arrangement as tangled_rope rather than rope prevents this reading's own advocacy from laundering the extraction: the reading is tempted to describe fair use as pure coordination (a user right needing only better facilitation), which would erase the settlement-leverage mechanism that currently funds opposition to facilitation. Classifying it as tangled_rope rather than snare prevents the inverse error: the coordination function is real, irreplaceable, and would not be reproduced by licensing markets, so a snare classification would license abolition and harm the reading's own beneficiaries. The founding problem is live and corroborated from outside the beneficiary set, so no mandatrophy declaration is authored: the arrangement is not persisting past its function; it is persisting with a parasitic extraction mechanism attached to a live function. Fixing the extraction (shared burden, market factor demoted from dispositive weight) is cost-prohibitive for the seats that could fix it: the legislative path has been blocked for decades and the doctrinal path requires the very courts whose precedent constitutes the drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_fair_use_kernel,
    'This constraint is one reading of the fair_use_statutory_exception kernel — the transformative_right_reading. What do the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compile and compare the sibling stories (fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading) against this one; the disagreement is located in three specific structural elements: the dispositive weight of the market-effect factor, the burden allocation on the use''s proponent, and whether a user right or a property defense names the exception''s legal character.',
    'The market_licensing_reading flips the victim set — rights holders become the injured party and any licensable use becomes extraction — moving epsilon to the substitutive-use side of the ledger; the narrow_defense_reading keeps the burden on defendants and construes narrowly, raising epsilon for all unlicensed uses. This story''s epsilon (0.58 over the standing arrangement) is valid only for this reading; the three values are not measurements of one thing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_fair_use_kernel, conceptual, 'Kernel-reading indexicality: epsilon is authored per reading of the fair use kernel, and sibling readings instantiate different constraints.').

omega_variable(
    burden_allocation_extraction_share,
    'How much of the measured extraction is produced by the burden placement on the fair use proponent rather than by the four factors'' substantive content?',
    'Natural experiments from jurisdictions or proposals that shift the burden or add pre-certification (expanded fair dealing regimes, proposed fair-use registries, insurer practice changes): if extraction falls when burden moves without any change in factor doctrine, burden placement is the dominant extractor.',
    'If burden placement dominates, a procedural fix would collapse most measured extraction without touching substantive doctrine, and this reading''s demand for shared burden is the operative reform; if substantive factor weighting dominates, procedural reform would leave the arrangement''s extraction intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_allocation_extraction_share, empirical, 'Decomposition of extraction into burden-placement and substantive-doctrine components.').

omega_variable(
    chilling_selection_effect,
    'The case-law record over-represents resourced parties on both sides; the true population of suppressed transformative uses is unobservable from decisions. How large is the unlitigated chilling mass, and does it track enforcement intensity?',
    'Creator surveys, errors-and-omissions insurance clearance data, and platform takedown statistics compared against litigated outcome rates over the same interval.',
    'A large unlitigated mass would mean the true extraction exceeds the case-law-derived measure and that doctrinal reform keyed to litigated cases under-corrects; a small mass would support reading the case law as representative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_selection_effect, empirical, 'Selection-effect uncertainty in the observable record of suppressed transformative uses.').

omega_variable(
    market_factor_weight_drift,
    'Is the market-effect factor''s contemporary weight a stable feature of the standing arrangement or a reversible drift from the transformative-use framework — and will AI training litigation re-weight it again?',
    'Track post-2023 appellate treatment of transformativeness and market harm across circuits, and observe how AI training cases resolve the tension between mass transformative reuse and emerging licensing markets.',
    'If the drift is reversible, this reading''s reference frame remains restorable through ordinary adjudication; if entrenched — or if AI litigation entrenches licensing-market logic — the standing arrangement converges on the market_licensing_reading''s structure and this reading''s epsilon rises further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_factor_weight_drift, empirical, 'Whether market-factor dominance is drift or equilibrium, and the AI-training stress test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t6, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(fair_tr_t6, observed).
narrative_ontology:measurement(fair_tr_t12, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(fair_tr_t12, observed).
narrative_ontology:measurement(fair_tr_t18, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(fair_tr_t18, observed).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(fair_tr_t24, observed).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(fair_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t6, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(fair_be_t6, observed).
narrative_ontology:measurement(fair_be_t12, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(fair_be_t12, observed).
narrative_ontology:measurement(fair_be_t18, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement_basis(fair_be_t18, observed).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(fair_be_t24, observed).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(fair_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t6, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(fair_su_t6, observed).
narrative_ontology:measurement(fair_su_t12, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(fair_su_t12, observed).
narrative_ontology:measurement(fair_su_t18, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(fair_su_t18, observed).
narrative_ontology:measurement(fair_su_t24, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement_basis(fair_su_t24, observed).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(fair_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'fair use' covers three structurally distinct readings of one statutory kernel (Section 107). This story instantiates the transformative_right_reading and authors epsilon (0.58) for the standing arrangement — fair use as administered — assessed by this reading's lights: moderate extraction from transformative users via litigation risk and settlement leverage. The siblings author different epsilon over the same referent: the narrow_defense_reading sees the arrangement as roughly faithful to a property defense (low extraction, burden properly placed), and the market_licensing_reading locates the extraction on the rights-holder side (uncompensated licensable use as the harm). The stories are linked because each cites the same statutory text and the same case law as evidence; the upstream doctrinal moment (the 1994 transformative-use decision) structures downstream application across all three readings, which is why the edges run from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
