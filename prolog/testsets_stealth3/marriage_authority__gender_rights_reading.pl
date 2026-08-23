% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Gender-Rights Reading of Marriage Authority: Patriarchal Personal Law under Constitutional Equality Contest
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the gender_rights_reading — of the
 *   contested marriage_authority kernel. The standing arrangement under
 *   contest (and therefore the ε referent, per the kernel-reading rule) is
 *   the patriarchal personal-law regime itself: community-administered
 *   marriage, divorce, maintenance, and inheritance rules that subordinate
 *   women, held in place by communal enforcement and state deference, and
 *   contested through a judicial channel that expands constitutional equality
 *   guarantees against discrete practices (instant unilateral divorce,
 *   maintenance denial, property exclusion) without reaching the structure.
 *   The reading's endorsed alternative — a fully equalized regime — is NOT
 *   the referent; ε is authored for the existing arrangement as this reading
 *   sees it, which is why ε is high. Sibling readings (communal_autonomy,
 *   secularist, federalist_millet, judicial_harmonization) are separate
 *   constraint files, not positions inside this one. KEY AGENTS (by
 *   structural relationship): - patriarchal_religious_establishment: Primary
 *   agenda-setter (institutional/constrained) — administers the rules,
 *   collects jurisdiction and status, enforces compliance -
 *   male_household_heads: Primary material beneficiary (moderate/arbitrage) —
 *   hold unilateral divorce, property control, maintenance avoidance;
 *   forum-shop between systems - women_within_patriarchal_personal_law:
 *   Primary target (powerless/trapped) — bear the terms; exit means children,
 *   community, and livelihood at once - women_rights_advocates:
 *   Contest-economy beneficiary (organized/mobile) — collect standing,
 *   funding, and wins from the ongoing contest - constitutional_courts:
 *   Reform-channel agenda-setter (institutional/analytical) — reshape the
 *   arrangement case by case - internal_theological_reformers: Excluded voice
 *   (moderate/constrained) — in-tradition equality argument shut out of both
 *   forums - community_based_womens_groups: Excluded voice
 *   (moderate/constrained) — absorb backlash, were not consulted on
 *   criminalization
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.84).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.78).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Gender-Rights Reading of Marriage Authority: Patriarchal Personal Law under Constitutional Equality Contest").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '4d681708-2ddc-470e-bce9-e03fc7fc9fcf').
narrative_ontology:cs_kernel_codification('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', fixed_text).
narrative_ontology:cs_authority_grounding('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', lineage).
narrative_ontology:cs_interpretation_layer_present('4d681708-2ddc-470e-bce9-e03fc7fc9fcf').
narrative_ontology:cs_reading_relation('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', foundational, intra_community_equality_overrides_communal_marriage_authority).
narrative_ontology:cs_axiom_status(intra_community_equality_overrides_communal_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', intra_community_equality_overrides_communal_marriage_authority, deontological).
narrative_ontology:cs_axiom('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', secondary, courts_may_enforce_equality_in_personal_law_absent_legislation).
narrative_ontology:cs_axiom_status(courts_may_enforce_equality_in_personal_law_absent_legislation, holdable).
narrative_ontology:cs_axiom_grounding('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', courts_may_enforce_equality_in_personal_law_absent_legislation, conventional).
narrative_ontology:cs_reference_frame('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', constitutional_equality_supremacy_over_personal_law).
narrative_ontology:cs_drift_state('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', contemporary_post_criminalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d681708-2ddc-470e-bce9-e03fc7fc9fcf', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, patriarchal_religious_establishment).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the community's marriage, divorce, and maintenance rules through councils, clerical offices, and family arbitration. Decides which marriages are valid, which divorces are effective, and what support a dismissed wife may claim. Collects jurisdiction, status, and fees from administering this role, and enforces compliance through social sanction, religious ruling, and pressure on kin. Its authority depends on the state continuing to recognize its decisions, so it cannot walk away from the arrangement without dissolving its own standing.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, patriarchal_religious_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, patriarchal_religious_establishment, beneficiary).

% Hold unilateral divorce rights, control household property, and can end marriages without consent or compensation under the prevailing rules. When the rules cut against them they invoke civil courts or statutory exceptions; when the rules favor them they insist on religious jurisdiction. This ability to choose the forum is what keeps the arrangement cheap for them to maintain.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_household_heads, beneficiary,
    moderate, biographical, arbitrage, national).

% Marry, bear children, and run households under rules they did not author and cannot renegotiate: divorce at will against them, maintenance discretionary and often denied, property and inheritance channeled along male lines. Leaving means losing children, community, and livelihood at once; staying means absorbing the terms. Many carry the terms as religious duty, which closes the exit from the inside as well as the outside.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).

% Litigate test cases, run legal-aid and campaign organizations, and speak for affected women before courts and treaty bodies. Landmark rulings bring funding, standing, and institutional growth; each new violation supplies the next case. Their material position improves with the contest's continuation, and most could redirect to other causes if the mandate ended, though the constituency they serve would not automatically follow them out.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Hear challenges to specific practices, strike down or reshape them case by case, and articulate the equality principles the arrangement must eventually accommodate. They set the pace and sequence of change without legislating a replacement structure, and they depend on litigants and advocates bringing violations to their dockets.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, constitutional_courts, observer).

% Scholars and clerics who argue the tradition itself supports equal divorce, maintenance, and inheritance terms. The establishment brands them as outside agitators and the national advocacy pipeline treats them as unreliable allies, so their route to influence runs through neither the community's forums nor the courtroom strategy.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, internal_theological_reformers, excluded,
    moderate, generational, constrained, national).

% Organizations rooted in the affected neighborhoods that handle the aftermath of every ruling: they shelter dismissed wives, negotiate support informally, and absorb the communal backlash that follows each courtroom win. Several opposed criminalizing the unilateral divorce practice on the ground that jailing husbands leaves their members poorer; their position was folded into neither the litigation strategy nor the statute.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, community_based_womens_groups, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, male_household_heads).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides communities with continuous, self-administered family law: marriage validity, dissolution, and support settled inside the group by familiar authorities, preserving religious identity and sparing the state a domain it long declined to govern. The judicial channel gives the constitutional order a pressure valve that absorbs equality claims one practice at a time without forcing a general confrontation over who governs family life.
% TRANSFER_FUNCTION: Moves decision power over women's marital status, residence, maintenance, and property from the women themselves to male kin and communal offices; moves the visible gains of each reform round — rulings, reputations, funding, standing — to the advocacy and judicial sectors; and moves the costs of every contest, from ostracism to retaliatory hardening, back onto the women whose cases supplied it.
% ABSENT_VOICES: Internal theological reformers who could ground equal terms inside the tradition are excluded from both community forums and courtroom strategy; neighborhood women's groups that must live with each ruling's aftermath were not consulted before criminalization; and the large majority of affected women who never litigate are spoken for by test-case plaintiffs whose consent to symbolize them is presumed rather than asked.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, marriage, divorce, maintenance, and inheritance within these communities would fall to constitutional defaults immediately; communal offices would lose the jurisdiction that constitutes them; the advocacy sector would lose its mandate; and household bargaining would reopen on terms the current rules were built to foreclose.
% FOUNDING_PROBLEM: Securing communal religious continuity in family life: colonial codification and post-colonial state-building froze each community's personal law as the price of allegiance, guaranteeing groups that family governance would remain theirs.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship documents the colonial bargain and its post-colonial continuation; constitutional court judgments and treaty-body reviews attest the equality failure the arrangement now sustains; statements by women inside the affected communities attest both the value they place on communal identity and the terms they reject. No attestation comes from the arrangement's principal beneficiaries, whose defense of the founding problem is self-interested by construction.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.84 at interval end) because the arrangement transfers decision power over women's marital status, support, and property to male kin and communal offices, and because the reform channel's gains land elsewhere than on the mass of affected women. Suppression (0.78) is a raw structural property, unscaled by power or scope: ostracism, economic dependency, custody threat, and state deference close exits externally, while internalized duty scripts close them internally — the split is carried by the suppression_structural_vs_internalized omega. Theater (0.46) rises across the interval because verdicts, hearings, and commemorations accumulate faster than material change: the reform channel performs progress while the structure adapts. Accessibility_collapse (0.68) reflects formally available civil alternatives that collapse in practice under cost, information, and community-sanction barriers; resistance (0.62) reflects sustained feminist litigation, internal reform currents, and neighborhood organizing. The measurement series share one grid (T=0..75, mapping 1950–2025, anchored on the 1985 maintenance reversal, the 2001 statutory compromise, the 2017 unilateral-divorce ruling, and the subsequent criminalization). The extractiveness series oscillates mildly rather than drifting monotonically: each landmark win produces a dip, followed by communal hardening, practice adaptation, and recovery — the cycle is partly the mechanism itself (intermittent reinforcement: communities learn that waiting out reform restores the terms), which is why suppression_requirement rises monotonically even as extraction oscillates. Coalition potential for the powerless seat exists — neighborhood women's federations and savings collectives have real latent bargaining power — but it is fragmented by communal boundary-policing that frames cross-community women's alliances as betrayal, which is itself an enforcement product. Claim and metrics are independent authored facts: I claim snare because the coordination story (religious continuity) functions as cover for extraction whose persistence depends on coercive enforcement and suppressed exits, with identifiable victims; the engine computes per-seat classifications from the structural data regardless of this claim.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is the finding. From the women_within_patriarchal_personal_law seat the arrangement is unredeemed extraction with a closed exit. From the patriarchal_religious_establishment seat it is legitimate inherited jurisdiction under siege by courts and activists. From the male_household_heads seat it is ordinary family life, occasionally inconvenienced by litigation and easily routed around by forum choice. From the women_rights_advocates seat it is a progressive arc in which each case builds on the last. From the constitutional_courts seat it is incremental harmonization toward a constitutional floor. Same structure, five incompatible experiences; the engine derives this from power, exit, and directional data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. male_household_heads sit near the beneficiary end: they collect the arrangement's material yields and hold arbitrage-grade exit (forum-shopping between religious and civil venues), so effective extraction from them is minimal or inverted. patriarchal_religious_establishment combines agenda-setting with collection (secondary beneficiary): low d, subsidized by the arrangement it runs. women_within_patriarchal_personal_law sit near the full-target end: they bear the transfer and are trapped, with no arbitrage and heavy identity loading. women_rights_advocates are the deliberate complication: the mechanical derivation (declared beneficiary + mobile exit) would push them near the full-beneficiary end, but their gains ride the CONTEST, not the arrangement's operation — they are funded by the violation stream, not by the extraction itself, and would forfeit their mandate under structural resolution. A directionality override on the organized power atom (d=0.3) places them mid-low: net gainers from persistence, but structurally opposed to the terms. No other stakeholder shares the organized atom, so the override isolates this seat. constitutional_courts derive near-symmetric: they neither collect the extraction nor bear it, while setting the reform agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — communal continuity in family governance — retains live adherents inside the affected communities, so founding_problem_status is contested rather than dead, and no zombie flag should fire. What HAS atrophied is the practice-level content: instant unilateral divorce, discretionary maintenance denial, and lineal property exclusion retain no function beyond the extraction they deliver, which is precisely why this reading targets practices rather than the structure. The mandatrophy danger this classification guards against runs both ways: reading the whole arrangement as pure coordination misses the snare core and licenses deference; reading it as pure extraction misses the genuine identity coordination that gives the arrangement its resilience and makes abrupt abolition backfire onto the women it names (the criminalization episode is the cautionary case). The deeper mandatrophy mechanism is the reform channel itself: it retires individual practices while preserving the mandate-shell that regenerates replacements, converting each obsolescence finding into renewed justification for the channel's own continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (gender_rights_reading) of the marriage_authority kernel among five — which structural elements would each sibling reading relocate, and where exactly does the disagreement sit?',
    'Generate the four sibling files (communal_autonomy_reading, secularist_reading, federalist_millet_reading, judicial_harmonization_reading) and compare epsilon, beneficiary/victim sets, and agenda_setter seats across the family; the disagreement is located in the locus of ultimate marriage authority and in whether intra-community equality claims override communal autonomy.',
    'Under communal_autonomy the equality lever disappears and this story''s victims become invisible private dissenters; under secularist the agenda_setter moves to the legislature and the victim set widens to all citizens under unequal codes; under federalist_millet the fragmentation itself becomes the protected good and this story''s intervention reads as majoritarian assault; under judicial_harmonization the identical court activity is read as neutral floor-setting rather than rights-driven contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: this story is one reading of the marriage_authority kernel; sibling readings instantiate different constraints from the same kernel.').

omega_variable(
    advocacy_sector_capture_ambiguity,
    'Does the advocacy sector''s structural dependence on a continuing stream of violations distort the reform channel toward practice-by-practice litigation (preserving the underlying arrangement), or is practice-targeting simply doctrinally necessary?',
    'Compare material outcomes for affected women in jurisdictions where structural reform occurred (comprehensive civil codes) against litigation-only jurisdictions; track resource flows into advocacy organizations relative to case volume; interview former litigants on whether organizational incentives tracked their outcomes.',
    'If captured, the arrangement''s persistence is co-maintained by its challengers and the contest overlay behaves as part of the extraction architecture; if not captured, advocates are pure challengers and the practice-targeting reflects doctrine, not incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_sector_capture_ambiguity, empirical, 'Whether the reform channel''s piecemeal shape reflects advocacy-sector incentive or doctrinal necessity.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression holding women in the arrangement structural (economic dependency, legal deference, ostracism, custody threat) or internalized (duty scripts, honor internalization, fused religious identity), and in what proportion?',
    'Post-exit suppression trajectory: follow women who leave the arrangement (civil remarriage, migration, estrangement) and measure whether the felt obligation and sanction-expectation persist after the structural barriers are removed.',
    'If substantially internalized, effective suppression exceeds the structural measure — the target carries the closure with her after exit, and purely legal remedies under-treat the constraint; if structural, removing the deference and dependency barriers collapses most of the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized components of the exit closure.').

omega_variable(
    reform_backlash_distribution,
    'Do judicial wins net-help or net-harm the women they name — does striking down or criminalizing a practice improve outcomes for the affected women, or does communal hardening and enforcement displacement leave them worse off?',
    'Longitudinal cohort studies of litigants and similarly-situated non-litigants after each landmark ruling: maintenance realization rates, informal support flows, ostracism incidence, and re-marriage prospects before versus after intervention.',
    'If wins net-harm the named women, measured extraction rises after each reform round (the snare reinforces through the reform channel itself) and intervention design must change; if wins net-help, the dips in the extractiveness series are real gains and the channel works despite friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_backlash_distribution, empirical, 'Net effect distribution of reform rounds on the women they target.').

omega_variable(
    coordination_cover_ambiguity,
    'Is the arrangement''s identity-coordination function genuine (real coordination content coexisting with extraction) or cover (the coordination story masking pure extraction)?',
    'Test whether the coordinating services — marriage formalities, succession certainty, intra-community dispute resolution — survive severance from the unequal terms: examine communities that have adopted egalitarian religious formulations and measure whether coordination services persist while the unequal terms drop.',
    'If genuine, part of the measured extraction is coordination cost and the reform target narrows to the unequal terms specifically; if cover, the full extraction is extractive overhead and structural replacement rather than practice-level repair is indicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cover_ambiguity, conceptual, 'Whether the identity-coordination framing is function or cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__gender_rights_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__gender_rights_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(marr_tr_t45, marriage_authority__gender_rights_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__gender_rights_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(marr_tr_t75, marriage_authority__gender_rights_reading, theater_ratio, 75, 0.46).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(marr_be_t15, marriage_authority__gender_rights_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(marr_be_t30, marriage_authority__gender_rights_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(marr_be_t45, marriage_authority__gender_rights_reading, base_extractiveness, 45, 0.79).
narrative_ontology:measurement(marr_be_t60, marriage_authority__gender_rights_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(marr_be_t75, marriage_authority__gender_rights_reading, base_extractiveness, 75, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(marr_su_t15, marriage_authority__gender_rights_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(marr_su_t30, marriage_authority__gender_rights_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(marr_su_t45, marriage_authority__gender_rights_reading, suppression_requirement, 45, 0.67).
narrative_ontology:measurement(marr_su_t60, marriage_authority__gender_rights_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(marr_su_t75, marriage_authority__gender_rights_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'marriage authority in personal-law systems' decomposes into five structurally distinct constraints — one per reading of the marriage_authority kernel — per the epsilon-invariance principle. Epsilon differs across the family because each reading locates the standing arrangement differently: this file authors epsilon for the intra-communal patriarchal arrangement as seen by the gender-rights reading (high); the communal_autonomy file would author epsilon for state interference with communal tradition (low from its seat); the secularist file for pluralism-as-anomaly; the federalist_millet file for majoritarian override risk; the judicial_harmonization file for the unlegislated constitutional floor. Family links run through affects_constraints in both directions; upstream doctrinal wins authored in this reading feed the harmonization reading's caseload and destabilize the federalist_millet assurance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
