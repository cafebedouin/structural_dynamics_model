% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Positivist Rule of Recognition for the U.S. Constitution
 *   domain: legal/constitutional/interpretive
 *
 * SUMMARY:
 *   Within American constitutional practice, the positivist reading operates
 *   as a procedural discipline: a norm counts as constitutional law because
 *   it was validly enacted and not validly amended away — not because it is
 *   just, wise, or historically understood. Judges are bound to
 *   source-validity rather than outcome-validity; arguments must terminate in
 *   enacted text; change routes through Article V. The arrangement genuinely
 *   coordinates — it gives courts, officials, and citizens a shared,
 *   decidable test for what law is — and it extracts asymmetrically: the cost
 *   of unenacted justice falls on litigants and movements whose claims lack
 *   textual anchor and whose access to the amendment channel is effectively
 *   closed. KEY AGENTS (by structural relationship): federal_judiciary —
 *   administering seat and primary beneficiary
 *   (institutional/identity_locked), collects decision economy and legitimacy
 *   insulation; unenacted_rights_claimants — primary target
 *   (powerless/trapped); entrenched_legislative_majorities — secondary
 *   beneficiary (powerful/mobile), hold the amendment monopoly;
 *   legal_profession — beneficiary (organized/identity_locked);
 *   social_movements_seeking_reform — secondary target
 *   (organized/constrained); future_generations_bound_by_entrenchment —
 *   diffuse target (powerless/trapped, civilizational horizon);
 *   moral_and_political_theorists — excluded voice (organized/constrained);
 *   legal_academy_observers — analytical observer. Claim and metrics are
 *   authored independently: the claimed type is tangled_rope because both the
 *   coordination function and the asymmetric extraction are structural facts
 *   of this arrangement, while the reading's internal self-description is
 *   closer to a pure coordination rule (the rule of recognition as
 *   constitutive of law); the divergence between the reading's self-claim and
 *   the structural data is part of what the engine measures. The epsilon
 *   referent is the standing arrangement — the source-validity discipline as
 *   it actually operates on constitutional adjudication — never the
 *   moral-validity alternative this reading rejects. Family note: sibling
 *   readings of the us_constitution_text kernel instantiate different
 *   constraints with different epsilon — the originalist reading's extraction
 *   attaches to actors whose interests postdate ratification; the living
 *   constitutionalist reading's extraction attaches to predictability- and
 *   entrenchment-dependent actors; this story's epsilon (0.58) attaches
 *   solely to the source-validity discipline and its costs on unenacted
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Positivist Rule of Recognition for the U.S. Constitution").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "legal/constitutional/interpretive").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '048fe2aa-a09a-41b7-9bee-8b9f359e2409').
narrative_ontology:cs_kernel_codification('048fe2aa-a09a-41b7-9bee-8b9f359e2409', fixed_text).
narrative_ontology:cs_authority_grounding('048fe2aa-a09a-41b7-9bee-8b9f359e2409', practice).
narrative_ontology:cs_interpretation_layer_present('048fe2aa-a09a-41b7-9bee-8b9f359e2409').
narrative_ontology:cs_reading_relation('048fe2aa-a09a-41b7-9bee-8b9f359e2409', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('048fe2aa-a09a-41b7-9bee-8b9f359e2409', us_constitution_text__living_constitutionalist_reading, influences).
narrative_ontology:cs_axiom('048fe2aa-a09a-41b7-9bee-8b9f359e2409', foundational, validity_derives_from_enactment_not_merit).
narrative_ontology:cs_axiom_status(validity_derives_from_enactment_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('048fe2aa-a09a-41b7-9bee-8b9f359e2409', validity_derives_from_enactment_not_merit, conventional).
narrative_ontology:cs_axiom('048fe2aa-a09a-41b7-9bee-8b9f359e2409', secondary, constitutional_change_requires_formal_amendment).
narrative_ontology:cs_axiom_status(constitutional_change_requires_formal_amendment, holdable).
narrative_ontology:cs_axiom_grounding('048fe2aa-a09a-41b7-9bee-8b9f359e2409', constitutional_change_requires_formal_amendment, conventional).
narrative_ontology:cs_reference_frame('048fe2aa-a09a-41b7-9bee-8b9f359e2409', enacted_text_supremacy).
narrative_ontology:cs_drift_state('048fe2aa-a09a-41b7-9bee-8b9f359e2409', contemporary_adjudicative_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('048fe2aa-a09a-41b7-9bee-8b9f359e2409', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, entrenched_legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, unenacted_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, social_movements_seeking_reform).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, future_generations_bound_by_entrenchment).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, hartian_rule_of_recognition).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, article_v_amendment_exclusivity).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, separation_of_law_validity_and_merit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which enacted texts bind and refuses claims anchored only in moral argument, tracing every constitutional holding to a source that was validly enacted and not validly amended away. The arrangement delivers a decidable test for hard cases and shields the bench from the objection that judges rule on personal morality. Leaving the discipline would mean repudiating the professional self-conception that separates judging from legislating, so the seat holds its position even where individual judges chafe at it.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, federal_judiciary, beneficiary).

% Bar associations, law schools, and practicing lawyers trade on the predictability the source-validity test produces: doctrinal argument is possible at all because the authoritative materials are identifiable. Members' professional identity is fused with the rule-of-law commitment, so treating moral adequacy as a source of validity feels like abandoning law itself rather than changing a method.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Hold the amendment monopoly: constitutional change happens only when two-thirds of both houses and three-quarters of the states agree. Statutes they enact within textual bounds cannot be displaced by judicial moral reasoning they dislike, and they bear almost none of the arrangement's costs because they control the only exit channel it recognizes.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, entrenched_legislative_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Litigants whose interests are urgent and morally compelling but lack a textual anchor: their arguments are received as policy preference rather than law and dismissed. There is no alternative forum for constitutional adjudication, and the amendment route requires supermajorities that have not formed in their favor within living memory, so waiting it out usually outlasts a lifetime.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, unenacted_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Organized campaigns for constitutional change must pour energy into a channel that has opened 27 times in more than two centuries and not at all since 1992. They retain political routes outside constitutional law — ordinary legislation, state action, cultural change — which keeps them from being fully closed off, but the formal constitutional route absorbs organizing capacity at a rate far beyond its yield.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, social_movements_seeking_reform, payer,
    organized, generational, constrained, national).

% People not yet born when the text and its amendments were enacted are bound by rules they had no hand in making, and the source-validity test gives them no ground for relief that does not run through the same supermajority process they inherit rather than control.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, future_generations_bound_by_entrenchment, payer,
    powerless, civilizational, trapped, national).

% Argue that law's authority depends on its moral merit and that validity from enactment alone entrenches injustice. Their arguments circulate in the academy and the public sphere but carry no legal force: the binding conversation happens in materials they do not control, and no procedural door admits them.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, moral_and_political_theorists, excluded,
    organized, civilizational, constrained, national).

% Jurisprudents, constitutional scholars, and comparativists map the gap between the official self-description (deciding by source) and actual adjudicative practice, track which claims the source test excludes, and supply the external critique that keeps the arrangement's costs visible to the other seats.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_academy_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the identification problem: gives courts, officials, and citizens a shared, decidable test for which norms count as constitutional law — trace the norm to valid enactment and the absence of valid amendment — so that adjudication, compliance, and planning do not require first settling moral truth or contested history.
% TRANSFER_FUNCTION: Moves interpretive authority from moral reasoners and unenacted claimants to the enacted text and to the institutions that control enactment (Congress, state legislatures, ratifying bodies); moves the cost of unenacted justice onto claimants, who must either win enactment or lose.
% ABSENT_VOICES: Those whose interests were unrepresented at enactment and amendment moments — disenfranchised communities at the founding, populations excluded from the franchise when key amendments passed, future generations — would object that source-validity entrenches past majorities' moral judgments as binding law. Moral and political theorists are present in public argument but absent from the binding conversation, which the rule of recognition confines to enacted sources.
% DISAPPEARANCE_RATIONALE: If the source-validity rule vanished overnight, courts would need a replacement rule of recognition — moral, historical, or pragmatic — and every constitutional decision's authority would be re-derived under it. Judicial review's legitimacy chain, the bar's doctrinal practice, and the amendment monopoly would all reorganize around whichever successor rule won. The legal system does not merely tolerate this rule; it is built on it.
% FOUNDING_PROBLEM: Legal indeterminacy and the legitimacy of judicial power: how can officials identify binding law, and judges decide cases non-arbitrarily, when texts are ambiguous and citizens disagree deeply about justice? The positivist reading was built to separate law's validity (a question of source) from its merit (a question of morality) so that 'what is the law' has a determinate answer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: analytic jurisprudence across rival schools — including Dworkin, the rule of recognition's fiercest critic, who took the positivist problem seriously enough to build his alternative against it — attests that the identification problem is real and persistent. Comparative constitutional practice corroborates it functionally: every modern legal system operates some source-based rule of recognition. Even the arrangement's victims corroborate it behaviorally: unenacted claimants and social movements argue within the formal channel, citing text and precedent, even while protesting its costs — which shows the identification problem the rule solves is not a beneficiary invention.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) reflects a genuine identification function carrying substantial asymmetric costs: the source-validity test concentrates its burdens on actors who lack an enacted anchor and cannot open the amendment channel — a channel that has produced 27 amendments in more than two centuries and none since 1992 — so the coordination price and the extraction are paid by different seats. Suppression (0.55) is structural rather than coercive-statutory: institutional hierarchy, stare decisis, professional discipline, and legitimacy sanctions hold judges to the source test, while rival readings remain openly practiced, which is why suppression is moderate rather than high. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and scope). Theater (0.32) captures the documented gap between the official self-description (deciding by source) and adjudicative practice, in which a substantial share of outcomes track moral and pragmatic judgment dressed in textual rhetoric; the enactment apparatus itself remains genuinely functional, so theater stays below one half. Accessibility collapse (0.40): the alternatives — living-constitutionalist adjudication, moral reading — remain live and do not collapse under this rule; it competes rather than forecloses. Resistance (0.60): sustained and institutionalized — legal realism, the Hart-Fuller debate, Dworkin's attack on the rule of recognition, critical legal studies, and living-constitutionalist judicial practice. Measurement grid: one shared grid at T = 0, 6, 12, 18, 24, 30, mapping approximately 1937 to 2025 (~2.9 years per unit); every point is a historical observation. The base_extractiveness series bottoms in the Warren-Court era (T≈6), when courts reasoned openly from moral principle and unenacted claims could win, then rises as formalist methodology returns and hardens; the theater_ratio series dips in the same era (honest departure) and recovers as formalist rhetoric returns; suppression_requirement tracks the enforcement apparatus (methodological appointments, canons of construction, legitimacy sanctions) maturing over the interval — the dip-and-recovery shape is a phase of the interpretive cycle, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the bench, the rule is constitutive: it is what makes judicial review legitimate and judging distinct from legislating, so its costs read as the price of law itself. From the unenacted claimant's seat, the same rule is a locked door: urgent, morally compelling claims arrive as non-law. From the legislative supermajority's seat, the amendment monopoly is a feature, not a cost. The engine derives these per-seat classifications from the structural data; the divergence is the measurement, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is both the administering seat and a collector (decision economy, legitimacy insulation) — declared beneficiary with identity-locked exit, so its directionality sits near the subsidized end. The legal profession shares the subsidy through predictability, also identity-locked. Entrenched legislative majorities hold the lowest effective burden: they control the amendment channel that is the arrangement's only recognized exit, so their directionality sits nearest the beneficiary pole. Unenacted rights claimants (powerless, trapped, biographical horizon) and future generations (powerless, trapped, civilizational horizon) sit near the full-target end; social movements sit high but below them — their organized coalition capacity and non-constitutional political routes give them partial exit, which is why their exit is constrained rather than trapped. Moral and political theorists carry no beneficiary/victim declaration: their position is carried by the excluded role and situation text, not by directionality — their grievance is exclusion from the binding conversation rather than a cost the arrangement levies on them. National scope keeps verification of valid enactment easy (the documents exist), so scope amplification is modest here; the arrangement's leverage runs through the amendment bottleneck, not through verification failure. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legal indeterminacy and the legitimacy of judicial power — is live: every legal system needs a rule of recognition, and the identification problem does not expire. The mandate has not atrophied; what has narrowed is its scope of honesty — a rule built to make law identifiable now also functions, under amendment deadlock, to foreclose substantive justice claims that cannot wait for supermajorities. The R5 mismatch consumer reads founding_problem_status = live against disappearance_verdict = world_rearranges: no dead-mandate/zombie flag fires, and the classification should not be pushed toward piton. The tangled_rope classification is what prevents mislabeling in both directions: calling this a snare would erase the genuine constitutive function no legal system can do without; calling it a rope would erase the documented extraction from claimants whose exit channel is functionally closed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint is one reading — the positivist_reading — of the us_constitution_text kernel. What structural changes would the sibling readings (originalist_reading, living_constitutionalist_reading) introduce, and does this story''s classification hold only under the source-validity reading? The disagreement is located in the rule of validity itself: this reading fixes validity in enactment procedure, the originalist sibling fixes meaning in ratification-era understanding, and the living sibling lets meaning track society.',
    'Author and classify the sibling stories as separate constraints; compare victim sets, epsilon, and per-seat classifications across the kernel family.',
    'The originalist reading would shift the victim population to actors whose interests postdate ratification and would ground authority in historical recovery rather than official practice; the living constitutionalist reading would shift costs onto predictability- and entrenchment-dependent actors. This story''s tangled_rope classification, its beneficiary/victim structure, and its epsilon of 0.58 hold only for the positivist reading''s constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Kernel-reading committer structure: this story is one reading of a three-reading kernel; sibling deltas are carried here rather than folded into this constraint.').

omega_variable(
    rule_of_recognition_acceptance,
    'Is the rule of recognition genuinely accepted by officials as constitutive practice, or is it a rationalization of institutional self-interest — courts prefer source-validity because it disciplines their docket and shields them from the counter-majoritarian objection?',
    'Behavioral study of official practice: do courts and officials treat source-validity as binding in cases where it cuts against their preferred outcomes, or do they depart whenever the moral stakes are high enough?',
    'If the acceptance is rationalization, the arrangement''s authority grounding shifts from practice toward extraction, the coordination function thins toward cover, and the classification drifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_of_recognition_acceptance, empirical, 'Whether the rule of recognition''s authority rests on genuine official acceptance or institutional self-interest.').

omega_variable(
    amendment_channel_viability,
    'Is Article V a live exit channel for the arrangement''s victims, or a functional dead letter? Twenty-seven amendments in over two centuries and none since 1992 suggest closure; state-application movements and periodic amendment proposals suggest residual life.',
    'Amendment success-rate analysis, tracking of Article V convention applications, and historical base rates for supermajority formation on rights-expanding versus rights-entrenching proposals.',
    'If the channel is a dead letter, the victims are trapped without exit and effective extraction is amplified toward the snare boundary; if the channel is live, the arrangement''s costs are temporary and extraction damps toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_viability, empirical, 'Whether the formal amendment route functions as a real exit for those the source-validity test excludes.').

omega_variable(
    exclusion_vs_extraction,
    'Are the costs borne by unenacted claimants inherent to any workable rule of recognition (the unavoidable price of a decidable validity test), or specific to this reading''s strict source-validity (extractive surplus a more permeable validity rule would not impose)?',
    'Comparative constitutional analysis: do systems with more permeable validity structures (unwritten principles, notwithstanding clauses, purposive validity doctrines) protect unenacted claims without collapsing legitimacy or determinacy?',
    'If the cost is inherent to any validity rule, extraction damps toward the coordination floor and the type moves toward rope; if the surplus is specific to strict source-validity, the tangled_rope classification holds with the full measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_vs_extraction, conceptual, 'Whether the burden on unenacted claims is a coordination constant or a reading-specific surplus.').

omega_variable(
    judicial_identity_lock_durability,
    'Is the judiciary''s adherence to source-validity identity-fused — professional self-conception (courts say what the law is, not what it should be), relational identity within the legal establishment, and institutional identity of judicial review itself — or a contingent strategic equilibrium that would break under sustained legitimacy crisis?',
    'Observe whether polarized confirmation politics and repeated legitimacy crises break the frame: do judges begin openly embracing outcome reasoning once the identity cost of admitting it falls below the strategic cost of maintaining the fiction?',
    'If the identity frame breaks, the arrangement loses its enforcement seat, practice drift accelerates, and suppression falls as the discipline becomes unenforceable; if the frame is durable, part of the measured suppression is internalized professional identity rather than external structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_identity_lock_durability, conceptual, 'Whether the enforcing seat''s commitment to source-validity is identity-locked or strategic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_positivist_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(us_const_positivist_tr_t0, observed).
narrative_ontology:measurement(us_const_positivist_tr_t6, us_constitution_text__positivist_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(us_const_positivist_tr_t6, observed).
narrative_ontology:measurement(us_const_positivist_tr_t12, us_constitution_text__positivist_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(us_const_positivist_tr_t12, observed).
narrative_ontology:measurement(us_const_positivist_tr_t18, us_constitution_text__positivist_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(us_const_positivist_tr_t18, observed).
narrative_ontology:measurement(us_const_positivist_tr_t24, us_constitution_text__positivist_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(us_const_positivist_tr_t24, observed).
narrative_ontology:measurement(us_const_positivist_tr_t30, us_constitution_text__positivist_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(us_const_positivist_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(us_const_positivist_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(us_const_positivist_be_t0, observed).
narrative_ontology:measurement(us_const_positivist_be_t6, us_constitution_text__positivist_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement_basis(us_const_positivist_be_t6, observed).
narrative_ontology:measurement(us_const_positivist_be_t12, us_constitution_text__positivist_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(us_const_positivist_be_t12, observed).
narrative_ontology:measurement(us_const_positivist_be_t18, us_constitution_text__positivist_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(us_const_positivist_be_t18, observed).
narrative_ontology:measurement(us_const_positivist_be_t24, us_constitution_text__positivist_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(us_const_positivist_be_t24, observed).
narrative_ontology:measurement(us_const_positivist_be_t30, us_constitution_text__positivist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(us_const_positivist_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_const_positivist_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(us_const_positivist_su_t0, observed).
narrative_ontology:measurement(us_const_positivist_su_t6, us_constitution_text__positivist_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(us_const_positivist_su_t6, observed).
narrative_ontology:measurement(us_const_positivist_su_t12, us_constitution_text__positivist_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(us_const_positivist_su_t12, observed).
narrative_ontology:measurement(us_const_positivist_su_t18, us_constitution_text__positivist_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(us_const_positivist_su_t18, observed).
narrative_ontology:measurement(us_const_positivist_su_t24, us_constitution_text__positivist_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(us_const_positivist_su_t24, observed).
narrative_ontology:measurement(us_const_positivist_su_t30, us_constitution_text__positivist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(us_const_positivist_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Constitution' covers three structurally distinct constraints corresponding to three readings of one kernel (us_constitution_text). This file is the positivist reading. The readings are decomposed rather than merged because each carries a single, stable epsilon over a different population: this reading's extraction attaches to claims lacking formal enactment under a functionally closed amendment channel; the originalist reading's extraction attaches to actors whose interests postdate ratification; the living constitutionalist reading's extraction attaches to predictability- and entrenchment-dependent actors. All three stories link via network.affects_constraints; the upstream/downstream structure between them runs through shared legitimacy conditions rather than logical implication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
