% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   In 1890, facing federal property seizure and institutional dissolution,
 *   the leadership of a major American religious institution issued the
 *   Manifesto, officially discontinuing the practice of polygamy and
 *   declaring it contrary to institutional doctrine. The Manifesto is
 *   presented here from the reading in which it is acknowledged as a response
 *   to federal coercion, with theological legitimacy derived from
 *   institutional survival necessity rather than new revelation or pure
 *   doctrinal development. This reading opens the authority structure to an
 *   explicit admission: federal law can dictate doctrinal change through
 *   coercive pressure, and institutional survival is a valid theological
 *   ground for such change. The constraint this reading instantiates is a
 *   tangled rope: it solves a genuine coordination problem
 *   (institutional-federal alignment) while extracting asymmetrically from
 *   practicing polygamists and women in plural marriages, sustained by
 *   institutional enforcement machinery (excommunication, ostracism) and
 *   federal legal machinery (prosecution, property seizure).
 *
 * KEY AGENTS:
 *   - institutional_leadership: sets and enforces the Manifesto; preserves institutional existence at the cost of doctrinal reversal
 *   - practicing_polygamists: bears the direct cost; identity-locked exit; forced to choose between marriage structure and institutional belonging
 *   - women_in_plural_marriages: structurally powerless; marriages dissolved; minimal exit options; trapped between social death and institutional separation
 *   - federal_authority: exerts coercive pressure; drives the institutional policy change; does not directly author the Manifesto
 *   - rank_and_file_believers: benefit from institutional preservation; pay indirect cognitive cost of absorbing doctrinal shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '311efb40-7dfc-45a5-8a04-96223d87f8b0').
narrative_ontology:cs_kernel_codification('311efb40-7dfc-45a5-8a04-96223d87f8b0', fixed_text).
narrative_ontology:cs_authority_grounding('311efb40-7dfc-45a5-8a04-96223d87f8b0', extraction).
narrative_ontology:cs_interpretation_layer_present('311efb40-7dfc-45a5-8a04-96223d87f8b0').
narrative_ontology:cs_reading_relation('311efb40-7dfc-45a5-8a04-96223d87f8b0', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('311efb40-7dfc-45a5-8a04-96223d87f8b0', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('311efb40-7dfc-45a5-8a04-96223d87f8b0', foundational, coercion_as_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(coercion_as_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('311efb40-7dfc-45a5-8a04-96223d87f8b0', coercion_as_valid_doctrinal_input, empirically_contingent).
narrative_ontology:cs_axiom('311efb40-7dfc-45a5-8a04-96223d87f8b0', foundational, institutional_survival_necessity_doctrine).
narrative_ontology:cs_axiom_status(institutional_survival_necessity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('311efb40-7dfc-45a5-8a04-96223d87f8b0', institutional_survival_necessity_doctrine, instrumental).
narrative_ontology:cs_reference_frame('311efb40-7dfc-45a5-8a04-96223d87f8b0', revelatory_doctrinal_authority).
narrative_ontology:cs_drift_state('311efb40-7dfc-45a5-8a04-96223d87f8b0', federal_coercion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('311efb40-7dfc-45a5-8a04-96223d87f8b0', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, practicing_polygamists).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, rank_and_file_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the Manifesto as doctrinal policy. Frames it as divinely authorized or at minimum doctrinally consistent. Maintains institutional continuity and legal standing by eliminating polygamy practice. Bears existential threat from federal coercion and property seizure; the Manifesto resolves that threat by demonstrating institutional reformation to the federal authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_leadership, agenda_setter,
    institutional, generational, trapped, national).

% Held plural marriages as a core religious practice and identity commitment. After the Manifesto, face institutional excommunication, social ostracism, and legal consequences if they continue. They must choose between their marriage structure and institutional belonging. Their exit from the religious community is costly because it dissolves their primary social identity and often requires geographic relocation away from religious communities.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, practicing_polygamists, payer,
    moderate, biographical, identity_locked, national).

% Experience the Manifesto as dissolving their marriages and social status simultaneously. They have minimal voice in either the original marriage arrangement or its removal. Exit options are severely constrained: leaving means social death, economic precarity, and separation from children. The institutional structure that organized their lives is dismantled without their consent, and they have no alternative institution available.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriages, payer,
    powerless, generational, trapped, national).

% Applies legal and economic pressure (property seizure, legal prosecution, disenfranchisement) to force institutional reformation. Does not directly author the Manifesto but creates the coercive environment that makes it the least-bad option for institutional survival. After the Manifesto, claims vindication of its authority and demonstrates that federal law supersedes claimed divine commands.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, federal_authority, observer).

% Benefit from institutional preservation (the organization survives and continues to provide community, meaning, and social structure). They also pay an indirect cost: their theological worldview must absorb the Manifesto as either divinely revealed or theologically justified, creating cognitive dissonance if they previously believed the prior teaching. Those who privately reject the Manifesto face internal conflict.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_believers, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, rank_and_file_believers, payer).

% The theological tradition vindicated by the Manifesto is institutional-survival-necessity doctrine: the claim that institutional preservation is a sufficient ground for doctrinal change, even when that change contradicts a prior command presented as divine. This proposition is embedded in the reading and extracted from institutional action.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theological_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(divine_marriage_command__coercion_visibility_reading, theological_tradition).

% Vindicated by the Manifesto as superior in binding force to internal religious authority when conflict arises. The reading acknowledges that federal law can dictate changes to religious doctrine via coercive pressure, and that institutional survival necessity is a valid justification for the resulting doctrinal change.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_legal_framework, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(divine_marriage_command__coercion_visibility_reading, federal_legal_framework).

% The continuationist reading (Manifesto is suspension, not rescission) and the substitutionist reading (new revelation that supersedes) would generate different constraint structures. This reading excludes their framing by treating the Manifesto as admitting coercion as a legitimate input to doctrinal authority.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, sibling_readings, excluded,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(divine_marriage_command__coercion_visibility_reading, sibling_readings).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of institutional alignment with federal law: the institution must either submit to federal authority (and demonstrate that submission) or face property seizure, legal prosecution, and dissolution. The Manifesto coordinates internal compliance with external legal requirement by making institutional authority subordinate to legal authority when the two conflict.
% TRANSFER_FUNCTION: Moves authority from claimed divine command to institutional survival necessity and federal legal coercion. Transfers the cost of institutional preservation from the federal authority (who would have to maintain enforcement machinery) to practicing polygamists (who must abandon their practice) and women in plural marriages (who lose their family structure). Transfers theological legitimacy from revelation-grounded commands to survival-grounded institutional decisions.
% ABSENT_VOICES: Practicing polygamists are partially present (they have some institutional voice before the Manifesto) but excluded from the Manifesto's authorship. Women in plural marriages are structurally absent from both the original marriage arrangement's decision-making and the Manifesto's promulgation. Federal dissenters who believe religious freedom should supersede legal conformity are excluded from institutional decision-making (though they may have external political voice).
% DISAPPEARANCE_RATIONALE: If this constraint disappeared (the Manifesto were rescinded and the prior divine command reinstated), the institution would immediately face the same federal coercion it capitulated to, leading either to renewed legal conflict or to a new capitulation. The political-theological world rearranges because the Manifesto's removal would restart the federal enforcement mechanism, and institutional leadership would face the same choice: capitulate again or dissolve.
% FOUNDING_PROBLEM: Federal legal authority asserts the right to prosecute and dissolve religious institutions that practice polygamy. The institution's doctrinal command to practice polygamy is subordinate to federal legal command to cease. The founding problem is the collision between federal law and institutional authority.
% FOUNDING_PROBLEM_CORROBORATION: Federal law enforcement, contemporary historical documentation, and the institution's own correspondence acknowledge the legal threat. Historians outside the institution (Richard Bushman, Fawn Brodie, and others) corroborate that federal coercion was a primary (though not the only) driver of the Manifesto. The institution itself admits in later commentary that 'the Lord has conditioned the blessings of the Priesthood upon the condition of strict obedience' to federal law on this point.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 by interval end) because the constraint extracts a core life-structure commitment (plural marriage) from those practicing it, sustained by coercive machinery both institutional and federal. Suppression is also high (0.71) because the mechanism depends on institutional enforcement (threat of excommunication and social ostracism) and legal enforcement (prosecution and property seizure) — the options to resist or exit are severely constrained. Theater is moderate-high (0.62) because the institutional rhetoric of doctrinal authority and theological justification performs legitimacy work that masks the coercive substrate; the Manifesto is authored as divine revelation or theological development, not as capitulation to federal pressure, yet the underlying causal story is visible to historians and critics. The time series shows extractiveness and theater rising together after the Manifesto's initial promulgation (t=3 onward), indicating that as the institutional policy is implemented, the rhetorical work of justifying it increases — performance intensifies to maintain legitimacy as the cost to payers becomes undeniable. Suppression rises then plateaus, suggesting that after initial enforcement, suppression stabilizes at a maintained level (institutional ostracism becomes routine, federal legal machinery is normalized).
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the constraint is genuine coordination (survival is coordination with federal authority; the institution preserves itself and its community). From the practicing polygamist's seat, the constraint is extraction (their fundamental life commitment is dissolved against their will; they face a forced choice). From the federal authority's seat, the constraint is a successful imposition of legal authority over religious authority. From women in plural marriages' seat, the constraint is a combination of extraction and abandonment (their marriages are dissolved, and they are offered no alternative institutional support). The engine computes per-seat classification from power + time_horizon + exit_options + the beneficiary/victim structure; the gap between seats reflects their different structural positions relative to the same constraint. This reading (coercion-visibility) makes that gap explicit: it treats the federal coercion as a legitimate input to institutional authority, which means that from the institutional leadership's perspective, federal pressure IS a valid justification for policy change, whereas from the payer's perspective, federal pressure IS the source of injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is a beneficiary (d near 0.0: the institution survives, leadership maintains authority, the community is preserved). Practicing polygamists and women in plural marriages are victims and payers (d near 1.0: they bear the cost, have trapped or identity-locked exit, face legal and social consequences). Rank-and-file believers sit near symmetric (d near 0.5: they benefit from institutional preservation but pay a cognitive cost). Federal authority is partially a beneficiary through the lens of this reading (d near 0.2: federal law is vindicated, federal authority is established as superior to religious authority), but the reading does not center federal authority as a formal stakeholder — the federal authority is the external pressure that shapes the constraint, not a seat within it. The directionality chain: beneficiaries (institutional leadership) derive low d; payers (polygamists, women) derive high d; the constraint's effective extraction is scaled by their high d values, meaning their seats experience the constraint as more extractive than the leadership's seat does.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids one common misclassification: it does not treat the Manifesto as a pure snare (pure coercion with false legitimacy cover) because it acknowledges a genuine institutional coordination problem — the institution truly would have faced legal dissolution without capitulation. However, it also avoids the pure-rope misclassification (pure coordination, no extraction) because it is clear that the cost of the institutional preservation is borne disproportionately by a subset of the community (polygamists and especially women), not distributed across the institution. Tangled rope is the correct classification because the constraint simultaneously solves a real coordination problem (institutional-federal alignment) and extracts asymmetrically from identified victims, sustained by active enforcement. The mandatrophy analysis here is that the founding problem (federal legal threat) is live, but the problem-solving mechanism (forced doctrinal change) creates a secondary problem (injustice to polygamists and women) that is not solved by the same constraint — it is generated by it. This is structurally typical of tangled rope: the coordination function is real, but it is paid for by the payer class, not by everyone equally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_ambiguity,
    'Did the institutional leadership experience the Manifesto as revealed divine command, or as institutional policy justified by survival necessity? These are phenomenologically distinct: one is a communication from the divine; the other is a pragmatic accommodation. Which did the leaders believe?',
    'Primary-source analysis of institutional leaders'' private correspondence, diaries, and later testimony. Comparison with how they describe undisputed revelations versus disputed policy changes.',
    'If revelation: the constraint is a snare (coerced institutional change with false divine cover). If survival-necessity: the constraint is a tangled rope (genuine institutional coordination need plus asymmetric extraction from polygamists). If mixed: the constraint is a tangled rope with unresolved epistemological authority (the revelation/coercion boundary is itself contested).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_coercion_ambiguity, conceptual, 'Whether the Manifesto was experienced as revealed or as pragmatic policy by institutional leadership.').

omega_variable(
    coercion_legitimacy_as_doctrinal_input,
    'Does this reading''s acknowledgment that coercion can be a valid input to doctrinal authority create a legitimacy crisis for the institutional theological framework? If coercion justifies doctrinal change, what prevents every coerced doctrinal change from being theologically valid?',
    'Systematic analysis of institutional theology after the Manifesto: does it develop principles for distinguishing legitimate coercion-driven change from illegitimate institutional capitulation? Or does it leave the boundary undefined?',
    'If principles developed: the reading''s theological framework contains institutional resources to prevent doctrinal whipsaw. If undefined: the reading leaves the institution vulnerable to future challenges claiming coercion justifies contradictory doctrinal changes. High impact for the institution''s long-term authority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_legitimacy_as_doctrinal_input, preference, 'Whether coercion can be a valid ground for doctrinal change without destabilizing the authority structure.').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading (coercion-visibility) logically foreclose the continuationist reading (Manifesto as suspension, not rescission)? Or do they coexist as different institutional postures toward the same doctrinal fact?',
    'Close reading of institutional theological statements distinguishing between ''suspension of practice'' (continuationist framing) and ''change of doctrine'' (substitutionist) and ''acknowledged coercion'' (this reading). Can all three be held simultaneously?',
    'If foreclosed: this reading claims exclusive epistemic authority over the Manifesto''s interpretation. If coexistent: different institutional factions hold different readings, and the constraint''s classification may differ per seat (as the schema intends). If foreclosure is only partial (some aspects are ruled out, others coexist), the network relationship between readings is asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading forecloses the continuationist and substitutionist siblings or allows them to coexist.').

omega_variable(
    women_suppression_mechanism,
    'Is the measured suppression of women in plural marriages structural (legal barriers, economic dependency, geographic isolation) or internalized (belief in doctrinal legitimacy, identity fusion with the role, isolation from alternative narratives)? The suppression metric is a single scalar and does not distinguish.',
    'Post-exit trajectory analysis: if women who leave the institution and the marriage later assert that the suppression was internalized (they believed the marriage was divinely sanctioned) and diminishes after exit, the suppression was partially internalized. If suppression persists after exit (economic precarity, family separation, social ostracism from former community), the suppression was structural.',
    'If internalized: the constraint''s effective suppression is higher than the authored 0.71 suggests — the target carries the suppression with them after exit, indicating deeper identity lock. If structural: the suppression is accurately captured as institutional and legal barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_suppression_mechanism, empirical, 'Whether suppression of women is structural or internalized through identity fusion.').

omega_variable(
    institutional_survival_necessity_doctrine_scope,
    'If institutional survival necessity is a valid theological ground for doctrinal change, does it apply only to the polygamy question, or more broadly? Could this doctrine justify future doctrinal changes on other issues where federal pressure or survival threat exists?',
    'Trace subsequent institutional responses to external pressure on other doctrinal questions (e.g., racial priesthood restrictions, gender roles in leadership). Does the institution invoke survival-necessity doctrine, or does it claim new revelation?',
    'Narrow scope: survival-necessity is a one-time exception. Broad scope: the doctrine becomes a general framework for negotiating institutional authority with external power, potentially multiplying tangled-rope constraints. Affects the network classification of future institutional policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_survival_necessity_doctrine_scope, conceptual, 'Whether survival-necessity grounds can justify multiple doctrinal changes or only the polygamy reversal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(divi_tr_t3, divine_marriage_command__coercion_visibility_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(divi_tr_t6, divine_marriage_command__coercion_visibility_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(divi_tr_t12, divine_marriage_command__coercion_visibility_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement(divi_tr_t18, divine_marriage_command__coercion_visibility_reading, theater_ratio, 18, 0.61).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__coercion_visibility_reading, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t3, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(divi_be_t6, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 6, 0.66).
narrative_ontology:measurement(divi_be_t12, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(divi_be_t18, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(divi_su_t3, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(divi_su_t6, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(divi_su_t12, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(divi_su_t18, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, federal_polygamy_prosecution_regime).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, institutional_apostasy_post_manifesto).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the divine_marriage_command kernel. The kernel is the standing religious commitment to polygamy (prior to 1890) and its institutional status after the Manifesto (post-1890). Each reading instantiates a different constraint because each reading assigns different ε values, beneficiary/victim structures, and authority grounding to the same kernel. The coercion_visibility_reading treats the constraint as a tangled rope with high extractiveness (institutional survival coordination plus asymmetric cost to polygamists). The continuationist_reading would treat the constraint as more of a rope or piton (suspension rather than rescission, emphasis on performance). The substitutionist_reading would treat the constraint as a scaffold or rope (new revelation that transitions to monogamy as divinely mandated). These readings are networked: each reading's viability depends partly on how convincingly it accounts for the empirical record that the other readings emphasize. This reading influences the others by admitting the coercive pressure that motivates them: the continuationist reading can maintain that doctrine is unchanged only if it also acknowledges the coercive pressure driving the suspension (this reading's core claim); the substitutionist reading must account for why the institution framed the change as new revelation rather than admitting federal pressure (this reading's transparency becomes a contrasting measure of substitutionist institutional rhetoric).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
