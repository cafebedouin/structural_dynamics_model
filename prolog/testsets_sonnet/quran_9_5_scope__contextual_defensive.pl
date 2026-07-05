% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Contextual-Defensive Reading of Q9:5 (Treaty-Breach Scope)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This story instantiates the contextual-defensive reading of Quran 9:5
 *   within the quran_9_5_scope kernel. Under this reading, the verse's harsh
 *   language ('slay the polytheists wherever you find them...') is bounded by
 *   the immediately surrounding verses (9:1-4, 9:7-8) which specify the
 *   addressees as polytheist tribes that had broken treaty obligations with
 *   the Medinan polity — it is a conditional wartime directive against
 *   identified treaty-breakers, not a categorical statement about polytheists
 *   as such. This reading does not claim the abrogating-universal reading is
 *   impossible to hold (many jurists across history have held it); it claims
 *   that read in its immediate textual and historical context, the verse's
 *   scope is treaty-breach-bounded and does not override the Quran's numerous
 *   peace-permitting and treaty-honoring verses (e.g., 8:61, 60:8-9). The
 *   sibling readings — abrogating_universal (verse establishes standing
 *   offensive obligation via nasikh) and progressive_synthesis (verse is
 *   time-bound political directive superseded by ethical trajectory) — are
 *   NOT part of this constraint; they are separate constraint stories linked
 *   via network.affects_constraints. ε here is low because, on this reading's
 *   own terms, the constraint licenses force only against a narrow,
 *   historically closed victim class and imposes no ongoing extraction on any
 *   present-day population.
 *
 * KEY AGENTS:
 *   - integrationist_muslim_majority_states: beneficiary (institutional/constrained) — draws political and religious legitimacy from the bounded-scope reading
 *   - muslim_minority_communities_in_plural_societies: beneficiary (moderate/constrained) — relies on the reading to ground civic coexistence
 *   - historical_treaty_breaking_meccan_polytheist_tribes: payer (powerless/trapped) — the historically closed addressee class
 *   - interfaith_treaty_partners: beneficiary (organized/mobile) — benefit from treaty-keeping as the operative variable
 *   - abrogationist_clerical_networks: excluded (organized/mobile) — hold the rejected sibling reading
 *   - classical_and_contemporary_exegetes: observer (analytical) — assess the historical-philological evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.18).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.22).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.18).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Contextual-Defensive Reading of Q9:5 (Treaty-Breach Scope)").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'd23a2e71-eb70-4c81-bfc1-d7d46e6c96ee').
narrative_ontology:cs_kernel_codification('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', fixed_text).
narrative_ontology:cs_authority_grounding('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', lineage).
narrative_ontology:cs_interpretation_layer_present('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee').
narrative_ontology:cs_reading_relation('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', quran_9_5_scope__progressive_synthesis, influences).
narrative_ontology:cs_axiom('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', foundational, verse_scope_bounded_by_treaty_breach_context).
narrative_ontology:cs_axiom_status(verse_scope_bounded_by_treaty_breach_context, holdable).
narrative_ontology:cs_axiom_grounding('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', verse_scope_bounded_by_treaty_breach_context, conventional).
narrative_ontology:cs_axiom('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', foundational, peaceful_verses_remain_operative_and_unabrogated).
narrative_ontology:cs_axiom_status(peaceful_verses_remain_operative_and_unabrogated, holdable).
narrative_ontology:cs_axiom_grounding('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', peaceful_verses_remain_operative_and_unabrogated, conventional).
narrative_ontology:cs_axiom('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', secondary, treaty_keeping_not_confessional_identity_determines_conflict_licensure).
narrative_ontology:cs_axiom_status(treaty_keeping_not_confessional_identity_determines_conflict_licensure, holdable).
narrative_ontology:cs_axiom_grounding('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', treaty_keeping_not_confessional_identity_determines_conflict_licensure, instrumental).
narrative_ontology:cs_reference_frame('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', classical_contextualist_exegesis).
narrative_ontology:cs_drift_state('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', contemporary_pluralist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d23a2e71-eb70-4c81-bfc1-d7d46e6c96ee', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_plural_societies).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, interfaith_treaty_partners).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, historical_treaty_breaking_meccan_polytheist_tribes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments and religious establishments in majority-Muslim states seeking legitimacy for pluralistic foreign policy, minority protections, and treaty-based diplomacy cite this reading to ground non-aggression as the juristic default rather than an exception requiring special justification. Their exit from the reading is constrained by the need for continued scholarly and popular legitimacy.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, constrained, national).

% Muslims living as minorities in non-Muslim-majority states rely on this reading to ground their own religious practice as compatible with civic loyalty and peaceful coexistence, defusing suspicion that scripture obligates them to permanent hostility toward non-Muslim governance. They cannot simply adopt a different reading without social and legal costs where the abrogating-universal reading is used against them by outsiders or coreligionist rivals.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, muslim_minority_communities_in_plural_societies, beneficiary,
    moderate, biographical, constrained, global).

% The specific 7th-century Arabian tribes named in the classical exegesis as having broken prior treaty obligations with the nascent Medinan polity are the historically bounded addressees of the verse's ultimatum under this reading. They are a closed historical set with no present-day exit, included here only because the reading's own scope declaration names them as the constraint's original object rather than a perpetually renewable category.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, historical_treaty_breaking_meccan_polytheist_tribes, payer,
    powerless, immediate, trapped, regional).

% Non-Muslim polities, communities, and individuals bound by treaties or coexistence arrangements with Muslim-majority actors benefit from a reading that makes treaty-keeping (not confessional identity) the operative variable determining peace or conflict. They retain exit options in the ordinary diplomatic sense — this reading does not trap them in the constraint's operation.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, interfaith_treaty_partners, beneficiary,
    organized, generational, mobile, global).

% Scholars and movements committed to the abrogating-universal reading are structurally excluded from this constraint's own framing — their interpretive tradition is the rejected alternative, not a party this reading coordinates with. They would object that this reading domesticates a command they hold to be a standing legal obligation, but that objection belongs to the sibling reading's story, not this one.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, abrogationist_clerical_networks, excluded,
    organized, generational, mobile, global).

% Jurists, historians of tafsir, and comparative religion scholars examine the philological, historical, and asbab al-nuzul (occasions of revelation) evidence bearing on whether 9:5's scope is properly bounded to treaty-breach context. They do not hold a stake in the outcome beyond scholarly accuracy, though their findings are cited by all three readings as support.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, classical_and_contemporary_exegetes, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, diffuse).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes that armed conflict is licensed only in response to prior treaty violation and active aggression by a specific historical party, coordinating expectations that treaty-keeping (rather than religious identity) determines the peace/conflict boundary for the Medinan polity and, by extension, for later readers applying the verse's logic.
% TRANSFER_FUNCTION: Moves interpretive legitimacy toward pluralist and integrationist political arrangements: it transfers argumentative weight away from readings that would licANCE ongoing or expansive conflict and toward readings that ground coexistence, at the interpretive cost of foreclosing (for those who adopt it) the abrogating-universal reading's broader claim of standing offensive license.
% ABSENT_VOICES: The historical treaty-breaking tribes have no voice in any modern reading of the verse describing them; adherents of the abrogating-universal reading are present in the broader kernel contest but are excluded from this constraint's own coordination story, since their tradition treats the very premise here (contextual limitation) as mistaken.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live interpretive option, integrationist religious establishments and minority Muslim communities would lose a primary textual ground for arguing that peaceful coexistence is the default juristic posture rather than a suspended exception; polemical arguments citing 9:5 as evidence of standing offensive obligation would face substantially less organized counter-argument, and interfaith treaty relationships premised on textual reciprocity would need alternative grounding.
% FOUNDING_PROBLEM: The interpretive problem this reading was built to solve is double: first, to account historically for why the Quran's harshest-sounding verse addresses a specific broken-treaty situation rather than polytheists as such; second, in the modern period, to answer the political-theological question of whether Islamic scripture obligates permanent hostility toward non-Muslim political orders.
% FOUNDING_PROBLEM_CORROBORATION: Classical exegetes including al-Tabari and later historians of tafsir attest the historical occasion-of-revelation material supporting a treaty-breach context, independent of any modern integrationist agenda. Comparative religion scholars and some historians outside any Muslim confessional community corroborate the treaty-context reading as textually and historically defensible, while noting that abrogationist scholars within the tradition dispute the scope-limiting conclusion drawn from that same context — the underlying occasion-of-revelation evidence is less contested than the legal conclusion drawn from it.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, under this reading's own scope declaration, the constraint's coercive force applies only to a bounded, historically closed set of treaty-violators and imposes no standing extraction on any living population — the beneficiary set (integrationist states, minority communities, treaty partners) receives interpretive legitimacy for peace, not rents. Suppression is likewise low-moderate (0.22): this reading does not require coercive enforcement to persist as a live interpretive option, though it faces real intellectual and political competition from the abrogating-universal reading, which is reflected in the resistance score (0.55) — the reading is contested, not settled, and its adoption by clerical authorities and states is actively resisted by rival exegetical networks. Accessibility collapse is moderate (0.35) because alternative readings remain fully available and widely held; this reading has not achieved anything like exclusive interpretive dominance. Theater ratio is low and rises only slightly over the interval, reflecting the reading's persistence as substantive scholarly argument rather than performative doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the beneficiary states and communities, this reading is straightforwardly a coordination mechanism — it grounds peaceful policy and coexistence in scripture read faithfully in context. From the seat of the abrogationist clerical networks (excluded from this constraint's own coordination story), the same verse-scope claim looks like an illegitimate narrowing that suppresses a standing legal obligation they hold to be textually clear. The engine computes each seat's classification from the structural data; this story does not attempt to adjudicate between the readings — that adjudication is exactly what the kernel-contest structure is for.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (integrationist states, minority communities, treaty partners) sit near the low-d end: the reading subsidizes their political and social legitimacy without extracting from them. The sole victim group — historical treaty-breaking tribes — sits at the high-d end but is a closed historical class with a trapped exit option definitionally (they have no continuing existence to exit from); this is included to satisfy the schema's structural requirement that the reading name who the constraint's coercive force falls on, not to suggest ongoing extraction. No present-day population is authored as a victim under this reading, which is the central structural delta distinguishing it from the abrogating_universal sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (does scripture obligate permanent hostility toward non-Muslim political orders, or was 9:5 addressed to a specific breach) remains contested rather than resolved, and this story does not claim resolution. What the classification apparatus prevents is treating either the abrogating-universal reading's textual literalism OR this reading's contextualism as automatically the 'true' scope of the verse — each reading is authored as its own constraint with its own ε, and the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) here signals live contest rather than settled capture: no single reading has achieved the kind of dominance that would make its disappearance inconsequential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_boundedness_vs_general_principle,
    'Is the treaty-breach context of 9:1-8 genuinely scope-limiting for verse 9:5, or is the historical context merely the occasion of revelation for a legal principle intended to apply more generally, as classical abrogationist jurists held?',
    'Philological and historical analysis of asbab al-nuzul literature, comparison with other Quranic verses using similar constructions, and examination of how the earliest generations of jurists (as opposed to later systematizers of nasikh doctrine) applied the verse in practice.',
    'If the context is merely occasional rather than scope-limiting, this reading''s core premise weakens substantially and the abrogating_universal reading''s claim strengthens; if genuinely scope-limiting, this reading''s textual grounding is vindicated independent of any modern political motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_boundedness_vs_general_principle, conceptual, 'Whether historical context limits or merely occasions the verse''s legal scope.').

omega_variable(
    modern_motivation_vs_textual_evidence,
    'To what extent is the contemporary popularity of this reading among integrationist Muslim-majority states driven by independent textual-historical evidence versus political convenience in a post-colonial, pluralist international order?',
    'Tracing the reading''s historical lineage prior to the 20th century to assess whether it predates the political pressures of modern statecraft, and comparing its adoption patterns across states with differing incentives.',
    'If the reading substantially predates modern political incentives (which classical sources suggest, given similar contextualist arguments in early tafsir), the beneficiary-alignment is coincidental rather than manufactured; if the reading is a modern retrofit, its status as genuine scholarship versus motivated reasoning is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_motivation_vs_textual_evidence, empirical, 'Whether this reading''s alignment with integrationist state interests reflects independent scholarship or motivated retrofitting.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel itself best framed as a dispute over the SCOPE of a single verse (as this decomposition assumes), or is the deeper contest actually over the doctrine of nasikh (abrogation) itself as a hermeneutic tool — such that resolving the abrogation question would settle all three readings simultaneously rather than leaving them as coexisting alternatives?',
    'Examine whether jurists who reject nasikh as applicable to 9:5 also reject nasikh doctrine generally, or accept nasikh elsewhere while rejecting it here specifically — if the latter, the scope-of-verse framing is correct; if the former, the kernel is better modeled at the level of abrogation-doctrine-itself.',
    'If the deeper kernel is abrogation doctrine, this reading and abrogating_universal would forecose one another at that deeper level rather than merely coexisting at the verse-scope level, changing the reading_relations declared here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framing: the contest may be over abrogation doctrine generally, not merely this verse''s scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 632, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quran_9_5_scope__contextual_defensive, theater_ratio, 632, 0.05).
narrative_ontology:measurement(qura_tr_t1258, quran_9_5_scope__contextual_defensive, theater_ratio, 1258, 0.08).
narrative_ontology:measurement(qura_tr_t1798, quran_9_5_scope__contextual_defensive, theater_ratio, 1798, 0.1).
narrative_ontology:measurement(qura_tr_t1924, quran_9_5_scope__contextual_defensive, theater_ratio, 1924, 0.12).
narrative_ontology:measurement(qura_tr_t2001, quran_9_5_scope__contextual_defensive, theater_ratio, 2001, 0.14).
narrative_ontology:measurement(qura_tr_t2025, quran_9_5_scope__contextual_defensive, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quran_9_5_scope__contextual_defensive, base_extractiveness, 632, 0.1).
narrative_ontology:measurement(qura_be_t1258, quran_9_5_scope__contextual_defensive, base_extractiveness, 1258, 0.12).
narrative_ontology:measurement(qura_be_t1798, quran_9_5_scope__contextual_defensive, base_extractiveness, 1798, 0.15).
narrative_ontology:measurement(qura_be_t1924, quran_9_5_scope__contextual_defensive, base_extractiveness, 1924, 0.16).
narrative_ontology:measurement(qura_be_t2001, quran_9_5_scope__contextual_defensive, base_extractiveness, 2001, 0.17).
narrative_ontology:measurement(qura_be_t2025, quran_9_5_scope__contextual_defensive, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_9_5_scope__contextual_defensive, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the quran_9_5_scope kernel. abrogating_universal claims verse 9:5 establishes standing offensive obligation via abrogation of peaceful verses (high ε, tangled_rope or snare depending on enforcement claims); contextual_defensive (this story) claims the verse is treaty-breach-bounded and does not abrogate peaceful verses (low ε, rope); progressive_synthesis claims the verse is a time-bound directive superseded by ethical trajectory (distinct ε and beneficiary structure again). Each carries its own ε per the ε-invariance principle — they are not the same constraint measured three ways, but three structurally distinct claims sharing a contested text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
