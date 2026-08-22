% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh as Progressive Restriction (Divine Pedagogy Reading)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   This constraint models the progressive-restriction reading of naskh: the
 *   doctrine that apparently conflicting Quranic verses on the same legal
 *   topic represent a developmental sequence of divine pedagogy rather than
 *   either simple chronological abrogation (classical_abrogation) or
 *   permanently co-valid contextual instructions (contextual_harmonization).
 *   Under this reading, earlier permissive verses are understood as
 *   transitional accommodations to a community not yet ready for the final
 *   ruling, and the later, more restrictive verse is read as the culminating,
 *   binding instruction. This is a distinct interpretive claim from its
 *   siblings, not a synonym for them: it shares the chronological-sequence
 *   intuition with classical_abrogation but denies that earlier verses are
 *   'abrogated' (invalid text) — instead framing them as valid-but-superseded
 *   pedagogy, which has different implications for how those verses may still
 *   be cited (as historical illustration of divine method, not as alternative
 *   live law). This distinction matters practically: someone citing an
 *   earlier permissive verse to justify present conduct is, under this
 *   reading, not citing bad law but is misreading a pedagogical stage as a
 *   permanent conclusion.
 *
 * KEY AGENTS:
 *   - evolutionary_reformist_jurists: institutional beneficiary and agenda-setter — administers which verses count as pedagogy vs. finality
 *   - restrictive_ruling_proponents: organized beneficiary — uses pedagogical-finality framing to close legal debate
 *   - practitioners_citing_earlier_permissive_verses: powerless payer — bears sanction for practices the earlier verse would permit
 *   - literalist_minority_schools: moderate-power payer — expends capital defending textual co-validity against the dominant frame
 *   - comparative_hermeneutics_scholars: analytical observer across the kernel's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.52).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh as Progressive Restriction (Divine Pedagogy Reading)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'c94f6b61-9a7f-4763-b991-802545d217f1').
narrative_ontology:cs_kernel_codification('c94f6b61-9a7f-4763-b991-802545d217f1', distributed).
narrative_ontology:cs_authority_grounding('c94f6b61-9a7f-4763-b991-802545d217f1', lineage).
narrative_ontology:cs_interpretation_layer_present('c94f6b61-9a7f-4763-b991-802545d217f1').
narrative_ontology:cs_reading_relation('c94f6b61-9a7f-4763-b991-802545d217f1', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('c94f6b61-9a7f-4763-b991-802545d217f1', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_axiom('c94f6b61-9a7f-4763-b991-802545d217f1', foundational, sequence_reflects_pedagogical_gradualism).
narrative_ontology:cs_axiom_status(sequence_reflects_pedagogical_gradualism, holdable).
narrative_ontology:cs_axiom_grounding('c94f6b61-9a7f-4763-b991-802545d217f1', sequence_reflects_pedagogical_gradualism, conventional).
narrative_ontology:cs_axiom('c94f6b61-9a7f-4763-b991-802545d217f1', foundational, earlier_verses_remain_textually_valid_but_legally_superseded).
narrative_ontology:cs_axiom_status(earlier_verses_remain_textually_valid_but_legally_superseded, holdable).
narrative_ontology:cs_axiom_grounding('c94f6b61-9a7f-4763-b991-802545d217f1', earlier_verses_remain_textually_valid_but_legally_superseded, conventional).
narrative_ontology:cs_reference_frame('c94f6b61-9a7f-4763-b991-802545d217f1', classical_naskh_scholarly_consensus).
narrative_ontology:cs_drift_state('c94f6b61-9a7f-4763-b991-802545d217f1', contemporary_reformist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c94f6b61-9a7f-4763-b991-802545d217f1', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_reformist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, restrictive_ruling_proponents).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, practitioners_citing_earlier_permissive_verses).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_minority_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, lay_congregants).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, lay_congregants).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogical_gradualism).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, final_revelation_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and teach the progressive-restriction framework in seminaries, fatwa councils, and academic institutions. They administer which verses count as 'transitional accommodation' versus 'final divine intent,' and their reading is what gets cited when restrictive rulings are defended as pedagogically final rather than merely one node in a chronological sequence. Their institutional standing and publishing careers are built on this interpretive apparatus.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_reformist_jurists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, evolutionary_reformist_jurists, beneficiary).

% Religious authorities, state religious bureaucracies, and social conservatives who benefit when a restrictive ruling is framed as the culmination of divine teaching rather than one historically situated instruction among several. They can invoke pedagogical finality to close further legal debate on the matter.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, restrictive_ruling_proponents, beneficiary,
    organized, generational, mobile, national).

% Ordinary believers, minority-tradition adherents, or reform-minded individuals who point to earlier, more permissive verses to justify a contemporary practice (e.g., regarding wine-adjacent trade, certain marital arrangements, or wartime conduct). Under this reading their citation is dismissed as invoking a superseded accommodation rather than a valid alternative reading; they bear social and sometimes legal sanction for practices the earlier verses would have permitted.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, practitioners_citing_earlier_permissive_verses, payer,
    powerless, biographical, trapped, local).

% Smaller schools of thought that hold each verse retains independent legal force in its own context. They must expend continuous scholarly and political capital defending their position against the progressive-restriction framework's claim that theirs is a category error — treating temporary pedagogy as permanent law. Their textual arguments are heard but structurally disadvantaged in institutions that have adopted the progressive-restriction consensus.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_minority_schools, payer,
    moderate, generational, constrained, regional).

% Receive a coherent narrative explaining why the Quran contains verses that seem to contradict — the pedagogy framing offers a satisfying developmental story. But they also inherit the restrictive endpoint as binding without full awareness that the reading itself is one of three live scholarly positions, not a settled fact.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, lay_congregants, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, lay_congregants, payer).

% Study naskh doctrine comparatively across classical and modern schools, documenting how each reading of the kernel distributes interpretive authority and legal consequence differently, without being party to any single tradition's enforcement mechanism.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, comparative_hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, evolutionary_reformist_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent developmental account of why the Quran contains verses of varying permissiveness on the same topic, allowing a single legal community to converge on one binding ruling instead of holding all historical permissions simultaneously live.
% TRANSFER_FUNCTION: Moves interpretive authority and legal finality toward those who administer the 'final divine intent' designation, and moves legal standing away from anyone whose contemporary practice or claim rests on an earlier, more permissive verse — their citation is reclassified as citing superseded pedagogy rather than valid law.
% ABSENT_VOICES: Individuals and minority schools who would argue the earlier verse remains independently valid in its own context (the contextual_harmonization position) are present in scholarly literature but structurally sidelined wherever the progressive-restriction consensus has captured seminary curricula and state fatwa councils; their objection is heard but rarely determines outcomes.
% DISAPPEARANCE_RATIONALE: Reformist and restrictive-ruling institutions would say the coherence of the entire naskh apparatus depends on the pedagogical-progression frame and its disappearance would destabilize settled rulings; practitioners of earlier-verse traditions and comparative scholars would say the underlying texts and their contextual meanings are unchanged either way — only the institutional gloss vanishes, and legal practice would simply become more contested rather than collapsing.
% FOUNDING_PROBLEM: Early Muslim jurists faced Quranic verses on the same topic (e.g., alcohol, warfare conduct, marital law) revealed at different times with apparently different permissiveness; a mechanism was needed to determine which ruling governs contemporary practice without treating the Quran as internally self-contradictory.
% FOUNDING_PROBLEM_CORROBORATION: Classical abrogation theorists and comparative hermeneutics scholars attest the underlying interpretive problem (reconciling apparently conflicting revealed rulings) remains genuinely live; contextual-harmonization scholars, writing from outside the progressive-restriction beneficiary set, argue the problem was always more apparent than real and that the progressive-restriction solution was adopted for its capacity to close legal debate rather than because supersession was textually demonstrated.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, contested).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 by the interval's end) because the doctrine's coordination function is real — it gives a working legal community a way to converge on one binding rule instead of endless contradiction — but the convergence mechanism systematically closes off a genuine alternative textual reading (contextual co-validity) that some practitioners rely on. Suppression (0.58) reflects that maintaining the progressive-restriction consensus requires active institutional work: seminary curricula, fatwa council rulings, and social sanction against citing earlier verses as still-operative law. Theater ratio is comparatively low (0.28) because the interpretive apparatus does perform genuine scholarly and pastoral function, not mere performance — but it rises over the interval as the doctrine ossifies into institutional orthodoxy defended more by authority than by fresh textual argument.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the doctrine looks like sound historical-critical method resolving real textual tension. From the payer seat (practitioners and literalist schools), the same doctrine looks like a device for converting one contestable interpretive choice into unchallengeable religious finality. The engine should compute divergent per-seat classifications from these structural positions rather than the story asserting one verdict for all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary reformist jurists and restrictive-ruling proponents sit near the beneficiary end: they administer and are vindicated by the doctrine, and their legal and institutional position is subsidized by its acceptance. Practitioners citing earlier permissive verses sit near the full-target end: trapped exit (they cannot simply choose a different revealed text), local scope, powerless — the doctrine's operation directly delegitimizes their citation practice. Literalist minority schools are moderate targets: organized enough to sustain scholarly resistance but structurally disadvantaged wherever the progressive-restriction consensus has captured formal institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling apparently conflicting revealed rulings) remains genuinely contested rather than resolved or dead — this prevents a simple snare or piton classification. The doctrine is not empty inertia: it performs real coordination work for communities that need one binding answer. But its persistence is sustained partly by the interpretive authority it grants those who administer the pedagogy/finality distinction, which is why tangled_rope (not rope) is the structurally accurate claim — genuine coordination function coexists with asymmetric extraction from those whose citation practices are foreclosed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogy_vs_abrogation_distinction_reality,
    'Is the pedagogy/finality distinction a genuine textual-historical finding, or a reclassification of abrogation that preserves its practical effect (closing off earlier verses as live law) while avoiding the doctrinal cost of calling them invalid?',
    'Comparative analysis of how progressive-restriction rulings differ in practical legal outcome from classical-abrogation rulings on the same verse pairs — if outcomes are identical, the pedagogy framing is largely rhetorical relabeling.',
    'If outcomes are identical to classical_abrogation, this reading''s claim to be structurally distinct (rather than a euphemism) weakens, and its extractiveness profile should more closely track classical_abrogation''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_vs_abrogation_distinction_reality, conceptual, 'Whether progressive restriction is analytically distinct from abrogation or a relabeling with the same practical effect.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Why do certain institutions adopt progressive_restriction over classical_abrogation or contextual_harmonization — is the selection driven by stronger textual/historical evidence, or by which reading best supports rulings those institutions already favor?',
    'Historical study of when and where the progressive-restriction framing gained institutional dominance, cross-referenced against which legal outcomes it was used to justify.',
    'If adoption tracks convenient outcomes rather than independent evidentiary strength, this raises the effective extractiveness of the reading as currently institutionalized, since the coordination story would be functioning partly as cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether reading-selection across the kernel is evidence-driven or outcome-driven.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do progressive_restriction and contextual_harmonization diverge in practice — is it solely about whether earlier verses may still be cited as operative law, or does it extend to broader questions of revelatory unity?',
    'Close comparison of specific fatwa rulings issued under each reading for identical fact patterns (e.g., regarding alcohol-adjacent commerce or wartime treatment of captives).',
    'Precisely locating the divergence clarifies which practitioners are actually harmed by adopting progressive_restriction over its sibling, sharpening the victim-group definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, empirical, 'Locating the precise practical divergence between this reading and contextual_harmonization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__progressive_restriction, theater_ratio, 200, 0.18).
narrative_ontology:measurement(nask_tr_t500, naskh_principle__progressive_restriction, theater_ratio, 500, 0.21).
narrative_ontology:measurement(nask_tr_t800, naskh_principle__progressive_restriction, theater_ratio, 800, 0.24).
narrative_ontology:measurement(nask_tr_t1100, naskh_principle__progressive_restriction, theater_ratio, 1100, 0.26).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__progressive_restriction, theater_ratio, 1400, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nask_be_t200, naskh_principle__progressive_restriction, base_extractiveness, 200, 0.36).
narrative_ontology:measurement(nask_be_t500, naskh_principle__progressive_restriction, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(nask_be_t800, naskh_principle__progressive_restriction, base_extractiveness, 800, 0.46).
narrative_ontology:measurement(nask_be_t1100, naskh_principle__progressive_restriction, base_extractiveness, 1100, 0.49).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__progressive_restriction, base_extractiveness, 1400, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(nask_su_t200, naskh_principle__progressive_restriction, suppression_requirement, 200, 0.46).
narrative_ontology:measurement(nask_su_t500, naskh_principle__progressive_restriction, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(nask_su_t800, naskh_principle__progressive_restriction, suppression_requirement, 800, 0.53).
narrative_ontology:measurement(nask_su_t1100, naskh_principle__progressive_restriction, suppression_requirement, 1100, 0.56).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__progressive_restriction, suppression_requirement, 1400, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_contextual_harmonization).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the naskh_principle kernel. classical_abrogation treats supersession as textual invalidation; contextual_harmonization denies supersession entirely; progressive_restriction (this story) holds an intermediate position — no invalidation, no permanent co-validity, but developmental pedagogy culminating in final restriction. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure; they are linked here rather than merged because they produce materially different legal outcomes for the same underlying verse pairs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
