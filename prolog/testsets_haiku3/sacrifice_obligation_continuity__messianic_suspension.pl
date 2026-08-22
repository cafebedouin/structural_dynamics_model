% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Suspended Pending Messianic Restoration
 *   domain: religious/legal/ritual
 *
 * SUMMARY:
 *   The Jewish sacrifice obligation, commanded in Levitical law, became
 *   physically impossible after the destruction of the Second Temple in 70
 *   CE. Rather than releasing the obligation or reducing it to historical
 *   memory, rabbinic tradition developed the messianic suspension reading:
 *   the obligation is not fulfilled, not violated, but suspended pending the
 *   restoration of Temple sacrifice in messianic times. In the interim,
 *   intensive study of sacrifice law serves as the readiness maintenance that
 *   substitutes for performance. This constraint exists at the intersection
 *   of legal theory, eschatology, and institutional practice: it solves the
 *   problem of how binding law survives physical impossibility by postulating
 *   a future reactivation. The reading is contested within Jewish tradition
 *   by three siblings: study_as_performance (study fulfills the obligation
 *   now), performance_only (study prepares for future restoration but is not
 *   itself fulfillment), archival_preservation (the obligation is released;
 *   study preserves cultural memory). This story instantiates the messianic
 *   suspension reading only — a clean constraint with its own ε,
 *   beneficiary/victim structure, and reading relations.
 *
 * KEY AGENTS:
 *   - Observant Jewish communities: bear the readiness-maintenance burden (study obligation) without the fulfillment option (performance). Identity-locked to the law-binding framing; exit requires abandoning their reading of Jewish legal authority.
 *   - Rabbinic interpreters: administer the suspension-and-readiness framework; derive institutional authority and legitimacy from their role as guardians of the legal tradition. Benefit from the continued demand for interpretive expertise in sacrifice law.
 *   - Messiah/restored Temple: structurally excluded, non-existent, but the eschatological target whose absence sustains the current reading. Its non-arrival is what keeps the obligation suspended rather than released.
 *   - Competing reading communities: institutionally excluded; hold alternative interpretations of the same kernel. They would redistribute authority and meaning if their readings became dominant.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.45).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.38).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Suspended Pending Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious/legal/ritual").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:has_sunset_clause(sacrifice_obligation_continuity__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, 'a6da0b7d-7aa0-46ff-b507-3ad227b357e3').
narrative_ontology:cs_kernel_codification('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', fixed_text).
narrative_ontology:cs_authority_grounding('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', lineage).
narrative_ontology:cs_interpretation_layer_present('a6da0b7d-7aa0-46ff-b507-3ad227b357e3').
narrative_ontology:cs_reading_relation('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', foundational, obligation_persists_under_impossibility).
narrative_ontology:cs_axiom_status(obligation_persists_under_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', obligation_persists_under_impossibility, deontological).
narrative_ontology:cs_axiom('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', foundational, messianic_restoration_is_genuine_future_event).
narrative_ontology:cs_axiom_status(messianic_restoration_is_genuine_future_event, holdable).
narrative_ontology:cs_axiom_grounding('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', messianic_restoration_is_genuine_future_event, theological).
narrative_ontology:cs_reference_frame('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', suspended_obligation_with_messianic_restoration).
narrative_ontology:cs_drift_state('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', contemporary_post_enlightenment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a6da0b7d-7aa0-46ff-b507-3ad227b357e3', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, observant_jewish_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, messianic_eschatology_postponement).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__messianic_suspension, study_as_readiness_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the burden of maintaining readiness for sacrifice restoration through intensive study of sacrifice law and ritual detail, despite the sacrificial system not being performable. They interpret this as a positive obligation to study and remember, but the inability to fulfill the original commandment creates a standing asymmetry: they bear the readiness cost without the fulfillment option. Exit from this obligation would require abandoning their reading of Jewish law as binding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, observant_jewish_community, payer,
    organized, generational, identity_locked, global).

% Administer and interpret the law that transforms an unfulfillable sacrifice obligation into a study obligation. They derive interpretive authority from their role as guardians of textual tradition and maintainers of Jewish legal continuity. They benefit from the sustained institutional demand for rabbinic expertise in sacrifice law, which would diminish under alternative readings (e.g., archival preservation, where study is memory rather than obligation).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_interpreters, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_interpreters, beneficiary).

% The eschatological entity or event whose arrival would lift the suspension and reactivate the original performative obligation. Its non-existence or non-arrival is what sustains the current arrangement; it has no voice in the present legal discussion but is the structural reason the obligation persists as suspended rather than fulfilled or released.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messiah_or_restored_temple, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__messianic_suspension, messiah_or_restored_temple).

% Hold alternative readings of the same kernel: study_as_performance (study is fulfillment), performance_only (study merely prepares), archival_preservation (study is memory without obligation). They are structurally excluded from the institutional apparatus that enforces this reading; their interpretive authority is lower or rejected within the primary rabbinic establishment that declared the suspension/readiness framing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, competing_textual_reading_communities, excluded,
    organized, generational, constrained, global).

% The abstract legal system benefits from the continued binding force of the sacrifice obligation (even in suspended form) because it maintains the principle that divine commandments do not lose their normative status through changed circumstances. An alternative reading that released the obligation entirely would weaken the claim that Jewish law's authority survives diaspora, political change, and historical discontinuity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, jewish_legal_tradition_itself, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__messianic_suspension, jewish_legal_tradition_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, rabbinic_interpreters).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the continuity and binding force of Jewish legal tradition across the diaspora (when Temple sacrifice became physically impossible) by maintaining sacrifice law as a living, studied obligation rather than abandoning, abrogating, or reducing it to historical record. Coordinates the community around the interpretive claim that obligation persists even when performance is impossible, and study is the current form of that obligation.
% TRANSFER_FUNCTION: Moves the burden of intensive study and ritual-law maintenance from optional scholarship to binding obligation on observant Jews. The transfer is from a potential state (released obligation) or alternative state (study as mere memory) to the current state (study as readiness maintenance under suspension). Rabbinic interpreters collect the institutional authority and legitimacy that flow from being the guardians of this obligation's interpretation.
% ABSENT_VOICES: Competing reading communities (study_as_performance, performance_only, archival_preservation) are structurally excluded from the primary interpretive authority within rabbinically-aligned traditions. They would argue that the suspension itself is a theological and legal error, or that the obligation has been transformed (not suspended), or that it has been released entirely. Jews who adopt these alternative readings are present but their institutional power to declare the operative halachic rule is lower.
% DISAPPEARANCE_RATIONALE: If the messianic suspension reading disappeared and were replaced by, say, archival_preservation (obligation released, study optional), Jewish practice would remain materially similar in the short term — study of sacrifice law would continue — but the normative character would shift from binding obligation to elective cultural memory. The long-term effects are contested: some argue the community's sense of incompleteness and readiness would persist by other means; others argue the loss of binding obligation would gradually erode transmission and commitment. The constraint's disappearance would not eliminate sacrifice study, but would unmoor it from the obligation structure, changing its social meaning.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), Jews could no longer perform the Temple sacrificial rituals that Halakhah commanded. The constraint was built to answer: How does the binding character of divine commandments survive the physical impossibility of their performance? How does the Jewish legal tradition maintain its authority and continuity across exile and political powerlessness?
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and legal scholars across traditions attest that the binding authority of Jewish law and the persistence of obligation despite changed circumstances remain live concerns in Jewish theology and practice. Outside the benefiting community, historians and comparative-legal scholars recognize that the suspension-and-study framing is a genuine problem-solving innovation in how legal traditions survive discontinuity. Messianically-skeptical Jewish scholars argue the founding problem is conceptually live but the solution is outdated — a contested attestation that confirms the problem's historical reality even while disputing the reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constraint produces a sustained burden without guilt: observant Jews must maintain expertise in unfulfillable ritual detail without the option of either performing it or abandoning it entirely. This is neither pure extraction (a genuine problem of legal continuity is solved) nor genuine coordination (the solution imposes asymmetric readiness burden on one community). Theater ratio is high (0.62) because study of sacrifice law is increasingly detached from its functional content — sacrifice cannot be performed, so the study maintains readiness for an indefinitely postponed reactivation. As time passes and messianic expectation declines (measured in the rising trajectory from t=0 to t=25, then slight decline at t=30), the theater aspect grows: the readiness maintenance becomes less about preparation and more about performative maintenance of tradition and eschatological hope. Suppression is moderate-low (0.38) because the obligation is self-imposed within a community that identifies with Jewish legal tradition; external coercion is minimal, but identity lock creates internalized constraint. The measurement grid shows extractiveness rising gradually from t=0 to t=25 (as the constraint's eschatological grounding becomes more strained and the burden becomes clearer) before stabilizing at t=30. Theater rises throughout the interval as messianic expectation recedes. Suppression remains stable because the identity lock is not historical drift; it is structural to the community's relationship with Jewish law. The sunset clause is implicit in the reading's own structure: the obligation persists until Temple restoration, at which point the constraint would disappear (the messianic event dissolves the suspension and reactivates the original performative obligation). The interval represents roughly 2000 years (70 CE to present, discretized into 30 units), framing the long-term trajectory of a constraint built on a delayed expectation.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, this constraint solves a genuine problem: maintaining legal continuity, preserving textual tradition, and preserving the normative force of divine law across exile. From the observant community's seat, the constraint is also a solution (no other reading preserves their faith in Jewish law's binding authority), but it carries a cost: indefinite readiness maintenance with no fulfillment option, and growing incoherence as messianic expectation declines. From the excluded reading communities' seat, the constraint is a false solution that preserves rabbinic institutional power at the cost of coherence and meaning. The engine computes this perspectival gap from the stakeholder directionalities: the institutional agenda-setter has lower extraction risk than the identity-locked payer, generating different seat-level classifications of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Observant Jewish communities are the target seat (d near 1.0): they bear the readiness burden, face identity lock (constrained exit), and have no option to abandon the obligation without leaving the community's interpretive framework. Rabbinic interpreters are near the beneficiary end (d near 0.0): they set the interpretive agenda, derive authority and legitimacy from maintaining the suspension-and-study framing, and face no binding constraint from the obligation itself (it is their role to administer, not their burden to live under). The Jewish legal tradition itself is a non-agent beneficiary: the constraint's continuation preserves the principle that binding law survives physical discontinuity, which strengthens the tradition's normative authority across other domains. Competing reading communities are excluded (outside the primary institutional apparatus that enforces this reading), so their directionality is not computed within this constraint — they are external stakeholders whose alternative readings would shift the entire distribution if they became dominant.
 *
 * MANDATROPHY ANALYSIS:
 *   The messianic suspension reading faces a mandatrophy risk that the other readings attempt to resolve differently. The founding problem (how does binding law survive physical impossibility?) was live and urgent in 70 CE. The solution (suspend the obligation, maintain readiness through study) was adaptive as long as messianic restoration was an immediate or near-term expectation. Two thousand years later, with no restoration forthcoming and messianic expectation substantially declined in many Jewish communities, the founding problem has shifted: the question is no longer how to preserve law through temporary exile, but how to preserve meaning through indefinite postponement. The suspension framing becomes incoherent under this shifted problem — an obligation suspended indefinitely begins to feel like an obligation released, only disguised. Study that was readiness maintenance becomes memory preservation; the normative character erodes. This is mandatrophy: the constraint's original function (bridging a temporary gap) has outlived its context, but the constraint persists through institutional inertia and identity lock. The messianic suspension reading's continued authority depends on either: (1) maintaining actual messianic expectation, or (2) transforming the meaning of suspension from temporary to eternal (a shift that some communities have made explicitly). Without one of these moves, pressure mounts for alternative readings (archival_preservation, performance_only, study_as_performance), each of which would resolve the mandatrophy differently by reframing the constraint's relationship to the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_sibling_foreclosure,
    'Is the messianic suspension reading logically foreclosed by the study_as_performance reading''s claim that study itself is fulfillment, or do these readings coexist as live alternatives?',
    'Textual and theological analysis: if the foundational axioms contradict such that no framework could hold both (e.g., study_as_performance entails obligation is fulfilled now, suspension entails it is not fulfilled), then foreclosure applies; if both readings can be held by different interpretive communities without logical contradiction in their own frameworks, they coexist.',
    'If foreclosed, this reading''s kernel claim is overridden by a stronger sibling. If coexistent, the readings are competing live options whose distribution across communities is sociological rather than logical. This affects whether the constraint''s authority is universal or sectarian.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether study-as-fulfillment logically rules out suspension in a shared framework.').

omega_variable(
    suspension_vs_release,
    'Is the suspension framing genuinely distinct from a release/abrogation of the obligation, or does calling it ''suspended'' while making performance impossible amount to de facto release under a different name?',
    'Test the logical coherence of the suspension claim: can an obligation be normatively binding while being performatively impossible indefinitely, or does indefinite non-performance entail that the obligation has lost its binding force? Examine rabbinic texts that distinguish suspension from release and the phenomenological difference (guilt/incompleteness under suspension vs. absence of obligation under release).',
    'If suspension is genuine, extractiveness remains moderate and the obligation structure is preserved. If suspension is terminologically disguised release, the actual constraint is closer to archival_preservation and extractiveness should be lower (study-as-memory carries less burden). This affects whether the readiness maintenance is a burden or a chosen practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_release, conceptual, 'Whether suspension is logically distinct from de facto release.').

omega_variable(
    messianic_expectation_decline,
    'As messianic expectation declines historically within the community (fewer Jews expect imminent restoration), does the suspension framing become increasingly incoherent or does it acquire a new meaning (eternal readiness, structural incompleteness)?',
    'Historical and sociological measurement: track the stated probability that Jewish communities assign to messiah/temple restoration over centuries; correlate with textual emphasis on suspension vs. alternative framings; examine whether the constraint''s meaning shifts from ''temporary suspension'' to ''permanent condition.''',
    'If declining expectation makes suspension incoherent, pressure mounts for a different reading (study_as_performance, archival_preservation, performance_only). If suspension acquires new meaning (eternal readiness becomes the point), extractiveness and theater may both rise (maintaining readiness forever is theatrical burden-bearing; study becomes existential marker rather than functional preparation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_expectation_decline, empirical, 'Whether historical decline in messianic expectation undermines the suspension framing''s coherence.').

omega_variable(
    identity_lock_mechanism,
    'For observant Jews, is the identity lock to this obligation structural (Jewish identity is constituted through law-binding) or internalized (the community has internalized the obligation as part of self-concept and would maintain study even if the law were released)?',
    'Compare study patterns and commitment intensity between Jews who fully embrace the suspension-as-binding framing (identity locked) and those who adopt alternative readings but remain in the community (identity-loosened); examine post-hoc commitment changes when Jews transition between reading communities or leave observance entirely.',
    'If identity lock is structural, the constraint''s suppression and extractiveness reflect genuine structural binding, not mere choice. If internalized, the community might maintain readiness through choice and habit even if the law were released, lowering measured extraction. The distinction affects whether the exit_options=''identity_locked'' classification is accurate or whether exit has become more mobile over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity lock to the obligation is structural or internalized.').

omega_variable(
    kernel_authenticity_vs_constructed,
    'Is the suspension framing a genuine hermeneutical discovery within the textual tradition, or is it a constructed rationalization built retroactively to explain why the community continued to study sacrifice law despite physical impossibility?',
    'Textual archaeology: examine the earliest texts claiming suspension vs. the chronology of the Temple''s destruction and the emergence of the study-as-obligation framing. Determine whether suspension was explicitly theorized before the practice of study emerged, or whether the theory followed practice and was authored to justify it.',
    'If suspension is hermeneutically authentic (discovered in the tradition''s own logic), it has stronger normative authority and the constraint''s legitimacy is higher. If constructed retroactively, the constraint is more fragile and subject to delegitimization through exposure of its constructed character. The kernel_codification and reading_relations may need revision if the reading''s genealogy is discovered to be modern invention rather than traditional interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_authenticity_vs_constructed, conceptual, 'Whether suspension is hermeneutical discovery or retroactive rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t5, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 5, 0.57).
narrative_ontology:measurement_basis(sacr_tr_t5, observed).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 10, 0.59).
narrative_ontology:measurement_basis(sacr_tr_t10, observed).
narrative_ontology:measurement(sacr_tr_t15, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 15, 0.61).
narrative_ontology:measurement_basis(sacr_tr_t15, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 20, 0.63).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 25, 0.64).
narrative_ontology:measurement_basis(sacr_tr_t25, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t5, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(sacr_be_t5, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t15, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(sacr_be_t15, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 25, 0.47).
narrative_ontology:measurement_basis(sacr_be_t25, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(sacr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t5, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(sacr_su_t5, observed).
narrative_ontology:measurement(sacr_su_t10, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(sacr_su_t10, observed).
narrative_ontology:measurement(sacr_su_t15, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(sacr_su_t15, observed).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(sacr_su_t20, observed).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(sacr_su_t25, observed).
narrative_ontology:measurement(sacr_su_t30, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(sacr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This story is one reading of the `sacrifice_obligation_continuity` kernel. Sibling readings instantiate different constraints from the same textual kernel: study_as_performance reframes the obligation (study is fulfillment), performance_only separates study from obligation (study prepares), archival_preservation releases the obligation (study is memory). Each reading has distinct ε, beneficiary/victim structure, and institutional implications. They are linked via network.affects_constraints to indicate kernel family membership and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__messianic_suspension, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
