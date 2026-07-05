% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Torah Study as Non-Fulfilling Preservation of the Sacrificial Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple in 70 CE, the biblical and
 *   rabbinic commandments to offer animal sacrifices became structurally
 *   unperformable. This constraint models one specific communal reading of
 *   that situation: the doctrine that intensive study of the laws of
 *   sacrifice (Zevachim, Kodashim, Maimonides' Temple-service codes)
 *   preserves the technical knowledge necessary for restoration, and carries
 *   independent religious value, but explicitly does NOT discharge or fulfill
 *   the underlying commandment. This is distinguished from two sibling
 *   readings evaluated as separate constraints: study_as_occupation (where
 *   the study itself IS held to constitute legitimate occupation of the
 *   obligation, resolving non-compliance) and messianic_suspension (where the
 *   obligation is held to be suspended rather than active, removing the
 *   compliance question entirely). The archiving reading is structurally the
 *   most extraction-tolerant of the three: it keeps the obligation live,
 *   keeps the interpretive class necessary to adjudicate that liveness, and
 *   never lets the laity's compliance status resolve.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_class: agenda_setter/beneficiary (institutional/identity_locked) — administers the archiving doctrine and derives status from it
 *   - yeshiva_institutions: beneficiary (organized/constrained) — funded and legitimized by the study-has-value-but-not-fulfillment framing
 *   - observant_laity: payer/beneficiary (moderate/constrained) — bear the unresolved compliance status, receive some liturgical comfort from study's partial value
 *   - laity_seeking_ritual_completion: payer (powerless/trapped) — structurally denied closure by this reading specifically
 *   - unfulfilled_divine_command: excluded, non-agent (powerless/trapped) — the object never retired
 *   - comparative_halakhic_observers: observer (analytical/analytical) — sees the full three-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.46).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.58).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.46).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Torah Study as Non-Fulfilling Preservation of the Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac').
narrative_ontology:cs_kernel_codification('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', fixed_text).
narrative_ontology:cs_authority_grounding('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', lineage).
narrative_ontology:cs_interpretation_layer_present('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac').
narrative_ontology:cs_reading_relation('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', foundational, study_preserves_but_does_not_discharge).
narrative_ontology:cs_axiom_status(study_preserves_but_does_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', study_preserves_but_does_not_discharge, conventional).
narrative_ontology:cs_axiom('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', foundational, obligation_remains_actively_binding_and_unmet).
narrative_ontology:cs_axiom_status(obligation_remains_actively_binding_and_unmet, holdable).
narrative_ontology:cs_axiom_grounding('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', obligation_remains_actively_binding_and_unmet, deontological).
narrative_ontology:cs_reference_frame('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', temple_era_active_sacrificial_performance).
narrative_ontology:cs_drift_state('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8e5848e0-8f3c-458e-a1f1-e5c8775ce9ac', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, textual_transmission_tradition).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, laity_seeking_ritual_completion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, observant_laity).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, observant_laity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, torah_study_equals_sacrifice_in_value_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, continuity_of_binding_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine that studying the laws of sacrifice (Talmudic tractate Zevachim, Maimonides' Laws of the Temple Service) preserves the knowledge needed for eventual restoration while explicitly holding that this study does NOT discharge the underlying commandment. This class occupies the interpretive seat: it decides how much emphasis to place on the unfulfilled status versus the substitute value of study, and its own social position, curricula, and institutional funding are built on the premise that this study is the highest available religious activity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, rabbinic_scholarly_class, beneficiary).

% Institutions of Torah study derive prestige, funding, and enrollment from the doctrine that intensive study of sacrificial law is a valuable religious act in its own right, distinct from and preserving the possibility of the actual sacrificial service. Their continued existence depends on maintaining the study as valuable even though it is authored to not fulfill the command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, yeshiva_institutions, beneficiary,
    organized, generational, constrained, national).

% Ordinary observant Jews live under a legal system that formally still obligates sacrificial offerings they cannot perform (no Temple exists), and are told that studying the relevant texts is meritorious but does not satisfy the command. They bear the psychological and liturgical weight of permanent non-compliance with a divine command, mitigated by but not resolved through the substitute activity of study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, observant_laity, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_archiving, observant_laity, beneficiary).

% A subset of observant individuals who desire actual ritual fulfillment, not textual substitution, and for whom the archiving doctrine offers no path to closure — only indefinite deferral. They cannot build the Temple, cannot offer sacrifice, and are told their study, however rigorous, leaves the obligation formally unmet. Exit from this bind would require either abandoning the framework or accepting the messianic-suspension or occupation readings instead, both of which are held by other communities, not this one.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, laity_seeking_ritual_completion, payer,
    powerless, biographical, trapped, global).

% Not an agent but the object of the constraint: the commandment to offer sacrifices at the Temple remains formally binding and formally unperformed across the entire post-70 CE period. Its status is invoked constantly in liturgy and law but the underlying performative debt is never actually retired by the archiving reading, which explicitly declines to claim otherwise.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).

% The corpus of sacrificial law itself (Mishnah Kodashim, Zevachim, Maimonides' Hilchot Avodat Yom HaKippurim, etc.) is kept alive, transmitted, and elaborated precisely because study is framed as archiving-for-restoration. The tradition's continued vitality is a byproduct of the doctrine, though the tradition itself collects no rents and is listed here for completeness.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, textual_transmission_tradition, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__study_as_archiving, textual_transmission_tradition).

% Scholars of comparative religious law and historians of halakha who examine how the study-as-archiving doctrine functions structurally: as a mechanism that keeps an unperformable law binding, keeps its interpretive class relevant, and defers rather than resolves the underlying command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, comparative_halakhic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_archiving, rabbinic_scholarly_class).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_archiving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of sacrificial procedure across a period of centuries-to-millennia during which the Temple does not exist, so that if restoration ever occurs the procedural knowledge is not lost. This is a genuine transmission/archiving function distinct from the underlying obligation.
% TRANSFER_FUNCTION: Moves religious authority, institutional prestige, and communal resources toward the interpretive and scholarly class that administers and teaches the sacrificial corpus, while explicitly withholding from the laity any doctrine that would let intensive study count as discharge of the command — the laity's compliance status remains permanently unresolved rather than settled in their favor.
% ABSENT_VOICES: Communities and individuals who hold the study-as-occupation reading (which WOULD let study discharge the obligation) or the messianic-suspension reading (which removes the obligation's active force entirely) are not voices within this reading's own framework — adopting either would dissolve the archiving reading's distinctive claim. Also absent: anyone arguing the command should be formally annulled given two millennia of non-performance.
% DISAPPEARANCE_RATIONALE: If the archiving doctrine vanished, its adherent communities would either drift toward study-as-occupation (resolving the non-fulfillment psychologically) or toward messianic-suspension (removing the tension by suspending the obligation's force) — both alternate readings already exist and are held by other communities. Whether 'the world rearranges' depends on which sibling reading absorbs the vacated position; the rabbinic institutions that currently administer the archiving reading would lose a specific doctrinal justification for their study's status, though study itself would likely continue under a different rationale.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial commandments became unperformable, creating an acute crisis: a body of divine law with no available means of compliance. The founding problem was how to keep the law and its practitioners religiously legitimate and coherent without either declaring the law void or pretending it was being fulfilled.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (e.g., Menachot 110a, on study of sacrifices being 'as if' offered, carefully hedged) and later halakhic authorities across competing schools attest that the absence of the Temple is an ongoing, unresolved condition — this is corroborated outside the rabbinic scholarly class by historians of Second Temple Judaism and by the very existence of the sibling readings (study_as_occupation, messianic_suspension), which are independent communal responses to the same acknowledged gap, not manufactured by the archiving reading's beneficiaries.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).
:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46) rather than high because the coordination function (knowledge preservation) is genuine and substantial — this is not pure extraction dressed as scholarship. But it is not zero because the doctrine's persistence structurally benefits an interpretive class whose relevance depends on the obligation remaining formally binding yet unmet; a resolution (either occupation-equivalence or suspension) would reduce the interpretive class's distinctive function. Suppression (0.58) reflects that alternative resolutions (declaring the law annulled, or accepting occupation-equivalence) are not doctrinally available within this reading's own framework — adopting them means leaving the reading, not modifying it. Theater ratio rises over the interval (0.20 to 0.42) as the practical urgency of Temple restoration recedes further into a diasporic horizon while institutional study apparatus around the doctrine (dedicated yeshiva tracks, published codifications) has grown; more of what is transmitted increasingly serves institutional continuity rather than restoration-readiness per se.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class sits nearest the beneficiary end: their institutional position depends on the sacrificial corpus remaining alive as a subject of intensive, expert-mediated study, without the psychological pressure valve of declaring it either fulfilled or suspended. Laity seeking ritual completion sit nearest the target end: they bear the full weight of an active-but-unperformable command with no doctrinal exit within this reading. Observant laity generally sit in between — genuine devotional value from study coexists with unresolved compliance anxiety.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destroyed, sacrificial law suddenly unperformable) is unambiguously live in the sense that the Temple has in fact not been rebuilt in nearly two millennia — this is not a case of an obsolete mandate being defended by a beneficiary class. What IS potentially mandatrophic is the specific choice among three available doctrinal resolutions: this reading (archiving) preserves the maximum institutional relevance for the interpretive class by refusing either resolution (occupation or suspension) that would reduce the tension. The classification does not allege the founding problem is fake — it is real — but flags that persistence of THIS SPECIFIC non-resolving reading, among three doctrinally available options, correlates with which reading maximizes the interpretive class's standing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archiving_vs_occupation_boundary,
    'Is the distinction between ''study preserves knowledge for restoration'' (this reading) and ''study occupies the commandment''s place'' (sibling reading) a substantive halakhic difference or primarily a difference in rhetorical emphasis serving the same institutional interest?',
    'Close textual analysis of primary halakhic sources (e.g., Rabbeinu Bahya, Ramban''s comments on Vayikra, comparative reading of Talmudic ''as if offered'' language) to determine whether authorities drawing this distinction derive different practical consequences from it, or whether the distinction is functionally inert.',
    'If functionally inert, the archiving reading and occupation reading may be a single constraint artificially split by rhetorical framing rather than genuine structural difference, which would violate the ε-invariance principle and require re-merging. If practical consequences differ (e.g., different liturgical formulas, different weight given to the mitzvah of study itself), the split is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archiving_vs_occupation_boundary, conceptual, 'Whether archiving and occupation are genuinely distinct kernels or a rhetorical split of one.').

omega_variable(
    institutional_interest_vs_genuine_theology,
    'Does the persistence of the archiving reading (as opposed to the resolving occupation or suspension readings) reflect genuine theological conviction independent of institutional interest, or does it correlate with which communities'' scholarly institutions most benefit from an unresolved, ever-relevant body of law?',
    'Comparative study of which communities/denominations hold which reading, cross-referenced against the relative size, funding, and prestige-structure of their scholarly institutions, controlling for theological tradition and historical lineage.',
    'If the reading correlates strongly with institutional size/funding independent of theological lineage, this supports treating the extraction component as real rather than incidental. If the reading tracks theological lineage independent of institutional interest, the extraction reading is weaker and the constraint sits closer to a pure rope with genuine, non-self-serving coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_interest_vs_genuine_theology, empirical, 'Whether reading-selection tracks institutional benefit or independent theological commitment.').

omega_variable(
    post_destruction_annulment_option,
    'Could the sacrificial obligation have been formally annulled or reinterpreted as non-binding given the multi-millennial impossibility of performance, and why has no reading within mainstream rabbinic tradition taken that path?',
    'Examination of the halakhic principle of ones (circumstances beyond control) as applied elsewhere in Jewish law, and comparison to cases where similarly long-unperformable obligations WERE formally annulled or reduced in force, to assess whether annulment was a live doctrinal option that was specifically avoided.',
    'If annulment was a genuinely available doctrinal path that was avoided specifically because it would dissolve the interpretive class''s relevance, this strengthens the tangled_rope classification. If annulment was never doctrinally available (e.g., because divine command cannot be annulled by human circumstance within this legal system), the persistence of the binding-but-unperformable status is closer to a structural/mountain-like feature of the legal system rather than an extractive choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_destruction_annulment_option, conceptual, 'Whether formal annulment was a foreclosed or merely unchosen option.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 300, 0.25).
narrative_ontology:measurement_basis(temp_tr_t300, observed).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 700, 0.3).
narrative_ontology:measurement_basis(temp_tr_t700, observed).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1100, 0.35).
narrative_ontology:measurement_basis(temp_tr_t1100, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.39).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t1955, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1955, 0.42).
narrative_ontology:measurement_basis(temp_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 300, 0.34).
narrative_ontology:measurement_basis(temp_be_t300, observed).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 700, 0.38).
narrative_ontology:measurement_basis(temp_be_t700, observed).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1100, 0.41).
narrative_ontology:measurement_basis(temp_be_t1100, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.44).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t1955, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1955, 0.46).
narrative_ontology:measurement_basis(temp_be_t1955, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 300, 0.52).
narrative_ontology:measurement_basis(temp_su_t300, observed).
narrative_ontology:measurement(temp_su_t700, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 700, 0.54).
narrative_ontology:measurement_basis(temp_su_t700, observed).
narrative_ontology:measurement(temp_su_t1100, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1100, 0.56).
narrative_ontology:measurement_basis(temp_su_t1100, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1500, 0.57).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t1955, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1955, 0.58).
narrative_ontology:measurement_basis(temp_su_t1955, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the status of the Temple sacrifice obligation after 70 CE' per the ε-invariance principle. study_as_archiving (this file) holds moderate extraction because it never resolves the compliance gap and thereby sustains ongoing interpretive relevance. study_as_occupation resolves the gap in the laity's favor (lower expected extraction). messianic_suspension removes the gap's active force entirely (different extraction profile again, tied to eschatological deferral rather than institutional study). Each has a distinct beneficiary/victim structure and must be evaluated as a separate constraint; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
