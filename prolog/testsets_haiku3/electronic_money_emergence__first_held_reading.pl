% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence (First Institutional Holding)
 *   domain: economic/monetary/technological
 *
 * SUMMARY:
 *   The emergence of electronic money in this reading is anchored to a
 *   discrete institutional event: the moment when the first institutional
 *   bearer (a central bank, commercial bank, or regulatory authority) held
 *   dematerialized currency in a form that was legally and operationally
 *   distinguishable from physical notes. This reading treats emergence as an
 *   ontological transition marked by institutional recognition and holding,
 *   not as a gradual technological diffusion or a retroactive statistical
 *   artifact. The constraint is claimed as a mountain—the threshold appears
 *   to be a natural feature of how technology and institutions co-evolved—but
 *   beneficiaries exist (regulatory authorities that gain measurement
 *   capacity). This triggers the false-summit evaluation gate: the constraint
 *   names a real institutional event, but that event confers regulatory power
 *   on authorities who benefit from setting its definition. The omegas
 *   address the unresolved question: Is the threshold a discovered fact
 *   (institutions recognize an already-existing state) or a constructed fact
 *   (institutional holding makes the category real)?
 *
 * KEY AGENTS:
 *   - Regulatory authorities: Set the legal threshold and recognize holdings
 *   - Central banks: Gain the ability to measure and model electronic money
 *   - Commercial banks: Bear the cost of compliance and new infrastructure
 *   - Depositors: Gain access to electronic holding; absorb overhead costs
 *   - Payment innovators: Excluded until regulatory recognition
 *   - Monetary theorists: Debate whether the emergence is natural or constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.31).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.08).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First Institutional Holding)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic/monetary/technological").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'eaca521a-310a-4bec-8967-65322c06fad0').
narrative_ontology:cs_kernel_codification('eaca521a-310a-4bec-8967-65322c06fad0', formalized).
narrative_ontology:cs_authority_grounding('eaca521a-310a-4bec-8967-65322c06fad0', lineage).
narrative_ontology:cs_interpretation_layer_present('eaca521a-310a-4bec-8967-65322c06fad0').
narrative_ontology:cs_reading_relation('eaca521a-310a-4bec-8967-65322c06fad0', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('eaca521a-310a-4bec-8967-65322c06fad0', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('eaca521a-310a-4bec-8967-65322c06fad0', foundational, emergence_marked_by_institutional_holding).
narrative_ontology:cs_axiom_status(emergence_marked_by_institutional_holding, holdable).
narrative_ontology:cs_axiom_grounding('eaca521a-310a-4bec-8967-65322c06fad0', emergence_marked_by_institutional_holding, empirically_contingent).
narrative_ontology:cs_axiom('eaca521a-310a-4bec-8967-65322c06fad0', foundational, legal_recognition_constitutes_money_ontology).
narrative_ontology:cs_axiom_status(legal_recognition_constitutes_money_ontology, holdable).
narrative_ontology:cs_axiom_grounding('eaca521a-310a-4bec-8967-65322c06fad0', legal_recognition_constitutes_money_ontology, deontological).
narrative_ontology:cs_reference_frame('eaca521a-310a-4bec-8967-65322c06fad0', pre_institutional_electronic_currency_experiments).
narrative_ontology:cs_drift_state('eaca521a-310a-4bec-8967-65322c06fad0', post_regulatory_formalization_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('eaca521a-310a-4bec-8967-65322c06fad0', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, regulatory_authorities).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, depositors).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, commercial_banks).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, depositors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the legal threshold for what counts as electronic money. Once the first institutional bearer held dematerialized currency in a legally recognized, distinguishable form, regulatory frameworks could measure and monitor it. The threshold itself becomes a fact-setting act: by recognizing the holding as money, authorities create the ontological status retroactively.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Gain the ability to track, regulate, and influence monetary aggregates once dematerialized currency is held in a measurable form. The emergence threshold defines when they can observe and model the money supply. The constraint's boundary is their empirical starting point.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).

% Must hold dematerialized currency in forms that meet the legal/regulatory threshold for 'electronic money' once the constraint activates. They bear the cost of compliance infrastructure, record-keeping, and regulatory oversight that follows from the holding being recognized as distinct from physical notes.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, commercial_banks, payer,
    powerful, biographical, constrained, national).

% Gain the ability to hold money in electronic form once the first institutional bearer recognizes and holds dematerialized currency. They depend on that recognition to know their deposits are real money, not claims on an undefined asset. They also absorb the costs of regulatory overhead and institutional rigidity that come with formalized electronic holding.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, depositors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, depositors, payer).

% Attempted to create alternative forms of dematerialized currency or payment systems before institutional recognition. Their innovations are excluded from the 'electronic money' category until a regulatory authority accepts them as meeting the institutional-holding threshold, making their early technical work legally invisible.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, payment_innovators, excluded,
    moderate, biographical, trapped, global).

% Debate whether the emergence is a natural economic fact (a threshold in technology and behavior that institutions merely recognize) or a constructed regulatory fact (the threshold emerges only when authorities declare it). This constraint is one reading of that contested question.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_theorists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, regulatory_authorities).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared ontological standard: once the first institutional bearer holds dematerialized currency in a distinguishable form, all subsequent institutional actors know what 'electronic money' means and can coordinate around measurement, regulation, and transfer of it.
% TRANSFER_FUNCTION: Transfers the right to define and measure money from implicit collective practice to explicit institutional authority. Before the first holding, money was a function of behavior; after, it is also a function of legal recognition. The transfer is from decentralized epistemic authority to centralized regulatory authority.
% ABSENT_VOICES: Non-institutional actors (peer-to-peer networks, alternative currency communities, technologists working outside regulatory frameworks) are excluded from the moment of emergence. They cannot trigger the constraint; only institutional bearers can. Their competing definitions of 'money' are unvoiced in the recognition process.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, institutional actors would continue to hold and recognize dematerialized currency anyway—the threshold would not disappear, only the formal recognition of it. The world would reorganize around a different narrative of emergence (perhaps the became_thinkable reading or the m4_m5_collapse reading), but dematerialized holding and the category distinctions would persist.
% FOUNDING_PROBLEM: How do we know when money transitions from physical to electronic form? What observable threshold marks the ontological change? Without a clear institutional marker, the category remains ambiguous.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and regulatory authorities attest that institutional holding creates the observable threshold. Economic historians outside the regulatory apparatus contest whether the threshold is a natural fact of technology or a retroactive regulatory construction. Theorists of money (Searle, Graeber, Zelizer) dispute whether institutional recognition creates the category or merely names what was already happening.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading models electronic money's emergence as a regulatory/institutional fact. Extractiveness (0.31 at interval end) reflects the constraint's capacity to concentrate power in authorities that define the threshold—once 'electronic money' is institutionally recognized, regulators gain authority to set standards, classify holdings, and intervene in monetary aggregates. Suppression is low (0.08) because the constraint is not maintained coercively; it persists because the institutional framework it establishes is genuinely useful for coordination. Theater ratio is low (0.12) because the actual function (defining the category) aligns closely with the stated function (recognizing electronic holding). Accessibility collapse is high (0.72) because once the threshold is set institutionally, alternative framings of 'electronic money' become difficult to maintain—regulators' definition becomes canonical. Resistance is low (0.18) because institutions benefit from the clarity the threshold provides, and the resistance that does exist comes from theorists and innovators outside the regulatory structure, not from powerful institutional actors. The measurement series is anchored at 1950 (pre-threshold, early experimentation) and runs to 1995 (post-threshold, mature regulatory regimes). Extractiveness rises sharply through the 1970s as institutional holding becomes formalized, then plateaus as the regime stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory seat, this constraint is a natural recognition of a technological and social fact: institutions needed to formally acknowledge electronic holding to manage monetary systems. From the innovator seat (if they could speak), the constraint is a constructed monopoly: regulators seized the right to define 'electronic money' and locked out competing definitions. From the bank seat, it is a necessary compliance burden with uneven costs across institutions. The engine should compute the regulatory seat's type as mountain-beneficiary (nearly natural, benefiting the authority), the bank's type as snare or tangled-rope (coordinated category-setting, but extraction accrues to authorities), and the innovator's type as snare (excluded, harmed, trapped). The authored metric profile (low suppression, high accessibility collapse, genuine coordination function) supports the mountain claim, but beneficiary presence triggers false-summit evaluation.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities are beneficiaries (d near 0.0): they gain the authority to define and measure electronic money, concentrate power over monetary aggregates, and can adjust standards to suit their purposes. Central banks are also beneficiaries: measurement capacity directly enhances their ability to implement monetary policy and maintain financial stability. Commercial banks are targets (d near 1.0): they must invest in infrastructure to hold electronic currency in compliant forms and absorb the ongoing costs of regulatory oversight. Depositors sit near symmetric (d near 0.5): they benefit from access to electronic holdings and the certainty that deposits are recognized as real money, but absorb the costs of institutional rigidity and the constraints imposed by regulatory classification. Payment innovators are harmed but excluded: their inability to participate in setting the threshold (trapped exit) places them at high d, but they are not present in the core institutional decision-making. Monetary theorists are analytical observers (d = 0.5 by convention): they have no structural stake in the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (establishing an ontological threshold for electronic money) remains live and contested. Regulatory authorities argue the problem is solved: institutional holding is now clearly defined and measured. Theorists and innovators argue the problem is not solved but merely displaced: by institutionalizing one definition, regulators silenced competing definitions (blockchain, community currencies, alternative payment networks). The mandatrophy question is whether the regulatory definition of the threshold is permanent or whether future innovations might force a redefinition. The m4_m5_collapse reading (measurement artifact hypothesis) directly contests the founding problem's status: if emergence is retroactively created by statistical distinction rather than institutional holding, then the problem was never 'solved'—it was never real to begin with, only constructed by measurement. This reading asserts the problem is live and the solution is institutional; the omega variables document the irreducible uncertainty about whether that solution is discovery or construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_recognition_vs_natural_emergence,
    'Is the threshold for electronic money emergence a natural fact that institutions discover, or a constructed fact that institutions create through recognition and holding?',
    'Examine historical records of early electronic money experiments (SWIFT, ACH, interbank settlement systems) to determine whether institutions recognized pre-existing dematerialized holdings or whether institutional formalization was the event that made holdings ''real'' in any operational sense. A counterfactual: would electronic money holdings have any legal or monetary status without institutional recognition?',
    'If the threshold is discovered (natural), the constraint is a genuine mountain: the emergence happened when technology and institutions met, and regulators merely observed it. If constructed (institutional act), the constraint is a false summit: regulatory authority benefits from setting the threshold, and the ''emergence'' is partly extraction (power to define). This omega determines whether the false-summit gate fires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_recognition_vs_natural_emergence, conceptual, 'Whether emergence is institutional recognition of a natural fact or institutional construction of a category.').

omega_variable(
    measurement_threshold_ambiguity,
    'What exact moment marks ''first institutional holding''? Is it the moment a bank''s computers first stored a currency value in electronic form, the moment regulators first legally recognized such storage as money, or some other threshold?',
    'Historical archive: identify the first central bank or commercial bank that, by its own records or regulatory records, held dematerialized currency and the date of that holding. Compare against regulatory codification dates (when central banks or treasuries formally recognized electronic holdings as distinct from physical notes in their classification schemes).',
    'If the threshold is technical (first storage), emergence happens earlier and is harder to pin down precisely. If regulatory (first legal recognition), emergence is later and clearer. The choice of threshold affects dating of the emergence and the classification of early payment systems as ''electronic money'' or not.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_threshold_ambiguity, empirical, 'Which specific event marks the first institutional holding and emergence?').

omega_variable(
    alternative_reading_foreclosure,
    'Does this reading''s claim (emergence via institutional holding) logically foreclose the became_thinkable reading, or do both remain coherently holdable?',
    'Logical analysis: the became_thinkable reading claims emergence marks the moment conceptual possibility became technically and socially thinkable. This reading claims it marks institutional holding. These are temporally different (possibility precedes holding) but not logically contradictory: one could hold that emergence is *marked* by institutional holding while *enabled* by prior conceptual possibility. The question is whether this reading''s framing rules out the thinkable reading''s core premise.',
    'If readings foreclose each other, the network relationship is ''forecloses''. If both can be held as live positions in different interpretive frameworks, the relationship is ''coexists_with''. Classification accuracy depends on getting this relationship right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical compatibility between this reading and the became_thinkable sibling reading.').

omega_variable(
    regulatory_capture_in_threshold_setting,
    'To what degree do central banks and regulators benefit from setting the emergence threshold, and does that benefit constitute extraction?',
    'Analyze regulatory authority consolidation over time: measure the range of policy tools available to central banks before and after electronic money emergence is institutionally recognized. Higher policy-tool range after recognition suggests regulators gain power from setting the threshold. Compare against the welfare outcomes for commercial banks and depositors; if their outcomes improve less than regulators'' authority expands, the benefit asymmetry suggests extraction.',
    'If regulators gain substantial and disproportionate power (relative to efficiency gains for the system), the false-summit signal strengthens and the constraint should be reclassified from mountain to tangled_rope or snare. If gains are genuinely symmetric (regulators gain authority, but banks and depositors gain proportionally in efficiency and access), the mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_in_threshold_setting, empirical, 'Whether regulatory authority gain from threshold-setting constitutes extraction or genuine coordination overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1950, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__first_held_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement_basis(elec_tr_t1950, projected).
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__first_held_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement_basis(elec_tr_t1960, observed).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement_basis(elec_tr_t1970, observed).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement_basis(elec_tr_t1980, observed).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).
narrative_ontology:measurement(elec_tr_t1995, electronic_money_emergence__first_held_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(elec_tr_t1995, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__first_held_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement_basis(elec_be_t1950, projected).
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__first_held_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement_basis(elec_be_t1960, observed).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement_basis(elec_be_t1970, observed).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement_basis(elec_be_t1980, observed).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement_basis(elec_be_t1990, observed).
narrative_ontology:measurement(elec_be_t1995, electronic_money_emergence__first_held_reading, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement_basis(elec_be_t1995, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1950, electronic_money_emergence__first_held_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement_basis(elec_su_t1950, projected).
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__first_held_reading, suppression_requirement, 1960, 0.04).
narrative_ontology:measurement_basis(elec_su_t1960, observed).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.06).
narrative_ontology:measurement_basis(elec_su_t1970, observed).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.07).
narrative_ontology:measurement_basis(elec_su_t1980, observed).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement_basis(elec_su_t1990, observed).
narrative_ontology:measurement(elec_su_t1995, electronic_money_emergence__first_held_reading, suppression_requirement, 1995, 0.08).
narrative_ontology:measurement_basis(elec_su_t1995, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'electronic_money_emergence'. The thinkable_reading and collapse_reading are sibling constraints instantiating different readings of the same kernel. All three share a referent (digital money) but diverge on what makes emergence real. This reading (first_held_reading) anchors emergence to institutional holding and regulatory recognition; reading relations and axioms are declared in cs_structure to model the contest. Each reading has its own ε (epsilon), stakeholder structure, and beneficiary/victim profile because the readings produce different structural facts about who benefits from the emergence and what extraction (if any) occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
