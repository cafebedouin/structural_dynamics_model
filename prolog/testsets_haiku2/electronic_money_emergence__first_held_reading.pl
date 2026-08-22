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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Electronic Money Emergence (First-Held Reading)
 *   domain: economic_history/monetary_theory
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'electronic_money_emergence'. This reading asserts that electronic money
 *   emerged as a discrete, observable institutional event: the moment when
 *   the first central bank or major commercial banking authority held
 *   dematerialized currency in electronic form and recognized it as legal
 *   tender equivalent to physical notes. The reading dates the emergence to
 *   approximately 1970–1978 across major jurisdictions (Fed automated
 *   clearing house, SWIFT founding, Bundesbank electronic settlement). This
 *   reading is ontologically fixed and empirically anchored: electronic money
 *   becomes a thing when an institution recognizes it as such through
 *   regulatory/legal codification, not when the concept becomes thinkable or
 *   when measurement artifacts retroactively construct it. The constraint is
 *   CLAIMED as mountain (a threshold fact about monetary architecture) while
 *   acknowledging deep contest about whether the referent is accurately
 *   identified as 'first institutional bearer' versus alternative
 *   demarcations.
 *
 * KEY AGENTS:
 *   - Central banks: first institutional bearers of dematerialized currency; set the regulatory standard for legal equivalence.
 *   - Commercial banking sector: adopted electronic settlement and depositor ledgers; extended the constraint to retail money.
 *   - Monetary authorities/regulators: codified the transition through policy and law; marked the institutional threshold.
 *   - Depositing public: experience electronic money as settled, invisible infrastructure; no agency in the emergence event itself.
 *   - Historians/theorists: contest when emergence occurred; hold alternative readings (became_thinkable, m4_m5_collapse).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.12).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First-Held Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'b07c4f6d-b5e8-421b-98f4-411efa51d94b').
narrative_ontology:cs_kernel_codification('b07c4f6d-b5e8-421b-98f4-411efa51d94b', distributed).
narrative_ontology:cs_authority_grounding('b07c4f6d-b5e8-421b-98f4-411efa51d94b', expertise).
narrative_ontology:cs_interpretation_layer_present('b07c4f6d-b5e8-421b-98f4-411efa51d94b').
narrative_ontology:cs_reading_relation('b07c4f6d-b5e8-421b-98f4-411efa51d94b', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('b07c4f6d-b5e8-421b-98f4-411efa51d94b', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('b07c4f6d-b5e8-421b-98f4-411efa51d94b', foundational, institutional_recognition_as_ontological_mark).
narrative_ontology:cs_axiom_status(institutional_recognition_as_ontological_mark, holdable).
narrative_ontology:cs_axiom_grounding('b07c4f6d-b5e8-421b-98f4-411efa51d94b', institutional_recognition_as_ontological_mark, conventional).
narrative_ontology:cs_axiom('b07c4f6d-b5e8-421b-98f4-411efa51d94b', foundational, discrete_threshold_emergence).
narrative_ontology:cs_axiom_status(discrete_threshold_emergence, holdable).
narrative_ontology:cs_axiom_grounding('b07c4f6d-b5e8-421b-98f4-411efa51d94b', discrete_threshold_emergence, empirically_contingent).
narrative_ontology:cs_reference_frame('b07c4f6d-b5e8-421b-98f4-411efa51d94b', physical_bearer_currency_as_default).
narrative_ontology:cs_drift_state('b07c4f6d-b5e8-421b-98f4-411efa51d94b', electronic_settlement_institutionalized, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('b07c4f6d-b5e8-421b-98f4-411efa51d94b', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, commercial_banking_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, depositing_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First institutional bearers of dematerialized currency in central bank settlement systems and reserves. Gained operational efficiency, reduced physical custody costs, and enhanced monetary control through electronic clearing. The constraint emerges when their ledgers first recorded bearer claims in purely electronic form distinguishable from physical notes.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, arbitrage, national).

% Adopted electronic ledgers for interbank settlement and customer accounts. Benefited from reduced clearing friction, lower custody overhead, and ability to scale transactions without physical infrastructure scaling. Electronic bearer status enabled modern payment systems.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, commercial_banking_sector, beneficiary,
    institutional, generational, arbitrage, national).

% Hold money as electronic claims on commercial banks and, derivatively, on central bank reserves. The constraint is invisible to them operationally — they experience money as already dematerialized, a settled institutional fact. Their exit options are constrained to whatever currency/banking system they inhabit.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, depositing_public, beneficiary,
    organized, biographical, constrained, national).

% Recognized, regulated, and institutionalized the shift from physical to electronic bearer status. Their regulatory frameworks codified which institutions could hold dematerialized currency and on what terms. The threshold moment — first legal recognition of electronic bearer claims as equivalent to physical notes — is the constraint's structural anchoring point.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_authorities_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Debate when electronic money 'emerged': was it the first institutional holding, the conceptual possibility, or the measurement artifact of M4/M5 separation? This reading's claim is that emergence is marked by the first institutional bearer physically transitioning bearer claims into dematerialized form recognized as legal tender equivalent.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, historians_and_monetary_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled institutional recognition and technical reproduction of bearer status in electronic form: money moved from physical custody to electronic ledgers while retaining legal equivalence and settlement finality. Solved the problem of scaling transaction velocity beyond physical transport speed.
% TRANSFER_FUNCTION: No direct transfer function (this reading frames emergence as institutional recognition, not extraction). Benefits flow to institutions gaining operational efficiency; no party bears extraction cost as a direct mechanism of the constraint itself.
% ABSENT_VOICES: Technical architects of early clearing systems (often employed by central banks, marginalized in policy discourse). Competing monetary theorists who would date emergence earlier (conceptual) or later (statistical). Physical currency holders who experienced the transition as opacity — the emergence was their money becoming invisible.
% DISAPPEARANCE_RATIONALE: Electronic money in its mature form is now built into the institutional substrate of every modern economy. If the constraint — the institutional recognition moment of first held dematerialized bearer status — had never been crossed, physical currency would have remained the default, clearing would remain manual, and transaction velocity would be orders of magnitude lower. But 'disappearance' would mean time-traveling back before that threshold crossed, making the counterfactual question incoherent. The constraint marks a threshold; once crossed, reversal is not a local institutional change but a civilizational reorganization.
% FOUNDING_PROBLEM: How to scale currency circulation beyond the velocity limits of physical transport and custody. Physical notes required couriers, armored transport, geographic arbitrage delays. Institutional holding of electronic ledger entries solved this by collapsing spatial friction.
% FOUNDING_PROBLEM_CORROBORATION: All monetary historians and settlement system engineers atttest the founding problem is still active — modern transaction velocity depends entirely on electronic bearer status. Central banks' own operational records and technical documentation from the 1960s-1980s transition period provide direct corroboration outside advocacy for any particular reading.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   This reading frames electronic money emergence as a sharp institutional threshold, not a gradual technological adoption. Extractiveness is low (0.38 at endpoint) because the constraint is primarily a change in organizational form, not an extraction mechanism — no party systematically loses wealth through the transition itself, though some gain efficiency. Suppression is minimal (0.12) because there is no active coercion needed to maintain the constraint once the legal framework is in place; the electronic ledger is self-perpetuating once institutions commit to it. Theater ratio is negligible (0.08) because the shift from physical to electronic is a genuine operational change, not a performative one — ledger entries do real work. Accessibility collapse is very high (0.92) because once electronic money is institutionalized, reverting to purely physical currency becomes effectively impossible without systemic reorganization; no agent can unilaterally exit electronic settlement and remain in modern banking. Resistance is low (0.15) because the efficiency gains align incentives; institutional resistance to the transition was minimal once central bank frameworks solidified. The measurement series tracks the transition period (1950–1985): extractiveness rises sharply from 1960–1970 as institutions adopt electronic systems, plateaus by 1978 (threshold crossed), and remains stable as the new form becomes institutionalized. Theater and suppression remain low throughout because they track the operational shift, not its justification.
 *
 * PERSPECTIVAL GAP:
 *   From the central bank and commercial banking perspective, the constraint is a natural institutional evolution driven by operational efficiency — a mountain of technical fact. From the depositing public's perspective, the constraint is invisible; it simply IS how money works. From historians' perspectives, the constraint is a moving target — which institutional event marks 'emergence' is exactly what different readings dispute. The engine computes these divergences from power + exit positioning. Central banks hold high power and arbitrage exit, so they experience the constraint as enabling; the public holds moderate power and constrained exit, so they experience it as institutional infrastructure they depend on but cannot alter. Theorists hold analytical seats outside the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading ascribes low to negative extraction because emergence is framed as institutional recognition of a technical fact, not as a coercive arrangement. Central banks and commercial banks are beneficiaries (d toward beneficiary end: they gain operational efficiency, reduced costs, enhanced control). The depositing public incurs no direct cost from the constraint itself — they experience money as already electronic, a settled background condition. The low extractiveness reflects the reading's claim: the constraint is a coordinate innovation, not a transfer mechanism. If a different reading (m4_m5_collapse) were operative, extractiveness might be recomputed as higher (if emergence is merely a statistical artifact, the constraint is hiding real distributional effects). This reading's directionality is anchored in the first-institutional-bearer axiom: emergence is the moment when a recognized institution chose to hold money electronically and regulators blessed it as equivalent to physical bearer claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy: the founding problem (scaling transaction velocity) remains live and is solved by the constraint. Electronic money has not outlived its function; it is constitutive of modern monetary architecture. No agent bears the constraint primarily because it is a coordination function, not an extraction mechanism. If the constraint were reclassified as tangled_rope or snare by alternative readings (e.g., if M4/M5 collapse reading established that electronic money is primarily a measurement tool that hides distributional effects), then mandatrophy analysis would need to address whether the regulation that created electronic money equivalence persists despite the function shifting from coordination to opacity. Under this reading, no such shift is evident.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_recognition_threshold,
    'Which specific institutional event marks the threshold where electronic money ''emerged'' under this reading? Is it the Federal Reserve''s founding of the ACH (1974), SWIFT''s operational start (1977), the Bundesbank''s electronic settlement directive (1972), or some other recognized institutional act?',
    'Archival analysis of central bank and regulatory records to identify which institution first codified electronic bearer claims as legal-tender equivalent and the date of that codification.',
    'A precise institutional date would anchor this reading''s empirical claim; absence of a canonical date would suggest the threshold is diffuse across institutions and jurisdictions, supporting the m4_m5_collapse reading''s claim that ''emergence'' is not a discrete event but a measurement artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_recognition_threshold, empirical, 'Whether a discrete institutional threshold can be identified or if emergence is necessarily diffuse.').

omega_variable(
    natural_law_vs_constructed_equivalence,
    'Is the legal equivalence between electronic bearer claims and physical notes a discovered natural fact about money, or a constructed institutional choice? Does electronic money emerge because it IS how money works at scale, or because institutions DECIDED it counts as equivalent?',
    'Counterfactual analysis: if central banks had not recognized electronic bearer status as equivalent to physical notes, would electronic settlement still have emerged? Would institutions have continued to back electronic ledger entries with physical custody?',
    'If natural law: the constraint is a mountain — emergence is inevitable, resistance to it is futile. If constructed choice: the constraint is tangled_rope or snare with beneficiaries (banks gain efficiency/control) and potential victims (if settlement velocity becomes a control mechanism, those excluded from electronic access). This omega directly maps to the false_summit_mountain scenario: the constraint is CLAIMED as mountain but has identifiable beneficiaries, requiring FSM evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_equivalence, conceptual, 'Whether electronic money emergence is discovered or constructed; FSM candidate due to beneficiary presence.').

omega_variable(
    competing_readings_boundary,
    'Can this reading (institutional threshold) and the became_thinkable reading coexist as live positions, or does the institutional threshold axiom logically foreclose the earlier-conceptual-possibility reading?',
    'Conceptual analysis: if emergence is defined as institutional recognition, does that definition rule out dating emergence to an earlier moment when the concept became thinkable? Or are these merely different framings of when the same phenomenon ''counts''?',
    'If forecloses: the readings are in direct logical conflict; the engine should compute constraint_forecloses/2. If coexists_with: both readings remain live as different parties'' perspectives on when money ''became electronic''; the engine should compute coexistence. This determines the reading_relations/1 triple declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_readings_boundary, conceptual, 'Whether institutional-threshold and became-thinkable readings are logically exclusive or can coexist.').

omega_variable(
    measurement_artifact_versus_real_event,
    'Is the emergence of electronic money an independent institutional fact, or is it primarily an artifact of the decision to separate M4 and M5 monetary aggregates? Did money ''become electronic'' or did economists'' measurement practice ''decide'' to treat electronic holdings differently?',
    'Historical analysis of when M4/M5 separation was introduced relative to first institutional electronic bearer claims. If M4/M5 separation postdates institutional electronic settlement by years, it is retroactive classification (supporting m4_m5_collapse reading). If M4/M5 was part of the original institutional framework, it is contemporaneous (supporting this reading).',
    'If emergence precedes measurement: this reading is ontologically primary; electronic money is a real institutional category. If measurement precedes clarity: emergence is partially retroactively constructed; the m4_m5_collapse reading gains support and the constraint''s ε and type classification may need revision toward snare or piton (measurement performing the work of institutional coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_artifact_versus_real_event, empirical, 'Whether electronic money emergence is independent of or dependent on measurement practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1950, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__first_held_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__first_held_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(elec_tr_t1978, electronic_money_emergence__first_held_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(elec_tr_t1985, electronic_money_emergence__first_held_reading, theater_ratio, 1985, 0.08).

% Extraction over time
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__first_held_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__first_held_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(elec_be_t1978, electronic_money_emergence__first_held_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(elec_be_t1985, electronic_money_emergence__first_held_reading, base_extractiveness, 1985, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1950, electronic_money_emergence__first_held_reading, suppression_requirement, 1950, 0.03).
narrative_ontology:measurement(elec_su_t1960, electronic_money_emergence__first_held_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.11).
narrative_ontology:measurement(elec_su_t1978, electronic_money_emergence__first_held_reading, suppression_requirement, 1978, 0.12).
narrative_ontology:measurement(elec_su_t1985, electronic_money_emergence__first_held_reading, suppression_requirement, 1985, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.18).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% The electronic_money_emergence kernel decomposes into three constraint stories, one per reading. Each reading instantiates a different constraint with a different ε, different referent for 'emergence', and different beneficiary/victim structure. This reading (first_held_reading) treats emergence as discrete institutional recognition. The became_thinkable_reading treats emergence as conceptual possibility (earlier, diffuse). The m4_m5_collapse_reading treats emergence as retroactive measurement classification. All three share the same kernel (the contested claim about when electronic money emerged) but have different ε values and different axioms. Links: first_held_reading influences became_thinkable_reading (institutional codification shapes what becomes thinkable), and first_held_reading coexists_with m4_m5_collapse_reading (institutional fact and measurement practice are independent framings of the same phenomenon).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
