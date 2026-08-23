% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity from Formal Enactment (Positivist Reading)
 *   domain: legal/political/philosophical
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution holds that constitutional
 *   validity derives exclusively from formal enactment procedures (Article V
 *   amendment, original ratification) and the institutional authority of the
 *   enacting bodies, not from external moral principles. This reading
 *   constrains judges to the enacted text and formal amendment history,
 *   excluding moral reasoning from validity determination. It coordinates
 *   legal practice around a clear, public criterion but extracts from
 *   substantive justice claims that lack formal textual anchors. When the
 *   amendment process gridlocks (as it has since the 1970s), the constraint
 *   collapses toward originalism in practice — judges revert to historical
 *   meaning as the only available interpretive anchor, because the positivist
 *   framework provides no other valid path for constitutional development.
 *
 * KEY AGENTS:
 *   - judges_constrained_by_text: Primary target (institutional/constrained) — bears extraction of moral reasoning from validity determination
 *   - legal_establishment: Primary beneficiary (institutional/arbitrage) — collects professional legitimacy and predictive stability
 *   - formal_amendment_institutions: Agenda setter (institutional/arbitrage) — controls the sole valid path for constitutional change
 *   - citizens_seeking_substantive_justice: Victim (moderate/constrained) — moral claims excluded unless ratified through Article V
 *   - marginalized_rights_claimants: Victim (powerless/trapped) — disproportionately excluded by amendment supermajority thresholds
 *   - originalist_judges: Excluded (institutional/analytical) — competing reading structurally incompatible with positivist validity criterion
 *   - living_constitutionalist_scholars: Excluded (organized/analytical) — moral-evolution reading excluded by core axiom
 *   - legal_theory_observers: Observer (analytical/analytical) — sees full structure from outside practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity from Formal Enactment (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "legal/political/philosophical").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '0b52352a-592f-4121-961f-787b2a85b804').
narrative_ontology:cs_kernel_codification('0b52352a-592f-4121-961f-787b2a85b804', formalized).
narrative_ontology:cs_authority_grounding('0b52352a-592f-4121-961f-787b2a85b804', lineage).
narrative_ontology:cs_interpretation_layer_present('0b52352a-592f-4121-961f-787b2a85b804').
narrative_ontology:cs_reading_relation('0b52352a-592f-4121-961f-787b2a85b804', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b52352a-592f-4121-961f-787b2a85b804', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('0b52352a-592f-4121-961f-787b2a85b804', foundational, legal_validity_from_enactment_only).
narrative_ontology:cs_axiom_status(legal_validity_from_enactment_only, holdable).
narrative_ontology:cs_axiom_grounding('0b52352a-592f-4121-961f-787b2a85b804', legal_validity_from_enactment_only, conventional).
narrative_ontology:cs_axiom('0b52352a-592f-4121-961f-787b2a85b804', foundational, moral_reasoning_excluded_from_validity_determination).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity_determination, holdable).
narrative_ontology:cs_axiom_grounding('0b52352a-592f-4121-961f-787b2a85b804', moral_reasoning_excluded_from_validity_determination, conventional).
narrative_ontology:cs_reference_frame('0b52352a-592f-4121-961f-787b2a85b804', formal_enactment_framework).
narrative_ontology:cs_drift_state('0b52352a-592f-4121-961f-787b2a85b804', contemporary_judicial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b52352a-592f-4121-961f-787b2a85b804', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, legal_establishment).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, formal_amendment_institutions).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, citizens_seeking_substantive_justice).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, marginalized_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, judges_constrained_by_text).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, legal_positivism_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, procedural_legitimacy_thesis).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_law_and_morality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound to decide cases by reference to enacted text and formal amendment history; moral reasoning excluded from validity determination. Career advancement depends on adherence to positivist methodology. Exit requires leaving the bench or shifting to a different interpretive framework, which carries professional costs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, judges_constrained_by_text, payer,
    institutional, biographical, constrained, national).

% Collects professional legitimacy, institutional authority, and predictive stability from a legal system grounded in formal enactment. Law schools, bar associations, and judicial appointments systems are structured around positivist credentials. Can influence the constraint through academic discourse and appointment politics.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, legal_establishment, agenda_setter).

% Congress and state legislatures control the formal amendment process (Article V). Their authority is the ultimate source of constitutional validity under this reading. They benefit from the constraint's insistence that all valid change flow through their procedures, but face supermajority thresholds that make amendment practically difficult.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, formal_amendment_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the cost when moral claims (equality, dignity, autonomy) lack formal textual support in the enacted Constitution. Their claims are excluded from validity determination unless ratified through the amendment process. Exit options: political mobilization for amendment (high threshold), litigation under existing text (limited), or extra-legal resistance.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, citizens_seeking_substantive_justice, payer,
    moderate, biographical, constrained, national).

% Disproportionately affected when substantive justice claims (voting rights, reproductive autonomy, due process protections) find no anchor in the 1787/1791/1868 text. The amendment process has historically failed them (e.g., ERA, child labor amendment). No viable exit from the constraint's exclusionary force.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, marginalized_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Hold a competing reading (original public meaning at ratification) that is structurally excluded from the positivist framework's validity criteria. They would object to the positivist claim that amendment-process output alone validates, insisting the original meaning constrains even valid amendments. Their exclusion is theoretical — they occupy judicial seats but their interpretive premise is not recognized as valid within this reading.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, originalist_judges, excluded,
    institutional, biographical, analytical, national).

% Advocate for a reading where constitutional principles evolve with social attitudes. Excluded from the positivist framework because their premise (moral reasoning as validity-condition) contradicts the core positivist axiom. They operate in academia and advocacy, not in the formal validity-determination machinery.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_scholars, excluded,
    organized, biographical, analytical, national).

% Analyze the constraint from outside the practice — philosophers of law, comparative constitutionalists, political scientists. They see the full structure: the coordination function (stable legal order), the extraction (moral claims excluded), and the collapse dynamic (gridlock drives reversion to originalism). No stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_theory_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, publicly accessible criterion for constitutional validity (formal enactment) that coordinates legal practice across time and personnel, preventing legitimacy contests from devolving into pure power struggles.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy-rent from moral-reasoning agents (citizens, scholars, judges doing moral reasoning) to formal-enactment agents (legislatures, text-bound judges, legal establishment). The transfer is the exclusion of moral claims from validity-determination.
% ABSENT_VOICES: Substantive justice claimants who cannot access the amendment process (marginalized groups, future generations, non-citizens affected by US constitutional law). They are structurally excluded because the constraint defines validity exclusively through a process they cannot trigger.
% DISAPPEARANCE_RATIONALE: If the positivist validity criterion vanished overnight, judicial decision-making would immediately open to moral reasoning, natural law arguments, and evolving standards — the entire structure of constitutional litigation, judicial appointments, and legal education would reorganize around competing validity criteria. The legal establishment's authority base would fracture.
% FOUNDING_PROBLEM: Post-revolutionary need for a stable, legitimate legal order that could bind judges and officials without relying on contested moral philosophy or royal prerogative. The founding problem was: how to make law authoritative in a republic without a sovereign's command.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists (Hart, Raz) attest the problem is live: modern legal systems still need formal validity criteria to avoid rule-of-recognition regress. Originalists and living constitutionalists attest the problem is dead or transformed: the founding concern was limiting judicial discretion, but positivism's formal criterion now enables discretion by making amendment-gridlock a dead end for substantive claims. Historical records of the Founding era (Farrand, Elliot) show contested understandings — no single founding consensus on 'validity from enactment alone.'
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the constraint's exclusion of moral reasoning from validity — a substantial transfer of interpretive authority from claimants to formal-enactment institutions. Suppression (0.72) is high because the constraint actively excludes competing validity criteria (originalism, living constitutionalism) from the official validity-determination machinery; this exclusion is maintained by judicial appointments, legal education, and professional gatekeeping. Theater ratio (0.28) is moderate-low: the formal enactment criterion is genuinely operative (not mere performance), but a growing share of judicial practice (unenumerated rights, substantive due process) operates in tension with the positivist axiom, creating performative adherence. Accessibility collapse (0.78) is high because once the positivist validity criterion is accepted, moral arguments are structurally excluded — they cannot be 'valid' within the system. Resistance (0.52) is moderate: sustained academic and judicial resistance from originalist and living constitutionalist camps, but the constraint holds the field in official doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The judge seat (payer, institutional/constrained) experiences the constraint as a binding interpretive discipline that excludes their moral judgment. The legal establishment seat (beneficiary/agenda_setter, institutional/arbitrage) experiences it as a legitimacy-generating coordination device. The marginalized claimant seat (payer, powerless/trapped) experiences it as a barrier that makes their justice claims legally invisible. The engine computes these divergences from the structural data — the positivist reading's claimed coordination function is real for the establishment, but its extraction falls on those without amendment-process access.
 *
 * DIRECTIONALITY LOGIC:
 *   Judges are structural payers (d ~ 0.75): constrained exit (career-dependent on adherence), institutional power but bound by the constraint. Legal establishment and amendment institutions are beneficiaries/agenda_setters (d ~ 0.15): they collect legitimacy and control the valid-change pathway, with arbitrage-grade exit (can shape the constraint from within). Citizens and marginalized claimants are payers (d ~ 0.85 and 0.95): constrained or trapped exit, moderate to powerless power. Excluded seats (originalist, living constitutionalist) are not subject to the constraint's extraction — they operate outside its validity machinery — but their exclusion is the suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve the post-revolutionary legitimacy problem (authoritative law without a sovereign). That problem is contested: positivists say it persists (legal systems still need formal validity criteria); critics say it's transformed (the constraint now blocks substantive justice claims the founding generation didn't anticipate). The mandatrophy is unresolved — the constraint persists with its founding justification contested, while its extraction profile has grown (amendment gridlock since 1971 means no valid path for new claims). This is not pure coordination (rope) because extraction is asymmetric and active enforcement excludes alternatives; not pure extraction (snare) because the coordination function (stable validity criterion) is genuine and beneficiaries include the legal system as a whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivist_reading_kernel_relation,
    'Is the positivist reading a distinct constraint from its siblings, or a limiting case of one of them?',
    'Compare the validity criteria: if ''formal enactment only'' produces a different set of valid norms than ''original public meaning'' or ''evolving principles'' across the same cases, the readings are distinct constraints. Empirical test: catalog cases where the three readings yield different validity outcomes.',
    'If distinct, each reading gets its own ε and classification. If a limiting case, the kernel has one constraint with parameterized ε. The current decomposition (three constraint stories linked by network.affects_constraints) assumes distinctness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_kernel_relation, conceptual, 'Whether the three declared readings of us_constitution_meaning are structurally distinct constraints or parameter variants of one constraint.').

omega_variable(
    amendment_gridlock_extraction_acceleration,
    'Does amendment-process gridlock (no successful amendments since 1992, none substantive since 1971) accelerate the constraint''s extraction by closing the only valid exit for substantive claims?',
    'Track base_extractiveness trajectory against amendment-success rate. If extraction rises as amendment frequency falls, the gridlock is an extraction accelerator.',
    'If confirmed, the constraint''s extraction is not static but dynamically amplified by institutional failure — a feedback loop where the constraint''s own supermajority design increases its extractiveness over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_gridlock_extraction_acceleration, empirical, 'Whether Article V gridlock functions as an extraction amplifier for the positivist validity constraint.').

omega_variable(
    collapse_into_originalism_mechanism,
    'When the positivist reading ''collapses into originalism in practice,'' is this a genuine type transition (tangled_rope → tangled_rope with different coordination function) or a rhetorical shift masking continued positivist validity claims?',
    'Analyze judicial opinions during gridlock eras (post-1971): do judges cite original meaning as a validity criterion (originalism) or as the only available evidence of enactment meaning (positivism with originalist evidence)?',
    'If genuine transition, the constraint story needs a temporal phase split. If rhetorical, the single story with rising extraction captures the dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_into_originalism_mechanism, conceptual, 'Whether the gridlock-driven reversion to originalist method represents a constraint-type change or a continuation of the same constraint under stress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t1789, us_constitution_meaning__positivist_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t1868, us_constitution_meaning__positivist_reading, theater_ratio, 1868, 0.18).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t1937, us_constitution_meaning__positivist_reading, theater_ratio, 1937, 0.22).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t1973, us_constitution_meaning__positivist_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t2000, us_constitution_meaning__positivist_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_tr_t2025, us_constitution_meaning__positivist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t1789, us_constitution_meaning__positivist_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t1868, us_constitution_meaning__positivist_reading, base_extractiveness, 1868, 0.42).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t1937, us_constitution_meaning__positivist_reading, base_extractiveness, 1937, 0.48).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t1973, us_constitution_meaning__positivist_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t2000, us_constitution_meaning__positivist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_be_t2025, us_constitution_meaning__positivist_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t1789, us_constitution_meaning__positivist_reading, suppression_requirement, 1789, 0.45).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t1868, us_constitution_meaning__positivist_reading, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t1937, us_constitution_meaning__positivist_reading, suppression_requirement, 1937, 0.62).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t1973, us_constitution_meaning__positivist_reading, suppression_requirement, 1973, 0.68).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t2000, us_constitution_meaning__positivist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_constitution_meaning__positivist_reading_su_t2025, us_constitution_meaning__positivist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint family (us_constitution_meaning) decomposes the colloquial 'constitutional interpretation' into three structurally distinct validity criteria. The positivist reading (this story) has ε=0.58 with formal enactment as sole validity source. The originalist reading has ε≈0.45 with historical meaning as constraint. The living constitutionalist reading has ε≈0.65 with evolving principles as validity-condition. They are linked because each is cited as a response to the others' failure modes: positivism's gridlock problem, originalism's dead-hand problem, living constitutionalism's legitimacy problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_meaning__positivist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
