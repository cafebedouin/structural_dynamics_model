% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__number_reading, []).

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
 *   constraint_id: zero_mathematical_status__number_reading
 *   human_readable: Zero as a Full Number with Defined Arithmetic Operations (Number Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates the number_reading of the kernel
 *   zero_mathematical_status: zero is a full number with defined arithmetic
 *   operations, codified by Brahmagupta (628 CE) and later absorbed as
 *   theorem-level structure of the algebraic framework (additive identity,
 *   multiplicative annihilation, closure of subtraction). The standing
 *   arrangement under contest is modern mathematics' full integration of
 *   zero; its epsilon is authored for THAT arrangement as this reading sees
 *   it, not for the arrangement any sibling reading would prefer. The
 *   historical arc runs codification under active ontological defense (t=0),
 *   transmission through Islamic algebra (t=2-4), European algorism adoption
 *   amid guild resistance (t=6), and axiomatic consolidation with calculus
 *   (t=10-12). Claim and metrics are authored independently: the claim is
 *   mountain (the rules are forced within any framework possessing an
 *   additive identity, and isolated traditions converged on identical rules);
 *   the metrics describe near-zero extraction, near-nil suppression,
 *   near-total accessibility collapse, and near-nil resistance. Where the
 *   engine's computed type diverges from the claim, that divergence is data.
 *   Assumptions recorded: interval units are centuries from 628 CE;
 *   provenance commit identifiers are inherited from the governing prompt and
 *   schema files in effect at generation; sampling parameters are stated as
 *   configured for this run.
 *
 * KEY AGENTS:
 *   - indian_astronomer_mathematicians: agenda_setter and beneficiary (organized/mobile) — codified the rules and defended zero's numberhood against ontological objection
 *   - islamic_algebraists: beneficiary (organized/mobile) — transmitted and extended the integrated arithmetic; built algebra on it
 *   - european_algorist_clerks: beneficiary (moderate/constrained) — adopted zero-integrated computation for commerce and universities
 *   - mediterranean_merchant_guilds: payer (organized/constrained) — bore zero-enabled fraud and audit costs; mounted the 1299 Florence ban; built double-entry controls
 *   - scholastic_ontological_resisters: payer (organized/identity_locked) — bore framework-obsolescence costs; held the Parmenidean line until the seat emptied
 *   - mayan_calendar_astronomers: beneficiary (organized/mobile) — independent inventors whose convergence corroborates the arrangement's structural necessity
 *   - modern_mathematical_practitioners: beneficiary (institutional/constrained) — inherit the fully integrated system; exit means leaving mathematics
 *   - historians_of_mathematics: observer (analytical/analytical) — sees the full contest and its resolution from outside the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.02).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.03).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as a Full Number with Defined Arithmetic Operations (Number Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, '2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b').
narrative_ontology:cs_kernel_codification('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', formalized).
narrative_ontology:cs_authority_grounding('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', expertise).
narrative_ontology:cs_interpretation_layer_present('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b').
narrative_ontology:cs_reading_relation('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', zero_mathematical_status__placeholder_reading, forecloses).
narrative_ontology:cs_axiom('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', foundational, zero_is_number_with_defined_operations).
narrative_ontology:cs_axiom_status(zero_is_number_with_defined_operations, holdable).
narrative_ontology:cs_axiom_grounding('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', zero_is_number_with_defined_operations, instrumental).
narrative_ontology:cs_axiom('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', secondary, arithmetic_closure_over_all_quantities).
narrative_ontology:cs_axiom_status(arithmetic_closure_over_all_quantities, holdable).
narrative_ontology:cs_axiom_grounding('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', arithmetic_closure_over_all_quantities, conventional).
narrative_ontology:cs_reference_frame('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', brahmagupta_integrated_arithmetic).
narrative_ontology:cs_drift_state('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', contemporary_axiomatic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2d4b04b9-219b-4bdd-a2d4-a924b67b2e8b', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, indian_astronomer_mathematicians).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, islamic_algebraists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, european_algorist_clerks).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, mayan_calendar_astronomers).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, modern_mathematical_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_mathematical_status__number_reading, mediterranean_merchant_guilds).
narrative_ontology:constraint_victim(zero_mathematical_status__number_reading, scholastic_ontological_resisters).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, brahmagupta_rule_correctness).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, arithmetic_closure_doctrine).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, positional_notation_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codified the rules governing zero's arithmetic in the Brahmasphutasiddhanta (628 CE) for planetary table computation and debt-ledger balancing, stating rules for addition, subtraction, multiplication, and division involving zero and defending zero's standing as a quantity against those who held that 'nothing' cannot be reckoned. Their schools trained successors in the integrated system; reverting to pre-zero methods would have forfeited the computational reach their astronomy depended on, so in practice they did not exit.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, indian_astronomer_mathematicians, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__number_reading, indian_astronomer_mathematicians, beneficiary).

% Adopted and extended the integrated arithmetic through translation houses and original treatises; al-Khwarizmi's algebra presupposes zero as a manipulable number. Gained equation-solving reach unavailable to purely geometric method, and their texts carried the system to Latin Europe. Exit meant abandoning the algebraic method itself — available in principle, rarely taken.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, islamic_algebraists, beneficiary,
    organized, generational, mobile, continental).

% Merchant-house and university computers who took up Hindu-Arabic numerals with zero for ledger work and calculation, gaining order-of-magnitude speed over abacus and Roman-numeral methods. Paid a transitional price in scrutiny: zero-bearing figures could be altered invisibly (0 to 6 or 9, silently appended zeros), drawing guild suspicion on the numerals themselves before controls matured.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, european_algorist_clerks, beneficiary,
    moderate, biographical, constrained, continental).

% Ran trade accounting across the Mediterranean and bore the fraud and audit costs that zero-bearing numerals created: silent digit alteration and forged appended zeros in commercial books. Responded with outright bans (Florence, 1299, outlawing the cipher numerals in account books) and later with control innovations — double-entry bookkeeping — that neutralized the fraud surface. Their objection was to the audit burden, not to zero's arithmetic standing; once controls matured, the objection lapsed.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mediterranean_merchant_guilds, payer,
    organized, biographical, constrained, regional).

% University-trained philosophers and theologians in the Aristotelian-Parmenidean lineage who held that 'nothing' cannot be a quantity and treated zero-as-number as a fiction corrupting the art. Their ontology constituted their intellectual identity; abandoning it meant abandoning the framework within which their learning was coherent. They wrote against algorism for centuries, gradually ceased to train successors in the objection, and the seat emptied by the seventeenth century.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, scholastic_ontological_resisters, payer,
    organized, generational, identity_locked, continental).

% Developed a zero glyph and Long Count arithmetic in isolation from Eurasian mathematics, using zero as a completed quantity in calendrical computation centuries before Brahmagupta's codification. Their independent arrival at the same structural role — a quantity for 'none' with operational rules — is the strongest external testimony on how the arrangement behaves when no tradition transmits it.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, mayan_calendar_astronomers, beneficiary,
    organized, generational, mobile, regional).

% Inherit the fully integrated system: zero as additive identity, annihilator under multiplication, cardinal of the empty set, origin of the number line. Every branch — algebra, analysis, topology, computing — presupposes it. Leaving the arrangement means leaving mathematics altogether; no practitioner exits, and none bears a cost from staying.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, modern_mathematical_practitioners, beneficiary,
    institutional, civilizational, constrained, global).

% Reconstruct the full contest — Greek refusal, Indian integration, Islamic transmission, European algorism disputes, axiomatic consolidation — from manuscripts, ledgers, and university records. Take no position inside the practice; their seat sees the structure whole, including which objections were ontological and which were audit-driven.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__number_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_mathematical_status__number_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single closed arithmetic shared by all computing communities: totals, balances, and empty places receive one representation with total operations, so ledger-keeping, astronomical tables, and later equation-solving run on one system instead of per-community workarounds.
% TRANSFER_FUNCTION: Moves no goods between parties. It moves capability — computational reach — to every adopting seat, and, during the adoption era, moved intellectual authority from geometry-centered and ontological traditions to symbol-centered computation.
% ABSENT_VOICES: No living constituency objects: ultra-finitists reject large numbers, not zero; constructivists accept zero outright. The historical objectors — Parmenidean ontologists and guild auditors — are converted or extinct, and the seat that would speak against zero's numberhood today is empty. The nearest surviving residue is pedagogical unease, which organizes no opposition.
% DISAPPEARANCE_RATIONALE: Algebra, calculus, analysis, positional computation, and digital computing all presuppose zero as a number; overnight removal would break equation-solving, limit notation, and every stored-value ledger. Any repair attempt reintroduces a 'none'-quantity with operational rules — the arrangement rebuilds itself under another name, as the independent Mayan and Chinese cases demonstrate.
% FOUNDING_PROBLEM: Representing 'none' as a quantity: balancing a ledger that reaches nothing, filling empty places in positional tables, and closing subtraction so that a minus a has a value with operational rules rather than a gap.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the declared beneficiary set: Chinese rod-numeral computists evolved empty-place notation independently of the Indian-Islamic-European lineage; the merchant guild bans constitute payer-seat testimony that zero-bearing numerals had real operational force worth policing; and the rules reproduce as theorems of any structure with an additive identity, an attestation seat-independent of any tradition. No attestor disputes that the founding problem was real; the recorded dispute is ontological (whether 'none' may be numbered at all), which is the kernel contest itself rather than testimony against the problem's reality.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__number_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__number_reading, 0.02, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__number_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_mathematical_status__number_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_mathematical_status__number_reading),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_mathematical_status__number_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.02: the arrangement transfers no goods between parties; the residual epsilon reflects extinct transitional costs (guild audit burdens, displaced manual methods) rather than any ongoing transfer. Suppression 0.03: nothing coerces adherence today; the historical defense of zero's numberhood was epistemic advocacy, not enforcement, and the one coercive episode (Florence 1299) targeted forgery risk, failed, and was answered by control innovation rather than escalation. Theater ratio 0.01: activity around zero is entirely functional — computation, proof, teaching; no ritual maintenance exists because nothing needs performing. Accessibility collapse 0.92: once the algebraic framework is grasped, alternatives close completely — every workaround for a zero-free arithmetic reintroduces a 'none'-quantity with operational rules, and isolated traditions (India, Maya, China) converged on the same structure independently. Resistance 0.03: no living constituency contests zero's numberhood; the scholastic objection dissolved for lack of successors. The temporal series share one grid (t = 0,2,4,6,8,10,12) with every tracked metric authored at every point. The suppression_requirement series is deliberately authored as a falling trajectory: this is the story's enforcement-history dynamic — the constraint required sustained championship in 628 CE and none by the axiomatic era, an enforcement decay through victory, the inverse of a snare's enforcement ratchet. No cyclical dynamics are present; the trajectories are monotone.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute differently from the beneficiary seats, and the divergence preserves the historical contest in structural form. From the scholastic resister seat (identity_locked), the integration of zero operated as the destruction of a coherent ontology — a seat that experienced the arrangement as conquest, not subsidy, and whose exit was unthinkable because their framework constituted their intellectual identity. From the merchant guild seat, the arrangement was a fraud surface demanding audit machinery. From the five beneficiary seats, the same structure is pure capability: costs of adoption were transitional and self-extinguishing. The agenda-setter seat carries authorship: its members wrote the rules and the rebuttals. The engine computes per-seat classifications from role, power, and exit; the gap between the resister seat's experienced burden and the story-level epsilon near zero is the measurement this story exists to preserve.
 *
 * DIRECTIONALITY LOGIC:
 *   Five seats declare as beneficiaries and derive directionality toward the subsidized end; the two payer seats derive toward the cost-bearing end. Because the payer seats' costs were transitional and no seat captures gains (see gain_flow: diffuse), story-level effective extraction stays pinned near the base epsilon; global spatial scope scales effective extraction modestly upward, which moves a near-zero quantity imperceptibly. Suppression enters the engine unscaled by design: the raw 0.03 is the whole story. No directionality overrides are authored: the beneficiary/victim-plus-exit derivation produces the right relationships, and the identity_locked exit on the resister seat already situates it nearer the target end than its organized power alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (representing 'none' as a quantity; closing subtraction) is live and permanently serviced by the arrangement. The classification guards two misreadings. First, the falling suppression series must not be read as snare-style enforcement collapse — the constraint's defense decayed because it won on merit, not because enforcement capacity eroded; a snare losing enforcement degrades into exposure, whereas this arrangement became MORE stable as defense became unnecessary. Second, the universal beneficiary structure must not be read as rope-style negotiated coordination: the rules were never bargained among interests; they are forced within the framework all parties now inhabit, which is mountain mechanics that wore rope-like clothing during the adoption era. The FSM-relevant ambiguity (declared beneficiaries on a claimed mountain) is routed to the natural_law_or_constructed_standard omega rather than resolved by tuning either the claim or the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed_standard,
    'Is zero''s number-status a discovered logical necessity (forced by any framework with an additive identity and distributivity), or a constructed standard whose adoption served identifiable communities and whose ''naturalness'' is retrospective?',
    'Comparative analysis of independent inventions (Indian integral zero, Mayan calendrical zero, Chinese empty-place rod arithmetic) against frameworks that rejected it (Greek geometric magnitude theory, Parmenidean ontology): convergence under isolation indicates structural necessity; divergence traceable to local interest indicates construction.',
    'Resolves whether the beneficiary declarations on this mountain trigger a genuine false-summit condition (constructed standard serving identifiable agents) or misfire on a genuine logical limit; determines mountain versus rope-family classification for the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_standard, empirical, 'Whether the constraint is natural law or a constructed standard with beneficiaries.').

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the number_reading of kernel zero_mathematical_status; do the sibling readings (parmenidean_rejection, placeholder_reading) instantiate constraints with structurally different epsilon, beneficiary sets, and types, or is the kernel''s millennium-long contest rhetorical dispute over a single stable arrangement?',
    'Generate the sibling stories as separate files; compare computed types, epsilon, and stakeholder structures across the kernel family.',
    'If the siblings diverge structurally, the historical contest maps to real constraint diversity and the kernel is genuinely indexical; if they converge, the contest was ontological rhetoric over one arrangement and the family collapses to a single classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the kernel''s epsilon and structure.').

omega_variable(
    division_by_zero_boundary,
    'Brahmagupta left n/0 indeterminate and later attempts (Bhaskara''s infinity) failed; is the undefinedness of division by zero a load-bearing logical limit that stabilizes this constraint, or an incompleteness awaiting resolution that would extend the ''defined operations'' scope?',
    'Examine consistent extensions (projective line, Riemann sphere, wheel theory): each admits a total division only by sacrificing field identities, confirming the boundary is forced rather than pending.',
    'Confirms the constraint''s defined-operations scope is maximal-consistent; a total division preserving field laws would enlarge the constraint and alter its accessibility-collapse profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(division_by_zero_boundary, empirical, 'Status of the division-by-zero boundary within the reading.').

omega_variable(
    hidden_cost_seat_search,
    'The beneficiary set is nearly universal and no victims are declared; are there durable cost-bearing seats the structure misses (displaced abacist traditions, oral-computation cultures extinguished by algorism, communities whose practices were erased without offsetting benefit)?',
    'Historical search for communities whose computational practices were permanently displaced by zero-integrated arithmetic and who received no compensating capability; distinguish transitional costs (guild audit burdens, ended by double-entry controls) from permanent uncompensated losses.',
    'Discovering durable uncompensated cost-bearers would support a false-summit reclassification toward the coordination/extraction hybrid; finding only transitional, mitigated costs supports the mountain reading with universal subsidy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_cost_seat_search, empirical, 'Search for missed cost-bearing seats beneath the universal-beneficiary surface.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_number_reading_tr_t0, zero_mathematical_status__number_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(zero_number_reading_tr_t2, zero_mathematical_status__number_reading, theater_ratio, 2, 0.06).
narrative_ontology:measurement(zero_number_reading_tr_t4, zero_mathematical_status__number_reading, theater_ratio, 4, 0.07).
narrative_ontology:measurement(zero_number_reading_tr_t6, zero_mathematical_status__number_reading, theater_ratio, 6, 0.05).
narrative_ontology:measurement(zero_number_reading_tr_t8, zero_mathematical_status__number_reading, theater_ratio, 8, 0.03).
narrative_ontology:measurement(zero_number_reading_tr_t10, zero_mathematical_status__number_reading, theater_ratio, 10, 0.02).
narrative_ontology:measurement(zero_number_reading_tr_t12, zero_mathematical_status__number_reading, theater_ratio, 12, 0.01).

% Extraction over time
narrative_ontology:measurement(zero_number_reading_be_t0, zero_mathematical_status__number_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(zero_number_reading_be_t2, zero_mathematical_status__number_reading, base_extractiveness, 2, 0.04).
narrative_ontology:measurement(zero_number_reading_be_t4, zero_mathematical_status__number_reading, base_extractiveness, 4, 0.04).
narrative_ontology:measurement(zero_number_reading_be_t6, zero_mathematical_status__number_reading, base_extractiveness, 6, 0.03).
narrative_ontology:measurement(zero_number_reading_be_t8, zero_mathematical_status__number_reading, base_extractiveness, 8, 0.03).
narrative_ontology:measurement(zero_number_reading_be_t10, zero_mathematical_status__number_reading, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(zero_number_reading_be_t12, zero_mathematical_status__number_reading, base_extractiveness, 12, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(zero_number_reading_su_t0, zero_mathematical_status__number_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(zero_number_reading_su_t2, zero_mathematical_status__number_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement(zero_number_reading_su_t4, zero_mathematical_status__number_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(zero_number_reading_su_t6, zero_mathematical_status__number_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement(zero_number_reading_su_t8, zero_mathematical_status__number_reading, suppression_requirement, 8, 0.12).
narrative_ontology:measurement(zero_number_reading_su_t10, zero_mathematical_status__number_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(zero_number_reading_su_t12, zero_mathematical_status__number_reading, suppression_requirement, 12, 0.03).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, placeholder_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'zero's mathematical status' decomposes into three structurally distinct constraints per the epsilon-invariance principle: this file (number_reading: full integration with defined operations, epsilon 0.02, universal beneficiaries, no victims), parmenidean_rejection (ontological denial — its own epsilon, stakeholder set, and type), and placeholder_reading (notation-only status — the historical compromise position, structurally distinct from both). The historical sequence ran rejection -> placeholder -> integration, so this reading is downstream of both siblings; each sibling is linked here, and both files should link back. Downstream dependents (positional notation systems, symbolic algebra, calculus foundations) inherit this reading's structure and are noted here rather than separately authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
