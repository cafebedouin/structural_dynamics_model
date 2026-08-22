% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Partial Withdrawal Reading
 *   domain: international_law/diplomatic_interpretation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) is one of international law's
 *   most durable ambiguities. It calls for 'withdrawal of Israeli armed
 *   forces from territories occupied in the recent conflict' — but the
 *   English indefinite article ('from territories') contrasts with the French
 *   definite article ('from the territories'), creating textual grounds for
 *   discretionary rather than mandatory complete withdrawal. This constraint
 *   story instantiates the PARTIAL WITHDRAWAL READING: the occupier and
 *   mediators invoke drafters' intent and the indefinite article to justify
 *   retention of strategically valuable territories under the cover of
 *   'secure and recognized boundaries.' The constraint is a TANGLED ROPE
 *   because it coordinates expectations (phased withdrawal,
 *   confidence-building) while simultaneously enabling extraction (indefinite
 *   retention, mediator leverage). The claim/metric divergence is deliberate:
 *   extraction rises over 59 years as the constraint hardens into de facto
 *   legitimacy for selective retention; theater rises as the negotiation
 *   frame becomes performative. The resistance from claimants remains robust
 *   because they reject the entire premise — the constraint persists not from
 *   consent but from occupier power and UNSC P5 veto.
 *
 * KEY AGENTS:
 *   - occupying_power: holds de facto authority over territories, interprets withdrawal as discretionary, defines 'secure boundaries' unilaterally
 *   - territorial_claimants: demand complete withdrawal under Article 2(4) Charter principle, lack enforcement mechanism for fixed boundary line
 *   - intermediary_mediators: extract value from ongoing phased negotiation, perpetuate indefiniteness as negotiating framework
 *   - drafting_states: encoded ambiguity intentionally; contemporary fidelity to 'drafters' intent' serves partial-withdrawal legitimacy
 *   - ICJ: holds interpretive authority but abstains from definitive reading, preserving ambiguity as live political arena
 *   - international_community: excluded majority favors complete withdrawal but lacks UNSC binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.64).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'a3733b08-ff80-4560-bb9f-76f8fedf84d8').
narrative_ontology:cs_kernel_codification('a3733b08-ff80-4560-bb9f-76f8fedf84d8', fixed_text).
narrative_ontology:cs_authority_grounding('a3733b08-ff80-4560-bb9f-76f8fedf84d8', extraction).
narrative_ontology:cs_interpretation_layer_present('a3733b08-ff80-4560-bb9f-76f8fedf84d8').
narrative_ontology:cs_reading_relation('a3733b08-ff80-4560-bb9f-76f8fedf84d8', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('a3733b08-ff80-4560-bb9f-76f8fedf84d8', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('a3733b08-ff80-4560-bb9f-76f8fedf84d8', foundational, withdrawal_scope_drafters_intent_discretionary).
narrative_ontology:cs_axiom_status(withdrawal_scope_drafters_intent_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('a3733b08-ff80-4560-bb9f-76f8fedf84d8', withdrawal_scope_drafters_intent_discretionary, empirically_contingent).
narrative_ontology:cs_axiom('a3733b08-ff80-4560-bb9f-76f8fedf84d8', foundational, indefinite_article_permits_strategic_retention).
narrative_ontology:cs_axiom_status(indefinite_article_permits_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('a3733b08-ff80-4560-bb9f-76f8fedf84d8', indefinite_article_permits_strategic_retention, conventional).
narrative_ontology:cs_reference_frame('a3733b08-ff80-4560-bb9f-76f8fedf84d8', phased_negotiated_withdrawal_with_occupier_discretion).
narrative_ontology:cs_drift_state('a3733b08-ff80-4560-bb9f-76f8fedf84d8', contemporary_international_law_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a3733b08-ff80-4560-bb9f-76f8fedf84d8', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, intermediary_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls occupied territory and interprets the withdrawal clause as permitting discretionary, phased withdrawal subject to 'secure and recognized boundaries.' Frames retained strategic zones as necessary for border security and settlement populations. The indefinite article in English ('from territories') provides textual cover for selective withdrawal. Maintains de facto control while technically complying with ambiguous language.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary).

% Seek return of all occupied territory under maximal withdrawal reading but lack enforcement mechanism to compel it. The indefinite article ambiguity means no fixed line demarcates 'complete' withdrawal. Their legal claims rest on Charter Article 2(4) but face a discretionary reading that permits the occupier to unilaterally define 'secure boundaries.' Exit from this regime would require military escalation or external enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    moderate, generational, constrained, regional).

% Broker phased withdrawal agreements by accepting the discretionary reading as the negotiating baseline. They extract value (mediation fees, diplomatic leverage, role as deal-keeper) from the indefiniteness itself — the longer withdrawal remains subject to 'confidence-building,' the longer mediation is necessary. Their interest lies in perpetuating the constraint as a framework for ongoing negotiation rather than resolving it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, intermediary_mediators, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, intermediary_mediators, beneficiary).

% The 1967 UK-USA drafting coalition encoded the indefinite article intentionally to permit phased withdrawal without triggering Article 2(4) violations. Contemporary observers invoke 'drafters' intent' as authoritative, though the original intent was to preserve occupier flexibility. This reading's legitimacy partly rests on claimed fidelity to hidden authorial intent.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_states, observer,
    institutional, civilizational, analytical, universal).

% Holds interpretive authority over Resolution 242 but has declined to adjudicate the withdrawal scope definitively, treating it as a political matter. By abstaining, the Court preserves textual ambiguity as a live arena for negotiation rather than foreclosing it judicially. Abstention serves all three readings simultaneously — maximalists can claim the Court has not rejected their reading, partial-withdrawalists cite judicial deference to state practice, and the authority structure itself avoids being tied to one reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, universal).

% Global consensus via UN General Assembly votes favors mandatory full withdrawal (maximalist reading), but UNSC P5 structure prevents enforcement of that consensus. The excluded majority voice is what enables the discretionary reading to persist — those who would object most strongly are structurally outside the binding decision-making apparatus.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_community_consensus, excluded,
    organized, generational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a phased withdrawal framework that permits incremental negotiation without requiring the occupier to admit illegality or commit to complete immediate withdrawal. Coordinates between occupier (seeking retention of strategic zones), claimants (seeking return of territory), and mediators (seeking to reduce escalation risk). The indefiniteness itself enables coordination by allowing each party to maintain their interpretation while negotiating specific withdrawal phases.
% TRANSFER_FUNCTION: Transfers authority over withdrawal timing and territorial scope from claimants (who would demand immediate return under Article 2(4)) to the occupier (who retains discretionary control over what counts as 'withdrawal') and mediators (who control the pace and sequencing of phased withdrawal). The occupier extracts indefinite retention authority; mediators extract mediation leverage; claimants bear the cost of non-finality.
% ABSENT_VOICES: The international community consensus (expressed through UN General Assembly resolutions favoring complete withdrawal) is structurally excluded by UNSC P5 veto structure. Populations in occupied territories would demand immediate return and non-negotiable borders but are transmitted through the territorial claimant state and subject to diplomatic filtering. Third-party states with no veto power but strong positions on territorial integrity are excluded from binding UNSC decision-making.
% DISAPPEARANCE_RATIONALE: If this reading and its textual ambiguity disappeared (replaced by definitive interpretation of Resolution 242 as requiring complete withdrawal, or by new binding agreement with fixed boundaries), the occupier would lose discretionary retention authority and mediators would lose their leverage — the indefiniteness itself is the constraint. The world would rearrange into either permanent settlement with fixed borders or continued escalation without the phased-negotiation frame.
% FOUNDING_PROBLEM: After 1967 war, UNSC needed to call for some form of withdrawal without requiring the occupier to admit illegality or commit to immediate/complete evacuation, and without empowering the General Assembly to impose binding boundaries. The indefinite article and 'secure boundaries' language provided textual ambiguity that permitted phased negotiation as an alternative to either binding settlement or continued military confrontation.
% FOUNDING_PROBLEM_CORROBORATION: The occupying power and mediators attest the founding problem remains live: escalation risk persists, complete immediate withdrawal would destabilize the region, phased negotiation under a 'secure boundaries' framework continues to reduce incidents. Territorial claimants and UN General Assembly majorities attest the problem is dead: 59 years of 'phased withdrawal' has produced permanent retention, not temporary occupation, and the indefinite language has become a mechanism for indefinite extraction rather than a path to settlement. Historian scholarship (Rostow 1994, Mansfield 1994) and UN records confirm the 1967 drafting coalition (US, UK) intentionally encoded ambiguity; contemporary ICJ practice has abstained from definitive reading, preserving the ambiguity as a live political arena.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 reflects moderate but increasing rent: the occupier extracts discretionary control over territory and timeline; mediators extract negotiating authority. It is not extreme (0.8+) because the constraint is conditionally enforced and can be overturned by negotiated settlement or external intervention — claimants retain a non-zero exit option (military escalation, diplomatic pressure, third-party enforcement). Suppression at 0.64 reflects active enforcement: the UNSC P5 structure suppresses the General Assembly majority position; the indefinite article suppresses the claimants' legal certainty; de facto control suppresses return. Theater at 0.42 indicates a substantial performative component — the 'confidence-building' and 'phased withdrawal' language fills time while strategic territory remains retained; but the coordination function is genuinely real for the mediators and partially real for states seeking to avoid repeated escalation. Accessibility collapse at 0.48 indicates that alternatives (complete immediate withdrawal, judicial boundary settlement) remain theoretically available but practically difficult for the claimants to access — the indefinite article creates exactly the kind of textual hold that keeps alternatives collapsed. Resistance at 0.71 reflects sustained claimant push-back and third-state opposition; the constraint's persistence comes from structural veto power, not from acceptance. The measurement trajectory shows extractiveness rising from 0.42 (1967, when phased negotiation seemed temporary) to 0.61 (2015, when indefinite retention hardened as de facto law), then slightly declining to 0.58 (2026, reflecting increased third-party pressure and delegitimation). Theater peaks at 0.46 around 2015 (maximum performance relative to function) before moderating as the performative frame becomes visible even to sympathetic observers. Suppression rises monotonically (0.48 → 0.64) as the constraint becomes institutionalized in UNSC practice and settlement patterns.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power and mediators compute this reading as a Rope (coordination function is real: phased withdrawal reduces escalation risk; confidence-building is genuine negotiating work). The claimants compute it as a Snare (the indefinite article is pure camouflage; 'secure boundaries' is code for strategic retention; mediators extract infinite leverage from their role). The ICJ's abstention sits between: the Court could apply Article 2(4) strictly and compute Snare, but abstains to preserve the constraint as a live arena for political settlement. From the claimants' seat, effective extraction is higher than 0.58 because they have no exit except military escalation; from the mediators' seat it is lower because they consent to the indefiniteness as profitable. The engine derives directionality from beneficiary/victim + exit: claimants are victims (d → 1.0) with constrained exit; occupier and mediators are beneficiaries (d → 0.0) with arbitrage or mobile exits. This structural asymmetry is the root of the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Occupying power: agenda-setter role, holds de facto authority, interprets the text to its advantage, mobile exit (could withdraw but chooses not to). Directionality d ≈ 0.15 (near-beneficiary): the constraint subsidizes this actor's retention of territory. Territorial claimants: payer role, bear the cost of indefinite non-return, constrained exit (can escalate but cannot unilaterally recover territory without external support). Directionality d ≈ 0.85 (near-target): effective extraction is amplified by their trapped position. Mediators: institutional power, moderate mobility (can withdraw mediation but choose not to because the constraint is profitable). Directionality d ≈ 0.35 (beneficiary-leaning): they collect mediation value without bearing territory costs. The divergence between beneficiary and victim seats should be acute: from the occupier's position the indefinite article is a feature of textual fidelity; from the claimant's position it is a feature of exclusion. No override is required; the structural data (beneficiary list, victim list, exit options) produce the correct divergence through normal derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was to enable phased negotiation without forcing the occupier to admit illegal occupation or commit to immediate withdrawal. This problem remains CONTESTED but arguably DEAD for many third parties: the international consensus now favors complete withdrawal and permanent border settlement. However, the intermediary mediators and the occupying power attest it is LIVE — ongoing escalation risk justifies continued phased-negotiation frameworks. The constraint persists not because it solves the founding problem for all parties but because it solves it for the most powerful parties (occupier + mediators) and because UNSC veto structure prevents enforcement of the alternative reading. This is the signature of Tangled Rope with mandatrophy drift: the coordination function is real but atrophying; the extraction function is real and growing. The solution is not to declare the constraint a Piton (it is not purely performative — real phased withdrawals have occurred, real confidence-building has reduced some escalation risks) but to acknowledge that extraction has become the primary function. The theater ratio moderates at 0.42 because both coordination and performance are present, neither dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_article_authorial_intent,
    'Was the indefinite article in the English text (''from territories'') chosen deliberately to permit discretionary withdrawal, or was it a translation artifact from the French ''from the territories'' (definite)?',
    'Historical analysis of 1967 UK-USA drafting process: recovered meeting minutes, draft legislation, translator notes. Direct quotes from drafters about article choice would settle this.',
    'If deliberate, the partial reading''s claim of fidelity to drafters'' intent is validated; if accidental, the claim is weakened and the maximal reading''s reliance on Article 2(4) as the overriding principle is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_article_authorial_intent, empirical, 'Whether the indefinite article choice was intentional discretionary-withdrawal encoding or translation artifact.').

omega_variable(
    secure_boundaries_semantic_range,
    'Does ''secure and recognized boundaries'' semantically permit retention of strategic zones (the partial reading''s interpretation), or does it presuppose boundaries that already exist and are simply recognized (implying the maximal reading''s complete withdrawal as prerequisite)?',
    'Linguistic analysis of the phrase across drafting records and contemporary diplomatic usage; textual comparison with other security agreements and their boundary-retention clauses; expert testimony on the phrase''s ordinary meaning in 1967 diplomatic context.',
    'A narrow reading of ''secure and recognized'' (boundaries already fixed, only recognized, not negotiated) would support the maximal reading; a broad reading (security can justify boundary adjustments) supports the partial reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_semantic_range, conceptual, 'Semantic range and presuppositions of ''secure and recognized boundaries'' language.').

omega_variable(
    reading_precedence_under_veto,
    'When two readings of a UNSC resolution are equally textually plausible but lead to opposite outcomes, does UNSC practice privilege the reading that avoids enforcement (preserving veto-holder discretion) or the reading that enforces Charter principles?',
    'Comparative case analysis of other UNSC resolutions with textual ambiguity (Cyprus, Kashmir, Golan, Korea); pattern recognition on how veto holders have interpreted ambiguous withdrawal language in their favor.',
    'If veto-preservation is the meta-rule, the partial reading is structurally favored by institutional incentives and will persist even if the maximal reading has stronger textual support. If Charter principles override, the partial reading faces long-term delegitimation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_precedence_under_veto, conceptual, 'Institutional meta-rule governing which reading of ambiguous UNSC language prevails when veto holders benefit from ambiguity.').

omega_variable(
    mediator_extraction_sustainability,
    'Do the intermediary mediators genuinely believe they are coordinating a phased settlement, or do they structurally prefer indefinite mediation over settlement (extraction mechanism)?',
    'Interview data with mediators; analysis of settlement proposals rejected or delayed; tracking of mediator resource allocation and career advancement relative to settlement outcomes vs. mediation longevity.',
    'If genuine coordination belief, the constraint''s theater ratio is lower and the Rope classification is more defensible. If extraction preference, theater ratio is higher and the constraint approaches Piton status (performance over function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mediator_extraction_sustainability, empirical, 'Whether mediators'' role perpetuates indefiniteness as a career/resource interest or as a functional necessity.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the partial and maximal readings foreclose each other (logically incompatible in any single framework), or do they merely coexist (held by different parties without logical contradiction)?',
    'Formal analysis: Can a single juridical framework accept both ''withdrawal is mandatory from all territories'' (maximal) and ''withdrawal scope is discretionary'' (partial) simultaneously without contradiction? Test against: (a) Article 2(4) Charter language, (b) 1967 drafting intent, (c) contemporary customary law.',
    'If foreclosing, one reading will eventually displace the other through political or judicial process. If coexisting, the ambiguity will persist indefinitely and the constraint will remain Tangled Rope. If coexisting, the partial reading benefits from temporal persistence; if foreclosing, it faces future delegitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between partial and maximal readings: do they foreclose or coexist?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement_basis(unsc_tr_t1967, observed).
narrative_ontology:measurement(unsc_tr_t1982, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1982, 0.35).
narrative_ontology:measurement_basis(unsc_tr_t1982, observed).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement_basis(unsc_tr_t1993, observed).
narrative_ontology:measurement(unsc_tr_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2005, 0.43).
narrative_ontology:measurement_basis(unsc_tr_t2005, observed).
narrative_ontology:measurement(unsc_tr_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(unsc_tr_t2015, observed).
narrative_ontology:measurement(unsc_tr_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(unsc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement_basis(unsc_be_t1967, observed).
narrative_ontology:measurement(unsc_be_t1982, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1982, 0.51).
narrative_ontology:measurement_basis(unsc_be_t1982, observed).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.56).
narrative_ontology:measurement_basis(unsc_be_t1993, observed).
narrative_ontology:measurement(unsc_be_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement_basis(unsc_be_t2005, observed).
narrative_ontology:measurement(unsc_be_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(unsc_be_t2015, observed).
narrative_ontology:measurement(unsc_be_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(unsc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.48).
narrative_ontology:measurement_basis(unsc_su_t1967, observed).
narrative_ontology:measurement(unsc_su_t1982, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1982, 0.56).
narrative_ontology:measurement_basis(unsc_su_t1982, observed).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement_basis(unsc_su_t1993, observed).
narrative_ontology:measurement(unsc_su_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2005, 0.67).
narrative_ontology:measurement_basis(unsc_su_t2005, observed).
narrative_ontology:measurement(unsc_su_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(unsc_su_t2015, observed).
narrative_ontology:measurement(unsc_su_t2026, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2026, 0.64).
narrative_ontology:measurement_basis(unsc_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2026
narrative_ontology:measurement(unsc_grid_01, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(class), 1967, 0.48).
narrative_ontology:measurement(unsc_grid_02, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(class), 2026, 0.52).
narrative_ontology:measurement(unsc_grid_03, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(individual), 1967, 0.55).
narrative_ontology:measurement(unsc_grid_04, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(individual), 2026, 0.58).
narrative_ontology:measurement(unsc_grid_05, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(organizational), 1967, 0.4).
narrative_ontology:measurement(unsc_grid_06, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(organizational), 2026, 0.51).
narrative_ontology:measurement(unsc_grid_07, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(structural), 1967, 0.35).
narrative_ontology:measurement(unsc_grid_08, unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse(structural), 2026, 0.42).
narrative_ontology:measurement(unsc_grid_09, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(class), 1967, 0.72).
narrative_ontology:measurement(unsc_grid_10, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(class), 2026, 0.68).
narrative_ontology:measurement(unsc_grid_11, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(individual), 1967, 0.65).
narrative_ontology:measurement(unsc_grid_12, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(individual), 2026, 0.58).
narrative_ontology:measurement(unsc_grid_13, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(organizational), 1967, 0.75).
narrative_ontology:measurement(unsc_grid_14, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(organizational), 2026, 0.71).
narrative_ontology:measurement(unsc_grid_15, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(structural), 1967, 0.68).
narrative_ontology:measurement(unsc_grid_16, unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance(structural), 2026, 0.64).
narrative_ontology:measurement(unsc_grid_17, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(class), 1967, 0.52).
narrative_ontology:measurement(unsc_grid_18, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(class), 2026, 0.61).
narrative_ontology:measurement(unsc_grid_19, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(individual), 1967, 0.58).
narrative_ontology:measurement(unsc_grid_20, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(individual), 2026, 0.67).
narrative_ontology:measurement(unsc_grid_21, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(organizational), 1967, 0.45).
narrative_ontology:measurement(unsc_grid_22, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(organizational), 2026, 0.58).
narrative_ontology:measurement(unsc_grid_23, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(structural), 1967, 0.38).
narrative_ontology:measurement(unsc_grid_24, unsc_242_withdrawal_clause__partial_withdrawal_reading, stakes_inflation(structural), 2026, 0.44).
narrative_ontology:measurement(unsc_grid_25, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(class), 1967, 0.58).
narrative_ontology:measurement(unsc_grid_26, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(class), 2026, 0.68).
narrative_ontology:measurement(unsc_grid_27, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(individual), 1967, 0.62).
narrative_ontology:measurement(unsc_grid_28, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(individual), 2026, 0.72).
narrative_ontology:measurement(unsc_grid_29, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(organizational), 1967, 0.51).
narrative_ontology:measurement(unsc_grid_30, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(organizational), 2026, 0.64).
narrative_ontology:measurement(unsc_grid_31, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(structural), 1967, 0.42).
narrative_ontology:measurement(unsc_grid_32, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression(structural), 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.18).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause kernel has been decomposed into three constraint stories per ε-invariance: this reading (partial withdrawal, ε=0.58, Tangled Rope), the maximal reading (complete withdrawal, ε=low, Mountain or Rope), and the authority structure reading (interpretive power, ε=unknown, contested). Each reading has a distinct beneficiary set, victim set, and ε-value. This reading influences the authority structure reading by requiring some interpretive methodology to adjudicate between partial and maximal — the authority choice determines which reading's ε becomes operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
