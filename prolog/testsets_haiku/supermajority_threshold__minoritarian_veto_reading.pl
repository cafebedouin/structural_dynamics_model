% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto Lock-In
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   A constitutional framework (e.g., the U.S. Constitution) requires
 *   supermajority approval (two-thirds of both chambers plus ratification by
 *   three-fourths of states, or equivalent in other systems) to amend the
 *   foundational text. This reading analyzes the supermajority threshold as a
 *   mechanism that converts the structural position of geographically
 *   dispersed or ideologically cohesive minorities into a permanent veto over
 *   constitutional change. Historical beneficiaries of the original
 *   arrangement (slaveholders at the Founding, Jim Crow majorities
 *   post-Reconstruction, propertied classes resisting redistribution) used
 *   the threshold to lock their privileges into place by blocking amendments
 *   that later majorities demanded. The reading treats the threshold as a
 *   snare: it is defended as a wisdom-safeguard (the sibling
 *   consensus_safeguard reading), but operates as an entrenchment device that
 *   transfers constitutional authority from electoral majorities to blocking
 *   minorities. The measured extraction is high and rising because the
 *   constraint's function has clarified over time: as more majorities
 *   mobilize for reform and are blocked, the extractive function becomes
 *   visible, and the theatrical justification (protection against passion)
 *   wears thin.
 *
 * KEY AGENTS:
 *   - Status quo beneficiaries: holders of entrenched privileges (property, citizenship, federalism distributions) that majorities would restrict
 *   - Blocking minorities: regionally or ideologically concentrated actors positioned to deny supermajority (e.g., small states in ratification, one chamber in bicameral systems)
 *   - Contemporary majoritarian coalitions: electoral majorities seeking constitutional reform blocked by the threshold
 *   - Reform constituencies: organized movements (suffragists, abolitionists, civil rights, labor) that mobilize majorities but cannot translate them to constitutional change
 *   - Constitutional framers: historical authors of the threshold (analyzable as either wisdom-protecting or privilege-locking depending on reading)
 *   - Future constituencies: unborn people whose constitutional status is foreclosed by the inability of current majorities to amend
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.71).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto Lock-In").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '360fc748-cad6-428f-a806-d0e38ef40baa').
narrative_ontology:cs_kernel_codification('360fc748-cad6-428f-a806-d0e38ef40baa', formalized).
narrative_ontology:cs_authority_grounding('360fc748-cad6-428f-a806-d0e38ef40baa', lineage).
narrative_ontology:cs_interpretation_layer_present('360fc748-cad6-428f-a806-d0e38ef40baa').
narrative_ontology:cs_reading_relation('360fc748-cad6-428f-a806-d0e38ef40baa', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('360fc748-cad6-428f-a806-d0e38ef40baa', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('360fc748-cad6-428f-a806-d0e38ef40baa', foundational, supermajority_requirement_entrenchment_mechanism).
narrative_ontology:cs_axiom_status(supermajority_requirement_entrenchment_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('360fc748-cad6-428f-a806-d0e38ef40baa', supermajority_requirement_entrenchment_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('360fc748-cad6-428f-a806-d0e38ef40baa', foundational, majoritarianism_as_legitimacy_ground).
narrative_ontology:cs_axiom_status(majoritarianism_as_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('360fc748-cad6-428f-a806-d0e38ef40baa', majoritarianism_as_legitimacy_ground, deontological).
narrative_ontology:cs_reference_frame('360fc748-cad6-428f-a806-d0e38ef40baa', electoral_majority_rule_legitimacy).
narrative_ontology:cs_drift_state('360fc748-cad6-428f-a806-d0e38ef40baa', contemporary_blocked_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('360fc748-cad6-428f-a806-d0e38ef40baa', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, historical_privilege_holders).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majoritarian_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holders of entrenched constitutional privileges (property rights regimes, citizenship hierarchies, suffrage boundaries, federalist power distributions) that a contemporary majority would restrict or redistribute. The supermajority threshold locks their position in place by making constitutional amendment nearly impossible. They benefit from the immobility of the framework itself.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    powerful, generational, trapped, national).

% Regionally concentrated or ideologically cohesive factions positioned to deny the supermajority (in bicameral systems, minorities in one chamber or enough states to block ratification). They enforce the threshold by refusing consent to any amendment, converting their structural veto power into a governing tool. Their power derives entirely from the threshold's existence.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocking_minorities, agenda_setter,
    organized, biographical, constrained, national).

% Electoral majorities seeking constitutional reform (expanding suffrage, reapportioning districts, removing class-based privileges, clarifying dormant powers). They bear the cost of blocked reform: policy stagnation, unredressed injustice, and the erosion of faith in majoritarian legitimacy. Their exit is exit from the constitutional system itself, which carries civilizational cost. They remain identity-locked to the constitution as the source of their own legitimacy claims.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majoritarian_coalitions, payer,
    powerful, biographical, identity_locked, national).

% Social movements and organizing blocs (suffragists, abolitionists, civil rights advocates, labor organizers) that mobilize electoral majorities for constitutional change. They invest political capital in amendment campaigns only to be indefinitely blocked. Their costs are organizational attrition, delegitimation, and being permanently locked out of constitutional voice. They are identity-locked because their entire project is framed as the realization of constitutional promises.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_constituencies, payer,
    moderate, biographical, identity_locked, national).

% The historical authors of the original framework (founding moment, Reconstruction, prior amendments). This reading treats their intent as contestable: did they author a supermajority threshold to protect against passing passions, or to entrench the privileges of the dominant coalition?
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_framers, observer,
    analytical, civilizational, analytical, national).

% Unborn or disenfranchised people whose constitutional status and rights are foreclosed by the inability of the current majority to amend. They would object to being bound by defunct arrangements but are structurally absent from the amendment process itself.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_constituencies, excluded,
    powerless, civilizational, trapped, national).

% Other constitutional systems with different supermajority thresholds or amendment mechanisms. Their existence as empirical reference points makes the particular threshold choice visible as constructed rather than natural or inevitable.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_democracies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The threshold is framed as a mechanism to prevent constitutional amendments driven by transient electoral swings — it is intended to require deep, stable consensus before the foundational framework changes. But this reading locates the actual coordination problem: how to lock entrenched minorities into veto position despite majoritarian will.
% TRANSFER_FUNCTION: Transfers power to alter the constitutional regime from any majority that can win electoral office to a supermajority (or super-coalition of blocking minorities) that must include geographically or ideologically dispersed actors. The transfer is from contemporary majorities to historical privilege holders and regionally dispersed minorities. The mechanism moves the ability to foreclose reform off the electoral arena and into the constitutional amendment machinery itself.
% ABSENT_VOICES: Future constituencies and people disenfranchised at the time the threshold was authored are excluded from the amendment process itself. The reform constituencies that mobilize majorities are present in electoral politics but barred from translating electoral majorities into constitutional change. Comparative democracies with different thresholds are not in the conversation, so the particular threshold's contingency is invisible.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared overnight (replaced by simple majority, or abolished entirely), the constitutional regime would reorganize rapidly: blocked reforms would be enacted, entrenched privileges would face immediate reapportionment, and the electoral majority would finally translate into constitutional authority. The status quo beneficiaries would lose their veto; blocking minorities would lose their outsized power; reform constituencies would shift from indefinite campaigns to actual legislation.
% FOUNDING_PROBLEM: The founding problem is framed by the consensus_safeguard reading as protection against constitutional amendments driven by temporary passions or narrow interests. This reading contests that framing: the 'founding problem' was how to lock the constitutional arrangement (slavery, property rights, limited suffrage, federalism) against the electoral majorities that would eventually mobilize to undo it.
% FOUNDING_PROBLEM_CORROBORATION: The consensus_safeguard reading attests the original founding problem — volatility, mob passion — remains live. This reading cites historical record: supermajority thresholds were deployed after moments of majoritarian reform (Reconstruction, civil rights mobilization) specifically to prevent further majoritarian amendments. The blocking minorities and status quo beneficiaries attest to protecting against 'radical change'; reform constituencies and historical study of the amendment record (the 27th Amendment is a 202-year exception; the 19th, 24th, 26th were blocked repeatedly before supermajority approval) corroborate that the threshold functions as entrenchment, not consensus-protection.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) is high because the supermajority threshold transfers constitutional authority away from electoral majorities and toward blocking minorities, and this transfer is decoupled from any coordination benefit to those minorities. They do not need protection from passing passion; they need protection from majoritarian will. Suppression (0.71) is substantial because the threshold actively suppresses majoritarian constitutional voice: it converts electoral victories into defeats, and the enforcement machinery (two-thirds votes, ratification canvassing) is designed and maintained specifically to deny power to majorities. Theater (0.42, rising) reflects growing performative justification: as blocking is exposed, defenders appeal more intensely to the wisdom-safeguard narrative, but the blocking function is increasingly transparent. The measurement series shows extractiveness and suppression both rising over the interval: this reflects both the accumulation of blocked reforms (more blocked amendments make the extractive function clearer) and the rising salience of the threshold as reform constituencies mobilize. Accessibility collapse (0.68) is moderate because alternatives do exist (constitutional convention, state nullification, de facto amendment through jurisprudence, civil conflict) but the threshold makes them costly and uncertain. Resistance (0.62) is substantial and rising because reform constituencies have mobilized repeatedly to break through the threshold, and their mobilization itself is a measured form of active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (majoritarian coalitions, reform constituencies) and the beneficiary/agenda-setter seats (status quo beneficiaries, blocking minorities) compute radically different directionalities and types. From the beneficiary seats, the threshold is coordination (stabilizing the constitutional order against destabilizing majorities). From the payer seats, it is pure extraction (locking majorities out of constitutional voice). The engine should register this divergence and classify the same constraint as coordination-like on the beneficiary seats and snare-like on the payer seats — that divergence is the signal that a snare is operating: one party reads it as legitimate protection, the other as illegitimate lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Status quo beneficiaries hold power (powerful, trapped exit, entrenched position in the original constitutional bargain) and benefit directly from the threshold's immobility — d is near 0.0, subsidy side. Blocking minorities are positioned to deny supermajority and benefit from the blocking power the threshold grants them — d is near 0.0, subsidy side (they collect veto authority). Contemporary majoritarian coalitions have powerful electoral position but constrained constitutional exit; the threshold extracts their constitutional voice and locks them out — d is near 1.0, target side. Reform constituencies have moderate power but identity-locked commitment to the constitutional system (they cannot exit it without delegitimizing their own claims to justice within it) — d is near 1.0, target side, with high χ due to identity-lock. The blocking minorities anchor the snare: they have no interest in exiting the constitutional system itself, so the constraint persists indefinitely. Their power is structural, not numerical; the threshold converts their structural position into veto authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The supermajority threshold presents a classic mandatrophy problem: the founding problem (protection against passionate majorities, safeguarding fundamental norms) is treated as permanently live by the consensus_safeguard reading. But the historical record shows the founding problem was solved and reframed once the identity of the majority changed. Early majorities (slaveholders, Jim Crow beneficiaries, propertied classes) supported the threshold as protective. Later majorities (abolitionists, civil rights movements, redistributionists) sought to break through it. The threshold's original function (tempering the early majorities' arbitrary use of power) became obsolete, but the mechanism persisted as a lock-in on later majorities' reform efforts. This is mandatrophy: the constraint's stated purpose outlived its actual necessity, and the mechanism became available for capture by whoever wanted to block majoritarian change. The snare classification captures this structure: the threshold was built for coordination (deep consensus formation) but now operates as extraction (minority veto), and the defense of it relies on conflating the dead founding problem with the live problem of blocking contemporary majorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_identification,
    'Was the supermajority threshold authored to protect against transient majoritarian passion, or to entrench the privileges of the dominant coalition that drafted it?',
    'Historical analysis of founding documents, debates, and subsequent usage: if the threshold was praised by beneficiaries of the original arrangement (slaveholders, Jim Crow majorities) as protection against amendment, and attacked by reform constituencies as entrenchment, the entrenchment reading is supported. If the threshold was invoked equally to defend reform amendments against blocking minorities, the consensus reading is supported.',
    'If the founding problem was entrenchment, the snare classification is confirmed. If the founding problem was genuinely protection against passion, the consensus_safeguard reading''s rope or mountain classification would be more appropriate. The mismatch between founding_problem_status (dead in this reading) and the continuing operation of the threshold is the diagnostic: if the problem is dead but the constraint persists, mandatrophy has occurred and the constraint is a snare, not a safeguard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_identification, empirical, 'Whether the supermajority threshold was designed to protect against passion or to entrench privilege.').

omega_variable(
    blocking_minorities_structural_position,
    'Do blocking minorities benefit from the supermajority threshold, or is their blocking power incidental to a legitimate consensus-formation mechanism?',
    'Comparative analysis: do smaller, regionally concentrated, ideologically cohesive minorities in constitutional systems with lower amendment thresholds report loss of veto power and subsequent constitutional reform? Do larger, geographically dispersed majorities in systems with supermajority thresholds report constitutional stagnation while their electoral majorities persist?',
    'If blocking minorities systematically benefit from supermajority thresholds (gaining veto power they would not have under simple majority rules), the snare classification is confirmed — the threshold is engineered to grant them veto authority. If constitutional reform rates are uncorrelated with amendment thresholds, the consensus reading''s assumption (that thresholds protect against passion, not veto minorities) is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_minorities_structural_position, empirical, 'Whether the supermajority threshold structurally benefits blocking minorities or is incidental to consensus formation.').

omega_variable(
    alternative_consensus_mechanisms,
    'Is a supermajority threshold the only mechanism that can protect against constitutional amendments driven by temporary passion, or could lower thresholds (simple majority, qualified majority) combined with temporal delays, supermajority ratification in only one chamber, or state-level variation achieve the same protective function?',
    'Comparative constitutional design and simulation: do other democracies achieve similar stability with different amendment thresholds? Can the consensus-protection function be disaggregated from the veto-granting function?',
    'If alternative mechanisms can provide consensus protection without supermajority requirements, the supermajority threshold is shown to be unnecessarily restrictive, and the snare classification is strengthened. If no alternative achieves equivalent protection, the consensus reading''s assumption that the supermajority threshold is necessary for wise governance is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_consensus_mechanisms, conceptual, 'Whether supermajority thresholds are the only design capable of consensus formation, or whether the veto-function and consensus-function could be separated.').

omega_variable(
    majoritarianism_vs_constitutional_stability,
    'Is the constraint''s core legitimacy anchored in democratic majoritarianism (one-person-one-vote, electoral majority translates to authority) or in constitutional permanence (fundamental law resists change even when majorities demand it)?',
    'Normative inquiry into foundational commitments: does the constitutional order ground legitimacy in electoral will or in transcendent principles? Different framings produce different readings of the threshold''s legitimacy.',
    'If legitimacy is grounded in majoritarianism, the threshold is exposed as anti-democratic and the snare classification is supported. If legitimacy is grounded in constitutional permanence, the threshold is protective wisdom and the consensus reading is supported. This is a preference-class omega with no empirical resolution, but it clarifies the normative frame on which the readings rest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majoritarianism_vs_constitutional_stability, preference, 'Whether the constitutional order''s legitimacy rests on electoral majority rule or on constitutional permanence.').

omega_variable(
    suppression_internalization,
    'Is the suppression of majoritarian constitutional voice exercised externally (the threshold''s institutional rules block majorities regardless of their willingness to fight) or partially internalized (majorities have internalized the norm that ''the Constitution can''t be changed'' and stopped trying)?',
    'Behavioral analysis: do reform constituencies continue to mobilize for constitutional amendment despite repeated blocking, or have they shifted strategy away from amendment? Post-threshold reform patterns in jurisdictions that lowered thresholds (or abolished them) would show whether suppression persists after the mechanism is removed.',
    'If suppression is structural (external), the measured suppression value (0.71) is accurate and the constraint''s enforcement is dependent on the institutional machinery. If suppression is substantially internalized, the effective suppression is higher than measured, because majorities carry the constraint with them even after institutional exit. The theta parameter for directionality would shift toward higher d for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of majoritarian voice is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(supe_tr_t5, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement_basis(supe_tr_t15, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(supe_tr_t25, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t35, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(supe_tr_t35, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(supe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement_basis(supe_be_t5, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(supe_be_t15, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(supe_be_t25, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t35, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(supe_be_t35, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(supe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(supe_su_t5, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(supe_su_t15, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(supe_su_t25, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t35, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(supe_su_t35, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(supe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel decomposes into three structurally distinct constraint stories: the consensus_safeguard reading (threshold as wisdom-protection, low extraction, coordination), the minoritarian_veto_reading (this story: threshold as entrenchment, high extraction, snare), and the adaptive_gradient reading (threshold as calibration problem). These are not three views of one constraint; they are three constraints grounded in three readings of the same constitutional text. ε-invariance requires separate stories because the readings instantiate different beneficiary/victim structures and different extraction profiles. The consensus reading treats all parties as coordination-beneficiaries (stabilizing the constitution). This reading treats status quo beneficiaries and blocking minorities as extractors and contemporary majorities as victims. These are incompatible structural accounts, not compatible measurements of the same constraint. The three readings coexist across different constitutional factions; none forecloses the others within a single party's framework, but this reading influences both siblings by reframing the threshold's legitimacy contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
