% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the parliamentary sovereignty reading of a
 *   contested kernel: who holds final interpretive authority over the basic
 *   law. Under this reading, the elected legislature — not the constitutional
 *   court — has the structurally final word on contested constitutional
 *   meaning, exercised through override legislation or amendment of
 *   interpretive provisions, justified by the argument that democratic
 *   accountability through periodic elections is a superior legitimating
 *   basis for interpretive finality than judicial expertise or independence.
 *   This is one of three sibling readings of the same kernel
 *   (judicial_supremacy_reading, popular_constitutionalism_reading); each is
 *   authored as its own constraint with its own ε, beneficiary/victim
 *   structure, and classification, per the ε-invariance principle. The
 *   genuine coordination function (resolving interpretive disputes without
 *   indefinite institutional deadlock, and grounding interpretive legitimacy
 *   in electoral accountability) coexists with asymmetric extraction:
 *   legislative majorities capture durable policy wins at the expense of
 *   constitutional minorities and future legislative minorities whose
 *   protections become majority-contingent.
 *
 * KEY AGENTS:
 *   - sitting_legislative_majority: agenda_setter/beneficiary (institutional/arbitrage) — sets override policy, captures durable wins
 *   - constitutional_court: payer (institutional/trapped) — rulings rendered provisional by override threat
 *   - constitutional_minorities: payer (powerless/trapped) — rights protections become majority-contingent
 *   - electoral_voters: beneficiary (organized/constrained) — genuine accountability good
 *   - future_legislative_minorities: payer (moderate/constrained) — inherit normalized override precedent
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — cross-national comparative view
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '7afe0820-1529-41ca-9ffd-130af3bdbadc').
narrative_ontology:cs_kernel_codification('7afe0820-1529-41ca-9ffd-130af3bdbadc', formalized).
narrative_ontology:cs_authority_grounding('7afe0820-1529-41ca-9ffd-130af3bdbadc', practice).
narrative_ontology:cs_interpretation_layer_present('7afe0820-1529-41ca-9ffd-130af3bdbadc').
narrative_ontology:cs_reading_relation('7afe0820-1529-41ca-9ffd-130af3bdbadc', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7afe0820-1529-41ca-9ffd-130af3bdbadc', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('7afe0820-1529-41ca-9ffd-130af3bdbadc', foundational, electoral_accountability_grounds_interpretive_legitimacy).
narrative_ontology:cs_axiom_status(electoral_accountability_grounds_interpretive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7afe0820-1529-41ca-9ffd-130af3bdbadc', electoral_accountability_grounds_interpretive_legitimacy, conventional).
narrative_ontology:cs_axiom('7afe0820-1529-41ca-9ffd-130af3bdbadc', secondary, unelected_judicial_finality_is_democratically_illegitimate).
narrative_ontology:cs_axiom_status(unelected_judicial_finality_is_democratically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('7afe0820-1529-41ca-9ffd-130af3bdbadc', unelected_judicial_finality_is_democratically_illegitimate, deontological).
narrative_ontology:cs_reference_frame('7afe0820-1529-41ca-9ffd-130af3bdbadc', electoral_mandate_supremacy).
narrative_ontology:cs_drift_state('7afe0820-1529-41ca-9ffd-130af3bdbadc', contemporary_override_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7afe0820-1529-41ca-9ffd-130af3bdbadc', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, sitting_legislative_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electoral_coalition_incumbents).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence_function).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, future_legislative_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electoral_voters).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to pass override legislation reversing judicial constitutional rulings, or to amend the basic law's interpretive provisions outright. Justifies this authority by appeal to the electoral mandate: the legislature answers to voters in a way courts do not, so its reading of contested constitutional questions should prevail when it conflicts with judicial interpretation. Captures durable policy wins that would otherwise be foreclosed by judicial review, and captures the institutional prestige of being the final word.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, sitting_legislative_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, sitting_legislative_majority, beneficiary).

% Issues rulings on constitutional questions that the legislature can override through ordinary or supermajority legislative process, or that political actors can route around by amending interpretive provisions. Cannot exit its role as final arbiter is contested, and cannot appeal to any higher authority when overridden; its rulings become provisional pending legislative reaction rather than terminal.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_court, payer,
    institutional, generational, trapped, national).

% Groups whose rights claims were vindicated by judicial interpretation face those protections becoming contingent on legislative majority preference rather than fixed. Where a legislative majority has electoral incentive to override a rights-protective ruling, this reading gives it the structural means to do so. Cannot exit the jurisdiction easily and has no institutional mechanism to insulate the ruling once the majority acts.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% Get a system in which the body that answers to them at the ballot box has final say over contested constitutional meaning, rather than an unelected judiciary. This is coordination-genuine: majoritarian accountability for interpretive outcomes is a real democratic good. Exit is limited to future elections, which is slow relative to the pace of legislative override action.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electoral_voters, beneficiary,
    organized, biographical, constrained, national).

% Political minorities who will someday hold legislative power inherit whatever precedent the current majority sets for override practice. If override becomes normalized as a routine tool rather than an exceptional one, minorities lose the judicial backstop they would otherwise rely on when out of power. They cannot bind the current majority's practice for their own future benefit.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, future_legislative_minorities, payer,
    moderate, generational, constrained, national).

% Study how override mechanisms and interpretive-authority allocations function across democracies, comparing gridlock rates, rights-erosion incidents, and legitimacy costs under parliamentary sovereignty regimes versus judicial supremacy regimes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves persistent disagreement about who has final say on contested constitutional meaning by locating that authority in the institution most directly and frequently accountable to the electorate, avoiding indefinite deadlock between unelected courts and elected representatives.
% TRANSFER_FUNCTION: Moves final interpretive authority from the judiciary to the legislative majority; in practice this transfers the durability of rights protections from constitutional-court-level fixity toward legislative-majority-level contingency, and transfers dispute-resolution costs onto whichever minority the current majority's override targets.
% ABSENT_VOICES: Constitutional minorities whose protections depend on judicial rulings surviving legislative override are not represented in the legislative chamber that can override them — that is structurally why judicial review existed for their claims in the first place. Future legislative minorities who will someday need the judicial backstop are also not present to object to precedents being set against their future interest.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over interpretation were abolished overnight in favor of judicial finality, legislative majorities would lose the override tool entirely; judicial rulings would become terminal, policy areas currently contested through override legislation would freeze at whatever the courts last decided, and legislative majorities would need supermajority constitutional amendment rather than ordinary override to reverse judicial interpretation — a substantial reallocation of institutional power.
% FOUNDING_PROBLEM: Historical suspicion of unelected judges substituting their own policy preferences for democratically legitimated choices, combined with the problem of an unaccountable body having the final word on how a democracy governs itself.
% FOUNDING_PROBLEM_CORROBORATION: Legislative majorities and their supporting electoral coalitions attest the problem remains live, citing ongoing instances of judicial rulings that override popular legislative outcomes. Independent comparative constitutional scholarship (outside both the legislative and judicial benefiting parties) documents that override mechanisms are used disproportionately against rights-protective rulings affecting politically weak minorities rather than against genuine judicial overreach, suggesting the founding problem as originally stated is only partially live and the mechanism has acquired uses beyond its founding justification.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that override authority is used both for its genuine democratic-accountability function and, per comparative evidence, disproportionately against rights-protective rulings affecting powerless minorities — a real but partial extraction. Suppression (0.58) is moderate-high: minorities affected by override have essentially no exit and no higher institutional appeal once the legislature acts, but the mechanism does not (unlike a snare) suppress the legislature's own accountability to voters — the suppression runs downstream onto minorities, not upward onto the constraint's operation. Theater ratio (0.28) is moderate-low: override is a real, consequential legislative act, not mostly performative, though its invocation increasingly carries rhetorical 'democratic mandate' framing beyond the cases where genuine deadlock exists. Accessibility collapse (0.45) is moderate: alternative institutional designs (judicial finality, hybrid dialogue models) remain visibly live in comparative practice, so alternatives have not collapsed the way they would under a mountain. Resistance (0.6) is substantial: judicial actors, rights advocacy groups, and future-minority coalitions actively contest override practice through litigation, public argument, and constitutional convention norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The sitting legislative majority is the clear structural beneficiary: it sets the override mechanism's terms and captures both policy durability and institutional prestige — d sits near the beneficiary end. The constitutional court is a structural target: its rulings are rendered provisional by a mechanism it cannot resist or appeal past — d sits near the target end despite its institutional power, because power here does not translate into exit or resistance capacity against the override tool. Constitutional minorities are the clearest victims: powerless, trapped, and bearing the concrete cost when override reverses a rights-protective ruling — d is pushed to the target extreme. Electoral voters sit closer to symmetric-to-beneficiary: they get a real accountability good, tempered by the fact that override decisions are made between elections, limiting their real-time control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unaccountable judges substituting policy preference for democratic choice — remains genuinely live in some fraction of cases, which is why this is authored as tangled_rope rather than snare: there is a real coordination function (electoral accountability for ultimate interpretive authority) that a pure-extraction reading would miss. But comparative evidence that override is disproportionately deployed against rights-protective rulings for powerless minorities, rather than against genuine judicial policy overreach, indicates the mechanism has drifted from its founding justification toward routine majoritarian tool use. Classifying this as tangled_rope rather than mountain or rope captures both facts at once: real coordination value plus documented asymmetric extraction, both riding the same override mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings of the basic_law_interpretive_authority kernel (judicial_supremacy, parliamentary_sovereignty, popular_constitutionalism) is institutionally operative in a given constitutional system, and is the selection itself a contested political question or a settled constitutional design choice?',
    'Examine the constitutional text''s own provisions for override mechanisms, amendment thresholds, and judicial review scope; where these are themselves silent or contested, the reading-selection question is itself unresolved and may cycle between readings across political eras.',
    'If a system''s own founding document or constitutional convention has genuinely settled on parliamentary sovereignty (e.g., through an express override clause), this reading''s classification is stable; if the reading is itself perpetually contested (as in systems with implicit or evolving conventions), all three sibling readings remain simultaneously live and no single classification captures the system''s actual operation at any moment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the parliamentary sovereignty reading is the settled operative reading or one of several perpetually contested candidates.').

omega_variable(
    override_frequency_as_extraction_signal,
    'Does the empirical frequency and target-selection pattern of legislative override (concentrated on rights-protective rulings for powerless minorities vs. genuine judicial overreach) provide a reliable signal for distinguishing this reading''s coordination function from its extractive drift?',
    'Comparative dataset of override invocations across parliamentary sovereignty jurisdictions, coded by whether the overridden ruling was rights-protective-for-minority vs. policy-overreach-correcting, tracked longitudinally.',
    'A pattern concentrated on minority-rights rulings would support reclassification toward snare in specific jurisdictions or eras; a pattern concentrated on genuine overreach correction would support a rope-leaning tangled_rope or even rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_frequency_as_extraction_signal, empirical, 'Whether override-usage patterns discriminate between the coordination and extraction functions of this reading.').

omega_variable(
    electoral_accountability_temporal_mismatch,
    'Is the democratic mandate justification for legislative interpretive finality undermined by the temporal mismatch between override timing (immediate, majority-driven) and electoral accountability timing (periodic, multi-year)?',
    'Track the interval between override enactment and the next election in which the override''s electoral consequences could be tested, across cases.',
    'A short interval supports the accountability claim as functionally real; a long interval (or override entrenchment via subsequent legislative majorities) suggests the accountability mechanism is substantially weaker than the reading''s founding justification assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_accountability_temporal_mismatch, empirical, 'Whether electoral accountability actually constrains override use in the timeframe that matters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the basic_law_interpretive_authority kernel. judicial_supremacy_reading locates final interpretive authority in the courts and would show judicial independence and rights-protective doctrine among its beneficiaries, with legislative majorities among those bearing constrained policy options as victims/payers. popular_constitutionalism_reading rejects terminal institutional adjudication altogether, distributing interpretive authority across ongoing democratic contestation with no single institutional beneficiary of finality. Each reading has a distinct beneficiary/victim structure and a distinct ε — they are not the same constraint measured three ways; they are three constraints sharing a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
