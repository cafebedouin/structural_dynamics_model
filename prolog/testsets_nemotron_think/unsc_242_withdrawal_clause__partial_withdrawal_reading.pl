% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Partial Withdrawal Reading — Indefinite Article Discretion
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israel armed forces
 *   from territories occupied in the recent conflict' (English) vs 'retrait
 *   des forces armées israéliennes des territoires occupés' (French). The
 *   partial withdrawal reading — championed by the UK/US drafters and adopted
 *   by Israel — treats the English indefinite article ('territories' not 'the
 *   territories') as encoding drafters' intent for discretionary withdrawal
 *   scope, balanced against the 'secure and recognized boundaries' clause.
 *   This reading instantiates a Ledger: the textual ambiguity becomes
 *   negotiating leverage, converting an irreconcilable textual dispute into a
 *   structured phased-withdrawal process. The occupying power (Israel) and
 *   mediators (US, UN, regional powers) benefit from controlling the pace,
 *   scope, and sequencing; territorial claimants (Palestinians, Syria,
 *   Lebanon) bear the costs of deferred sovereignty and conditional
 *   withdrawal without a fixed enforcement line. The constraint is a tangled
 *   rope: genuine coordination function (preventing war, structuring peace
 *   diplomacy) combined with asymmetric extraction (occupying power retains
 *   strategic territories, mediators extract geopolitical rent).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.48).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.52).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Partial Withdrawal Reading — Indefinite Article Discretion").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3').
narrative_ontology:cs_kernel_codification('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', fixed_text).
narrative_ontology:cs_authority_grounding('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', lineage).
narrative_ontology:cs_interpretation_layer_present('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3').
narrative_ontology:cs_reading_relation('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', foundational, indefinite_article_encodes_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_encodes_discretion, holdable).
narrative_ontology:cs_axiom_grounding('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', indefinite_article_encodes_discretion, conventional).
narrative_ontology:cs_axiom('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', foundational, secure_boundaries_principle_justifies_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_principle_justifies_retention, holdable).
narrative_ontology:cs_axiom_grounding('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', secure_boundaries_principle_justifies_retention, conventional).
narrative_ontology:cs_reference_frame('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', unsc_242_original_mandate).
narrative_ontology:cs_drift_state('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', post_oslo_stalled_final_status, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97b9b5bb-7e3e-4eb0-a4eb-08a0c2bf18e3', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, secure_boundaries_principle).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafters_intent_indefinite_article).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, phased_withdrawal_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls territory captured in 1967; invokes indefinite article ('withdrawal from occupied territories') to claim discretion over scope and pace of withdrawal; retains strategic territories (East Jerusalem, Golan, settlement blocs) under 'secure boundaries' principle; negotiates phased withdrawals in exchange for recognition and security guarantees; exit from constraint means unilateral annexation or total withdrawal — both politically costly.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary).

% Great powers (US, USSR/Russia, EU, UN) and regional actors (Egypt, Jordan) who manage the negotiation process; benefit from controlling the diplomatic agenda, sequencing, and verification; extract geopolitical leverage from managing the ambiguity; exit options include shifting to alternative frameworks (bilateral, multilateral) but lose the central convening role.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediators, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediators, agenda_setter).

% Palestinian national movement, Syria (Golan), Lebanon (Shebaa Farms); claim full withdrawal per Charter Article 2(4) and French text ('retrait des territoires occupés'); lack fixed enforcement line — withdrawal is conditional, phased, and reciprocal; bear costs of continued occupation, settlement expansion, and deferred sovereignty; exit from constraint means abandoning diplomatic track for armed resistance or unilateral declaration — both carry high suppression risk.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    organized, generational, trapped, regional).

% ICJ, UNSC, drafting states (UK, US, France, USSR) claiming authority to resolve the textual ambiguity; ICJ asserts judicial interpretation (Advisory Opinion on Wall, 2004); drafting states assert authorial intent; occupying state asserts customary practice; none can impose binding resolution on the others; their contest structures the meta-constraint (interpretive_authority_structure).
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, interpretive_authority, observer,
    institutional, civilizational, analytical, universal).

% UK and US as primary drafters of English text; France as primary drafter of French text; their divergent recollections of intent (Caradon, Rostow, Goldberg vs. French delegates) are cited but not dispositive; excluded from current enforcement mechanism — their intent is evidence, not authority.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_states, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_states, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a negotiated framework for phased Israeli withdrawal from territories occupied in 1967, converting the textual ambiguity of 'withdrawal from occupied territories' (English) vs 'retrait des territoires occupés' (French) into a structured diplomatic process linking withdrawal extent to Arab recognition, secure boundaries, and peace agreements.
% TRANSFER_FUNCTION: Moves territorial control and security guarantees from claimants (Palestinians, Syria, Lebanon) to the occupying power (Israel) through a phased, conditional withdrawal process mediated by great powers; the occupying power transfers recognition and normalization in stages; mediators extract geopolitical leverage from managing the sequencing and verification.
% ABSENT_VOICES: Palestinian civilian population under occupation (no formal representation in 1967 framework); refugee populations (1948 and 1967) whose return/compensation claims are deferred; future generations affected by territorial settlements; Israeli peace camp and settler movement as domestic constituencies with opposing views on withdrawal scope.
% DISAPPEARANCE_RATIONALE: If the partial withdrawal framework vanished overnight, the entire architecture of Arab-Israeli peace diplomacy (Rogers Plan, Camp David, Madrid, Oslo, Roadmap, Arab Peace Initiative) would lose its foundational textual anchor; the territorial dispute would revert to either frozen conflict (no agreed framework) or unilateral action (annexation or full withdrawal imposed by force), fundamentally rearranging the regional order.
% FOUNDING_PROBLEM: Post-1967 War need for a Security Council framework linking Israeli withdrawal from occupied territories to termination of belligerency, recognition of sovereignty, and secure and recognized boundaries — resolving the immediate crisis while deferring final borders to negotiation.
% FOUNDING_PROBLEM_CORROBORATION: UNSC Resolution 242 text (1967); Rogers Plan (1969-70) explicitly interpreting 'withdrawal from occupied territories' as not requiring full withdrawal; Camp David Accords (1978) and Egypt-Israel Treaty (1979) implementing partial withdrawal (Sinai) for full peace; Oslo Accords (1993) extending phased withdrawal logic to Palestinian track; ICJ Advisory Opinion on Wall (2004) rejecting partial withdrawal reading; scholarly consensus outside drafting states (Dinstein, Blum, Stone, Higgins) that French text controls but English ambiguity was deliberate negotiating technique.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48) is moderate: the constraint extracts territorial sovereignty from claimants conditionally, not absolutely; the phased nature and reciprocity (land for peace) dampen raw extraction. Suppression (0.52) reflects that the constraint's persistence depends on active diplomatic enforcement — great power mediation, UNSC monitoring, bilateral treaties — not self-execution; claimants lack exit to a binding adjudicative forum. Theater ratio (0.28) is low-moderate: the negotiation process has real function (Sinai withdrawal, Jordan treaty, Oslo interim agreements) but growing performative elements (endless process, settlement expansion during negotiations). Accessibility collapse (0.55) is partial: alternatives exist (ICJ adjudication, unilateral action, BDS) but are structurally suppressed by the framework's legitimacy. Resistance (0.58) is significant: intifadas, legal challenges, diplomatic campaigns contest the reading but operate within or against the framework.
 *
 * PERSPECTIVAL GAP:
 *   From occupying power + mediator seats, the constraint appears as a genuine coordination achievement — the only framework that produced actual withdrawals (Sinai, Gaza-Jericho, Jordan Valley). From claimant seats, the same structure operates as enforced extraction — territorial sovereignty conditional on reciprocal acts they cannot control, with no deadline or enforcement. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges both coordination and extraction as structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Occupying power sits near beneficiary end (d ~ 0.2): controls territory, sets withdrawal terms, extracts security guarantees; constrained exit (unilateral annexation/withdrawal both costly) prevents full arbitrage. Mediators sit near symmetric (d ~ 0.45): genuine coordination benefit (regional stability) plus extracted geopolitical leverage; mobile exit (can shift frameworks). Territorial claimants sit near target end (d ~ 0.85): bear costs of occupation, deferred sovereignty, conditional withdrawal; trapped exit (no binding enforcement, armed resistance suppressed). Interpretive authority and drafting states are observers (d ~ 0.5): analytical seats with no direct extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (linking withdrawal to peace and secure boundaries) remains contested: Israel argues secure boundaries not achieved (Iranian proxies, Hamas, Hezbollah); claimants argue withdrawal incomplete (West Bank, Golan, East Jerusalem). The mandate has not atrophied — it remains the active diplomatic framework — but its coordination function has narrowed (only Egypt, Jordan completed withdrawal) while extraction function persists (settlement expansion during process). Not a piton: active maintenance, not inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint is one reading (partial_withdrawal_reading) of kernel unsc_242_withdrawal_clause; sibling readings are maximal_withdrawal_reading and interpretive_authority_structure. What structural elements differ across readings?',
    'Comparative constraint-story analysis across the three readings: each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and type. The kernel contest is modeled as a constraint family linked by network.affects_constraints.',
    'Confirms ε-invariance: each reading has a stable ε assessed against the same standing arrangement (the 242 framework as it operates), not against the reading''s preferred alternative. Prevents averaging or hedging across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Committer frame: this story instantiates one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    textual_ambiguity_control,
    'Does the English indefinite article (''territories'') or French definite article (''les territoires'') control the withdrawal scope, or is the ambiguity itself the operative mechanism?',
    'ICJ authoritative interpretation (already issued: French text controls, full withdrawal required) vs. state practice (partial withdrawals accepted as compliance) vs. drafting history (Caradon/Rostow: ambiguity deliberate). The engine treats this as a conceptual omega — resolution depends on which interpretive community''s authority is recognized.',
    'If French text controls, ε rises (full withdrawal mandated, current partial compliance is violation); if English text controls, ε falls (discretion validated); if ambiguity is the mechanism, current ε stands as coordination-extraction hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_control, conceptual, 'Whether textual ambiguity is a bug to be resolved or the feature that enables the Ledger.').

omega_variable(
    secure_boundaries_operationalization,
    'What constitutes ''secure and recognized boundaries'' in practice — 1967 lines with minor swaps, 1949 armistice lines, or boundaries incorporating strategic depth (Jordan Valley, settlement blocs, East Jerusalem)?',
    'Negotiated outcomes (Camp David 2000, Annapolis 2008, Trump Plan 2020) show convergent but unratified operationalizations; ICJ 2004 rejects strategic-depth interpretation. Empirical resolution requires a ratified final-status agreement.',
    'Narrow interpretation (1967 lines) reduces ε toward rope; broad interpretation (strategic depth) increases ε toward snare. Current moderate ε reflects the unresolved operationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_operationalization, empirical, 'Operational definition of the secure boundaries principle that permits retention.').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Does the constraint have an independent enforcement mechanism (UNSC Chapter VII, ICJ binding judgment) or does it rely entirely on great power mediation and bilateral reciprocity?',
    'Historical record: no Chapter VII enforcement for withdrawal; ICJ opinions advisory; enforcement via US mediation, bilateral treaties, Arab League consensus. Structural resolution: if a binding enforcement mechanism emerges, suppression measurement shifts from diplomatic to legal-coercive.',
    'If enforcement remains mediation-only, the constraint stays tangled_rope (coordination dependent on voluntary compliance). If binding enforcement emerges, it could shift toward rope (if enforcement is symmetric) or snare (if enforcement targets only one side).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Nature of the enforcement backing the withdrawal obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_partial_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unsc242_partial_tr_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(unsc242_partial_tr_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(unsc242_partial_tr_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(unsc242_partial_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(unsc242_partial_tr_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(unsc242_partial_tr_t57, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 57, 0.28).

% Extraction over time
narrative_ontology:measurement(unsc242_partial_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unsc242_partial_be_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(unsc242_partial_be_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(unsc242_partial_be_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(unsc242_partial_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(unsc242_partial_be_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(unsc242_partial_be_t57, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 57, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_partial_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(unsc242_partial_su_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(unsc242_partial_su_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(unsc242_partial_su_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(unsc242_partial_su_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(unsc242_partial_su_t50, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(unsc242_partial_su_t57, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 57, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, oslo_accords_interim_framework).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, camp_david_accords_sinai_withdrawal).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, arab_peace_initiative_2002).

% DUAL FORMULATION NOTE:
% This constraint (partial_withdrawal_reading) and maximal_withdrawal_reading are dual formulations of the same kernel: one reads the English indefinite article as encoding discretionary scope; the other reads the French definite article and Charter Art. 2(4) as requiring full withdrawal. The interpretive_authority_structure is the meta-constraint governing which reading prevails. All three form the unsc_242_withdrawal_clause constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
