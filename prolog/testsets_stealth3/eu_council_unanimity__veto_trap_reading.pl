% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule — Veto-Trap Reading
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Inside the EU Council, files in designated policy areas legally require
 *   every member state's assent before adoption. This story models that
 *   standing arrangement through its blocking dynamic: a government that can
 *   credibly withhold assent prices its acquiescence, converting
 *   supermajority preferences into negotiated concessions — budget
 *   corrections, protocol opt-outs, softened sanctions, indefinite delay. A
 *   consent-guaranteeing core survives (no state is bound against declared
 *   vital interests), but the observable operation is dominated by hold-up
 *   exchanges whose gains concentrate in the blocking seat while costs spread
 *   across the majority coalition, the Commission's legislative program, and
 *   the unseated populations the blocked measures would have served. This
 *   file is one member of a three-story family decomposing the colloquial
 *   label 'Council unanimity'; the decomposition record lives in
 *   network.dual_formulation_note and commentary.kernel_context. KEY AGENTS
 *   (by structural relationship): - minority_blocking_states: Primary
 *   beneficiary (moderate/constrained) — converts veto leverage into
 *   exemptions and side-payments - policy_majority_member_states: Primary
 *   target (organized/constrained) — bears dilution, delay, and bilateral
 *   payments - european_commission: Secondary target and broker
 *   (institutional/identity_locked) — loses program content, gains brokerage
 *   relevance - council_presidency_rotating: Administrator
 *   (institutional/mobile) — chairs and sequences but cannot amend the rule -
 *   accession_candidate_states: Excluded party (powerless/trapped) — futures
 *   gated by votes they cannot cast -
 *   national_electorates_of_conceding_states: Excluded bearers
 *   (powerless/trapped) — absorb diluted policy after the trade -
 *   academic_institutional_designers: Analytical observer — traces
 *   concessions to threats and publishes unused counterfactuals
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.72).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.65).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule — Veto-Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political/institutional").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '40e57bbc-ee97-4533-937f-7ce015785ae7').
narrative_ontology:cs_kernel_codification('40e57bbc-ee97-4533-937f-7ce015785ae7', fixed_text).
narrative_ontology:cs_authority_grounding('40e57bbc-ee97-4533-937f-7ce015785ae7', lineage).
narrative_ontology:cs_interpretation_layer_present('40e57bbc-ee97-4533-937f-7ce015785ae7').
narrative_ontology:cs_reading_relation('40e57bbc-ee97-4533-937f-7ce015785ae7', eu_council_unanimity__sovereignty_guarantor_reading, influences).
narrative_ontology:cs_reading_relation('40e57bbc-ee97-4533-937f-7ce015785ae7', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('40e57bbc-ee97-4533-937f-7ce015785ae7', foundational, blocking_transfers_value_to_minority).
narrative_ontology:cs_axiom_status(blocking_transfers_value_to_minority, holdable).
narrative_ontology:cs_axiom_grounding('40e57bbc-ee97-4533-937f-7ce015785ae7', blocking_transfers_value_to_minority, empirically_contingent).
narrative_ontology:cs_axiom('40e57bbc-ee97-4533-937f-7ce015785ae7', secondary, unanimity_exceeds_vital_interest_scope).
narrative_ontology:cs_axiom_status(unanimity_exceeds_vital_interest_scope, holdable).
narrative_ontology:cs_axiom_grounding('40e57bbc-ee97-4533-937f-7ce015785ae7', unanimity_exceeds_vital_interest_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('40e57bbc-ee97-4533-937f-7ce015785ae7', luxembourg_compromise_consensus_norm).
narrative_ontology:cs_drift_state('40e57bbc-ee97-4533-937f-7ce015785ae7', post_enlargement_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40e57bbc-ee97-4533-937f-7ce015785ae7', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, minority_blocking_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, policy_majority_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, european_commission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, european_commission).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, holdup_bargaining_model).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, intergovernmental_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments of individual member states that periodically convert their treaty veto into concessions: budget corrections, protocol opt-outs, softened sanctions language, delayed conditionality. They announce red lines, threaten or cast blocking votes, and trade acquiescence for exemptions their publics can celebrate as national victories. Leaving the Union would forfeit the veto asset along with market access, so the leverage is exercised from inside; the gains accrue domestically to the governing party.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, minority_blocking_states, beneficiary,
    moderate, biographical, constrained, national).

% The shifting coalitions of twenty-plus governments that prefer a given measure and command the votes to pass it under qualified majority. When the file falls under unanimity their preference is hostage: they dilute the text, postpone adoption, grant bilateral side-payments, or shelve the initiative. Formal withdrawal exists but the single completed precedent priced it as unusable leverage; their working counter-leverage is patience, package deals, and the rare enhanced-cooperation valve.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, policy_majority_member_states, payer,
    organized, generational, constrained, continental).

% Proposes and stewards Union legislation; under unanimity its proposals enter a chamber where any government can hold them, so flagship initiatives arrive diluted or die quietly. The same friction raises the Commission's value as broker: presidencies and delegations lean on it to assemble packages that satisfy every veto holder. Its institutional self-concept is fused with the integration method it polices, so abandoning that role is not a live option.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_commission, payer,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, european_commission, beneficiary).

% Chairs Council configurations for six-month rotations: schedules files, drafts compromise texts, decides when an objection is 'noted' rather than resolved. It administers the unanimity procedure but cannot amend it; its principal instruments are sequencing and silence. Each team exits automatically at rotation's end, passing unresolved blockers to its successor.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, council_presidency_rotating, agenda_setter,
    institutional, immediate, mobile, continental).

% Countries whose accession requires the unanimous assent of every existing member. They reform, negotiate, and wait while a single government's bilateral dispute can freeze their file indefinitely; they hold no vote on the decisions that gate their future and cannot shop their application elsewhere.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, accession_candidate_states, excluded,
    powerless, generational, trapped, regional).

% Voters in the majority states whose public services, energy terms, or security support depend on measures a blocking government can dilute. They learn of the concessions after they are traded; their recourse runs through national elections that select the very governments doing the trading.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, national_electorates_of_conceding_states, excluded,
    powerless, biographical, trapped, national).

% Scholars of voting rules and treaty design who trace concession episodes to blocking threats, compare unanimity chambers across federations, and publish the counterfactual designs — weighted votes, qualified-majority thresholds, exit-proof safeguards — that the participants decline to adopt.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, academic_institutional_designers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, minority_blocking_states).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that no member state is bound by collective action it has not consented to: every government retains a legal hold on decisions touching its declared vital interests, which is the condition under which heterogeneous sovereign states accept deep shared regulation at all.
% TRANSFER_FUNCTION: Moves policy content and side-payments from supermajority-preferred outcomes toward the blocking government's position: opt-outs, budget corrections, protocol carve-outs, weakened sanctions language, delayed disbursements — paid by the majority coalition and, diffusely, by the populations the full-strength measures would have served.
% ABSENT_VOICES: Accession candidates whose admission requires the unanimous assent they cannot influence, and residents of majority states whose services, energy terms, or security support depend on measures a single government can dilute, have no seat; national electorates encounter the concessions only after they are traded.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse the blocking seat's return model: pending files would pass on qualified-majority thresholds, accumulated opt-outs and corrections would face renegotiation without leverage, and governments currently funding themselves politically through visible national victories would lose the instrument — the Council's internal bargaining order would reorganize around majority formation within months.
% FOUNDING_PROBLEM: The Empty Chair Crisis of 1965-66, in which a major member state paralyzed the Community rather than accept majority voting on agricultural financing, ended in a settlement entrenching the practice that discussion continues until every government agrees — restoring participation by guaranteeing that no state could be outvoted on what it declared vital.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the blocking governments' set: intergovernmental-conference records and the Laeken Declaration attest the consent-protection concern; academic voting-rule literature and European Parliament debates attest both that the underlying interstate heterogeneity persists and that the arrangement's operation has shifted toward routine hold-up — the same external sources that confirm liveness document the drift.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored from the arrangement's observable operation, independently of the claimed type. Extractiveness 0.72: concession episodes recur across decades and policy areas — the 1984 budget correction, the 1992 opt-out protocols, softened sanctions language and delayed aid packages in the 2010s — each transferring policy content or side-payments from supermajority preferences to the blocking seat. Suppression 0.65: the majority's override route is legally closed (unanimity applies by treaty, and treaty change itself requires unanimity), leaving patience, packaging, and enhanced cooperation — a narrow valve used only a handful of times — as the remaining alternatives, hence accessibility_collapse 0.58 rather than higher. Resistance 0.55 reflects persistent, repeatedly defeated pressure to widen qualified majority voting. Theater 0.36: consensus language, constructive abstentions, and 'noted' objections perform harmony while the substantive trades happen in bilateral corridors. Enforcement intensity rose with enlargement — each accession added potential blockers, raising the monitoring and brokerage burden of holding the consensus line — which is why suppression_requirement is tracked and rises across the grid. All three series share one six-point grid (1966-2024); end-state values equal the base_properties scalars. The escalation is a ratchet, not a cycle: each successful extraction demonstrates the technique's profitability to other governments, so trajectories are monotone rather than oscillating.
 *
 * PERSPECTIVAL GAP:
 *   All member states hold formally identical standing — one government, one veto — yet their computed seats diverge sharply. Differentiation is constraint-specific, not global-power-driven: exposure salience (how much a dossier touches the state's declared red lines), domestic electoral payoff from visible confrontation, and government composition determine who blocks. From the blocking seat the arrangement prices its consent and returns exemptions it can present as national victories; from the majority seat the same dossier is a hostage negotiation it did not choose; from the presidency seat it is a scheduling burden; from the excluded seats it is a decision made entirely over their heads. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration: minority_blocking_states — damped directionality, effective extraction inverted toward subsidy, strongest precisely because exit is constrained (the veto is exercisable only from inside). Victim declarations: policy_majority_member_states (organized, constrained exit) and european_commission (identity_locked) sit near the full-target end; identity lock pushes the Commission further toward full target than its formal position alone would imply. The rotating presidency carries no declared benefit or victimhood and falls back near symmetry: it administers the procedure without collecting from it. Excluded seats (candidate states, conceding-state electorates) are recorded as absences, not correction-grade inputs: they shape the consensus-provenance picture but do not move the directionality arithmetic. Continental scope modestly amplifies effective extraction on the paying seats — verifying who blocked what, and why a text changed, is hardest exactly where the files are largest. Suppression enters the computation as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting sovereign consent after the 1965-66 legitimacy crisis — remains live: member-state interests stay heterogeneous and sovereignty-sensitive, so mandatrophy is not declared. The drift risk runs the other way: the arrangement's reform path runs through the consent of the governments it empowers, so if targeted safeguards ever replaced the universal veto, the residue would persist by self-protection rather than by function — the boundary this classification watches. Holding the consent-guaranteeing core and the hold-up operation together in one honest hybrid prevents both romanticizing the rule as pure consensus craft and reducing it to pure extortion; the engine's per-seat computation carries the asymmetry that a single label would flatten.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_epsilon_status,
    'This story instantiates the veto_trap_reading of kernel eu_council_unanimity; do the sibling readings (sovereignty_guarantor_reading, diplomatic_capital_reading) assess the same standing arrangement with different epsilon, confirming reading-indexed values over a fixed referent?',
    'Compile all three sibling stories and compare computed per-seat classifications and epsilon over the identical referent (Council unanimity as practiced); divergence attributable to the reading rather than to referent drift confirms indexicality.',
    'Merging the readings into one story would average incompatible mechanism attributions and fabricate epsilon stability; kept separate, cross-reading divergence is the measurable signal the corpus exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_epsilon_status, conceptual, 'Committer-frame routing: one reading, one constraint, one epsilon; siblings are separate files.').

omega_variable(
    blocker_class_structural_split,
    'Do large-state extractions (budget-correction style) and small-state obstructions (conditionality and sanctions-delay style) constitute one extraction mechanism or two structurally distinct ones?',
    'Disaggregate documented concession episodes by blocker size, gain type, and durability of the extracted concession; test whether the extraction and persistence profiles separate.',
    'Two distinct profiles would violate single-epsilon authorship and require decomposition into separate stories linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocker_class_structural_split, empirical, 'Whether the blocking-extraction class is homogeneous enough for one constraint story.').

omega_variable(
    threat_veto_extraction_ratio,
    'What share of extracted value flows from vetoes actually cast versus credible threats absorbed before a vote occurs?',
    'Process-trace Council dossiers: code pre-vote text changes traceable to identified red lines, distinguishing threat-priced concessions from ordinary negotiation movement.',
    'A dominant threat share means observed veto counts badly understate epsilon and that the coordination surface functions partly as hold-up pricing; the computed type could shift toward the pure-extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_veto_extraction_ratio, empirical, 'Invisible extraction via anticipated vetoes versus visible casting.').

omega_variable(
    majority_exit_cost_realism,
    'Is formal withdrawal a live modulator of majority leverage, or do demonstrated exit costs reduce majority states to effective trapping despite formal mobility?',
    'Model counterfactual coalition hardening under varied exit-cost assumptions, using the single completed withdrawal as the cost anchor.',
    'Higher effective trapping raises the majority seats'' effective extraction and pushes the computed type toward the pure-extraction pole; lower costs support the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_exit_cost_realism, empirical, 'Whether the majority''s formal exit option disciplines blockers in practice.').

omega_variable(
    residual_coordination_share,
    'What fraction of the arrangement''s ongoing operation still performs consent-guaranteeing coordination rather than hold-up management?',
    'Classify a decade of unanimity-file outcomes by whether the consent constraint bound a genuine vital interest or priced an ordinary preference.',
    'Below a coordination floor, the hybrid label misdescribes a pure-extraction arrangement; above it, the extraction share rides on real coordination worth preserving.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_coordination_share, conceptual, 'Boundary between surviving coordination function and captured operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1966, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1966, eu_council_unanimity__veto_trap_reading, theater_ratio, 1966, 0.2).
narrative_ontology:measurement_basis(eu_c_tr_t1966, observed).
narrative_ontology:measurement(eu_c_tr_t1979, eu_council_unanimity__veto_trap_reading, theater_ratio, 1979, 0.24).
narrative_ontology:measurement_basis(eu_c_tr_t1979, observed).
narrative_ontology:measurement(eu_c_tr_t1984, eu_council_unanimity__veto_trap_reading, theater_ratio, 1984, 0.28).
narrative_ontology:measurement_basis(eu_c_tr_t1984, observed).
narrative_ontology:measurement(eu_c_tr_t1992, eu_council_unanimity__veto_trap_reading, theater_ratio, 1992, 0.31).
narrative_ontology:measurement_basis(eu_c_tr_t1992, observed).
narrative_ontology:measurement(eu_c_tr_t2004, eu_council_unanimity__veto_trap_reading, theater_ratio, 2004, 0.33).
narrative_ontology:measurement_basis(eu_c_tr_t2004, observed).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__veto_trap_reading, theater_ratio, 2024, 0.36).
narrative_ontology:measurement_basis(eu_c_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1966, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1966, 0.55).
narrative_ontology:measurement_basis(eu_c_be_t1966, observed).
narrative_ontology:measurement(eu_c_be_t1979, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1979, 0.58).
narrative_ontology:measurement_basis(eu_c_be_t1979, observed).
narrative_ontology:measurement(eu_c_be_t1984, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1984, 0.63).
narrative_ontology:measurement_basis(eu_c_be_t1984, observed).
narrative_ontology:measurement(eu_c_be_t1992, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1992, 0.66).
narrative_ontology:measurement_basis(eu_c_be_t1992, observed).
narrative_ontology:measurement(eu_c_be_t2004, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2004, 0.69).
narrative_ontology:measurement_basis(eu_c_be_t2004, observed).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2024, 0.72).
narrative_ontology:measurement_basis(eu_c_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1966, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1966, 0.45).
narrative_ontology:measurement_basis(eu_c_su_t1966, observed).
narrative_ontology:measurement(eu_c_su_t1979, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1979, 0.5).
narrative_ontology:measurement_basis(eu_c_su_t1979, observed).
narrative_ontology:measurement(eu_c_su_t1984, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1984, 0.55).
narrative_ontology:measurement_basis(eu_c_su_t1984, observed).
narrative_ontology:measurement(eu_c_su_t1992, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1992, 0.58).
narrative_ontology:measurement_basis(eu_c_su_t1992, observed).
narrative_ontology:measurement(eu_c_su_t2004, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2004, 0.62).
narrative_ontology:measurement_basis(eu_c_su_t2004, observed).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement_basis(eu_c_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% 'Council unanimity' conflates at least three structurally distinct claims about one treaty rule: that it protects sovereign consent (sovereignty_guarantor_reading), that it manufactures diplomatic capital through forced iteration (diplomatic_capital_reading), and that it exposes supermajority preferences to minoritarian hold-up (this file). The readings share a single referent but attribute different mechanisms and therefore author different epsilon values; per the epsilon-invariance principle they are separate stories linked here. This reading is downstream of the sovereignty settlement historically — the protection arrangement created the exploitability the trap reading documents — and each extraction episode this file records feeds back as legitimacy pressure on the sibling accounts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
