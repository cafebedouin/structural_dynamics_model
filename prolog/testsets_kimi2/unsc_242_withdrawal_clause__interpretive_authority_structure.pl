% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC 242 Withdrawal Clause: Contested Interpretive Authority Structure
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israeli armed forces
 *   from territories occupied in the recent conflict.' The English text uses
 *   'territories' (indefinite), while the French text uses 'des territoires'
 *   (definite). Rather than resolving this textual ambiguity, the
 *   international legal order has witnessed a sustained contest over WHO has
 *   authority to interpret the clause: the ICJ asserts judicial
 *   interpretation under the Charter, drafting states (UK, US) assert
 *   authorial intent, and the occupying state asserts subsequent customary
 *   practice. This constraint story instantiates the
 *   'interpretive_authority_structure' reading of the
 *   unsc_242_withdrawal_clause kernel. It models the meta-dispute as a snare:
 *   the authority contest itself perpetuates substantive ambiguity,
 *   benefiting actors with veto or non-cooperation capacity while extracting
 *   legal certainty from territorial claimants and closure-seeking
 *   institutions. The sibling readings (maximal_withdrawal,
 *   partial_withdrawal) are object-level textual interpretations that this
 *   authority structure keeps simultaneously live.
 *
 * KEY AGENTS:
 *   - occupying_state: Primary beneficiary â claims customary practice authority, benefits from territorial retention under ambiguity, possesses non-cooperation capacity.
 *   - veto_capable_council_members: Structural beneficiary â can block UNSC enforcement resolutions that would impose a definitive interpretation, preserving diplomatic flexibility.
 *   - icj: Analytical observer/authority claimant â asserts judicial interpretation power but lacks enforcement mechanism against non-consenting powerful states.
 *   - drafting_states: Historical observer â assert authorial intent but lack contemporary institutional channel to enforce textual primacy.
 *   - territorial_claimant_communities: Primary payer/target â seek definitive withdrawal and return, trapped by the perpetuated procedural deadlock.
 *   - legal_closure_seeking_states: Secondary payer â states and advocates seeking ICJ-led resolution, constrained by veto politics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Withdrawal Clause: Contested Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '0ac25446-90a0-469d-80be-cc069e1af720').
narrative_ontology:cs_kernel_codification('0ac25446-90a0-469d-80be-cc069e1af720', fixed_text).
narrative_ontology:cs_authority_grounding('0ac25446-90a0-469d-80be-cc069e1af720', distributed).
narrative_ontology:cs_reading_relation('0ac25446-90a0-469d-80be-cc069e1af720', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ac25446-90a0-469d-80be-cc069e1af720', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('0ac25446-90a0-469d-80be-cc069e1af720', foundational, no_definitive_interpretive_hierarchy).
narrative_ontology:cs_axiom_status(no_definitive_interpretive_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('0ac25446-90a0-469d-80be-cc069e1af720', no_definitive_interpretive_hierarchy, conventional).
narrative_ontology:cs_axiom('0ac25446-90a0-469d-80be-cc069e1af720', foundational, customary_practice_as_valid_authority).
narrative_ontology:cs_axiom_status(customary_practice_as_valid_authority, holdable).
narrative_ontology:cs_axiom_grounding('0ac25446-90a0-469d-80be-cc069e1af720', customary_practice_as_valid_authority, conventional).
narrative_ontology:cs_reference_frame('0ac25446-90a0-469d-80be-cc069e1af720', consensual_interpretive_pluralism).
narrative_ontology:cs_drift_state('0ac25446-90a0-469d-80be-cc069e1af720', contemporary_enforcement_paralysis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ac25446-90a0-469d-80be-cc069e1af720', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_capable_council_members).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, territorial_claimant_communities).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, legal_closure_seeking_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains that subsequent state practice and customary international law govern the interpretation of Resolution 242, allowing discretionary territorial retention. Benefits from sustained ambiguity by avoiding definitive legal condemnation while retaining control of occupied territories. Can defy or ignore unfavorable interpretations without facing enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, arbitrage, global).

% Can block Security Council resolutions that would impose a definitive interpretation of the withdrawal clause or mandate enforcement. Benefits from structural ambiguity by preserving diplomatic flexibility and avoiding forced alignment with either total withdrawal or permanent occupation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_capable_council_members, beneficiary,
    institutional, generational, arbitrage, global).

% Claims authority under the UN Charter to provide binding or advisory interpretations of resolution texts. Lacks effective enforcement mechanisms when powerful states reject jurisdiction or fail to comply. Issues advisory opinions that are politically filtered by the same power dynamics.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj, observer,
    institutional, civilizational, analytical, global).

% The original negotiators maintain that the English indefinite article was deliberately chosen to permit partial withdrawal. Their authorial claims are invoked by various parties but lack an institutional mechanism to override competing interpretive sources in contemporary practice.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, observer,
    powerful, generational, analytical, global).

% Seek definitive legal resolution mandating full withdrawal and enabling return and self-determination. Trapped by the perpetual procedural meta-dispute, which substitutes contested authority claims for substantive resolution of their territorial status.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, territorial_claimant_communities, payer,
    powerless, biographical, trapped, local).

% Member states and legal advocates pushing for ICJ jurisdiction or Council enforcement to resolve the ambiguity. Constrained by the threat of veto and the occupying state's refusal to submit to judicial determination.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, legal_closure_seeking_states, payer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no real coordination problem solved by this authority contest; rather, the contest fragments interpretive authority across three incompatible sources (judicial, authorial, customary), preventing unified resolution of the underlying territorial dispute.
% TRANSFER_FUNCTION: Moves the capacity to delay definitive legal resolution and retain territorial flexibility from territorial claimants and judicial institutions to the occupying state and veto-capable council members, who capture diplomatic and strategic advantage from the perpetual ambiguity.
% ABSENT_VOICES: Drafting states whose authorial intent is cited but not institutionally empowered in current UNSC dynamics; displaced territorial communities whose self-determination claims are buried under procedural meta-disputes; lower-tier UN member states without veto power who would favor textual determinacy and binding judicial settlement.
% DISAPPEARANCE_RATIONALE: If a single authoritative interpreter were definitively established (e.g., binding ICJ jurisdiction or automatic drafting-state textual supremacy), the maximal and partial withdrawal readings would collapse into a resolvable legal question; territorial claimants would gain a clear enforcement pathway, and the occupying state would lose the structural shelter of ambiguity.
% FOUNDING_PROBLEM: The drafting of UNSC Resolution 242 in 1967 required reconciling Anglo-French textual discrepancies and competing great-power visions of Middle East territorial settlement without alienating any permanent member or the occupying state.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and UN archives scholars attest that the textual ambiguity was a deliberate diplomatic compromise to secure unanimous Council adoption; contemporary legal scholars outside the beneficiary states confirm the drafting problem was solved by ambiguity, not by interpretive authority assignment, and the present contest is a function of power politics, not textual necessity.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The authority structure is a snare because it presents itself as a legal interpretive framework while functioning to prevent resolution. Extractiveness (0.82) is high because the ambiguity perpetuates indefinite territorial occupation and foreclosed self-determination for claimant communities. Suppression (0.75) is substantial: the suppression is not of violence but of legal closure â veto power and non-cooperation actively suppress definitive interpretation. Theater ratio (0.45) captures the performative aspect: repeated Council debates, ICJ advisory opinions, and diplomatic statements simulate legal process without resolving authority. Accessibility collapse (0.60) reflects that once an actor understands the tri-partite authority contest, alternatives (like unilateral adjudication or automatic textualism) are structurally blocked by the same contest. Resistance (0.70) is high because the ICJ, claimant states, and civil society continuously challenge the occupying and veto-wielding positions.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying state and veto-capable members' seats, the authority contest is a necessary feature of sovereign equality and consensual international law â a rope-like coordination of plural interpretive sources. From the territorial claimant and judicial-advocate seats, the same structure is pure extraction: it converts legal text into permanent strategic flexibility for the powerful. The engine computes this divergence from structural data (beneficiary vs victim, institutional power vs powerlessness, arbitrage vs trapped exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Occupying state and veto-capable council members are beneficiaries: they collect diplomatic flexibility and territorial retention from the ambiguity (low d). Territorial claimant communities and legal-closure-seeking states are victims: they bear the cost of indefinite unresolved status (high d). The ICJ and drafting states occupy intermediate/analytical positions â they do not collect extraction but lack power to impose directionality on the powerful beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The authority structure was built to manage a diplomatic drafting crisis (founding_problem_status: dead). It has outlived that function by decades. However, it is not a piton because active beneficiaries (occupying state, veto powers) still derive substantial concentrated benefits from its maintenance, and victims actively resist. It is not a tangled rope because there is no genuine coordination function being served by the authority contest â the contest fragments rather than coordinates interpretive authority. The classification as snare prevents misreading the performative legalism as residual inertia or genuine pluralism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_hierarchy_resolvability,
    'Can the VCLT framework or UN Charter hierarchy definitively rank judicial, authorial, and customary interpretive authorities, or is the contest irreducible?',
    'Comparative international legal analysis establishing an uncontested hierarchy among these sources for treaty interpretation.',
    'If irreducible, the snare is structurally inherent to international law; if reducible, the extraction is politically maintained and could be resolved by institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_hierarchy_resolvability, conceptual, 'Whether interpretive authority conflict is structurally necessary or politically sustained.').

omega_variable(
    occupying_state_customary_claim_validity,
    'Does the occupying state''s customary practice claim reflect genuine opinio juris and consistent state practice, or is it post-hoc justification for territorial retention?',
    'State practice surveys and ICJ advisory proceedings examining the customary status of the claimed interpretive rule.',
    'If invalid, the customary authority claim is theatrical cover for extraction; if valid, the ambiguity has a genuine legal source independent of power politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupying_state_customary_claim_validity, empirical, 'Empirical validity of the customary practice interpretive authority claim.').

omega_variable(
    committer_kernel_sibling_relation,
    'Does settling the interpretive authority question collapse the sibling maximal and partial withdrawal readings into a single resolvable constraint, or do they remain distinct even under unified authority?',
    'Counterfactual legal analysis: if ICJ authority were binding and definitive, would the textual disagreement persist as a separate dispute?',
    'If they merge, the authority structure is the true locus of extraction; if they remain distinct, the textual ambiguity itself is the deeper constraint requiring separate decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_sibling_relation, conceptual, 'Structural relationship between this meta-level authority reading and its object-level sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_ia_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unsc_242_ia_tr_t14, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 14, 0.3).
narrative_ontology:measurement(unsc_242_ia_tr_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 28, 0.38).
narrative_ontology:measurement(unsc_242_ia_tr_t42, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 42, 0.42).
narrative_ontology:measurement(unsc_242_ia_tr_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 57, 0.45).

% Extraction over time
narrative_ontology:measurement(unsc_242_ia_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unsc_242_ia_be_t14, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(unsc_242_ia_be_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(unsc_242_ia_be_t42, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 42, 0.78).
narrative_ontology:measurement(unsc_242_ia_be_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 57, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(unsc_242_ia_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unsc_242_ia_su_t14, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 14, 0.6).
narrative_ontology:measurement(unsc_242_ia_su_t28, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 28, 0.68).
narrative_ontology:measurement(unsc_242_ia_su_t42, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(unsc_242_ia_su_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 57, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form a constraint family derived from the kernel unsc_242_withdrawal_clause. This reading (interpretive_authority_structure) operates at the meta-level; the siblings operate at the object-level. The epsilon values differ: the authority structure is substantially extractive (snare), while the object-level readings, if isolated from authority contestation, might present as contested empirical claims or coordination attempts. The decomposition follows the epsilon-invariance principle: the authority question and the textual scope question have different structural properties, different stakeholders, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
