% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Dispute
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) establishes the legal framework for
 *   post-conflict settlement in the Middle East, with a withdrawal clause
 *   whose scope is disputed. The French text reads 'le retrait des forces
 *   armées' (withdrawal OF forces, suggesting all forces); the English text
 *   reads 'withdrawal of armed forces from territories' (indefinite article,
 *   suggesting some territories may be retained). The interpretive authority
 *   itself is now contested: the International Court of Justice claims the
 *   authority to give the clause definitive meaning through judicial
 *   interpretation; the drafting states claim authorial intent controls; the
 *   occupying state invokes customary international practice and
 *   non-enforcement to preserve an alternative reading. This constraint story
 *   instantiates the meta-dispute over WHO DECIDES — the reading authority
 *   conflict — not the substantive withdrawal scope itself (those are the
 *   sibling constraint stories: maximal_withdrawal_reading and
 *   partial_withdrawal_reading). The extractiveness is high (0.81 at interval
 *   end) because the meta-dispute itself perpetuates substantive ambiguity,
 *   allowing the occupying state and its allies to indefinitely avoid
 *   definitive settlement. Suppression is high (0.72) because non-cooperation
 *   with the ICJ and contestation of its authority prevent enforcement
 *   mechanisms from activating. Theater is moderate-high (0.48) because much
 *   of the diplomatic activity around the clause consists of invoking
 *   alternative authority sources (drafting records, customary practice)
 *   without submitting to binding adjudication.
 *
 * KEY AGENTS:
 *   - ICJ — claims interpretive authority under VCLT; can issue advisory opinions and contentious judgments but cannot compel enforcement
 *   - Occupying state with veto capacity — non-cooperates with maximal reading, invokes alternative authority sources, maintains occupation indefinitely
 *   - Drafting states opposing maximal reading — endorse occupying state's reading, claim authorial intent, contest ICJ jurisdiction
 *   - Displaced populations — powerless, trapped, bear cost of indefinite ambiguity, cannot participate in interpretive processes
 *   - States seeking closure — powerful but constrained, pay cost of legal uncertainty, cannot unilaterally force resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Dispute").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'a09d7de4-29dc-4bf2-8566-e2dc37aa31db').
narrative_ontology:cs_kernel_codification('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', fixed_text).
narrative_ontology:cs_authority_grounding('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', extraction).
narrative_ontology:cs_interpretation_layer_present('a09d7de4-29dc-4bf2-8566-e2dc37aa31db').
narrative_ontology:cs_reading_relation('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', unsc_242_withdrawal_clause__unsc_242_maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', unsc_242_withdrawal_clause__unsc_242_partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', foundational, institutional_authority_multiplicity).
narrative_ontology:cs_axiom_status(institutional_authority_multiplicity, holdable).
narrative_ontology:cs_axiom_grounding('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', institutional_authority_multiplicity, conventional).
narrative_ontology:cs_axiom('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', secondary, non_cooperation_as_legitimate_dissent).
narrative_ontology:cs_axiom_status(non_cooperation_as_legitimate_dissent, holdable).
narrative_ontology:cs_axiom_grounding('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', non_cooperation_as_legitimate_dissent, deontological).
narrative_ontology:cs_reference_frame('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', multilateral_treaty_interpretation_authority).
narrative_ontology:cs_drift_state('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', contemporary_non_resolution_equilibrium, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a09d7de4-29dc-4bf2-8566-e2dc37aa31db', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_opposing_maximal_reading).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_definitive_closure).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_consensus_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds formal interpretive authority over treaty language under the Vienna Convention on the Law of Treaties. Claims jurisdiction to resolve the withdrawal clause ambiguity through judicial interpretation of text, context, and object-and-purpose. Has issued advisory opinions and decisions that various parties selectively invoke or reject.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter,
    institutional, generational, analytical, global).

% Occupies disputed territories and claims the withdrawal clause permits retention of strategically significant areas under the 'secure boundaries' principle and indefinite article reading. Benefits from interpretive ambiguity because non-cooperation with ICJ process and invocation of alternative authority structures (customary practice, drafting state intent) prevents definitive enforcement of maximal withdrawal. Carries veto power in Security Council and can block enforcement mechanisms.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity, payer,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity, beneficiary).

% Include several permanent Security Council members and major powers who supported the indefinite English article formulation and contest ICJ authority to override it with maximal readings. Claim authorial intent as the controlling interpretive source and assert that states' own understanding of their own treaty commitment should prevail over judicial reinterpretation. Benefit from the ambiguity because it keeps the occupying state aligned with them rather than facing mandatory withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states_opposing_maximal_reading, beneficiary,
    institutional, generational, constrained, global).

% Remain in exile or under occupation pending resolution of the withdrawal scope. Bear the human and material cost of indefinite territorial ambiguity: refugee status, loss of property, restricted movement, family separation. Their return is contingent on definitive resolution of the withdrawal clause, which the interpretive authority dispute prevents.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, displaced_populations, payer,
    powerless, biographical, trapped, regional).

% Occupy or are adjacent to disputed territories and seek clear legal resolution of the withdrawal scope so they can plan governance, settle populations, or recover territory. Pay the cost of indefinite ambiguity through suspended development, security volatility, and unresolved legal status. Their exit would be to accept either reading, but doing so unilaterally surrenders leverage.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_definitive_closure, payer,
    powerful, generational, constrained, regional).

% Dispute ICJ's authority to interpret the clause authoritatively and invoke customary international practice, state consent, and drafting records as alternative sources. Include the occupying state and its allies. Are structurally excluded from the ICJ process not by formal rules but by non-cooperation and contestation of the court's legitimacy as the final arbiter.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, icj_skeptical_states, excluded,
    institutional, generational, constrained, global).

% Analyze the textual, contextual, and historical evidence for both readings. Produce technical commentary and expert testimony that various parties selectively invoke. Have published detailed analyses of the French definite article, drafting minutes, and subsequent state practice without converging on definitive resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_law_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state_with_veto_capacity).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: UNSC Resolution 242 (1967) was drafted to establish a legal framework for post-conflict settlement in the Middle East, coordinating great-power recognition of legitimate withdrawal obligations with security concerns of all parties. The resolution aims to provide a common reference point for negotiations.
% TRANSFER_FUNCTION: The interpretive authority dispute moves legitimacy and enforcement power among competing institutional actors: ICJ claims the authority to give the clause definitive meaning; drafting states claim that authority inheres in authorial intent; the occupying state claims that customary practice and non-enforcement preserve its reading. The constraint transfers the cost of indefinite ambiguity to displaced populations and states seeking closure.
% ABSENT_VOICES: Displaced populations themselves are excluded from formal interpretive processes and cannot directly petition the ICJ; their representatives participate only through state advocacy, which often diverges from their interests. Non-aligned states with weak legal capacity are structurally absent from the high-level authority debate and must accept whatever interpretation dominant institutional actors settle on.
% DISAPPEARANCE_RATIONALE: If the interpretive authority dispute vanished and one definitive reading were adopted, the territorial settlement would be legally binding and enforced; displaced populations would either return, be permanently resettled, or receive compensation; the occupying state would face either mandatory withdrawal or internationally recognized sovereignty over retained areas. The entire regional settlement architecture and subsequent legal disputes would reorient. The ambiguity itself is what permits indefinite non-resolution.
% FOUNDING_PROBLEM: UNSC Resolution 242 (1967) was drafted urgently to establish a ceasefire and post-conflict settlement framework after territorial occupation in the Middle East. The withdrawal clause was meant to reconcile two competing commitments: the principle of territorial integrity and the security interests of all parties, including the occupying power.
% FOUNDING_PROBLEM_CORROBORATION: The occupying state and its allies attest the founding problem demanded the indefinite English article formulation to protect security interests; states seeking maximal withdrawal and international law scholars attest the founding problem required unconditional return of occupied territory per Charter Article 2(4). Neither attestation comes from outside the benefiting parties. The ICJ has issued opinions supporting maximal readings, but the occupying state disputes the court's authority to do so. No neutral external authority has authoritatively resolved the founding problem's own status.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   This reading focuses on the AUTHORITY STRUCTURE DISPUTE, not the substantive withdrawal scope. The extractiveness trajectory rises from 0.68 to 0.81 because each failed attempt at judicial resolution through the ICJ ratifies the authority contest and entrenches the non-cooperation position — the more the ICJ claims authority, the more the occupying state and its allies invoke alternative sources (authorial intent, customary practice, state consent), and the more the ambiguity persists as a weapon. Theater ratio rises from 0.25 to 0.48 because diplomatic activity increasingly consists of performing alternative interpretations (citing drafting records, invoking customary norms) without submitting to binding third-party adjudication. Suppression rises from 0.58 to 0.72 as non-cooperation with the ICJ becomes institutionalized in diplomatic practice and the occupying state builds a coalition of allies who reject the court's authority. The measurements are authored on a single shared time grid (interval 0-57, representing roughly 1967-2024) so every metric is present at every examined point. The theater and suppression trajectories are rising and plateauing, consistent with a constraint that has calcified into institutional non-resolution.
 *
 * PERSPECTIVAL GAP:
 *   The ICJ seat computes the constraint as a failed coordination attempt (the occupying state wrongfully non-cooperates); the occupying state seat computes it as a legitimate multi-source authority structure where its reading remains live; the displaced population seat computes it as pure extraction masked by legal theater; the states-seeking-closure seats compute it as an institutional obstacle that prevents them from settling with either party.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and drafting states opposing maximal reading are the beneficiaries — they benefit from the authority dispute because it prevents definitive resolution and allows the occupying state to maintain its current position indefinitely. The occupying state has high institutional power and mobile exit options (it can non-cooperate with the ICJ, invoke alternative authority sources, maintain occupation); the drafting states have high institutional power and constrained exit options (they can contest the ICJ but must maintain diplomatic alignment). Displaced populations and states seeking closure are the victims — they pay the cost of the authority dispute in lost settlements, legal uncertainty, and stalled development. The displaced populations are powerless and trapped (cannot exit the region, cannot participate in interpretive processes); states seeking closure are powerful but constrained (they can invoke the ICJ but cannot compel the occupying state to comply). The directionality for the occupying state and its allies is low (near beneficiary end, d near 0.2-0.3) because they benefit from the arrangement without bearing its cost. The directionality for displaced populations is high (near full target end, d near 0.85-0.95) because they bear the cost with no benefit and no exit. States seeking closure sit intermediate (d near 0.6) — they have institutional power but cannot unilaterally force resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a common framework for territorial settlement) was live in 1967 but is contested by the occupying state and drafting states who benefit from indefinite ambiguity. The founding problem is now DEAD in the sense that no party is attempting to achieve definitive, mutually binding settlement — the interpretive authority dispute itself has become the substitute for the original coordination attempt. Yet the constraint persists because it allows parties with veto power to indefinitely avoid either accepting the maximal reading or negotiating a new settlement. This is a classic mandatrophy signature: the original mandate (establish binding withdrawal terms) has atrophied, but the authority structure persists in institutional form (the ICJ claims authority even though compliance is voluntary) and through diplomatic theater (invoking alternative interpretive sources without submitting to binding adjudication). The constraint is neither a genuine rope (because compliance is not voluntary and the occupying state actively resists) nor a pure snare (because the ICJ and displaced population advocates continue to invoke legal authority as if it were binding). It is a snare whose extraction mechanism is the contestation of authority itself — the more the ICJ asserts authority, the more the occupying state invokes alternative sources, and the more the ambiguity deepens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_supremacy,
    'Which institutional actor holds supreme authority to resolve the withdrawal clause: the International Court of Justice through judicial interpretation, the drafting states through authorial intent, the occupying state through customary practice and non-cooperation, or some other source?',
    'Either a binding Security Council resolution explicitly granting interpretive authority to one source, or a new international treaty that supersedes and reinterprets UNSC 242 with clear language and unambiguous procedural authority. Absent these, the authority question remains contested.',
    'If ICJ authority is accepted, the maximal withdrawal reading becomes binding and the occupying state must comply or face Security Council enforcement. If authorial intent is accepted, the drafting states'' reading becomes authoritative and the partial withdrawal framework becomes legally binding. If customary practice is accepted, the occupying state''s non-cooperation becomes a valid exercise of sovereignty and the constraint persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_supremacy, conceptual, 'The unresolved dispute over which institution or source has supreme interpretive authority').

omega_variable(
    authority_multiplicity_as_extraction_mechanism,
    'Is the multiplicity of authority sources (ICJ, drafting states, customary practice, state consent) a genuine feature of international law that permits legitimate contestation, or a flaw in international legal architecture that enables indefinite non-resolution?',
    'Comparative analysis of other treaty disputes resolved through similar multiplicity (e.g., UN Charter interpretation, human rights treaty jurisprudence) to determine whether authority multiplicity consistently produces extractive ambiguity or allows productive pluralism.',
    'If multiplicity is a genuine feature, the constraint should be reclassified as a rope or tangled rope with contested but legitimate plural sources. If multiplicity is a flaw that enables extraction, the constraint remains a snare and the authority dispute itself becomes the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_multiplicity_as_extraction_mechanism, conceptual, 'Whether multiple authority sources produce contestation or legitimate pluralism').

omega_variable(
    occupying_state_veto_power_sustainability,
    'How long can the occupying state maintain non-cooperation with the ICJ and invocation of alternative authority sources before diplomatic and legal pressure force it to either comply with a definitive reading or negotiate a new settlement?',
    'Temporal analysis: if non-cooperation persists beyond the functional lifetime of the occupied territories'' political relevance, or until the occupying state''s allies exhaust their Security Council veto power, the veto capacity has structural limits.',
    'If veto power is sustainable indefinitely, the extractiveness plateau at 0.81 is the stable attractor and the constraint is a durable snare. If veto power has a time horizon, the constraint may eventually transition to either rope or tangled rope as the occupying state negotiates a settlement to avoid forced resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(occupying_state_veto_power_sustainability, empirical, 'The temporal sustainability of the occupying state''s non-cooperation strategy').

omega_variable(
    displaced_population_powerlessness_boundary,
    'At what point does the displaced population''s powerlessness and entrapment become politically unsustainable, forcing third parties (regional powers, the ICJ, the Security Council) to impose resolution against the occupying state''s wishes?',
    'Political analysis of coalition-building among third parties, military balance changes, or shifts in the occupying state''s strategic interests that weaken its commitment to maintaining the status quo.',
    'If the boundary is crossed, the constraint transitions from snare to contested and the authority dispute may be forcibly resolved. If powerlessness remains indefinitely sustainable, the constraint persists as snare and the cost accumulates on the displaced population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_population_powerlessness_boundary, empirical, 'The temporal and political limits of the displaced population''s entrapment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(unsc_tr_t8, observed).
narrative_ontology:measurement(unsc_tr_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(unsc_tr_t16, observed).
narrative_ontology:measurement(unsc_tr_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 24, 0.43).
narrative_ontology:measurement_basis(unsc_tr_t24, observed).
narrative_ontology:measurement(unsc_tr_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 32, 0.46).
narrative_ontology:measurement_basis(unsc_tr_t32, observed).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 40, 0.47).
narrative_ontology:measurement_basis(unsc_tr_t40, observed).
narrative_ontology:measurement(unsc_tr_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 57, 0.48).
narrative_ontology:measurement_basis(unsc_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 8, 0.72).
narrative_ontology:measurement_basis(unsc_be_t8, observed).
narrative_ontology:measurement(unsc_be_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 16, 0.76).
narrative_ontology:measurement_basis(unsc_be_t16, observed).
narrative_ontology:measurement(unsc_be_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 24, 0.78).
narrative_ontology:measurement_basis(unsc_be_t24, observed).
narrative_ontology:measurement(unsc_be_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 32, 0.8).
narrative_ontology:measurement_basis(unsc_be_t32, observed).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(unsc_be_t40, observed).
narrative_ontology:measurement(unsc_be_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 57, 0.81).
narrative_ontology:measurement_basis(unsc_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t8, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(unsc_su_t8, observed).
narrative_ontology:measurement(unsc_su_t16, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(unsc_su_t16, observed).
narrative_ontology:measurement(unsc_su_t24, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(unsc_su_t24, observed).
narrative_ontology:measurement(unsc_su_t32, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(unsc_su_t32, observed).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(unsc_su_t40, observed).
narrative_ontology:measurement(unsc_su_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 57, 0.72).
narrative_ontology:measurement_basis(unsc_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause kernel has been decomposed into three constraint stories: (1) interpretive_authority_structure (this story) — the meta-dispute over who decides the clause's meaning; (2) maximal_withdrawal_reading — the substantive reading grounded in Charter Article 2(4) and French definite article; (3) partial_withdrawal_reading — the substantive reading grounded in secure boundaries principle and indefinite English article. The three stories are linked by network.affects_constraints: this story (authority structure) influences both substantive readings because the authority question prevents either from being authoritatively adopted. The substantive readings would have different ε and extracted-from seats if their respective authority bases were accepted, but this story's ε (0.81) derives from the authority dispute itself perpetuating ambiguity. All three stories share the same referent: the standing arrangement of occupied territories and the legal framework meant to govern withdrawal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
