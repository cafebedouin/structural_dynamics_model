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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: UNSC 242 Withdrawal Clause: Interpretive Authority Structure
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) contains a withdrawal clause whose scope is
 *   ambiguous: the French definite article ("les territoires") can be read to
 *   mandate withdrawal from all occupied territories, while the English
 *   indefinite article ("territories") permits reading withdrawal scope as
 *   discretionary. Rather than the textual ambiguity itself being the
 *   constraint, this story focuses on the META-DISPUTE: the authority to
 *   resolve which reading is correct. Three parties claim interpretive
 *   authority — the International Court of Justice (formal treaty doctrine),
 *   the drafting states (authorial intent and legislative history), and the
 *   occupying state (customary practice and strategic necessity). The
 *   unresolved authority dispute perpetuates the substantive ambiguity and
 *   extracts costs from parties seeking definitive legal closure. This is ONE
 *   READING of the contested kernel unsc_242_withdrawal_clause: the reading
 *   that instantiates the constraint as Snare by virtue of the broken
 *   authority structure itself.
 *
 * KEY AGENTS:
 *   - International Court of Justice — claims judicial interpretation authority based on Vienna Convention doctrine; issues advisory opinions that attempt systematic legal resolution
 *   - Occupying state — claims authority grounded in customary practice and military/political fait accompli; refuses to concede that the ICJ's reading should be binding
 *   - Drafting states — claim authority grounded in recorded legislative history and intent; produce conflicting accounts of what the compromise text was meant to achieve
 *   - Territorial claimants — seek definitive legal closure but are trapped in the ambiguity because no single authority's ruling carries universal acceptance
 *   - International legal community — bears the cost of doctrinal incoherence; must author frameworks that coexist with incompatible authority claims
 *   - Status quo coalition — benefits from ambiguity; includes permanent UNSC members and aligned states that profit from non-resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.71).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC 242 Withdrawal Clause: Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, 'cd4225ae-a3c7-4c42-aa12-a1342288cad2').
narrative_ontology:cs_kernel_codification('cd4225ae-a3c7-4c42-aa12-a1342288cad2', fixed_text).
narrative_ontology:cs_authority_grounding('cd4225ae-a3c7-4c42-aa12-a1342288cad2', distributed).
narrative_ontology:cs_reading_relation('cd4225ae-a3c7-4c42-aa12-a1342288cad2', unsc_242_withdrawal_clause__unsc_242_maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd4225ae-a3c7-4c42-aa12-a1342288cad2', unsc_242_withdrawal_clause__unsc_242_partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('cd4225ae-a3c7-4c42-aa12-a1342288cad2', foundational, interpretive_authority_contestable).
narrative_ontology:cs_axiom_status(interpretive_authority_contestable, holdable).
narrative_ontology:cs_axiom_grounding('cd4225ae-a3c7-4c42-aa12-a1342288cad2', interpretive_authority_contestable, conventional).
narrative_ontology:cs_axiom('cd4225ae-a3c7-4c42-aa12-a1342288cad2', secondary, unresolved_authority_perpetuates_ambiguity).
narrative_ontology:cs_axiom_status(unresolved_authority_perpetuates_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('cd4225ae-a3c7-4c42-aa12-a1342288cad2', unresolved_authority_perpetuates_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('cd4225ae-a3c7-4c42-aa12-a1342288cad2', single_binding_authority_frame).
narrative_ontology:cs_drift_state('cd4225ae-a3c7-4c42-aa12-a1342288cad2', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cd4225ae-a3c7-4c42-aa12-a1342288cad2', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, status_quo_coalition).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, territorial_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims the authority to interpret UNSC 242 based on the Vienna Convention on the Law of Treaties and judicial precedent. Issues advisory opinions and rulings that attempt to fix the textual meaning through systematic legal reasoning. Position is that treaty interpretation follows established doctrinal methods, not the parties' post-hoc assertions of intent.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter,
    institutional, generational, analytical, universal).

% Benefits from the ambiguity: the unresolved interpretive authority allows it to claim the clause permits partial withdrawal while refusing full withdrawal. The ambiguity itself becomes a tool for non-compliance. Pays enforcement costs only if a single authoritative interpretation is imposed, which the fragmented authority structure makes unlikely.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, payer).

% Claim authority to interpret based on recorded drafting intent and legislative history. Several drafting states advance conflicting accounts of intent, fragmenting the authorial voice further. They assert that the ICJ's formalist reading ignores the compromise text that produced UNSC 242.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, agenda_setter,
    organized, biographical, constrained, universal).

% Bear the cost of interpretive ambiguity: cannot secure a definitive legal ruling that mandates full withdrawal; forced to negotiate within a framework where the occupying state can claim legitimate disagreement. Their exit (unilateral claims, armed assertion) carries costs they prefer to avoid.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, territorial_claimants, payer,
    moderate, generational, trapped, regional).

% States and institutional actors that benefit from continued ambiguity: it allows them to avoid costly enforcement actions, maintain diplomatic relationships across fault lines, and defer decisions about contested territories. Includes permanent UNSC members who can veto enforcement measures and states aligned with the occupying power.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, status_quo_coalition, beneficiary,
    organized, biographical, arbitrage, universal).

% Argue that interpretive authority derives from customary international law norms and practice rather than treaty text or drafting intent. This third authority source is structurally barred from the negotiation because UNSC membership and formalist treaty doctrine dominate the interpretive frame.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, customary_law_advocates, excluded,
    moderate, generational, constrained, universal).

% Scholars, practitioners, and tribunals bear the cost of interpretive uncertainty: must author doctrinal frameworks that can coexist with incompatible rulings from different authority sources; cannot build definitive case law on UNSC 242's scope because the authority question undermines precedent.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_community, payer,
    moderate, generational, identity_locked, universal).

% Witness and analyze the meta-dispute without formal adjudicatory power. Watch how the ICJ, drafting states, and occupying state each claim interpretive authority and note that the absence of a settlement mechanism perpetuates the substantive ambiguity.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, observer_states, observer,
    moderate, biographical, mobile, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: UNSC 242 was meant to solve the withdrawal question once: provide a binding resolution on territorial restoration that all parties could reference in legal arguments and diplomatic negotiation. The kernel text exists to be interpreted and applied.
% TRANSFER_FUNCTION: The ambiguity transfers enforcement costs and legal closure from the occupying state (which would lose an argument for partial withdrawal under a definitive reading) to the territorial claimants and international legal community (which must live with perpetual ambiguity and cannot obtain the binding judgment that would settle policy). Authority to interpret is withheld from any single seat.
% ABSENT_VOICES: Customary law scholars and non-state actors are structurally excluded from the interpretive authority dispute, which is confined to formal treaty doctrine and UNSC-member diplomacy. Indigenous populations and affected communities in disputed territories have no seat at the interpretive table; affected future generations cannot represent their interests.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if the interpretive authority dispute were resolved (e.g., via a binding ICJ advisory opinion with enforcement capacity, or via a successor treaty that specified a single interpretive method) — the occupying state would lose the cover of ambiguity and would face immediate pressure for full withdrawal. Diplomatic maneuvering would shift from authority contestation to compliance negotiation. The perpetual non-resolution enables the substantive deadlock.
% FOUNDING_PROBLEM: UNSC 242 (1967) was drafted to establish a binding principle for territorial settlement: the clause on withdrawal encodes a legal standard that warring parties agreed to, so that future disputes over compliance could reference the text and its meaning.
% FOUNDING_PROBLEM_CORROBORATION: Drafting states' own legislative records show the compromise was struck on the text's wording (definite vs. indefinite article), with each side believing the resulting ambiguity favored its interpretation. Subsequent decades of ICJ advisory opinions, UNSC debates, and scholarly commentary confirm that no single authority has been accepted as definitive; the founding problem (settle the withdrawal question once) remains unsettled because the interpretive authority structure prevents settlement. Corroboration comes from the UN's own institutional history and independent legal analysis.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising (0.58→0.82 over 59 years) because the constraint's persistence depends on preventing any single interpretive authority from settling the question definitively. The occupying state extracts freedom from definitive legal obligation; the status quo coalition extracts non-enforcement of territorial restoration. Suppression rises steeply (0.42→0.71) as enforcement mechanisms are blocked by veto power and authority contestation — each authority source claims legitimacy, and their conflict produces paralysis rather than settlement. Theater rises from 0.25 to 0.58 because the interpretive apparatus (ICJ opinions, UNSC debates, scholarly commentary) performs the function of debate and deliberation while the meta-dispute prevents resolution — the machinery runs but produces no binding output. This is the piton signature: a constraint maintained by institutional inertia and performance while the primary function (legal settlement) has atrophied. Accessibility collapse is moderate (0.64) because the constraint is a creature of formal law and diplomacy; alternatives (unilateral assertion, regional hegemon imposition, successor treaties) exist but carry high political costs. Resistance is high (0.73) because territorial claimants, international legal scholars, and courts mount real resistance to the ambiguity, but the resistance is channeled into competing authority claims rather than breaking the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits marked seat divergence: occupying state vs. territorial claimants experience opposing d values (beneficiary vs. target); ICJ vs. drafting states experience conflicting authority claims despite similar institutional power; legal community experiences high d despite its position of analytical sophistication (identity-locked into a dispute they cannot resolve). The engine should compute this divergence from the structural data without adjustment.
 *
 * DIRECTIONALITY LOGIC:
 *   d-value derivation: Occupying state: powerful, mobile exit (can simply ignore adverse rulings and cite authority contestation), benefits from ambiguity → d ≈ 0.15-0.25 (full beneficiary). Territorial claimants: moderate power, trapped exit (cannot escape the ambiguity without unilateral assertion or military action), bear the cost of non-settlement → d ≈ 0.85-0.95 (full target). ICJ: institutional power, analytical exit (can withdraw from interpretation work, though this would concede authority to other sources), claims agenda-setting but lacks enforcement → d ≈ 0.45-0.55 (symmetric). Legal community: moderate power, identity-locked exit (cannot leave the profession without abandoning scholarly identity), bears the cost of doctrinal incoherence → d ≈ 0.75-0.85 (high target). Status quo coalition: organized power, arbitrage exit (can shift positions if the political alignment changes), benefits from non-resolution → d ≈ 0.10-0.30 (beneficiary). The claim/metric independence is deliberate: the constraint is CLAIMED as snare (the authority ambiguity extracts from those seeking legal closure), and the authored metrics describe highly extractive, enforced operation with rising theater — the engine computes whether the metrics confirm the claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settle the withdrawal question once) is DEAD: UNSC 242 has not settled anything; instead, it has become the site of a perpetual meta-dispute about interpretive authority that prevents settlement. The disappearance verdict is WORLD_REARRANGES: if the authority dispute were resolved (e.g., via a binding ICJ advisory opinion with enforcement capacity), the occupying state would immediately face pressure for full withdrawal and the status quo coalition would lose its cover for non-enforcement. The constraint persists because the authority structure is broken — not because the arrangement serves a live coordination function, but because the meta-dispute creates paralysis. This is the mandatrophy signature: a constraint whose founding rationale (legal settlement) has expired, but which persists because the mechanism of dispute resolution has atrophied into a tool for indefinite non-resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_legitimacy,
    'Which authority source — the ICJ''s formalist treaty interpretation, the drafting states'' legislative history, or the occupying state''s customary practice — has the strongest legitimacy claim to settle UNSC 242?',
    'A binding agreement by the UNSC member states to adopt one authority source as definitive (e.g., establish that the ICJ''s advisory opinion is binding), or a successor treaty that explicitly specifies the interpretive method.',
    'If the ICJ''s authority is accepted, the maximal_withdrawal_reading gains force and the occupying state loses its veto position. If drafting states'' intent is accepted, the reading depends on which state''s account of intent is credited. If customary practice is accepted, non-resolution itself becomes customary and the constraint becomes a Piton. The authority choice determines which substantive reading dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Which interpretive method carries the strongest normative legitimacy?').

omega_variable(
    performance_vs_function_drift,
    'Is the interpretive apparatus (ICJ rulings, UNSC debates, scholarly commentary) performing a genuine settlement function, or has it become primarily theatrical — maintaining the appearance of dispute resolution while perpetuating non-resolution?',
    'Compare the density and binding force of ICJ opinions on UNSC 242 over time: if opinion volume rises while compliance does not, theater has risen relative to function. Track UNSC enforcement votes: if blocked or abstained, enforcement capacity has eroded relative to debate.',
    'High theater and rising theater_ratio suggest the constraint is drifting toward Piton (degraded function maintained by institutional inertia). If theater dominates, the constraint extracts less from legal closure and more from institutional performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_function_drift, empirical, 'Has the interpretive machinery retained functional legitimacy or become performative?').

omega_variable(
    identity_lock_binding_for_legal_community,
    'Is the international legal community''s inability to resolve the authority dispute a structural constraint (they are trapped by the ambiguity), or an internalized constraint (they have accepted the ambiguity as an inevitable feature of treaty law)?',
    'Post-authority-resolution observation: if a single authority source becomes accepted and the legal community quickly moves to apply it, the constraint was structural (they were trapped). If the community continues to cite the ambiguity even after one source is accepted, the constraint has become internalized (they have fused their identity with uncertainty).',
    'If internalized, the suppression is lower than the structural measure suggests — the legal community carries the constraint with them even if external authority shifts. If structural, a shift in accepted authority would quickly break the community''s paralysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_binding_for_legal_community, empirical, 'Is the legal community''s constraint structural or internalized?').

omega_variable(
    kernel_reading_divergence,
    'Does the interpretive_authority_structure reading genuinely coexist with the maximal and partial_withdrawal readings, or does resolving the authority question logically foreclose one of the substantive readings?',
    'Theoretical: if the ICJ''s authority is accepted and it rules definitively for maximal withdrawal, the partial reading is foreclosed as non-binding legal doctrine (though some parties might still assert it extra-legally). Conversely, if the authority question is resolved in favor of drafting states'' intent and they attest to a partial-withdrawal compromise, the maximal reading is foreclosed. This reading remains live until the authority question is settled.',
    'If this reading forecloses either substantive sibling, the relation should be `forecloses`, not `coexists_with`. If the authority resolution would foreclose only one sibling, the structure is not symmetric coexistence but rather authority-determined foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Is the authority-structure reading truly coexistent with both substantive siblings, or does authority resolution foreclose one of them?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(unsc_tr_t1967, observed).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(unsc_tr_t1980, observed).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1995, 0.45).
narrative_ontology:measurement_basis(unsc_tr_t1995, observed).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2010, 0.53).
narrative_ontology:measurement_basis(unsc_tr_t2010, observed).
narrative_ontology:measurement(unsc_tr_t2020, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2020, 0.56).
narrative_ontology:measurement_basis(unsc_tr_t2020, observed).
narrative_ontology:measurement(unsc_tr_t2026, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(unsc_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement_basis(unsc_be_t1967, observed).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement_basis(unsc_be_t1980, observed).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement_basis(unsc_be_t1995, observed).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement_basis(unsc_be_t2010, observed).
narrative_ontology:measurement(unsc_be_t2020, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement_basis(unsc_be_t2020, observed).
narrative_ontology:measurement(unsc_be_t2026, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2026, 0.82).
narrative_ontology:measurement_basis(unsc_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.42).
narrative_ontology:measurement_basis(unsc_su_t1967, observed).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1980, 0.54).
narrative_ontology:measurement_basis(unsc_su_t1980, observed).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement_basis(unsc_su_t1995, observed).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(unsc_su_t2010, observed).
narrative_ontology:measurement(unsc_su_t2020, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(unsc_su_t2020, observed).
narrative_ontology:measurement(unsc_su_t2026, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(unsc_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.18).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The kernel unsc_242_withdrawal_clause decomposes into three constraint stories per the ε-invariance principle: (1) interpretive_authority_structure (this story) — the meta-dispute over who decides, ε high because authority ambiguity itself perpetuates substantive ambiguity; (2) maximal_withdrawal_reading — the reading favored by definite-article interpretation and territorial-integrity doctrine, ε varies by which beneficiary/victim is assessed; (3) partial_withdrawal_reading — the reading favored by drafting-state intent and strategic-necessity arguments, ε varies by assessment seat. The three stories share a kernel (the contested text) but have different ε-values because the constraint OBJECT differs: story 1 is about interpretive authority, stories 2 and 3 are about substantive withdrawal scope. This story (interpretive_authority_structure) instantiates the snare because the meta-dispute enables both substantive readings to remain live indefinitely — neither can be definitively refuted or accepted because no single authority source is accepted as binding. The network links show how authority contestation propagates into substantive deadlock.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__interpretive_authority_structure, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
