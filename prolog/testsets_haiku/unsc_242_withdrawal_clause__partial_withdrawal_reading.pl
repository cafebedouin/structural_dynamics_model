% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_partial_withdrawal, []).

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
 *   human_readable: UNSC Resolution 242 Withdrawal Clause (Partial Withdrawal Reading)
 *   domain: international_law/diplomatic_negotiation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) mandates 'withdrawal of Israeli
 *   armed forces from territories occupied in the recent conflict.' The
 *   textual ambiguity — specifically the indefinite article in the English
 *   version ('from territories' rather than 'the territories') — has anchored
 *   decades of diplomatic negotiation over the scope and pace of Israeli
 *   withdrawal. The partial-withdrawal reading interprets this indefiniteness
 *   as deliberate: the drafters intended to preserve the occupying state's
 *   discretion to negotiate which territories, under what conditions, and on
 *   what timeline. The 'secure boundaries' principle is invoked to justify
 *   the retention of strategically significant areas. This reading
 *   instantiates a constraint that benefits the occupying military power and
 *   international mediators while extracting from territorial claimants who
 *   lack a fixed enforcement line. The constraint is not purely extractive
 *   (it coordinates the termination framework and permits negotiation), but
 *   it asymmetrically distributes the benefits of ambiguity to the occupying
 *   power and the costs to claimants.
 *
 * KEY AGENTS:
 *   - occupying_military_power: Controls territory; interprets indefiniteness as permitting phased/conditional withdrawal; sets the agenda for implementation terms
 *   - mediation_gatekeepers: International mediators, guarantor states, UN bureaucracy; collect influence and relevance by managing the implementation ambiguity
 *   - territorial_claimants: States and populations claiming the occupied territories; must negotiate over what 'withdrawal' requires rather than invoking fixed obligation
 *   - displaced_populations: Powerless; bear costs of continued occupation; excluded from interpretation authority
 *   - maximal_withdrawal_advocates: Excluded from this reading's framework; their core premise (definite article controls, full withdrawal mandatory) is foreclosed by the partial-withdrawal reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.71).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause (Partial Withdrawal Reading)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_negotiation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '1110c2bd-3628-49dd-ac62-f4cd5f964d6c').
narrative_ontology:cs_kernel_codification('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', fixed_text).
narrative_ontology:cs_authority_grounding('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', extraction).
narrative_ontology:cs_interpretation_layer_present('1110c2bd-3628-49dd-ac62-f4cd5f964d6c').
narrative_ontology:cs_reading_relation('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', unsc_242_withdrawal_clause__maximal_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', foundational, indefinite_article_encodes_occupying_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_encodes_occupying_discretion, holdable).
narrative_ontology:cs_axiom_grounding('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', indefinite_article_encodes_occupying_discretion, conventional).
narrative_ontology:cs_axiom('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', foundational, secure_boundaries_permits_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permits_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', secure_boundaries_permits_strategic_retention, instrumental).
narrative_ontology:cs_reference_frame('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', indefinite_discretionary_withdrawal).
narrative_ontology:cs_drift_state('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', contemporary_post_2010, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1110c2bd-3628-49dd-ac62-f4cd5f964d6c', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediation_gatekeepers).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, indefinite_article_ambiguity_doctrine).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__partial_withdrawal_reading, secure_boundaries_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the territories in question and interprets Resolution 242's indefinite English phrasing ('from territories') as permitting phased, conditional withdrawal. Maintains that the 'secure boundaries' principle justifies retention of strategically important areas. Uses the textual ambiguity to negotiate the terms and scope of withdrawal, trading territorial concessions for security arrangements. The constraint's persistence depends on this power's ability to enforce its reading and defend against alternative interpretations.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power, agenda_setter,
    institutional, generational, arbitrage, regional).

% International mediators, guarantor powers, and the UN apparatus collect influence and structural relevance by managing the ambiguity. The more indefinite the withdrawal obligation, the more mediation is required to negotiate the 'terms' of compliance. They benefit from the constraint's indefiniteness by positioning themselves as necessary intermediaries between the occupying power and territorial claimants. Their authority and fee-collection depend on the ambiguity remaining live.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediation_gatekeepers, beneficiary,
    institutional, generational, arbitrage, global).

% States and populations claiming the occupied territories. Under this reading, they cannot point to a fixed, unambiguous withdrawal obligation; instead they must negotiate with the occupying power and mediators over what 'withdrawal from territories' actually requires. They bear the cost of continued occupation, fragmented governance, and the erosion of territorial claims while waiting for 'implementation' of an ambiguous mandate. Exit would mean abandoning the claim entirely or pursuing non-legal remedies.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimants, payer,
    moderate, generational, constrained, regional).

% Residents of the occupied territories and those displaced by occupation. They bear the tangible costs of continued occupation (military control, settlement restrictions, fragmented services, loss of property). They are excluded from the textual interpretation debate — which reading controls is decided by states and international lawyers, not by those living under the constraint. Their exit is flight or permanent displacement; they have no seat in the negotiation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations, excluded).

% The International Court of Justice claims authority to resolve textual ambiguities in the Charter and resolutions through judicial interpretation. Under this reading, the ICJ's role is advisory: the court can issue opinions on what the text 'means,' but the occupying power retains practical discretion over compliance scope. The tension between judicial authority and practical enforcement is the structural gap this reading leaves open.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, icj_interpretive_authority, observer,
    institutional, generational, analytical, global).

% The UN member states that drafted Resolution 242, especially the permanent members who negotiated the compromise text. This reading claims to honor their intent: the English phrasing ('from territories') was deliberately chosen to avoid the French definite article ('from the territories'), encoding flexibility into the withdrawal obligation. Drafting-state intent is invoked as the authoritative frame, yet that intent is itself contested and unwritten.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, charter_drafting_states, observer,
    institutional, generational, analytical, global).

% International law scholars, states, and advocacy groups that read Resolution 242 through the maximal-withdrawal lens: that indefiniteness is an accident of translation, the true mandate (especially in the French version) is complete withdrawal, and the occupying power has no discretion to retain territories. They are excluded from this reading's framework — their core premise is foreclosed by the partial-withdrawal reading's adoption of the indefinite article as authoritative.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, maximal_withdrawal_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolution 242 coordinates a framework for conflict termination post-1967: cessation of hostilities, withdrawal from occupied territories, mutual recognition, and refugee solutions are bundled into a single mandate requiring international mediation and negotiation to implement. The indefiniteness of 'withdrawal from territories' (rather than 'the territories') permits phased negotiation of what 'implementation' requires, allowing the occupying power and claimants to reach settlement terms without either side pre-committing to fixed boundaries.
% TRANSFER_FUNCTION: Moves interpretive authority and negotiating leverage from the textual mandate to the occupying power and mediators: the more ambiguous the withdrawal obligation, the more the occupying power can condition withdrawal on security guarantees, boundary adjustments, or phased implementation. Mediators extract relevance and structural necessity by managing the gap between textual obligation and practical implementation. Claimants lose the ability to invoke a fixed, unambiguous right and must instead negotiate from a weaker position.
% ABSENT_VOICES: Displaced populations and residents of occupied territories have no seat in the interpretation of Resolution 242; their situation is decided by state diplomacy and international legal doctrine. Maximal-withdrawal advocates and states aligned with territorial claimants are also excluded from this reading's authoritative framework — their arguments are treated as alternative readings rather than legitimate contestation within the same legal system.
% DISAPPEARANCE_RATIONALE: If the indefinite-article reading of Resolution 242 disappeared and maximal-withdrawal mandates took its place, the occupying power would lose its negotiating discretion, territorial claimants would gain a fixed, unambiguous legal claim, mediation gatekeepers would lose their necessity, and the conflict's terms would shift from open-ended negotiation to implementation of a closed mandate. Phased negotiations would be replaced by compliance demands; the occupying power would face either full withdrawal or direct breach of binding obligation.
% FOUNDING_PROBLEM: The 1967 war created occupation and territorial disputes that required a UN mandate capable of being accepted by all permanent members. The Charter's territorial integrity principle (Article 2(4)) and the doctrine of non-acquisition by force (uti possidetis) created pressure for withdrawal, yet the occupying power and its allies sought discretion over scope and timing. Resolution 242's indefinite phrasing ('from territories' rather than 'the territories') was a compromise encoding that flexibility into the legally binding text.
% FOUNDING_PROBLEM_CORROBORATION: The occupying power and its allies attest that the founding problem required a negotiable, phased mandate. Maximal-withdrawal advocates and the territorial claimants attest the founding problem required an unambiguous withdrawal obligation; they cite the French text and Charter Article 2(4) as evidence that the indefiniteness was a drafting error, not intent. Independent international law scholarship is divided; major commentators (Schwebel, Rostow on one side; Crawford, Brownlie on the other) contest whether the drafters deliberately encoded ambiguity or whether translation variants obscured a single intended meaning.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.62 at interval end) because the constraint provides genuine coordination (cessation framework, phased negotiation, mediation structure) while systematically advantaging the occupying power in scope determination. Suppression is high (0.71) because maintaining the constraint requires the occupying power to defend the indefinite-article reading against maximal-withdrawal challenges and to suppress the direct application of Charter Article 2(4)'s territorial integrity default. Theater is moderate (0.48): some portion of the enforcement activity consists of diplomatic theatre — rhetorical invocation of 'secure boundaries' and 'phased implementation' — while a substantial portion is structural (military control, gated access to negotiation, control over what counts as 'implementation'). The measurement series trace the constraint's operation over 55 years: extractiveness and suppression rise through the 1970s–1990s (the period of most intensive negotiation and competing interpretive claims), stabilize in the 2000s–2010s, and show slight decay in the projected period as the founding problem's political salience shifts. Theater peaks mid-interval (time 32–40) when the constraint's rhetorical maintenance required maximum diplomatic effort.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power experiences the constraint as coordination with discretionary scope — a framework that permits both implementation and strategic retention. The mediation gatekeepers experience it as necessary intermediary function — the constraint generates demand for their services. Territorial claimants experience it as a fixed obligation rendered unenforceable by textual ambiguity — a constraint that blocks their primary claim while permitting negotiation of secondary terms. Displaced populations experience it as military occupation with indefinite duration — they have no seat in the interpretation and no path to exit except flight. These perceptual divergences arise from the structural asymmetry: the constraint's indefiniteness is an asset for some (occupying power, mediators) and a liability for others (claimants, displaced). The engine's per-seat classification should surface this divergence as Type variation across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power sits at the beneficiary end of directionality (d ≈ 0.1–0.2): the constraint's entire function is to preserve its discretion, and its exit options are arbitrage-grade (it can comply fully, comply partially with negotiated exceptions, or withdraw selectively and claim compliance with a new interpretation). Mediation gatekeepers sit near symmetric (d ≈ 0.4–0.5): they benefit from the constraint's necessity but must defend it against collapse, so their position is genuinely dual. Territorial claimants sit at the target end (d ≈ 0.75–0.85): they bear the extraction (continued occupation without a fixed withdrawal date) and have severely constrained exit (they can abandon the claim, accept subordinate status, or pursue military challenge outside the legal framework). Displaced populations sit at maximum extraction (d ≈ 0.9): they have trapped exit and bear the full cost of the constraint's operation with no benefit or voice. These directionality differences should produce per-seat type variation: from the occupying power's seat, this is genuine coordination (Rope or Tangled Rope on the beneficiary side); from the claimant seat, this is enforced extraction (Snare on the victim side). The engine computes this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1967 conflict termination requiring negotiated withdrawal) was live at Resolution 242's adoption and remains live in some form (the occupying power retains territories that claimants dispute; no final status agreement has closed the question). However, the mechanism for solving that problem has substantially atrophied: the indefinite-article reading was credible as a transitional framework in the 1970s–1980s when active, time-bound negotiations were ongoing; today the same indefiniteness functions primarily as a cover for indefinite retention rather than as a time-limited negotiating tool. The constraint has shifted from Scaffold (a transitional coordination framework with an implicit sunset contingent on settlement) to Piton (a mechanism maintained theatrically long after its functional purpose has atrophied). The theater_ratio's plateau at ~0.48 reflects this: diplomatic rhetoric about 'final status negotiations' and 'secure boundaries' persists, but the underlying coordination function has not produced a settlement in 55 years. The constraint is mandatrophic: it solved its founding problem (termination of active fighting, establishment of a negotiation framework) but persists as a barrier to solving the next problem (final territorial settlement, refugee return). Declaring mandatrophy_resolved would be premature — the constraint's function has atrophied but its enforcement structure (military occupation, mediation gatekeeping, textual authority disputes) remains active. The constraint is in the Piton-failure trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_article_authorial_intent,
    'Did the drafters of Resolution 242 deliberately choose the indefinite English article (''from territories'') to encode occupying-power discretion, or is the indefiniteness an accident of translation — a side effect of rendering the French ''des territoires'' (which some read as plural-definite) into English indefinite form?',
    'Declassification of drafting-state position papers, negotiation records, and UN Secretariat drafting notes from the 1967 period. Comparative analysis of French and English originals alongside drafting-state communications at the time.',
    'If deliberate: the partial-withdrawal reading''s core premise is vindicated, and the constraint is structurally legitimate as a phased negotiation framework. If accidental: the indefiniteness is a drafting error, and maximal-withdrawal readings gain authority. If ambiguous (genuinely contested intent): the constraint''s persistence depends on the occupying power''s ability to defend its interpretation against challenges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indefinite_article_authorial_intent, empirical, 'Whether the indefinite article represents deliberate drafting intent or translation artifact').

omega_variable(
    secure_boundaries_principle_scope,
    'What does the ''secure boundaries'' principle permissibly justify in terms of territorial retention? Does ''secure'' mean military-strategic necessity (the occupying power''s reading), or defensive sufficiency without territorial expansion (the maximal-withdrawal reading)?',
    'Comparative jurisprudence of territorial law: how other international bodies and courts have interpreted ''secure boundaries'' in analogous border disputes; ICJ advisory opinions on Charter Article 2(4) and customary law principles; security assessments from neutral sources establishing what territorial holdings are necessary vs. strategic.',
    'If ''secure'' permits substantial retention based on strategic military judgment: the partial-withdrawal reading gains authority and the constraint remains highly extractive. If ''secure'' is limited to demonstrable defensive necessity: the constraint''s extractiveness is reduced and phased withdrawal becomes more obligatory. If the principle is unresolved: the constraint remains indefinite and extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_principle_scope, conceptual, 'Whether ''secure boundaries'' justifies strategic territorial retention or only defensive minimum holdings').

omega_variable(
    interpretive_authority_hierarchy,
    'Which interpretive authority is legitimate for resolving textual ambiguities in Resolution 242: the International Court of Justice (judicial interpretation), the drafting states (authorial intent), customary international law (practice-based interpretation), or the occupying state (de facto compliance authority)?',
    'Meta-legal analysis of the UN Charter''s dispute-resolution provisions (Chapter VI judicial role, General Assembly authority, Security Council primacy); comparative review of how disputes over other Security Council resolutions have been adjudicated; examination of whether the interpretive authority dispute is itself resolvable or structurally inherent.',
    'If ICJ authority is decisive: judicial interpretation could settle the question via advisory opinion (though enforcement would still depend on state compliance). If drafting-state intent is decisive: declassified records become controlling. If occupying-state practice is decisive: the constraint''s current extraction maintains until a counterforce challenges it. If the question is structurally unresolvable: the interpretive dispute is permanent, and the constraint persists through perpetual contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_hierarchy, conceptual, 'Which authority has the right to resolve ambiguity in the withdrawal clause — judicial, authorial, customary, or de facto').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural — enforced by military control, gated access to mediation, and institutional barriers to maximal-withdrawal interpretation — or internalized — territorial claimants have culturally or politically accepted indefinite retention as inevitable, and would resist re-engagement even if barriers were removed?',
    'Post-barrier counterfactual: if mediation gatekeepers withdrew or neutral arbitration was offered, would claimants re-engage with enforcement vigor, or would they treat the constraint as permanent? Comparative analysis of suppression trajectories in analogous territorial disputes that experienced barrier removal (e.g., decolonization processes, Cold War territorial settlements). Ethnographic and discourse analysis of claimant-state and population attitude shifts.',
    'If primarily structural: the constraint''s suppression is contingent on the occupying power''s enforcement capacity; removal of gatekeepers could shift the balance. If internalized: the suppression persists even after structural barriers are lifted; claimants have absorbed the indefiniteness as legitimate or unchangeable. If mixed: identifying the ratio of structural to internalized suppression would clarify where intervention could be most effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is enforced by external barriers or internalized by claimants as inevitable').

omega_variable(
    partial_vs_maximal_reading_foreclosure,
    'Do the partial-withdrawal and maximal-withdrawal readings logically foreclose each other (only one can be true within any single legal framework), or do they coexist as live interpretive options held by different parties with genuinely opposed material interests?',
    'Structural logic test: can a single legal interpreter (e.g., the ICJ or a treaty body) hold both readings simultaneously without internal contradiction? If yes, they coexist; if no, one forecloses the other. Examine whether the readings differ in premises (one says the article is indefinite, one says it''s definite) — if so, foreclosure applies — or whether they differ in implications (both agree on the text but disagree on what it justifies) — if so, coexistence applies.',
    'If partial forecloses maximal: adoption of the partial reading rules out the maximal reading as legally incoherent, and the maximal-withdrawal constraint becomes unsustainable. If they coexist: both readings remain live options, and the dispute is managed through negotiation rather than resolution. Determining this is essential for understanding whether the two readings describe one constraint viewed from two seats, or two genuinely different constraints with incompatible structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partial_vs_maximal_reading_foreclosure, conceptual, 'Whether partial-withdrawal and maximal-withdrawal readings logically foreclose each other or coexist as live options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(unsc_tr_t8, observed).
narrative_ontology:measurement(unsc_tr_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement_basis(unsc_tr_t16, observed).
narrative_ontology:measurement(unsc_tr_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(unsc_tr_t24, observed).
narrative_ontology:measurement(unsc_tr_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement_basis(unsc_tr_t32, observed).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement_basis(unsc_tr_t40, observed).
narrative_ontology:measurement(unsc_tr_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 48, 0.48).
narrative_ontology:measurement_basis(unsc_tr_t48, projected).
narrative_ontology:measurement(unsc_tr_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 55, 0.48).
narrative_ontology:measurement_basis(unsc_tr_t55, projected).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(unsc_be_t8, observed).
narrative_ontology:measurement(unsc_be_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(unsc_be_t16, observed).
narrative_ontology:measurement(unsc_be_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(unsc_be_t24, observed).
narrative_ontology:measurement(unsc_be_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement_basis(unsc_be_t32, observed).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(unsc_be_t40, observed).
narrative_ontology:measurement(unsc_be_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 48, 0.61).
narrative_ontology:measurement_basis(unsc_be_t48, projected).
narrative_ontology:measurement(unsc_be_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 55, 0.62).
narrative_ontology:measurement_basis(unsc_be_t55, projected).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t8, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(unsc_su_t8, observed).
narrative_ontology:measurement(unsc_su_t16, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(unsc_su_t16, observed).
narrative_ontology:measurement(unsc_su_t24, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(unsc_su_t24, observed).
narrative_ontology:measurement(unsc_su_t32, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement_basis(unsc_su_t32, observed).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(unsc_su_t40, observed).
narrative_ontology:measurement(unsc_su_t48, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement_basis(unsc_su_t48, projected).
narrative_ontology:measurement(unsc_su_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 55, 0.71).
narrative_ontology:measurement_basis(unsc_su_t55, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.22).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, icj_territorial_integrity_doctrine).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, customary_uti_possidetis_norm).

% DUAL FORMULATION NOTE:
% The partial-withdrawal reading of UNSC 242 is one of three constraints decomposing the same kernel. The maximal-withdrawal reading interprets the same text with a different reference frame (definite article control, Charter 2(4) default). The interpretive-authority-structure reading addresses the meta-question of who has the authority to choose between readings. All three share the same referent (the 1967 withdrawal mandate) but instantiate different ε values and beneficiary structures due to reading-specific framing. Each is a complete, ε-invariant constraint story; they are linked via network.affects_constraints to show the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, moderate, 0.82).
constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, powerless, 0.91).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
