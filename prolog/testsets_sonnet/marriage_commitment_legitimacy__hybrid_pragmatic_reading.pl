% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: The Manifesto as Strategic Institutional Adaptation (Hybrid Pragmatic Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This story instantiates the hybrid pragmatic reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto is treated
 *   neither as pure revelation (the endogenous_reinterpretation_reading) nor
 *   as pure coerced capitulation with doctrine unchanged (the
 *   exogenous_override_reading), but as a deliberately dual-purpose
 *   institutional artifact — leadership crafted language ambiguous enough to
 *   be read as prophetic guidance internally and as compliance externally,
 *   buying survival time while deferring the theological question. Under this
 *   reading the constraint is a tangled rope: it genuinely coordinates the
 *   institution's survival against an existential federal threat (real
 *   coordination function), but it also extracts certainty and
 *   consequence-bearing from rank-and-file members and dissenters who pay the
 *   ambiguity's cost through interpretive whiplash and excommunication. This
 *   is a distinct constraint from its siblings — the
 *   exogenous_override_reading has a different beneficiary/victim shape (a
 *   captured institution vs. a defiant one) and the
 *   endogenous_reinterpretation_reading treats the declaration as
 *   low-extraction genuine coordination (closer to rope or even
 *   mountain-adjacent legitimacy). Per the ε-invariance principle, each
 *   reading is authored as its own file with its own stable ε; this file does
 *   not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "The Manifesto as Strategic Institutional Adaptation (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e3073c1d-fc3f-4906-86e4-806621a536a1').
narrative_ontology:cs_kernel_codification('e3073c1d-fc3f-4906-86e4-806621a536a1', distributed).
narrative_ontology:cs_authority_grounding('e3073c1d-fc3f-4906-86e4-806621a536a1', lineage).
narrative_ontology:cs_interpretation_layer_present('e3073c1d-fc3f-4906-86e4-806621a536a1').
narrative_ontology:cs_reading_relation('e3073c1d-fc3f-4906-86e4-806621a536a1', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3073c1d-fc3f-4906-86e4-806621a536a1', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('e3073c1d-fc3f-4906-86e4-806621a536a1', foundational, prophetic_authority_admits_strategic_scope_management).
narrative_ontology:cs_axiom_status(prophetic_authority_admits_strategic_scope_management, holdable).
narrative_ontology:cs_axiom_grounding('e3073c1d-fc3f-4906-86e4-806621a536a1', prophetic_authority_admits_strategic_scope_management, conventional).
narrative_ontology:cs_axiom('e3073c1d-fc3f-4906-86e4-806621a536a1', secondary, doctrinal_continuity_preserved_beneath_practice_suspension).
narrative_ontology:cs_axiom_status(doctrinal_continuity_preserved_beneath_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('e3073c1d-fc3f-4906-86e4-806621a536a1', doctrinal_continuity_preserved_beneath_practice_suspension, instrumental).
narrative_ontology:cs_reference_frame('e3073c1d-fc3f-4906-86e4-806621a536a1', revelatory_command_continuity).
narrative_ontology:cs_drift_state('e3073c1d-fc3f-4906-86e4-806621a536a1', post_second_manifesto_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3073c1d-fc3f-4906-86e4-806621a536a1', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_survival_apparatus).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_dissenting_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Manifesto as a declaration whose scope and theological status are deliberately left ambiguous — is it revelation, policy, or forced compliance? Leadership retains discretion to invoke whichever framing best serves institutional survival in a given moment (federal negotiation, missionary respectability, internal doctrinal continuity). Controls incorporation, temple access, and succession; can escalate or relax enforcement of the new marital norm as circumstances require.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership, beneficiary).

% The organization itself (statehood prospects, restored property, legal personhood, missionary access) benefits from the ambiguity: federal authorities can treat the Manifesto as capitulation while the tradition's own memory can treat it as continued prophetic guidance. This dual-currency legitimacy is what buys survival, but it is not a named actor who collects rents personally — the benefit accrues to the institution as a persisting entity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_survival_apparatus, beneficiary,
    institutional, civilizational, analytical, national).

% Already-existing plural families bear the full weight of the scope ambiguity: told the practice is suspended but not disavowed, they receive contradictory signals about whether their marriages remain valid, whether new plural unions may still occur quietly, and whether they will be protected or abandoned by the same leadership that once commanded the practice. They have no forum to demand a clear ruling and no exit from either the marriages already contracted or the community that is their entire social world.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families, payer,
    powerless, biographical, trapped, regional).

% Members who continue plural marriage after the Manifesto, believing (with textual justification) that the declaration was policy rather than revelation, are excommunicated or prosecuted once leadership needs the compliance framing to hold. They absorb the cost of the ambiguity leadership deliberately preserved — punished for reading the Manifesto the way its authors sometimes also described it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_dissenting_practitioners, payer,
    powerless, biographical, trapped, regional).

% Wants unambiguous cessation of plural marriage as the condition for ending prosecution, seizure, and disincorporation. Is not party to the internal theological negotiation and is given only the compliance-facing version of the declaration; it has no visibility into whether the doctrine has actually changed or only the practice's enforcement posture.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, excluded,
    institutional, immediate, analytical, national).

% Study the Manifesto's drafting history, its multiple contemporaneous framings by leadership to different audiences, and its aftermath (continued clandestine plural marriages, the Second Manifesto, excommunications) to reconstruct which reading — revelation, coercion, or strategic ambiguity — best fits the documentary record.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, future_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single institutional exit ramp from an existential legal and political crisis: without some declaration ending plural marriage, the Church faced disincorporation, property seizure, and the collapse of statehood prospects. The Manifesto coordinates a controlled institutional pivot that preserves the organization's continuity.
% TRANSFER_FUNCTION: Moves legitimacy currency from the rank-and-file (who lose certainty about their marital and covenantal status) to the institution (which gains room to negotiate simultaneously with federal authorities and its own theological tradition). It also moves consequence-bearing from leadership, who authored the ambiguity, onto ordinary practitioners and dissenters, who are excommunicated for resolving the ambiguity the way leadership itself sometimes encouraged privately.
% ABSENT_VOICES: Rank-and-file plural wives and children, whose marriages and inheritance status hinge on how the ambiguity resolves, are not consulted on the declaration's wording or its later enforcement; federal negotiators see only the compliance framing and are not told the internal doctrinal preservation was also a design goal.
% DISAPPEARANCE_RATIONALE: Without the Manifesto's engineered ambiguity, the institution would have faced a binary choice — genuine doctrinal reversal (alienating a theologically committed core) or continued open defiance (risking institutional destruction). Either binary outcome would have produced a materially different Church: either a doctrinally altered one or a legally destroyed one. The ambiguity itself is load-bearing; removing it collapses the space in which the institution actually operated for the following decades.
% FOUNDING_PROBLEM: The Church faced simultaneous, mutually exclusive pressures: federal anti-polygamy prosecution threatening disincorporation and property seizure, and a theological commitment (plural marriage as divinely commanded, tied to exaltation doctrine) that could not be simply abandoned without undermining the prophetic authority that had commanded it in the first place.
% FOUNDING_PROBLEM_CORROBORATION: Federal officials and courts of the period attested the practical problem (open defiance of the Edmunds-Tucker Act) as unresolved before 1890 and substantially addressed after; contemporary and later Mormon fundamentalist splinter groups attest that the theological problem was never actually resolved, only suspended, and that the founding commitment remains live in their reading — a claim made from outside the institution's own benefiting leadership. Independent historians (non-adherent) corroborate that the declaration's language was deliberately negotiated to satisfy both audiences without settling the underlying doctrinal question.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly from 0.38 to 0.52 over 1890-1910 as leadership repeatedly redeploys the ambiguity (first to secure statehood in 1896, later to manage the 1904 Smoot hearings and the Second Manifesto) rather than resolving it once. Theater ratio starts high (0.5) — the declaration itself is largely a performative document addressed to federal audiences — and settles near 0.44 as some genuine enforcement (excommunications, the Second Manifesto's teeth) accumulates alongside continued rhetorical ambiguity. Suppression requirement falls from 0.7 to 0.58 as the crisis peak passes and enforcement becomes routinized rather than existential.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, the Manifesto looks like skillful stewardship — genuine coordination that saved the institution without doctrinal capitulation. From the trapped payer seats, the identical document produces years of uncertainty about whether one's marriage is recognized, whether new plural unions are tolerated, and whether one will be protected or sacrificed depending on the institution's momentary negotiating needs. The engine should register this as tangled_rope from the payer seats even where leadership's own seat would compute closer to rope, because the same structure that coordinates institutional survival also enforces asymmetric extraction of certainty from those least able to exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchical leadership sits as agenda_setter and beneficiary: it authored the ambiguous text, controls its later invocation, and gains negotiating room with two audiences simultaneously — d sits near the beneficiary end. The institutional survival apparatus benefits diffusely as an entity rather than through any individual's rent-collection, which is why it is listed as a non-personal beneficiary rather than an agenda_setter. Rank-and-file plural families and excommunicated dissenters are trapped payers: they cannot renegotiate their marital status, cannot appeal to an external forum, and bear the cost of a strategic ambiguity they did not design. Federal government is excluded rather than a payer or beneficiary under this reading — it receives only the compliance-facing framing and is structurally outside the internal theological negotiation the ambiguity was also serving.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential legal threat to the institution) is genuinely dead by 1900 in the narrow sense (statehood achieved 1896, major prosecution wound down), yet the ambiguity apparatus persisted and was redeployed for the 1904 Smoot hearings and the Second Manifesto — suggesting the mechanism outlived its original emergency function and became a standing tool of institutional discretion. This is why founding_problem_status is authored as contested rather than dead: the institution's own tradition treats the underlying theological question as still open in some sense, while outside observers (and especially the excommunicated dissenters) treat the emergency as having ended decades before the ambiguity stopped being exercised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intentional_vs_emergent,
    'Was the scope ambiguity in the Manifesto''s language deliberately engineered by leadership as a dual-audience strategy, or did it emerge from genuine internal disagreement among leadership about what the declaration meant, later exploited opportunistically?',
    'Comparative analysis of private correspondence and diaries among the drafting leadership versus the public declaration''s language; degree of internal consistency in private statements about the Manifesto''s theological status across the 1890s-1900s would distinguish engineered ambiguity from genuine unresolved disagreement.',
    'If deliberately engineered, the tangled_rope classification is strongly supported (asymmetric extraction by design). If emergent from genuine disagreement later exploited, the constraint is closer to a scaffold that failed to sunset, or a piton where the ambiguity persisted through inertia rather than active strategic redeployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentional_vs_emergent, empirical, 'Whether the ambiguity was engineered strategy or emergent disagreement later exploited.').

omega_variable(
    sibling_reading_partition,
    'Do the three kernel readings (hybrid_pragmatic, exogenous_override, endogenous_reinterpretation) partition the same historical record exhaustively, or is there a residual fourth reading — e.g., a reading centered on internal factional competition within leadership rather than external crisis management — that none of the three captures?',
    'Systematic review of leadership faction alignments (Woodruff vs. other apostles) during and after the Manifesto''s drafting to test whether internal power competition, rather than external crisis or genuine revelation, best explains the specific language chosen.',
    'If a factional-competition reading is structurally distinct and non-reducible to the three declared readings, a fourth kernel reading constraint file should be authored; if factional competition is adequately subsumed within hybrid_pragmatic_reading''s ''strategic adaptation'' framing, no further decomposition is needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_partition, conceptual, 'Whether the three-reading kernel partition is exhaustive or under-specified.').

omega_variable(
    victim_consent_retroactive,
    'Did excommunicated dissenting practitioners who continued plural marriage after 1890 have reasonable access to the information that leadership itself sometimes described the Manifesto as policy rather than binding revelation, such that their continued practice reflected a defensible reading rather than simple defiance?',
    'Review of contemporaneous sermons, private counsel given to specific plural families by leadership, and court testimony from excommunication proceedings to establish what dissenters were actually told versus what was publicly declared.',
    'If dissenters had access to leadership''s private ambiguous framing, their excommunication is a clearer case of extraction (punished for a reading leadership itself encouraged); if dissenters acted on outdated private assurances after leadership had firmly closed the ambiguity, the victim/beneficiary asymmetry is somewhat less severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_consent_retroactive, empirical, 'Whether excommunicated dissenters had genuine access to leadership''s private ambiguous framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1892, 0.47).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1896, 0.44).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.43).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1907, 0.44).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1910, 0.44).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.38).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1892, 0.42).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1896, 0.47).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.5).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1907, 0.51).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1910, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1892, 0.66).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1896, 0.62).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.59).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1907, 0.58).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1910, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'the Manifesto' per the ε-invariance principle. Each reading has a distinct beneficiary/victim structure and a distinct ε: exogenous_override_reading treats the institution itself as the primary victim of federal coercion (low institutional agency, doctrine held constant); endogenous_reinterpretation_reading treats the declaration as low-extraction genuine prophetic coordination (institution and members as joint beneficiaries of continuity); this hybrid_pragmatic_reading treats institutional leadership as the primary beneficiary of a deliberately preserved ambiguity, with rank-and-file members and dissenters bearing the resulting interpretive and legitimacy costs. All three are linked via affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
