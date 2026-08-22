% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation: International Regime Reading
 *   domain: international_law/space_law/commons_governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) declares that celestial
 *   bodies and space are the 'province of all mankind' and subject to a
 *   non-appropriation principle. The international_regime reading treats
 *   Article II as deliberately ambiguous on whether 'non-appropriation' bars
 *   private resource extraction by companies or only sovereign territorial
 *   claims by states. It interprets the deferral to a future international
 *   regime (Article XI) as a structural feature, not a gap: the treaty
 *   intentionally preserved regime-negotiation authority for when space
 *   activity became technically feasible and economically material. This
 *   reading competes with the conservation_reading (which reads
 *   non-appropriation as a substantive prohibition on extraction) and the
 *   extraction_permissive reading (which reads it as barring sovereignty but
 *   permitting private resource ownership). The international_regime reading
 *   acknowledges legal uncertainty as its defining feature — neither
 *   extraction nor conservation has treaty authority absent a binding
 *   multilateral regime. First-mover firms and spacefaring states benefit
 *   from this uncertainty because it permits operational freedom;
 *   resource-dependent and future actors bear its costs because they are
 *   locked into whatever precedent accumulates in the grey zone.
 *
 * KEY AGENTS:
 *   - First-mover firms: private companies operating in-space extraction and resource surveys, benefiting from regulatory grey zone
 *   - Space-technology states: nations with advanced capabilities (USA, EU, Russia, China, Japan) that dominate regime negotiation and preserve their firms' operational freedom
 *   - Resource-dependent developing states: powerless parties locked into waiting for a regime that never solidifies; their negotiating leverage erodes as first-movers accumulate precedent
 *   - Future space actors: civilizational-horizon stake-holders whose rights are determined by what the grey zone permits now
 *   - UN Committee on Peaceful Uses: the formal agenda-setter for regime development, constrained by zero-sum distributional conflict
 *   - Conservation advocates: excluded from formal authority but asserting a competing reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.58).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.42).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation: International Regime Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_law/space_law/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'b37e4dd0-6971-4425-82ee-cb2321e14b32').
narrative_ontology:cs_kernel_codification('b37e4dd0-6971-4425-82ee-cb2321e14b32', fixed_text).
narrative_ontology:cs_authority_grounding('b37e4dd0-6971-4425-82ee-cb2321e14b32', lineage).
narrative_ontology:cs_interpretation_layer_present('b37e4dd0-6971-4425-82ee-cb2321e14b32').
narrative_ontology:cs_reading_relation('b37e4dd0-6971-4425-82ee-cb2321e14b32', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('b37e4dd0-6971-4425-82ee-cb2321e14b32', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_axiom('b37e4dd0-6971-4425-82ee-cb2321e14b32', foundational, article_ii_ambiguity_intentional_deferral).
narrative_ontology:cs_axiom_status(article_ii_ambiguity_intentional_deferral, holdable).
narrative_ontology:cs_axiom_grounding('b37e4dd0-6971-4425-82ee-cb2321e14b32', article_ii_ambiguity_intentional_deferral, conventional).
narrative_ontology:cs_axiom('b37e4dd0-6971-4425-82ee-cb2321e14b32', foundational, multilateral_regime_binding_authority_required).
narrative_ontology:cs_axiom_status(multilateral_regime_binding_authority_required, holdable).
narrative_ontology:cs_axiom_grounding('b37e4dd0-6971-4425-82ee-cb2321e14b32', multilateral_regime_binding_authority_required, conventional).
narrative_ontology:cs_reference_frame('b37e4dd0-6971-4425-82ee-cb2321e14b32', treaty_deference_to_future_regime).
narrative_ontology:cs_drift_state('b37e4dd0-6971-4425-82ee-cb2321e14b32', contemporary_grey_zone_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b37e4dd0-6971-4425-82ee-cb2321e14b32', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, space_technology_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, resource_dependent_developing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_space_actors).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, treaty_deference_principle).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, multilateral_regime_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private companies from wealthy states that operate in-space resource operations (asteroid mining, lunar extraction, orbital manufacturing). The regulatory uncertainty permits them to explore and claim resource deposits before any regime-wide rules bind. They benefit from the grey zone because they can establish operational precedent and technical infrastructure while no extraction prohibitions yet apply. An extraction-permissive future affirms their investments; a conservation-based regime would retroactively delegitimize their operations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Nations with advanced space capabilities (USA, EU states, Russia, China, Japan) and the industrial base to support private space activities. They benefit from the international_regime reading because it defers a binding prohibition indefinitely, preserving their firms' operational freedom and avoiding a negotiating loss in a future multilateral treaty. Their ability to withdraw from a regime-negotiation process (or to dominate it) gives them exit options unavailable to non-spacefaring states.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_technology_states, beneficiary,
    institutional, generational, mobile, global).

% Countries with limited space technology but significant economic dependence on terrestrial resource extraction. The international_regime reading defers a binding rule that would protect their interest in preventing off-world resource flooding (which would crash commodity prices) or in equitable benefit-sharing from space resources. They bear the cost of legal uncertainty: they cannot negotiate a protective regime while the grey zone permits first-movers to operate. Once extraction is technically operational, their negotiating leverage for benefit-sharing evaporates.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, resource_dependent_developing_states, payer,
    powerless, generational, trapped, global).

% Spacefaring nations and firms that emerge after the current first-mover period. They are locked into whatever precedent the grey zone establishes: if first-movers claim resource deposits and establish operational control, future actors must negotiate around those claims. The deferral reading directly imposes costs on them by locking in first-mover advantage. They have no voice in the current treaty interpretation because they do not yet exist as actors.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, future_space_actors, payer,
    powerless, civilizational, trapped, global).

% The UN body charged with coordinating space-law regime development under Article XI. It formally sets the agenda for any future international regime but faces zero-sum distributional conflict: spacefaring states will not agree to a conservation-based regime; resource-dependent states will not accept an extraction-permissive regime without benefit-sharing. The deferral reading leaves the committee in negotiating stalemate, with no authority to impose rules in the interim grey zone.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, un_committee_on_peaceful_uses, agenda_setter,
    institutional, generational, constrained, global).

% Environmental organizations, indigenous-rights advocates, and commons-protection movements that read Article II as prohibiting appropriation outright. They are excluded from formal treaty-negotiation authority and cannot enforce the conservation reading without a binding regime. Their position is treated as a policy proposal rather than a legal interpretation with standing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, conservation_advocacy_coalition, excluded,
    moderate, generational, constrained, global).

% International lawyers, policy analysts, and academic commentators who document the contest between readings and track whether the grey zone produces de facto regimes (precedent, operational norms, industry standards) that substitute for formal international law.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__international_regime, space_technology_states).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__international_regime, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defers binding rules on space resource appropriation to a future multilateral negotiation, preserving treaty-interpretation authority in a dedicated regime-building process rather than locking in a permanent rule now. Coordinates the commitment to future regime-building and prevents either reading from imposing unilateral authority on the other parties.
% TRANSFER_FUNCTION: The deferral redistributes negotiating power from immediate rule-setting to time-dependent technological and economic facts: the longer the grey zone persists, the more operational precedent first-movers accumulate, which weakens resource-dependent states' ability to negotiate benefit-sharing in the eventual regime. The transfer is of negotiating leverage, not material resources — yet.
% ABSENT_VOICES: Future spacefaring and resource-dependent states have no representation in current treaty interpretation and negotiation. They will inherit whatever precedent the grey zone establishes. Conservation advocates have no formal standing in regime negotiation and must operate through diplomatic pressure and soft-law proposals.
% DISAPPEARANCE_RATIONALE: If the deferral reading disappeared (if a binding regime were suddenly imposed), the distribution of space-resource rights would be fundamentally altered: extraction-permissive parties would lose their operational freedom; conservation-reading parties would win a prohibition; developing states would either gain benefit-sharing rights or be locked into a market-based regime. The entire geopolitical economy of space would reorganize. If the deferral were removed and the space was treated as unregulated, each reading would claim victory and operate unilaterally, triggering a conflict-of-laws scenario.
% FOUNDING_PROBLEM: The Outer Space Treaty was negotiated in 1967 during the Cold War by a small set of spacefaring powers. Article II bars sovereignty claims but is ambiguous on whether 'non-appropriation' extends to resource extraction by private actors or only to territorial claims by states. The founding problem was to defer the resource-appropriation question until space activity became technically and economically material — because if locked in one direction prematurely, either spacefaring states or resource-dependent states would face permanent disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by legal historians and space-law specialists (outside the current benefiting parties): the treaty's negotiating record shows deliberate ambiguity on resource extraction to permit future consensus-building. Current regime-negotiation stalemate and technological acceleration (private asteroid mining, lunar resource surveys) confirm the problem remains live. Spacefaring states and first-mover firms dispute whether regime-building is urgent; resource-dependent states and conservation advocates attest the deferral itself now creates harm by permitting first-mover advantage to accumulate uncontested.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement runs at 0.58 by interval end, moderately high but not peak-snare territory, because the reading's core claim is that no party yet has treaty authority to extract or to prevent extraction — the cost is uncertainty, not active oppression. The suppression (0.42) is lower than theater (0.67) because the grey zone's persistence does not rest on coercive suppression; it rests on stalemate: spacefaring states will not agree to conservation, and conservation parties cannot enforce a prohibition without a regime. The theater ratio (0.67 at end) rises sharply over the interval because the formal regime-building machinery (UNCOPUOS committees, diplomatic initiatives, soft-law proposals) operates as theater — no binding authority emerges from it, but diplomatic activity continues to signal commitment to future resolution. The accessibility_collapse (0.48) is moderate because the grey zone itself is the open alternative: first-movers can operate; developing states can pressure for future benefit-sharing; both readings remain live. The resistance (0.71) is high because conservation advocates and resource-dependent states actively contest the extraction-permissive interpretation, even without formal treaty standing. The measurement series shows extractiveness growing slowly (from 0.35 to 0.58) as first-mover operations accumulate and set de facto norms, while theater ratio rises faster (0.45 to 0.67) because regime negotiation becomes more visible (and more futile) as the stakes clarify.
 *
 * PERSPECTIVAL GAP:
 *   From the spacefaring-states and first-mover-firms seats, the deferral is genuine coordination: it preserves flexibility for future regime-building and avoids locking in a conservation prohibition prematurely. From the resource-dependent and future-actor seats, the deferral is extractive — it permits first-movers to establish operational and legal precedent that will constrain future regime negotiations. From the UN Committee seat, the deferral is structural paralysis: the committee cannot impose authority without spacefaring-state consent, but spacefaring states veto any regime that restricts extraction. Each seat should compute a different type from the engine: the spacefaring-state seat may compute rope (genuine future-binding coordination); the resource-dependent seat computes snare (locked into a distribution they did not negotiate); the committee seat computes piton (formal authority that persists through theatrical activity but has no functional power). The international_regime reading is CLAIMED as scaffold because it acknowledges the deferral itself as the constraint — the absence of a binding rule is what structures the situation.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms and spacefaring states benefit from legal uncertainty and receive the extracted negotiating advantage (they defer while accumulating precedent); they sit near d=0.0 (full beneficiary). Resource-dependent states bear the cost of not being able to negotiate a regime that protects their interests (commodity-price protection, benefit-sharing); they sit near d=1.0 (full target). The UN Committee and conservation advocates sit higher on directionality (0.5–0.7) because they nominally have standing but lack enforcement power — they are targets of the stalemate that the deferral maintains. Future actors, though unnamed in the stakeholder list, carry the highest structural directionality (approaching 1.0) because they will inherit whatever precedent the grey zone establishes without having participated in its formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulate space-resource appropriation after technological maturity) was live in 1967 and remains live now (2026). The deferral reading asserts that the treaty's ambiguity on extraction was intentional — a way to preserve regime-negotiation space. But the founding problem has not been solved; regime negotiation has stalled. The threat of mandatrophy is high: if the grey zone persists for decades, first-mover operations accumulate legal and technical precedent such that any future regime-negotiation will begin from a state of factual appropriation already accomplished. This converts the deferral from a coordination device (preserving future flexibility) into an extraction mechanism (locking in first-mover advantage under the cover of 'awaiting a regime'). The scaffold classification acknowledges this threat: the reading claims the constraint is transitional (waiting for a binding regime) but the measurements show extractiveness rising and theater rising faster — signals that the transition is failing. If the founding problem remains dead (if spacefaring states successfully argue that regime-negotiation is no longer necessary because market-based extraction is already the de facto norm), then the scaffold's sunset clause is expired and the constraint reclassifies as tangled_rope (coordination between spacefaring states and first-movers, extraction from resource-dependent and future actors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_as_coordination_vs_stalemate,
    'Is the deferral to a future regime a genuine coordination device preserving flexibility for future consensus-building, or a structural stalemate that will never produce a binding regime?',
    'Empirical: if regime negotiation produces a binding agreement within 10 years, the deferral was coordination. If regime negotiation remains deadlocked or produces only soft-law instruments (declarations, guidelines) without binding force, the deferral is stalemate. Conceptual: if spacefaring states can demonstrate that regime-negotiation deadlock arises from resource-dependent states'' incompatible demands (not from spacefaring-state obstruction), the deferral may remain defensible as coordination-preserving. If evidence shows spacefaring states actively preventing regime closure, the deferral becomes extraction-cover.',
    'If coordination is confirmed, the constraint remains scaffold and the typing is robust. If stalemate is confirmed, the constraint reclassifies as tangled_rope (spacefaring states and first-movers coordinate to extract from resource-dependent and future actors). The classification hinge is whether the deferral itself generates the distributional conflict or whether it merely preserves a conflict that exists independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_as_coordination_vs_stalemate, empirical, 'Whether the deferral is a designed-in coordination flexibility or a masking of irresoluble zero-sum conflict.').

omega_variable(
    first_mover_precedent_lock,
    'Does operational activity in the grey zone (extraction surveys, resource claims, infrastructure placement) create legal or technical precedent that constrains future regime negotiations, such that the deferral''s cost to resource-dependent states rises over time?',
    'Track the accumulation of first-mover operational claims and analyze how future regime negotiations weight existing operations: if a regime-negotiation process acknowledges first-mover rights and builds benefit-sharing around retained operational claims, precedent-lock is confirmed. If future regime-building treats all prior operations as void or subject to renegotiation, precedent-lock is absent.',
    'If precedent-lock is confirmed, extractiveness rises monotonically with time-in-grey-zone, and the scaffold''s temporary framing becomes strained — the constraint approaches tangled_rope as first-movers accumulate vested interests. If precedent-lock is absent (a future regime could roll back all prior operations), the deferral''s costs are more symmetrical and the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_mover_precedent_lock, empirical, 'Whether the grey zone converts the deferral from a coordination device into an irreversible first-mover advantage.').

omega_variable(
    treaty_interpretation_authority,
    'Is Article II''s ambiguity on extraction a deliberate deferral to future regime-building (international_regime reading), a substantive prohibition on appropriation interpreted narrowly to extraction (commons_conservation reading), or an intentional silence that permits private extraction as long as no sovereign claims are made (extraction_permissive reading)?',
    'Historical: the 1967 treaty negotiating record shows whether the drafters discussed extraction explicitly and chose ambiguity, or implicitly assumed extraction was not yet a concern. Legal: subsequent ICJ decisions, International Court of Arbitration rulings, and state practice in related treaty domains (Law of the Sea resource regimes) may clarify whether the treaty''s silence is interpreted as deference or as permission. Conceptual: the three readings correspond to different interpretive methodologies (originalism, living constitutionalism, plain-language constructivism), and the conflict may be non-resolvable through evidence alone.',
    'If the international_regime reading is confirmed (ambiguity + intentional deferral), the scaffold classification holds and regime negotiation is the proper institutional path. If commons_conservation or extraction_permissive readings are affirmed by authoritative interpreters (ICJ, treaty amendments), the constraint reclassifies into their respective types. The classification hinge is the nature of Article II''s authority: is it the text of a deferred agreement, or is it the final word on an implicit prohibition or permission?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_interpretation_authority, conceptual, 'Which reading of Article II''s non-appropriation language carries treaty authority.').

omega_variable(
    regime_negotiation_distributional_conflict,
    'Is the deadlock in regime negotiation (spacefaring states refusing conservation, resource-dependent states refusing extraction-permissive regimes) a genuine zero-sum conflict with no overlapping acceptable outcomes, or a coordination failure that a redesigned negotiating process could resolve?',
    'Formal game-theory analysis of the negotiation space: compute the Pareto-efficient frontier of possible benefit-sharing and extraction-control arrangements and test whether it includes any point acceptable to both spacefaring and resource-dependent coalitions. If Pareto improvements exist but are not negotiated, the conflict is a coordination failure and process redesign (e.g., modified voting rules, benefit-sharing mechanisms) could break the stalemate. If the Pareto frontier contains no overlap, the conflict is genuinely zero-sum.',
    'If coordination failure is confirmed, the deferral may be resolvable through institutional innovation (new negotiating forums, binding arbitration, linkage to other resource regimes). If zero-sum conflict is confirmed, the deferral will persist indefinitely, extractiveness will continue rising, and the constraint will asymptotically approach snare (first-movers extract, resource-dependent states bear costs, no resolution mechanism exists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_negotiation_distributional_conflict, empirical, 'Whether regime-negotiation deadlock is a genuine zero-sum conflict or a coordination failure.').

omega_variable(
    kernel_reading_foreclosure,
    'Can the international_regime reading be logically foreclosed by either sibling reading, or do all three readings remain logically coherent given the text of Article II?',
    'Textual: examine Article II''s language (''non-appropriation'', ''province of all mankind'', ''use or occupation'') and determine whether any single interpretation is the only logically consistent reading of the text. If the text permits all three readings, none foreclosures the others. If one reading''s core premise directly contradicts another''s (e.g., if conservation reading requires ''non-appropriation'' to include extraction while extraction-permissive reading requires it to exclude extraction), that contradiction does not foreclose either reading because different parties hold different premises.',
    'If no reading foreclosures another (logically, all three remain coherent), then all three readings coexist_with each other, and the kernel contest is political/distributional, not textual. If one reading forecloses another, the engine''s signature detection fires and the foreclosed reading is flagged as logically incoherent (though politically live). The classification of each reading is independent of this result; the result determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether Article II''s text permits multiple coherent interpretations or forecloses some readings as logically inconsistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(ost__tr_t0, observed).
narrative_ontology:measurement(ost__tr_t5, ost_article_ii_non_appropriation__international_regime, theater_ratio, 5, 0.51).
narrative_ontology:measurement_basis(ost__tr_t5, observed).
narrative_ontology:measurement(ost__tr_t10, ost_article_ii_non_appropriation__international_regime, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(ost__tr_t10, observed).
narrative_ontology:measurement(ost__tr_t15, ost_article_ii_non_appropriation__international_regime, theater_ratio, 15, 0.63).
narrative_ontology:measurement_basis(ost__tr_t15, observed).
narrative_ontology:measurement(ost__tr_t20, ost_article_ii_non_appropriation__international_regime, theater_ratio, 20, 0.66).
narrative_ontology:measurement_basis(ost__tr_t20, projected).
narrative_ontology:measurement(ost__tr_t25, ost_article_ii_non_appropriation__international_regime, theater_ratio, 25, 0.67).
narrative_ontology:measurement_basis(ost__tr_t25, projected).
narrative_ontology:measurement(ost__tr_t30, ost_article_ii_non_appropriation__international_regime, theater_ratio, 30, 0.67).
narrative_ontology:measurement_basis(ost__tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(ost__be_t0, observed).
narrative_ontology:measurement(ost__be_t5, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(ost__be_t5, observed).
narrative_ontology:measurement(ost__be_t10, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(ost__be_t10, observed).
narrative_ontology:measurement(ost__be_t15, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(ost__be_t15, observed).
narrative_ontology:measurement(ost__be_t20, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(ost__be_t20, projected).
narrative_ontology:measurement(ost__be_t25, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(ost__be_t25, projected).
narrative_ontology:measurement(ost__be_t30, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(ost__be_t30, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ost_article_ii_non_appropriation__international_regime, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, lunar_mining_precedent_accumulation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, asteroid_resource_claim_priority).

% DUAL FORMULATION NOTE:
% The ost_article_ii_non_appropriation kernel decomposes into three structurally distinct constraint stories corresponding to three live readings of Article II: commons_conservation (substantive prohibition on extraction, beneficiaries: conservation advocates and future actors; victims: first-mover firms); extraction_permissive (permission for private extraction absent sovereignty, beneficiaries: first-mover firms and spacefaring states; victims: resource-dependent states); and international_regime (deferral to future binding multilateral regime, beneficiaries: spacefaring states and first-movers due to grey-zone freedom; victims: resource-dependent and future states due to precedent lock). Each reading has a different ε (extractiveness assessment of the standing arrangement being read), different beneficiary/victim structures, and different type. The three readings coexist as live political positions held by different parties; none logically forecloses the others given the text of Article II. They are linked by network.affects_constraints because the persistence of one reading constrains the others' feasibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, powerless, 0.92).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
