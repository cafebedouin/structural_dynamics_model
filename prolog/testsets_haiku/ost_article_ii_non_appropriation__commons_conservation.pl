% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation (Commons Conservation Reading)
 *   domain: international_law/space_law/commons_governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) declares that celestial
 *   bodies and space itself are not subject to national appropriation 'by
 *   claim of sovereignty, by means of use or occupation, or by any other
 *   means.' The commons conservation reading interprets this to prohibit de
 *   facto appropriation via resource extraction: use and occupation mean
 *   extraction itself, creating a resource commons that requires multilateral
 *   authorization for any appropriation regime. Spacefaring states and
 *   private mining entities increasingly argue an extraction_permissive
 *   reading: Article II bars state territorial claims but permits private
 *   ownership of extracted resources. A third reading (international_regime)
 *   holds that Article II intentionally defers the appropriation question to
 *   future Article XI-type negotiation, leaving both conservation and
 *   extraction-permissive readings contestable. This story instantiates the
 *   commons conservation reading as a single constraint with stable ε,
 *   beneficiary/victim structure, and enforcement mechanism. The other
 *   readings are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Non-spacefaring states (collective beneficiary, preserve veto power over enclosure)
 *   - Spacefaring mining investors (first-mover payers, face stranded capital)
 *   - Global scientific community (beneficiary, preserved research access)
 *   - Future multilateral regime negotiators (institutional agenda-setter, gate keeper of regime change)
 *   - Extraction-permissive interpreters (excluded, positioned against commons conservation)
 *   - International regime protagonists (observer, measure contest between readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.68).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.72).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation (Commons Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_law/space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '79001e9e-9222-419b-9f2d-990b6be67f73').
narrative_ontology:cs_kernel_codification('79001e9e-9222-419b-9f2d-990b6be67f73', fixed_text).
narrative_ontology:cs_authority_grounding('79001e9e-9222-419b-9f2d-990b6be67f73', lineage).
narrative_ontology:cs_interpretation_layer_present('79001e9e-9222-419b-9f2d-990b6be67f73').
narrative_ontology:cs_reading_relation('79001e9e-9222-419b-9f2d-990b6be67f73', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('79001e9e-9222-419b-9f2d-990b6be67f73', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('79001e9e-9222-419b-9f2d-990b6be67f73', foundational, extraction_is_appropriation).
narrative_ontology:cs_axiom_status(extraction_is_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('79001e9e-9222-419b-9f2d-990b6be67f73', extraction_is_appropriation, deontological).
narrative_ontology:cs_axiom('79001e9e-9222-419b-9f2d-990b6be67f73', foundational, collective_veto_preserves_future_negotiation).
narrative_ontology:cs_axiom_status(collective_veto_preserves_future_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('79001e9e-9222-419b-9f2d-990b6be67f73', collective_veto_preserves_future_negotiation, instrumental).
narrative_ontology:cs_reference_frame('79001e9e-9222-419b-9f2d-990b6be67f73', collective_stewardship_commons_model).
narrative_ontology:cs_drift_state('79001e9e-9222-419b-9f2d-990b6be67f73', contemporary_mining_capability_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('79001e9e-9222-419b-9f2d-990b6be67f73', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, global_scientific_community).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_mining_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_resource_extraction_entities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_extraction_entities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserve collective veto power over space resource appropriation via the non-appropriation principle. They cannot unilaterally extract resources but retain negotiating position in future regime-setting forums. Their material absence from space capability is protected by a rule that requires multilateral consent to change appropriation rules. Benefits from the principle: exclusion of unilateral enclosure that would lock them out permanently.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, global).

% Face stranded investments and delayed monetization of exploration and mining infrastructure under the commons conservation reading. They have sunk capital into locating and surveying resources (lunar minerals, asteroid metals) but cannot claim ownership under this interpretation. Their exit is constrained: abandoning the assets forfeits the investment; continuing assumes eventual regime change or successful interpretive challenge. The constraint's enforcement (international consensus, treaty interpretation machinery) prevents unilateral claims.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_mining_investors, payer,
    powerful, biographical, constrained, global).

% Bear the cost of the non-appropriation rule most acutely: they have the technical capacity to extract but no legal framework granting ownership. The constraint traps them between operational capacity and legal prohibition. Their only exit is interpretive success (convincing other parties the extraction_permissive reading is correct) or a negotiated new regime that pays them for past exploration.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_extraction_entities, payer,
    powerful, biographical, trapped, global).

% Benefits from preservation of space environments for scientific study and discovery. The non-appropriation principle aligns with scientific norms against privatization of natural history sites and preserves access to pristine extraterrestrial geology, astrobiology sampling, and fundamental research. Could theoretically support extraction-permissive reading if it guaranteed research access; currently benefits from commons regime that keeps resources open to scientific inquiry.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, global_scientific_community, beneficiary,
    organized, generational, mobile, global).

% Hold the power to formalize appropriation rules via Article XI-type multilateral agreement. The commons conservation reading vests them with gatekeeping authority: no extraction regime is legitimate without consensus (or near-consensus). They manage the constraint's evolution and can reframe it toward extraction_permissive or international_regime readings if enough parties agree. The constraint creates a negotiating position for every state.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_multilateral_regime_negotiators, agenda_setter,
    institutional, generational, analytical, global).

% Would argue that Article II bars territorial appropriation by states but permits private ownership of extracted materials (the extraction_permissive reading). They are not excluded from discourse but structurally positioned against the commons conservation interpretation. Their position is contested within the international legal community; the commons conservation reading's enforcement machinery (treaty consensus, interpretive authority) keeps their reading at bay without formally refuting it.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, extraction_permissive_interpreters, excluded,
    powerful, biographical, constrained, global).

% Hold that Article II intentionally defers the appropriation question to a future framework (Article XI). They do not endorse either commons conservation or extraction_permissive as the governing rule; instead they hold that neither reading is authoritative absent multilateral framework agreement. They view the constraint as suspension of the question, not as commitment to conservation. Their seat is analytical: they measure the corpus against both readings and adjudicate which interpretation the treaty actually supports.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_regime_protagonists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, future_multilateral_regime_negotiators).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes collective stewardship norm: prevents unilateral appropriation that would create irreversible enclosure; preserves future negotiating power for all signatories; coordinates around a presumption that space resources are not subject to sovereign territorial or private ownership claims absent explicit multilateral authorization.
% TRANSFER_FUNCTION: Transfers regulatory veto power from early-mover spacefaring states to the collective of all signatories. Early investors and spacefaring miners pay the cost of delayed commercialization and stranded assets; non-spacefaring states and future negotiating forums receive the benefit of preserved optionality and negotiating leverage.
% ABSENT_VOICES: Actors not party to the OST (private mining corporations in non-signatory states, future space settlement populations, hypothetical extraterrestrial life forms) are not in the negotiating conversation. Private space companies advocate for extraction_permissive reading but lack formal treaty voice. Future generations who might benefit or suffer from resource depletion are represented only through institutional proxies.
% DISAPPEARANCE_RATIONALE: If the non-appropriation principle vanished and unilateral extraction claims were legally permitted, spacefaring states and well-capitalized mining entities would claim and privatize the most valuable lunar and asteroid deposits within years. Non-spacefaring states would lose negotiating leverage permanently, scientific research access would fragment along proprietary lines, and the legal status of space would reorganize as a patchwork of competing territorial and ownership claims rather than a commons.
% FOUNDING_PROBLEM: Prevention of a space 'wild west': avoid the race dynamics and territorial fragmentation that characterized colonial appropriation of Earth. Preserve the capacity for future multilateral governance by preventing de facto appropriation that would foreclose future negotiation.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states, scientific organizations, and some international law scholars affirm the founding problem remains live: space resource regulation is unsettled and early privatization would foreclose future options. Spacefaring mining interests and some legal scholars argue the founding problem is obsolete: market mechanisms and private property incentives are the efficient path. No consensus exists on whether unregulated appropriation is the threat the principle was designed to prevent, or an inevitable efficiency.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The commons conservation reading produces high extractiveness (0.68) because it constrains early investors and spacefaring miners from monetizing exploration investments—a direct transfer from their capital to the collective negotiating position of all signatories. Suppression is high (0.72) because the constraint's persistence depends on active enforcement via treaty interpretation consensus and rejection of unilateral appropriation claims; if consensus eroded, spacefaring parties could simply declare extraction_permissive as the governing reading and proceed. Theater ratio rises from 0.05 to 0.41 over the interval: in the early period (1967–1987), the constraint was largely theoretical (no mining yet attempted); as commercial space activity increased (1990s–present), enforcement shifted toward diplomatic assertion of the non-appropriation principle without substantive regulatory framework—negotiations were deferred repeatedly (theater). The measurement series track extraction growth (as spacefaring capability expanded and mining proposals multiplied) and enforcement intensification (more explicit treaty interpretation statements, more diplomatic pressure on early movers to seek multilateral approval). Suppression_requirement rises as the technical capacity for extraction increases: more force is needed to hold the constraint when more parties have the means to violate it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (future regime negotiators) experiences the constraint as optionality they steward. The payer seats (spacefaring investors) experience it as a veto they cannot overcome. The beneficiary seats (non-spacefaring states) experience it as protection they must actively defend against interpretive challenges. The constraint's type diverges across seats: from the regime negotiator's analytical position it is suspension (international_regime reading); from the investor's position it is snare (prohibits extraction without providing exit); from the beneficiary's position it is rope (coordinates collective veto). The engine computes these divergences from the stakeholder power atoms and exit options—the authored claim (tangled_rope) reflects the structural reality that one reading benefits some parties (non-spacefaring states, scientific community) through coordination, while harming others (investors) through active extraction prevention.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states benefit from the constraint: it preserves their veto power in future regime negotiations and prevents irreversible enclosure by early movers. They are organized, constrained in exit (they cannot opt out of the treaty without losing standing in space law), and derive benefit from the rule structure itself—low directionality toward extraction (d ≈ 0.2). Spacefaring mining investors are structurally targeted: they can extract (capability exists) but are prohibited (the constraint); they pay through stranded capital and delayed monetization. Their exit is trapped: they cannot unilaterally escape the constraint (treaty consensus blocks them) nor abandon the assets (sunk cost). High directionality toward extraction (d ≈ 0.85). Scientific community is a secondary beneficiary: they benefit from preservation of pristine environments and open access but could theoretically accept extraction-permissive reading if research access were guaranteed. Their exit is more mobile (they can argue for either reading and shift allegiance based on which preserves research). The future regime negotiators are institutional observers: they hold the power to redefine the constraint, so their directionality is analytical (d ≈ 0.5, symmetric position in adjudication machinery).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent wild-west territorial fragmentation and preserve future negotiating capacity) remains contested. Non-spacefaring states and scientific institutions attest it is live and urgent: premature appropriation would foreclose future options irreversibly. Spacefaring mining interests attest it is obsolete: property rights and market mechanisms are efficient and self-stabilizing. The constraint's persistence despite this contest is sustained by treaty obligation (OST Article II is binding) and diplomatic consensus (repeated reaffirmations of non-appropriation in UN committees). Theater has risen substantially (0.05→0.41) as actual mining proposals multiplied and international bodies produced interpretive statements without substantive regulatory framework—the machinery of affirmation intensified while concrete rule-making stalled. This is not mandatrophy (the constraint has not become purely performative; it still prevents unilateral claims), but the ratio shows mounting pressure: the founding problem's contestedness is now visible in the performance gap between assertion and implementation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_authority,
    'Which reading (commons_conservation vs. extraction_permissive vs. international_regime) represents the actual intent of Article II''s drafters, and is that intent binding on interpretation?',
    'Historical analysis of treaty negotiation records; subsequent state practice; advisory opinions from International Court of Justice or international arbitral bodies; multilateral regime agreement under Article XI that codifies one reading.',
    'If commons_conservation reading is established as correct via authoritative interpretation, the constraint''s type is Tangled Rope (coordination + extraction prevention). If extraction_permissive is established, the constraint reverts to Mountain (physical appropriation law follows inevitably from resource value). If international_regime is established, the constraint dissolves into suspension pending regime negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_authority, empirical, 'Interpretation authority and the kernel''s intended reading').

omega_variable(
    private_vs_state_actor_scope,
    'Does ''appropriation'' in Article II apply equally to private actors (mining companies) and state actors, or does it bind states only?',
    'State practice in licensing private mining entities; rulings by international arbitral bodies on private operator claims; explicit regime agreement clarifying scope of Article II.',
    'If private actors are fully bound, the constraint prevents private mining companies from claiming ownership and may require multilateral benefit-sharing mechanisms. If private actors are excluded from Article II''s reach, states can unilaterally license extraction and grant ownership, converting the constraint from Tangled Rope to Rope (state-to-state coordination, no victim seat for private investors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_vs_state_actor_scope, empirical, 'Whether Article II constrains private actors or states only').

omega_variable(
    appropriation_means_extraction,
    'Does ''appropriation'' require formal ownership/territorial claim, or does the act of extraction itself constitute appropriation?',
    'Legal analysis of ''appropriation'' in property law analogues; state practice in accepting or rejecting unowned extraction; regime agreement defining what constitutes appropriation.',
    'If extraction = appropriation, the commons_conservation reading holds and the constraint prevents unilateral mining. If appropriation requires formal claim (territory, ownership declaration), extraction could proceed without triggering the constraint, supporting extraction_permissive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_means_extraction, conceptual, 'Whether resource extraction alone constitutes appropriation').

omega_variable(
    suppression_internalized_vs_structural,
    'Is the suppression of extraction internalized (spacefaring states genuinely believe non-appropriation is just and maintain it voluntarily) or structural (the rule persists because multilateral consensus machinery blocks defection)?',
    'Post-regime-change analysis: if a multilateral regime is negotiated and spacefaring states immediately attempt extraction, suppression was structural; if they continue to accept restrictions, suppression is partly internalized.',
    'If suppression is internalized, the constraint is more stable than structural suppression alone; if purely structural, regime instability and defection risk are high as technical capacity increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized_vs_structural, empirical, 'Whether suppression is internalized norm or externally enforced').

omega_variable(
    future_regime_path_dependency,
    'Does the commons conservation reading lock future regime negotiators into conservation-only outcomes, or does Article XI negotiation remain fully open?',
    'Regime agreement negotiations post-2025; extent to which non-spacefaring coalitions cite Article II as foreclosing extraction-permissive outcomes; successful challenge to commons_conservation via extraction_permissive interpretation.',
    'If Article II is read as path-dependent toward conservation, the constraint has strong precedential weight in regime design. If Article XI negotiation is truly open, the constraint''s influence weakens and both readings remain viable outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_regime_path_dependency, conceptual, 'Whether commons conservation reading forecloses extraction-permissive outcomes in future regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_article_ii_conservation_tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t1967, observed).
narrative_ontology:measurement(ost_article_ii_conservation_tr_t1987, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1987, 0.12).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t1987, observed).
narrative_ontology:measurement(ost_article_ii_conservation_tr_t2005, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2005, 0.22).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t2005, observed).
narrative_ontology:measurement(ost_article_ii_conservation_tr_t2015, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2015, 0.32).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t2015, observed).
narrative_ontology:measurement(ost_article_ii_conservation_tr_t2022, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t2022, observed).
narrative_ontology:measurement(ost_article_ii_conservation_tr_t2026, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(ost_article_ii_conservation_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ost_article_ii_conservation_be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t1967, observed).
narrative_ontology:measurement(ost_article_ii_conservation_be_t1987, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1987, 0.28).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t1987, observed).
narrative_ontology:measurement(ost_article_ii_conservation_be_t2005, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t2005, observed).
narrative_ontology:measurement(ost_article_ii_conservation_be_t2015, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t2015, observed).
narrative_ontology:measurement(ost_article_ii_conservation_be_t2022, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t2022, observed).
narrative_ontology:measurement(ost_article_ii_conservation_be_t2026, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(ost_article_ii_conservation_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost_article_ii_conservation_su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.42).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t1967, observed).
narrative_ontology:measurement(ost_article_ii_conservation_su_t1987, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1987, 0.5).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t1987, observed).
narrative_ontology:measurement(ost_article_ii_conservation_su_t2005, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t2005, observed).
narrative_ontology:measurement(ost_article_ii_conservation_su_t2015, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t2015, observed).
narrative_ontology:measurement(ost_article_ii_conservation_su_t2022, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t2022, observed).
narrative_ontology:measurement(ost_article_ii_conservation_su_t2026, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(ost_article_ii_conservation_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.22).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_xi_benefit_sharing_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, space_mining_license_framework).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation kernel decomposes into three constraint stories: commons_conservation (this story—extraction prohibited absent multilateral authorization), extraction_permissive (Article II bars state appropriation but permits private extraction ownership), and international_regime (Article II defers appropriation to future framework negotiation). These are not the same constraint viewed from different angles; they have different ε values, different beneficiary/victim structures, and different type classifications. Commons_conservation carries high extractiveness for spacefaring miners (stranded capital); extraction_permissive would carry near-zero extractiveness for miners and high extractiveness for non-spacefaring states (exclusion from benefits); international_regime carries uncertainty extractiveness (suspended). The three stories are linked via network.affects_constraints because the legal contest between readings is causally interconnected—establishment of one reading forecloses or influences the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__commons_conservation, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
