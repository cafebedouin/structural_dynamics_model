% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS EEZ Strict Reading: Exclusive 200-NM Coastal Authority
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story instantiates the STRICT_EEZ_READING of the
 *   contested UNCLOS sovereignty boundary kernel. Under this reading, UNCLOS
 *   Article 57 establishes an exclusive, uniformly enforceable
 *   200-nautical-mile economic zone for all coastal states regardless of
 *   ratification status, historical occupation, or pre-existing customary-use
 *   claims. No overlay claims (historical rights, customary subsistence
 *   access, freedom-of-navigation doctrine) are valid within the zone.
 *   Coastal states benefit from exclusive resource control; overlapping
 *   claimants and subsistence communities bear the cost of foreclosed access.
 *   The constraint is CLAIMED as tangled rope (coordination + asymmetric
 *   extraction) and AUTHORED with high suppression (0.76) and substantial
 *   extractiveness (0.68) reflecting the reading's enforcement profile — the
 *   engine will measure whether the metrics support or diverge from the
 *   claim.
 *
 * KEY AGENTS:
 *   - Unclos-ratifying coastal states: Agenda-setter (institutional power) — enforce the boundary, license exclusive access, collect resource rents.
 *   - Overlapping claimants (South China Sea states, Arctic rim states, Mediterranean): Payer (powerful but constrained) — historical rights denied by the strict reading, forced into negotiation or exclusion.
 *   - Subsistence fishing communities: Payer (powerless, trapped) — traditional grounds now require licensing; no customary-use exemption under the strict reading.
 *   - Non-ratifier states: Payer (moderate power, identity-locked) — cannot claim treaty-external exemptions; customary-law arguments foreclosed by this reading's assertion of UNCLOS as binding custom.
 *   - Developed maritime economies: Beneficiary (institutional power, arbitrage exit) — vast exclusive zones, resource monopoly, jurisdictional clarity.
 *   - Freedom-of-navigation advocates (especially U.S. Navy doctrine): Excluded (moderate power, constrained) — their passage-right arguments are subordinated to coastal sovereignty.
 *   - International maritime commerce: Beneficiary (organized, mobile exit) — predictable jurisdictional boundaries lower transaction costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.76).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS EEZ Strict Reading: Exclusive 200-NM Coastal Authority").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '3ed67dd5-e0e8-4d96-92f3-61cf0d264969').
narrative_ontology:cs_kernel_codification('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', fixed_text).
narrative_ontology:cs_authority_grounding('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', lineage).
narrative_ontology:cs_interpretation_layer_present('3ed67dd5-e0e8-4d96-92f3-61cf0d264969').
narrative_ontology:cs_reading_relation('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, forecloses).
narrative_ontology:cs_axiom('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', foundational, unclos_boundary_is_customary_law).
narrative_ontology:cs_axiom_status(unclos_boundary_is_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', unclos_boundary_is_customary_law, empirically_contingent).
narrative_ontology:cs_axiom('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', foundational, no_overlay_claims_valid_within_eez).
narrative_ontology:cs_axiom_status(no_overlay_claims_valid_within_eez, holdable).
narrative_ontology:cs_axiom_grounding('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', no_overlay_claims_valid_within_eez, deontological).
narrative_ontology:cs_reference_frame('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', absolute_uniform_eez_boundary).
narrative_ontology:cs_drift_state('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', contemporary_enforcement_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ed67dd5-e0e8-4d96-92f3-61cf0d264969', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, developed_maritime_economies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, subsistence_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_commerce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control and enforce the 200-nautical-mile EEZ boundary as exclusive sovereign economic zone under UNCLOS Article 57. License fishing, extract minerals and hydrocarbons, and exclude non-consenting access. Benefit from resource monopolization within the zone. Enforce compliance through coast guard patrols, licensing denial, and diplomatic pressure. The strict reading gives them unambiguous authority independent of occupation history or customary practice claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations with adjacent or overlapping EEZ claims under the strict reading (e.g., South China Sea, Eastern Mediterranean, Barents Sea). The strict reading forecloses their historical-rights and customary-practice arguments; they must negotiate maritime boundaries or lose access to contested waters entirely. Their exit is limited to costly boundary negotiation, armed confrontation, or economic withdrawal from the zone.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimants, payer,
    powerful, generational, constrained, global).

% Nations that have not ratified UNCLOS (notably the United States until ratification debates; some small island states and landlocked nations in contested regions). The strict reading asserts UNCLOS provisions are binding customary international law regardless of ratification status. Non-ratifiers cannot claim the freedom-of-navigation or historical-rights exemptions that the alternative readings permit; they bear the enforcement cost without the treaty-based legitimacy defense.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_states, payer,
    moderate, biographical, identity_locked, global).

% Small-scale fishing communities whose traditional fishing grounds fall within another state's EEZ under the strict reading. They lose access unless the coastal state grants licenses or permits (typically at commercial market rates that exceed subsistence-level economics). No historical-use exemption exists under this reading; customary subsistence rights are subordinated to the coastal state's sovereign exclusive economic right.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, subsistence_fishing_communities, payer,
    powerless, biographical, trapped, regional).

% Nations with large territorial bases and deep-water resource exploration capacity (Japan, Norway, Australia, Canada) benefit most from the strict EEZ reading because they control vast ocean resources and can afford exploration and enforcement. They also benefit from the jurisdictional clarity the strict reading provides for international commerce and investment.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, developed_maritime_economies, beneficiary,
    institutional, generational, arbitrage, global).

% Shipping lanes, fishing fleets, and commerce benefit from the jurisdictional clarity of the 200-nm boundary: it reduces ambiguity about which state's courts, regulations, and enforcement apply. The strict reading creates predictable maritime law, lowering transaction costs for international trade.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_commerce, beneficiary,
    organized, biographical, mobile, global).

% Naval powers (especially the United States and other navies exercising freedom-of-navigation operations) whose strategic interests depend on unchallenged passage through disputed waters. The strict reading they dispute is precisely what limits their maneuvering room. They argue customary international law permits navigation rights independent of UNCLOS; this reading's foreclosure of that claim is the enforcement object.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_advocates, excluded,
    moderate, biographical, constrained, global).

% The institutional apparatus that codifies and interprets UNCLOS provisions. Not an agent (it does not collect or pay), but the framework whose interpretation this reading instantiates.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_treaty_framework, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(unclos_sovereignty_boundary__strict_eez_reading, unclos_treaty_framework).

% The International Court of Justice, UNCLOS dispute settlement procedures, and regional maritime courts interpret and apply the strict reading to specific cases. Their rulings determine whether alternative readings (historical rights, non-ratifier exemptions) survive in practice.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, maritime_dispute_resolution_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform maritime boundary (200-nm from baseline) that prevents overlapping sovereignty claims from colliding continuously. Solves the coordination problem of maritime resource allocation by providing one rule applicable everywhere, eliminating the need for ad-hoc bilateral boundary negotiation in every overlapping case.
% TRANSFER_FUNCTION: Transfers exclusive resource control (fishing stocks, minerals, hydrocarbons, water-column biological resources) from commons frameworks and from contested-rights frameworks to the coastal state. Also transfers enforcement and jurisdictional authority from open-ocean norms to coastal-state control within the EEZ.
% ABSENT_VOICES: Non-ratifier naval powers (especially the U.S., whose freedom-of-navigation doctrine the strict reading forecloses). Historical claimants (China, Russia, and others in overlapping zones) whose pre-UNCLOS occupation claims the reading subordinates. Subsistence fishing communities and traditional maritime users whose customary-use exemptions the reading does not recognize. These groups would object if present and if the alternative readings (historical_rights_reading, non_ratifier_enforcement_reading) were allowed, but the strict reading's foreclosure of those readings silences their objections.
% DISAPPEARANCE_RATIONALE: If the strict EEZ reading vanished overnight, overlapping maritime claims would re-emerge (historical-rights and customary-use frameworks would resurface as live options), subsistence fishing communities would attempt to re-occupy traditional grounds outside the licensing system, and naval powers would resume freedom-of-navigation operations unchallenged within claimed EEZ zones. The uniform 200-nm boundary would collapse and maritime contestation would return to the pre-UNCLOS state of competing regional frameworks.
% FOUNDING_PROBLEM: Before UNCLOS, no uniform maritime boundary existed. Coastal states claimed varying distances (3 nm, 12 nm, 50 nm, 200 nm) and different jurisdiction types (territorial seas, contiguous zones, continental shelf claims). Overlapping claims were frequent, and there was no single authoritative rule for where territorial sovereignty ended and the commons began. The strict EEZ reading was designed to replace this ambiguity with a uniform 200-nm boundary binding on all states.
% FOUNDING_PROBLEM_CORROBORATION: Coastal states and maritime-law scholars supporting the strict reading attest the founding problem of boundary ambiguity is solved: the 200-nm rule eliminates overlaps and provides jurisdictional clarity. Overlapping claimants and naval powers attest the problem has SHIFTED, not solved: the strict reading creates enforced uniformity that subordinates legitimate historical and navigational claims, shifting from ambiguity to imposed asymmetry. Independent maritime-law analysis from outside the benefiting parties (UN academic commentaries, NGO reports on fishing-community displacement, law-review articles from non-beneficiary jurisdictions) corroborates the shift reading: the founding problem of boundary ambiguity is addressed, but at the cost of foreclosing legitimate alternatives.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness series (0.55→0.68 over 50 time units) tracks the gradual hardening of coastal-state exclusive control as the reading becomes institutionalized through case law (particularly UNCLOS dispute settlement rulings) and as enforcement capacity increases. Early-interval extractiveness is lower (0.55) because overlapping claims and freedom-of-navigation operations still contest the reading; by t=50 it plateaus at 0.68 because the reading has become entrenched and challengers accept the loss of access. Suppression (0.62→0.76) rises more steeply: the constraint's persistence requires ACTIVE foreclosure of alternative framings (historical-rights arguments, customary-use exemptions, non-ratifier naval doctrine). Without continuous enforcement suppression (naval patrols, licensing denial, diplomatic pressure), the reading would collapse and overlapping claims would resurface. Theater (0.18→0.28) remains moderate-low because the constraint does perform real coordination (uniform boundaries prevent overlapping-claim collisions), but an increasing share of enforcement activity serves pure exclusion (blocking freedom-of-navigation operations, denying subsistence-use permits) rather than coordination maintenance. The measurements share one time grid (every metric authored at every time point) to avoid the OQ-105 misalignment trap.
 *
 * PERSPECTIVAL GAP:
 *   The coastal-state beneficiary seat and the overlapping-claimant payer seat will compute to different types. From the beneficiary seat (institutional power, arbitrage exit), the reading is coordination: one rule, predictable zones, genuine resource-control benefit. From the payer seat (powerful but constrained), the same structure operates as enforcement of asymmetric extraction: historical rights denied, customary practices foreclosed, access costs imposed unilaterally. The engine derives directionality from beneficiary/victim + exit options; the structural asymmetry in exit (coastal states can arbitrage; overlapping claimants cannot) produces this divergence. From subsistence communities (powerless, trapped), the reading is pure extraction — no coordination function they perceive, only loss of access.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states derive low directionality (d≈0.1-0.2) as beneficiaries: they control the boundary, license access, and can exit via arbitrage (they set the rules). Overlapping claimants derive high directionality (d≈0.7-0.8) as victims: they lose access, are forced into negotiation, and have constrained exit (they cannot unilaterally reject the boundary). Non-ratifier states derive moderately-high directionality (d≈0.6-0.7) as payers because they are identity-locked (cannot claim treaty status as exemption) and trapped (the strict reading asserts UNCLOS as binding custom regardless). Subsistence communities derive the highest directionality (d≈0.9) as they are powerless and trapped with zero exit options. Developed maritime economies derive low directionality (d≈0.05-0.15) as beneficiaries because they control vast zones and have arbitrage options. International maritime commerce derives near-symmetric directionality (d≈0.45) as it benefits from coordination clarity but bears enforcement costs in restricted passage areas.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maritime boundary ambiguity pre-UNCLOS) is CONTESTED per six_questions.founding_problem_status. Coastal states attest it is solved (uniform 200-nm boundary eliminates ambiguity). Overlapping claimants and maritime-law scholars outside the benefiting parties attest it is SHIFTED, not solved: instead of ambiguous overlaps, the strict reading imposes forced-choice subordination of pre-existing claims. This mismatch (founding problem attested as 'live' by beneficiaries, as 'dead' by independent scholars) is exactly the mandatrophy signature: the constraint persists by foreclosing the alternative readings that would settle the founding dispute. If the historical_rights_reading were allowed, overlapping claimants could negotiate settlements based on occupation history; if the non_ratifier_enforcement_reading were permitted, non-signatories could maintain freedom-of-navigation operations. The strict reading persists by suppressing these alternatives, which means it collects rents from the foreclosure. The constraint is therefore a mandatrophy candidate: the founding problem it was designed to solve (boundary clarity) is not the primary thing keeping it in place — the suppression of alternative readings is. The theater_ratio trajectory (rising from 0.18 to 0.28 but plateauing) shows this: early on, the coordination function was primary; as time passes, enforcement becomes theatrical (coast guard patrols defending the boundary against freedom-of-navigation operations that pose no real boundary ambiguity) and the pure-extraction function (denying subsistence-use and historical-rights arguments) becomes primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_customary_law_vs_treaty_creation,
    'Is the 200-nm EEZ rule binding UNCLOS Article 57 as codified treaty law, or is it binding as customary international law independent of ratification status?',
    'Case law from the International Court of Justice and UNCLOS dispute settlement tribunals; state practice by non-ratifiers (particularly the U.S. Navy freedom-of-navigation operations); expert legal analysis of state acquiescence vs. explicit agreement.',
    'If the rule is treaty-only, non-ratifiers can claim exemption (non_ratifier_enforcement_reading becomes live). If the rule is customary, it binds all states including non-ratifiers (strict_eez_reading is sustained). This resolves the foreclosure between strict_eez_reading and non_ratifier_enforcement_reading: one reading''s core claim forecloses the other''s core premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_customary_law_vs_treaty_creation, empirical, 'Whether the 200-nm rule is treaty-law-specific or customary-law-universal.').

omega_variable(
    historical_rights_overlap_vs_absolute_boundary,
    'Can a coastal state''s pre-UNCLOS historical occupation (centuries of fishing, resource use, military presence) create a legitimate overlay claim on the 200-nm boundary, or does Article 57 extinguish all such claims?',
    'ICJ rulings in overlapping-zone cases (South China Sea, Arctic delimitation); treaty-text interpretation analysis; state practice in boundary negotiations where historical claims are explicitly renounced or preserved.',
    'If historical occupation survives Article 57, overlapping claimants can assert rights that coexist with the strict reading (boundary remains 200-nm, but historical claims create secondary negotiation spaces — a hybrid outcome). If historical claims are foreclosed, the strict reading is sustained: 200-nm boundary with no overlay, and overlapping claimants lose negotiation leverage entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_overlap_vs_absolute_boundary, empirical, 'Whether historical occupation creates persistent overlay claims or is erased by UNCLOS Article 57.').

omega_variable(
    subsistence_use_exemption_vs_coastal_sovereignty,
    'Does UNCLOS Article 62 (coastal-state obligation to allow fishing access for others'' nationals in surplus catches) create a subsistence-use exemption, or does it remain subordinate to coastal-state discretion?',
    'UNCLOS dispute settlement rulings on Article 62 interpretation; state practice in licensing subsistence fishing; regional agreements granting traditional-user exemptions.',
    'If Article 62 creates a binding exemption for subsistence users, the strict reading''s foreclosure of customary-use rights is partial (subsistence remains valid). If Article 62 is discretionary, the reading is sustained: coastal states can deny subsistence access entirely, and powerless fishing communities have no anchor claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsistence_use_exemption_vs_coastal_sovereignty, empirical, 'Whether subsistence fishing access is a protected right under UNCLOS or discretionary to the coastal state.').

omega_variable(
    kernel_reading_assignment_ambiguity,
    'Is the strict_eez_reading the ONLY defensible interpretation of UNCLOS Articles 55–57, or is it one legitimate reading among multiple coherent framings?',
    'This is the meta-question about the kernel itself: comparative analysis of how different states, courts, and legal traditions read the same text. If multiple coherent readings are documented (with different institutional authority backing each), then this reading is one among alternatives, not the uniquely correct one.',
    'If the reading is one among multiple, the kernel is indeed contested and sibling readings (historical_rights_reading, non_ratifier_enforcement_reading) are live, and the three readings coexist with competing institutional authority (different states, courts, schools of interpretation backing each). If the reading is uniquely correct, the sibling readings are either incoherent or strategically asserted despite legal invalidity, and the kernel is contested only as a matter of politics, not law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_assignment_ambiguity, conceptual, 'Kernel contest: whether the strict reading is uniquely correct or one legitimate interpretation of a genuinely polysemous text.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.76) the structural cost of enforcing the boundary against overlapping claims and freedom-of-navigation operations, or is it partly internalized — the claimant states have accepted the reading''s legitimacy and now ''enforce'' it on themselves through compliance rather than external coercion?',
    'Analysis of state behavior in boundary disputes over time: does suppression require active enforcement (naval presence, patrol intensity) or has compliance become normalized? Do states dispute the boundary''s legitimacy in forums where they could contest it (ICJ, UNCLOS tribunals, diplomatic negotiations), or do they accept it and negotiate terms within it?',
    'If suppression is structural, the constraint is more extractive (external force required). If suppression is internalized, the constraint is more rope-like (acceptance of the boundary as legitimate reduces the suppression requirement). This affects both the terminal classification and the sustainability assessment: internalized suppression is more stable over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized compliance with a legitimized boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(uncl_tr_t50, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(uncl_be_t50, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(uncl_su_t50, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.18).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, freedom_of_navigation_doctrine__naval_passage).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, arctic_maritime_sovereignty__sovereignty_competition).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, south_china_sea_eez_disputes__overlapping_claims).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNCLOS_SOVEREIGNTY_BOUNDARY kernel. Three readings exist: strict_eez_reading (this file) asserts the 200-nm boundary is absolute and binding as customary law; historical_rights_reading asserts pre-UNCLOS occupation creates legitimate overlay claims; non_ratifier_enforcement_reading asserts freedom of navigation is customary law independent of ratification. Each reading has different beneficiary/victim structures (strict reading favors developed maritime states and coastal governments, harms overlapping claimants and non-ratifiers; historical reading redistributes benefits to historical claimants; freedom-of-navigation reading benefits naval powers). The readings share a referent (the UNCLOS Article 55–57 boundary system) but diverge on ε (different extraction signatures) and on the suppression targets (different alternatives foreclosed). See cs_structure.reading_relations for structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
