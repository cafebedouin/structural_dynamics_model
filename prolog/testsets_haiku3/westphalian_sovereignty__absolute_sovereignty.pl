% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty Doctrine (Non-Interference)
 *   domain: political/international
 *
 * SUMMARY:
 *   The absolute-sovereignty reading of the westphalian_sovereignty kernel
 *   asserts that states possess unconditional authority over their domestic
 *   affairs and that external intervention is categorically illegitimate,
 *   regardless of what occurs within a state's borders. This reading grounds
 *   its legitimacy in the formal structure of international law (the UN
 *   Charter Article 2.7, the Montevideo Convention) and in the principle that
 *   no state may arrogate judgment over another's governance. Under this
 *   reading, sovereignty is indivisible: either a state is sovereign (and
 *   thus shielded from interference) or it is not. The reading produces
 *   asymmetric effects: states—especially those with authoritarian
 *   governance—benefit from the non-interference shield; populations
 *   suffering systematic human rights violations inside those states lack
 *   recourse because the doctrine forecloses external remedies. The
 *   constraint is thus structurally a Tangled Rope: it solves a genuine
 *   coordination problem (deterrence of great-power domination over smaller
 *   states) while simultaneously enabling extraction (authoritarian regimes
 *   extract from their populations with reduced risk of external interference
 *   or sanction).
 *
 * KEY AGENTS:
 *   - sovereign_states: Collectively benefit from the mutual non-interference shield; can govern without fear of external regime-change operations or humanitarian intervention. Their interest in the absolute reading is maximally unified across regime types during periods of great-power stability.
 *   - authoritarian_regimes: Subgroup of sovereign states; derive disproportionate benefit from absolute sovereignty doctrine because it protects them from external pressure on internal repression, genocide, or displacement.
 *   - persecuted_domestic_populations: Victims of state violence; excluded from the constraint because their exit from the jurisdiction is blocked and international remedies are doctrine-forbidden. They cannot appeal to external actors because the absolute-sovereignty reading prohibits external standing.
 *   - democratic_states: Subset of sovereign states; have mixed position—benefit from mutual non-interference shield for their own sovereignty, but face domestic political pressure to sanction or intervene against human rights violations by others, creating enforcement friction.
 *   - international_human_rights_bodies: Institutional observers (UN Human Rights Council, International Court of Justice); formally prohibited from enforcement action against sovereign states by the same doctrine, though they can document and publicize violations.
 *   - great_powers: Institutional actors with enforcement capacity; the absolute-sovereignty doctrine constrains their ability to pursue geopolitical objectives via regime change while granting them protection from similar operations. Constraint persistence depends on their collective willingness to enforce mutual non-interference.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.68).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty Doctrine (Non-Interference)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "political/international").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'ee538c6c-9eff-4471-8fdb-c24acf2882d8').
narrative_ontology:cs_kernel_codification('ee538c6c-9eff-4471-8fdb-c24acf2882d8', fixed_text).
narrative_ontology:cs_authority_grounding('ee538c6c-9eff-4471-8fdb-c24acf2882d8', lineage).
narrative_ontology:cs_interpretation_layer_present('ee538c6c-9eff-4471-8fdb-c24acf2882d8').
narrative_ontology:cs_reading_relation('ee538c6c-9eff-4471-8fdb-c24acf2882d8', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('ee538c6c-9eff-4471-8fdb-c24acf2882d8', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('ee538c6c-9eff-4471-8fdb-c24acf2882d8', foundational, sovereignty_indivisible_unconditional).
narrative_ontology:cs_axiom_status(sovereignty_indivisible_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('ee538c6c-9eff-4471-8fdb-c24acf2882d8', sovereignty_indivisible_unconditional, deontological).
narrative_ontology:cs_axiom('ee538c6c-9eff-4471-8fdb-c24acf2882d8', foundational, non_interference_categorically_legitimate).
narrative_ontology:cs_axiom_status(non_interference_categorically_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ee538c6c-9eff-4471-8fdb-c24acf2882d8', non_interference_categorically_legitimate, conventional).
narrative_ontology:cs_reference_frame('ee538c6c-9eff-4471-8fdb-c24acf2882d8', westphalian_mutual_non_interference).
narrative_ontology:cs_drift_state('ee538c6c-9eff-4471-8fdb-c24acf2882d8', contemporary_humanitarian_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee538c6c-9eff-4471-8fdb-c24acf2882d8', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, sovereign_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, persecuted_domestic_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, democratic_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, democratic_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, state_independence_principle).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, territorial_integrity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively author and maintain the absolute-sovereignty doctrine through UN bodies, treaty participation, and formal recognition practices. Each state has incentive to defend the doctrine because it shields them from external interference. They adjudicate what counts as 'domestic affairs' and enforce the non-interference principle through diplomatic recognition and sanctions against violators. Their exit option is to defect to rival frameworks (conditional or graduated sovereignty), but this risks delegitimization and isolation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit disproportionately from absolute-sovereignty doctrine because it protects them from external pressure to reform governance, restrict repression, or accommodate rights-claiming populations. They have strong incentive to defend the doctrine; exit would mean acceptance of external accountability mechanisms. Their identity as 'sovereign states' is bound to the absolute reading—rejection of it would delegitimize their claim to rule. The constraint enables them to extract from their domestic populations (military conscription, taxation, forced labor, surveillance) with reduced risk of international intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, biographical, identity_locked, global).

% Bear the costs of state-level extraction, violence, and repression. They are explicitly excluded from standing in international law by the absolute-sovereignty doctrine—their suffering is classified as 'domestic affairs' beyond external remedy. They cannot exit the jurisdiction without state permission (border controls, passport restrictions) and cannot appeal to external authorities for protection. The constraint operates as pure extraction from their perspective: they pay in violence, displacement, and rights deprivation while receiving no protection or remedy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, persecuted_domestic_populations, payer,
    powerless, immediate, trapped, local).

% Benefit from the mutual non-interference shield (no external regime-change operations targeting them) but face domestic political pressure from constituencies demanding intervention in humanitarian crises. They experience the constraint as both coordination (the mutual shield) and asymmetric extraction (they are prevented from helping persecuted populations despite domestic demands to do so, and this creates internal legitimacy friction). Their exit options are constrained by the need to maintain international legal standing and alliance relationships.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, democratic_states, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, democratic_states, payer).

% Institutional seats (UN Human Rights Council, International Court of Justice, International Criminal Court) that document violations of international humanitarian law but are formally prohibited by the absolute-sovereignty doctrine from taking enforcement action without state consent or UN Security Council authorization. They can publicize and condemn but cannot intervene. Their role is to witness the gap between the doctrine's claims and its effects.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_bodies, observer,
    analytical, generational, analytical, global).

% Have de facto capacity to violate the absolute-sovereignty doctrine (military intervention, regime change, sanctions) but have incentive to enforce it against each other to prevent mutual interference. They negotiate the boundaries of 'domestic affairs' through geopolitical bargaining and can selectively invoke humanitarian exception to justify interventions serving their interests. The constraint's persistence depends on their willingness to maintain mutual non-interference at the level of great-power relations, even while they pursue strategic interests through proxy involvement in weaker states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, great_powers, agenda_setter,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power domination and territorial conquest by establishing a mutual non-interference principle: if states respect each other's sovereignty, they avoid cycles of intervention and counter-intervention that destabilize the international system. Solves the commitment problem of great-power relations.
% TRANSFER_FUNCTION: Transfers authority over domestic populations from international law to state governments. The constraint moves the legitimacy to use force, coerce, surveil, and tax from international regulatory bodies to domestic states. It transfers political freedom of action to governments (which can repress without external sanction) and transfers political constraint away from persecuted populations (who cannot appeal to external authorities).
% ABSENT_VOICES: Persecuted domestic populations are explicitly excluded from standing; they cannot participate in international negotiations about the sovereignty doctrine. Rival great powers that might intervene are excluded by the doctrine itself. Non-state actors (multinational corporations, NGOs, individuals with cross-border grievances) are excluded from the sovereignty framework and thus have no formal standing. Alternative framings of legitimacy (cosmopolitan human rights, indigenous sovereignty, diaspora constituencies) are excluded by the state-centric structure of the doctrine.
% DISAPPEARANCE_RATIONALE: If absolute-sovereignty doctrine disappeared, international law would immediately reorganize around conditional or graduated frameworks. States would face new external accountability for human rights violations, genocide, displacement, and environmental damage. Intervention capacity would shift from geopolitical negotiation to rules-based legitimacy criteria. Persecuted populations would gain external recourse and protection options. The international system would move toward cosmopolitan legitimacy rather than state-centric legitimacy. Authoritarian regimes would lose their current shield and face new external pressure. The entire structure of UN bodies, treaty law, and diplomatic standing would require renegotiation.
% FOUNDING_PROBLEM: After the Thirty Years War, European states competed for territorial dominion, religious authority over populations, and hegemonic control. This generated cycles of intervention, counter-intervention, and devastating wars. The foundational problem was mutual insecurity in a multipolar system where any state could invade another with minimal international consequence. The Westphalian solution was mutual recognition of sovereignty and territorial integrity: if states agreed not to interfere in each other's internal affairs, they could achieve security through mutual deterrence rather than through conquest and hegemony.
% FOUNDING_PROBLEM_CORROBORATION: International Relations scholars (Krasner, Philpott, Keene) from outside the state-centric tradition attest that the founding problem (great-power domination and territorial conquest) remains live in certain geopolitical regions (contested borders, competing spheres of influence, resource competition). However, humanitarian law scholars, human rights advocates, and persecuted populations widely attest that the founding problem has been substantially displaced by a different problem: the shield against interference enables systematic internal violence against populations. The state-centric security framework solved the interstate war problem but created a population-level security crisis that the absolute-sovereignty doctrine prevents from being remedied.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) reflects that the constraint genuinely solves a coordination problem—prevention of great-power domination and territorial conquest—but simultaneously enables authoritarian regimes to extract from their populations with reduced external constraint. The measurement series show creeping extractiveness over the interval (0.38→0.52), indicating that as humanitarian crises accumulate and refugee flows cross borders, the doctrine's protective function for persecuted populations degrades and its protective function for authoritarian extractors strengthens. Suppression (0.68) is high because enforcement of absolute non-interference requires active prohibition of humanitarian intervention, refugee sanctuary, and external enforcement of international law—these prohibitions are maintained against rising domestic political pressure in democracies. Theater ratio (0.42) is moderate, indicating that performative adherence to sovereignty doctrine (rhetorical invocation, symbolic UN gestures) increases relative to actual enforcement as the gap between doctrine and humanitarian crisis widens. Accessibility collapse (0.71) reflects that once a state is recognized as sovereign by the international system, alternatives to submission to the doctrine are structurally unavailable—a state cannot simultaneously exist in the international system and reject sovereignty norms without loss of diplomatic standing and legal capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a sovereign state (especially a stable, powerful one), this constraint appears as genuine coordination—a mutual non-aggression pact that enables all states to exist securely. From the seat of an authoritarian regime, it is a shield against external accountability. From the seat of a persecuted population inside that regime, it is pure extraction: the doctrine explicitly forbids external rescue. The engine will compute different effective extractiveness values for each seat because directionality differs—beneficiaries see low χ (subsidy), targets see high χ (extraction). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states (powerful, organized, institutional, arbitrage exit—they can form alliances or withdraw from institutions) experience the constraint with low directionality (d ≈ 0.2-0.3): the constraint benefits them via the mutual shield and they actively maintain it. Authoritarian regimes (institutional power, constrained exit via diplomatic isolation, but gatekeeping domestic population) experience it with very low directionality (d ≈ 0.1-0.2): extraction flows to them from the populations they govern, shielded by the non-interference doctrine. Persecuted domestic populations (powerless, trapped exit, universal scope as they are dispersed across borders) experience the constraint with high directionality (d ≈ 0.75-0.85): they bear the cost of non-interference and have no remedy. The asymmetry is structural: the beneficiary-set (states claiming sovereignty) is organized and has enforcement capacity; the victim-set (persecuted populations) is dispersed across jurisdictions and formally excluded from standing in international law. Democratic states occupy a middle position (d ≈ 0.45-0.55): they benefit from the mutual shield but face domestic pressure to intervene, creating internal conflict about the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolute-sovereignty reading avoids mislabeling pure extraction as coordination by explicitly naming the asymmetry: the constraint genuinely coordinates state behavior (mutual deterrence of intervention) while simultaneously enabling extraction from populations that cannot appeal outside their state. A pure-rope reading would deny the extraction; a pure-snare reading would deny the coordination. The Tangled Rope classification holds the asymmetry intact: coordination for one party (states), extraction enabled for another (regimes), and victims excluded (persecuted populations). The measurement series documenting rising theater_ratio and rising extractiveness over the interval suggests mandatrophy may be developing—as humanitarian crises accumulate and the gap between doctrine and reality widens, the constraint increasingly persists by rhetorical maintenance rather than genuine coordination value. The engine will compute whether this pattern triggers the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_sovereignty_vs_normative_claim,
    'Is absolute non-interference a descriptive claim about how sovereignty actually functions, or a normative prescription for how it should function?',
    'Historical analysis of state-to-state interference patterns vs. formal doctrine declarations. Quantify covert interventions, sanctions regimes, and humanitarian exceptions relative to stated absolute-sovereignty commitments.',
    'If descriptive (actual practice), the constraint''s ε should lower substantially (~0.25) because the gap between doctrine and practice is massive. If normative, ε remains high (~0.52) as a formal binding norm despite widespread violation. The reading lives in the normative space; if the empirical record dominates, the reading becomes a false-summit mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_sovereignty_vs_normative_claim, empirical, 'Whether absolute sovereignty is a true empirical constraint on state behavior or a formal doctrine widely violated in practice.').

omega_variable(
    repressive_regime_sovereignty_asymmetry,
    'Do repressive authoritarian regimes genuinely benefit from the absolute-sovereignty doctrine in a way democracies do not, or is the benefit equally distributed across regime types?',
    'Comparative analysis: measure frequency and scale of external interference (military, economic sanctions, diplomatic pressure) across regime types controlling for geopolitical position. Partition by regime transparency/repression indices.',
    'If benefit is asymmetric (authoritarian regimes experience less interference per human rights violation than democracies per equivalent acts), the doctrine functions as a Snare specifically for repressive states, not a Tangled Rope serving all states equally. This shifts the reading toward capturing the asymmetry as a secondary victim classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repressive_regime_sovereignty_asymmetry, empirical, 'Whether the absolute-sovereignty shield protects repressive regimes asymmetrically relative to other state types.').

omega_variable(
    kernel_reading_boundary,
    'Is this reading of the westphalian_sovereignty kernel internally stable, or does the contradiction between ''absolute authority over domestic affairs'' and ''observed state practice'' force drift into conditional_sovereignty or graduated_sovereignty framings?',
    'Track institutional responses to major humanitarian crises (genocide, mass displacement) over the measurement interval. If formal doctrine remains unchanged (reference_frame stable) while practice increasingly routes through humanitarian exceptions, drift_state direction is practice_drift. If institutional authority shifts to acknowledge conditions on sovereignty, direction is authority_erosion.',
    'This omega documents whether the reading''s reference frame (absolute sovereignty as doctrine) remains coherent or whether the drift vector itself resolves the kernel contest toward a sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the absolute-sovereignty reading remains internally coherent or whether observed drift forecloses it into a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(west_tr_t5, observed).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(west_tr_t15, observed).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(west_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(west_be_t5, observed).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(west_be_t15, observed).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(west_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(west_su_t5, observed).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(west_su_t15, observed).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(west_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_prohibition).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, state_recognition_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalian_sovereignty kernel. The absolute_sovereignty reading (this file) asserts unconditional non-interference; the conditional_sovereignty and graduated_sovereignty readings propose conditions or gradations on sovereignty claims. Each reading has its own ε, beneficiary/victim structure, and constraint type. They coexist as competing framings in contemporary international law rather than as single constraint viewed from multiple angles. The network links identify the family kinship and the direction of influence: absolute_sovereignty grounds the formal international law regime but faces pressure from humanitarian interventionism and human-rights-conditional frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, powerless, 0.82).
constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
