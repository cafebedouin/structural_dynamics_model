% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate Requires Unmediated Sovereign Exercise (Loyalist Restoration Reading)
 *   domain: political_philosophy/constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The loyalist restoration reading asserts that the divine mandate vested
 *   in the Japanese emperor requires unmediated, active exercise of sovereign
 *   authority — that legitimacy and governance are inseparable. This reading
 *   frames the shogunal system (bakufu) as a corruption or usurpation of the
 *   mandate, not a legitimate delegation. Under this reading, the emperor
 *   must personally govern, or the mandate is void. The constraint is the
 *   institutional pressure to either restore direct imperial rule or
 *   theoretically delegitimize the shogunate by exposing the separation of
 *   authority from legitimacy. Neo-Confucian scholars and restoration
 *   intellectuals authored and maintained this reading; the shogunal
 *   bureaucracy and samurai administrative class paid in institutional
 *   jeopardy. The reading gained material force in the 19th century when
 *   foreign pressure and regime instability made the institutional rupture
 *   plausible. The constraint is CLAIMED as tangled rope (genuine
 *   coordination problem of unified sovereignty + asymmetric extraction from
 *   the bureaucracy) and the metrics match: substantial extractiveness (the
 *   reading threatens entire power structures) and suppression (the bakufu
 *   must suppress the reading's institutional implications to survive).
 *
 * KEY AGENTS:
 *   - Restoration faction intelligentsia (neo-Confucian scholars, court advisors): articulate and maintain the doctrine; benefit from its intellectual authority and eventual political power
 *   - Shogunal bureaucracy (bakufu officials, magistrates): defend the delegation doctrine; face institutional threat from the unmediated-sovereignty claim
 *   - Samurai class administrators: derive status from shogunal hierarchy; face status erosion under the restoration reading
 *   - Imperial court: elevated from figurehead to active sovereign in principle; material gains if the reading becomes institutional fact
 *   - Foreign powers: structurally excluded but materially relevant; their pressure creates the crisis that makes the restoration reading urgent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate Requires Unmediated Sovereign Exercise (Loyalist Restoration Reading)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '5d441321-78b3-4f69-906e-fd58fd5e3557').
narrative_ontology:cs_kernel_codification('5d441321-78b3-4f69-906e-fd58fd5e3557', distributed).
narrative_ontology:cs_authority_grounding('5d441321-78b3-4f69-906e-fd58fd5e3557', lineage).
narrative_ontology:cs_interpretation_layer_present('5d441321-78b3-4f69-906e-fd58fd5e3557').
narrative_ontology:cs_reading_relation('5d441321-78b3-4f69-906e-fd58fd5e3557', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('5d441321-78b3-4f69-906e-fd58fd5e3557', foundational, mandate_requires_unmediated_governance).
narrative_ontology:cs_axiom_status(mandate_requires_unmediated_governance, holdable).
narrative_ontology:cs_axiom_grounding('5d441321-78b3-4f69-906e-fd58fd5e3557', mandate_requires_unmediated_governance, deontological).
narrative_ontology:cs_axiom('5d441321-78b3-4f69-906e-fd58fd5e3557', foundational, legitimacy_inseparable_from_active_rule).
narrative_ontology:cs_axiom_status(legitimacy_inseparable_from_active_rule, holdable).
narrative_ontology:cs_axiom_grounding('5d441321-78b3-4f69-906e-fd58fd5e3557', legitimacy_inseparable_from_active_rule, deontological).
narrative_ontology:cs_reference_frame('5d441321-78b3-4f69-906e-fd58fd5e3557', unified_imperial_sovereignty_doctrine).
narrative_ontology:cs_drift_state('5d441321-78b3-4f69-906e-fd58fd5e3557', meiji_restoration_rupture_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('5d441321-78b3-4f69-906e-fd58fd5e3557', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restoration_faction_intelligentsia).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunal_bureaucracy).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_class_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Neo-Confucian scholars and politically connected literati who articulate the doctrine that the imperial mandate requires unmediated sovereign exercise. They hold administrative and advisory positions that gain legitimacy and scope through the restoration ideology. Their reading of classical texts frames delegation as usurpation and direct imperial governance as restoration of natural order. Exit means abandoning the intellectual framework and career position that constitutes their identity.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restoration_faction_intelligentsia, agenda_setter,
    organized, generational, identity_locked, national).

% The administrative apparatus of the shogunal regime (bakufu) — magistrates, officials, military governors — whose institutional legitimacy is premised on the reading that the emperor delegated sovereign authority to the shogunate. The restoration reading threatens their entire institutional justification. They must either defend the delegation doctrine against intellectual challenge or lose standing. They cannot exit without the shogunate's dissolution.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunal_bureaucracy, payer,
    institutional, generational, trapped, national).

% The military-administrative class whose status and authority derive from service within the shogunal hierarchy. The restoration reading frames their authority as delegated by the shogun (itself an illegitimate usurpation) and thus doubly compromised. They face status erosion and potential displacement if the restoration reading gains institutional traction. Their exit is constrained by the absence of alternative power structures that would accept them with preserved rank.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, samurai_class_administrators, payer,
    powerful, generational, constrained, national).

% The court has long been dependent on the shogunal regime for material support while maintaining symbolic ritual primacy. The restoration reading promises to elevate the court from ceremonial figurehead to active sovereign — a gain in legitimacy and potential authority without requiring the court to overturn centuries of de facto delegation. The court can adopt the restoration reading without major institutional restructuring if external pressure (foreign contact, regime crisis) provides the opening.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court, beneficiary,
    institutional, generational, mobile, national).

% Western imperial powers and Japanese merchant contacts who would not participate in Japanese political theology directly but whose presence and military/economic pressure drives the internal political contest. They are excluded from the framework of the constraint itself but shape the material conditions that make the restoration reading urgent and plausible as a response to perceived institutional weakness.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_powers_and_colonial_pressure, excluded,
    powerful, biographical, trapped, universal).

% Anti-shogunal samurai factions, ronin movements, and rural uprisings that serve as the structural counterweight to shogunal authority. They do not author the restoration reading but benefit from its intellectual legitimation of anti-shogunal action. They testify to the reading's power through their organization and resistance.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_restoration_movements, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, restoration_faction_intelligentsia).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a principle of unified imperial sovereignty that resolves ambiguity about legitimate governance authority: by insisting the emperor must actively govern (not merely legitimate), the reading eliminates the medieval compromise where the emperor delegated while retaining symbolic supremacy. It coordinates a vision of governance in which administrative authority and legitimacy authority cannot be separated.
% TRANSFER_FUNCTION: Transfers political legitimacy and institutional authority from the shogunal bureaucracy toward the imperial court and the restoration intelligentsia who interpret the mandate doctrine. The shogunate and samurai class pay in prestige, authority, and ultimately institutional survival if the reading displaces the delegation doctrine.
% ABSENT_VOICES: The voices of bakufu ideologues who defend delegation doctrine are structurally excluded from the restoration reading's own framework — the reading defines their position as illegitimate usurpation, making their objection incoherent within its logic. Foreign commercial interests and colonial powers are materially excluded but not theoretically — they cannot speak from within the imperial mandate theology.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the delegation doctrine regained undisputed authority, the shogunate's institutional legitimacy would be restored, samurai administrative hierarchies would stabilize, and the institutional rupture of the 1868 Meiji Restoration would not occur (or would be framed differently). The political history of Japan hinges on whether unmediated imperial sovereignty is doctrinally required.
% FOUNDING_PROBLEM: Centuries of de facto shogunal governance created an institutional contradiction: the emperor retained symbolic-ritual supremacy but lacked administrative authority. The restoration reading was built to resolve this contradiction by asserting that legitimacy and governance cannot be separated — the emperor must rule actively or the mandate is void. The founding problem is theological: how can delegated authority be legitimate if legitimacy derives from the person of the emperor?
% FOUNDING_PROBLEM_CORROBORATION: Neo-Confucian scholars cited classical Chinese texts (Mencius on the mandate, Confucian administrative virtue) as corroboration that the founding problem is real and urgent. However, shogunal scholars offered competing textual readings affirming that delegation preserves the emperor's essential sacred function. Foreign observers (Western diplomats, historians) noted the institutional instability but did not author the theological solution. The corroboration is entirely internal to Japanese political theology and contested between reading communities.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the reading imposes a legitimacy standard the bakufu cannot fully meet — it extracts institutional authority by redefining legitimacy itself. The shogunate can defend against military challenge but cannot defend against the claim that its very existence violates the mandate doctrine. Suppression is high (0.72) because the reading's institutional implications are so threatening that active suppression is required: censoring texts, limiting school curricula, controlling which scholars gain positions. Theater is moderate (0.48) because the restoration reading contains a real coordination function (unified sovereignty) but increasingly operates as intellectual legitimation for anti-shogunal movements — the performative element grows as the doctrine becomes a cover story for political struggle. Accessibility collapse is high (0.79) because once the reading is articulated, the alternative (delegation as permanent feature) becomes theoretically unthinkable within the restoration framework — the logic forecloses the delegation option. The measurement series shows rising extraction and suppression over the interval: the reading gains intellectual force and institutional threat as foreign pressure mounts and regime instability increases. Theater stabilizes at t=15 onwards, suggesting the constraint transitions from intellectual argument to political strategy.
 *
 * PERSPECTIVAL GAP:
 *   From the intelligentsia seat, this is a coordination function resolving a theological contradiction — the mandate requires active governance, delegation is incoherent, restoration is necessary. From the shogunal seat, it is pure extraction: an intellectual attack on the legitimacy of the entire institutional structure, justified by textual reinterpretation, with no practical benefit to the shogunate. From the imperial court seat, it is a gift — legitimacy and authority transferred without requiring the court to act militarily. These divergences should compute: the engine derives directionality from the structural data (who benefits, who bears costs, what exit looks like), and the classified types should diverge across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The restoration intelligentsia are the beneficiaries and agenda-setters: they define the mandate doctrine and gain authority as their reading becomes institutionalized. Their directionality is near the beneficiary end (d~0.15-0.25) — they collect authority and legitimacy without running the government initially. The shogunal bureaucracy and samurai class are the targets: they pay in institutional prestige and face displacement if the reading becomes institutional fact. Their directionality is near the target end (d~0.75-0.85) — they bear the threat of institutional rupture. The imperial court is positioned as a beneficiary but with higher exit options (mobile) — they gain legitimacy and eventual authority without having to overturn the regime themselves; the intelligentsia and external pressure do that work. Foreign powers are excluded from the doctrinal framework but shape its urgency material conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy potential: the founding problem (theological incoherence of separated authority and legitimacy) is a real problem, but the restoration reading conflates solving the theological problem (declaring the mandate requires unmediated governance) with solving the material problem (actually governing effectively). The reading gains institutional power not because it solves the founding problem but because it provides intellectual legitimation for anti-shogunal movements that have other motives (power, foreign response, samurai status anxiety). By t=30, the constraint is operating partly as legitimation theater — the actual drivers of institutional change are military pressure and foreign contact, not theological coherence. This is the mandatrophy signature: the constraint persists because it narrates change, but change is driven by material conditions the constraint does not address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_interpretation_closure,
    'Is the restoration reading''s textual interpretation of classical sources (Mencius, Confucian governance texts) the only defensible reading, or can the delegation doctrine claim equal textual grounding?',
    'Comparative analysis of textual scholarship from restoration and bakufu intellectual communities; philological examination of the source texts themselves to determine whether the sources support multiple readings or enforce one.',
    'If the sources admit multiple interpretations with equal grounding, the restoration reading is a choice among live options, not a discovery of eternal doctrine — this would reframe it as extractive preference rather than coordinate solution. If the sources enforce the restoration reading, the reading gains firmer theoretical foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_interpretation_closure, conceptual, 'Whether classical texts permit or preclude the delegation doctrine.').

omega_variable(
    identity_locked_exit_persistence,
    'If the restoration reading vanishes and the delegation doctrine regains exclusive authority, does the intelligentsia''s identity-locking persist, or can they seamlessly reidentify with the new doctrine?',
    'Historical comparison to scholars who shifted between bakufu and Meiji service; examination of reorientation costs (loss of status, retraining, ideological dissonance) faced by scholars who switched doctrinal allegiance.',
    'High persistence indicates the identity-locking is internalized and the suppression of the restoration reading must be particularly intensive. Low persistence suggests the intelligentsia can exit without identity rupture, making the reading less extractive than appears.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_persistence, empirical, 'Whether identity-locking to the restoration doctrine is structural or ideologically contingent.').

omega_variable(
    foreign_pressure_orthogonality,
    'Is the institutional rupture (Meiji Restoration) driven primarily by the internal theological contradiction the restoration reading articulates, or primarily by foreign pressure and regime instability that make the reading suddenly useful as legitimation?',
    'Counterfactual analysis: what would the path of institutional change be if foreign contact had not occurred? Did foreign pressure merely accelerate inevitable theological reckoning, or was the theological problem latent and activated by material crisis?',
    'If driven primarily by foreign pressure, the restoration reading is an opportunistic narrative for underlying structural change — highly extractive, moderate coordination. If driven by theological necessity, it is genuine coordinate solution to a standing problem. The measurements show rising extraction precisely when foreign pressure intensifies (t=0–15), supporting the opportunistic narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_pressure_orthogonality, preference, 'Whether the restoration reading solves a real theological problem or narrates material change driven by external pressure.').

omega_variable(
    delegation_doctrine_alternative_formulation,
    'Could the delegation doctrine be reformulated to assert that the emperor delegates the governing function but retains the legitimacy-granting and oversight function — thus preserving unified sovereignty while permitting administrative delegation?',
    'Examination of whether bakufu-era intellectuals actually proposed such a formulation, or whether the constraint requires choosing between delegation (illegitimate per restoration) and unmediated rule (impractical per bakufu).',
    'If a middle-ground formulation is historically available, the restoration reading''s foreclosure of delegation is a choice among live options, not a logical necessity. This would reframe the reading as extractive preference rather than coordinate solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_doctrine_alternative_formulation, conceptual, 'Whether the restoration and delegation readings exhaust the logical space or whether a synthesizing position is theoretically available.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(impe_tr_t5, imperial_mandate__loyalist_restoration_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(impe_tr_t10, imperial_mandate__loyalist_restoration_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(impe_tr_t15, imperial_mandate__loyalist_restoration_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(impe_tr_t20, imperial_mandate__loyalist_restoration_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(impe_tr_t25, imperial_mandate__loyalist_restoration_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(impe_tr_t30, imperial_mandate__loyalist_restoration_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(impe_be_t5, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(impe_be_t10, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(impe_be_t15, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(impe_be_t20, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(impe_be_t25, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(impe_be_t30, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(impe_su_t5, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(impe_su_t10, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(impe_su_t15, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(impe_su_t20, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(impe_su_t25, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(impe_su_t30, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel contains two structurally distinct readings: the loyalist restoration reading (this constraint) asserts unified sovereignty requires unmediated imperial governance; the bakufu delegation reading asserts the mandate operates through institutional delegation. Each reading instantiates a different constraint with a different ε, different victims, and different institutional implications. The readings are not the same constraint viewed from different seats — they have fundamentally different structural referents (the meaning of the mandate itself) and cannot coexist in a single commitment framework, though both remain live historical positions held by different political factions. Link this constraint to its sibling via affects_constraints for constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__loyalist_restoration_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
