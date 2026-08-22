% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Jihad as Immediate Individual Obligation (Fard 'Ayn)
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Qur'anic jihad
 *   corpus: the revolutionary vanguard reading, which frames immediate armed
 *   obligation (fard 'ayn) against apostate rulers and foreign occupiers as
 *   binding on individual Muslims independent of state authority or classical
 *   jurisprudential gatekeeping. The reading employs takfir doctrine to
 *   categorize state leaders and civilians as legitimate targets, and invokes
 *   emergency jurisprudence (dharura) to override classical safeguards like
 *   non-combatant immunity and imam authority. This is structurally distinct
 *   from the defensive-spiritual reading (which emphasizes internal jihad and
 *   defensive response with non-combatant protections) and the
 *   expansionist-legalist reading (which permits offensive campaigns but
 *   requires imam authority and classical conditions). The revolutionary
 *   vanguard reading decentralizes authority, expands victim sets through
 *   collective guilt, and prioritizes immediate individual conscience over
 *   institutional hierarchy. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as a coordinate obligation structure (coordination
 *   function: mobilize decentralized resistance) but the authored metrics
 *   describe substantially extractive operation (0.89 extractiveness, 0.78
 *   suppression) with theater rising as the vanguard institutionalizes itself
 *   — the gap documents how coordination framing serves extraction.
 *
 * KEY AGENTS:
 *   - Revolutionary vanguard leadership: interprets takfir, sets emergency conditions, decentralizes obligation (organized/identity_locked)
 *   - Apostate rulers: state leaders deemed non-Islamic, targets of overthrow (institutional/trapped)
 *   - Occupier militaries: foreign forces occupying Muslim-majority territory (powerful/constrained)
 *   - Civilian populations categorized as combatants: non-combatants reclassified through collective guilt (powerless/trapped)
 *   - Orthodox jurists: classical scholars maintaining imam requirement and safeguards, structurally excluded (institutional/constrained)
 *   - Non-combatant Muslims in conflict zones: neither joining nor accepting takfir, caught between forces (powerless/trapped)
 *   - Recruitment networks and donor constituencies: benefit from mobilization and prestige (organized/powerful, with variable exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.89).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.78).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Jihad as Immediate Individual Obligation (Fard 'Ayn)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8').
narrative_ontology:cs_kernel_codification('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', fixed_text).
narrative_ontology:cs_authority_grounding('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', extraction).
narrative_ontology:cs_interpretation_layer_present('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8').
narrative_ontology:cs_reading_relation('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_axiom('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', foundational, individual_obligation_unmediated_by_institutional_authority).
narrative_ontology:cs_axiom_status(individual_obligation_unmediated_by_institutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', individual_obligation_unmediated_by_institutional_authority, deontological).
narrative_ontology:cs_axiom('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', foundational, emergency_jurisprudence_overrides_safeguard_permanence).
narrative_ontology:cs_axiom_status(emergency_jurisprudence_overrides_safeguard_permanence, holdable).
narrative_ontology:cs_axiom_grounding('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', emergency_jurisprudence_overrides_safeguard_permanence, empirically_contingent).
narrative_ontology:cs_axiom('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', secondary, apostate_and_occupier_categorization_permits_collective_guilt_targeting).
narrative_ontology:cs_axiom_status(apostate_and_occupier_categorization_permits_collective_guilt_targeting, holdable).
narrative_ontology:cs_axiom_grounding('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', apostate_and_occupier_categorization_permits_collective_guilt_targeting, conventional).
narrative_ontology:cs_reference_frame('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', individual_conscience_as_sufficient_jihad_authority).
narrative_ontology:cs_drift_state('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', contemporary_state_security_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e0c22bb2-4ac8-43a5-84b0-e40ca4ead9c8', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_doctrine_interpreters).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupier_militaries).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_categorized_as_combatants).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, orthodox_jurists_resisting_reinterpretation).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, non_combatant_muslims_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, recruitment_networks).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, donor_constituencies).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, state_authority_monopoly_on_violence_illegitimate_under_emergency).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_permits_targeting_co_religionists).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, individual_conscience_overrides_institutional_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Qur'an and hadith to extract immediate individual obligation to engage in armed struggle against apostate rulers and occupiers. Sets conditions for takfir (excommunication) that categorize vast populations as legitimate targets. Claims authority to override classical jurisprudential safeguards via emergency doctrine (dharura). Maintains the vanguard's role as guide and beneficiary of the mobilization it orchestrates.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership, agenda_setter,
    organized, generational, identity_locked, global).

% Muslim-majority state leaders deemed by the vanguard to have abandoned Islamic law in favor of secular governance, foreign alliances, or insufficiently strict Sharia implementation. Targeted by armed campaigns for overthrow or replacement. Classified as apostates by takfir doctrine, thereby losing conventional combatant protections. Face existential threat to their regime.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, biographical, trapped, national).

% Foreign military forces controlling territory or conducting operations deemed occupation. Defined as aggressor-combatants and targeted for removal. The presence of occupation is the canonical emergency justifying decentralized jihad doctrine and override of institutional safeguards.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupier_militaries, payer,
    powerful, biographical, constrained, regional).

% Muslims and non-Muslims in conflict zones, civilians who support secular governments, or those judged complicit through passive participation in occupier-influenced systems. Reclassified through collective guilt, affiliation, or proximity as legitimate targets. Have no institutional recourse and cannot exit the categorization.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilian_populations_categorized_as_combatants, payer,
    powerless, immediate, trapped, local).

% Classical Islamic jurisprudents (fuqaha) and contemporary scholars who maintain that individual jihad obligation (fard 'ayn) requires imam authority, prior invitation to Islam, and strict non-combatant protections. Marginalized, discredited, or targeted for their textual authority that contradicts the vanguard reading. Their exclusion from authority is structural to the constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, orthodox_jurists_resisting_reinterpretation, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, orthodox_jurists_resisting_reinterpretation, excluded).

% Muslims who do not join the armed struggle, who flee conflict zones, or who refuse to accept takfir categorizations. Caught between occupier-state violence and vanguard pressure to join. Lack exit and face social marginalization, material pressure, or targeting as non-combatants supporting apostate regimes.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, non_combatant_muslims_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Networks of sympathetic clerics, community leaders, and ideological organizers who benefit from the prestige and resource mobilization that vanguard doctrine provides. May frame participation as religious obligation while materially benefiting from organizational hierarchy and donor support.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, recruitment_networks, beneficiary,
    organized, biographical, constrained, global).

% State and non-state actors (foreign powers, diaspora funding, ideological supporters) who provide resources to revolutionary movements. May benefit from destabilization of rivals, proxy geopolitical advantage, or sectarian preference. Maintain exit through funding channels and plausible deniability.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, donor_constituencies, beneficiary,
    powerful, biographical, mobile, global).

% Scholars of Islamic law, security analysts, and comparative religionists who study the constraint's operation without participating in its legitimacy claims or its armed actions. Attempt to document how textual reinterpretation reshapes obligation structures.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leadership).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes decentralized armed action against state and occupier authority by reframing individual conscience as carrying binding obligation (fard 'ayn), collapsing the distinction between personal piety and armed combat. Solves the collective-action problem of coordinating resistance without institutional hierarchy by making defection a religious violation.
% TRANSFER_FUNCTION: Transfers authority from classical jurisprudential gatekeepers and state institutions to the vanguard interpreters and individual conscience. Moves loyalty from national governments to transnational religious obligation. Transfers willingness to risk death from state-conscription frameworks to religious-duty frameworks. Transfers legitimacy from proportionality-constrained warfare to emergency-doctrine-overridden targeting.
% ABSENT_VOICES: Classical Islamic jurists and their contemporary descendants are structurally excluded — their textual authority contradicts the vanguard reading and is actively discredited. Non-combatant Muslims in conflict zones who resist conscription or question takfir doctrine are silenced through social pressure and collective-guilt reclassification. Victims of vanguard violence have no authorized voice in the interpretation process that justifies their targeting.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if the vanguard reading lost all authority, if takfir doctrine ceased operative, if emergency overrides of classical safeguards collapsed — decentralized armed mobilization would fragment, recruitment would plummet, and state security forces would regain monopoly on violence. The constraint's removal would reshape power dynamics between central authority and distributed actors across multiple regions simultaneously.
% FOUNDING_PROBLEM: Occupation and governance by rulers deemed non-Islamic or insufficiently Islamic; classical institutional safeguards (imam requirement, textual consensus) block immediate armed response; state monopoly on legitimate violence prevents populations from defending against aggression or apostasy without institutional permission.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard interpreters attest the founding problem is live and permanent under occupation; classical jurists attest the stated founding problem is a false diagnosis, one that misreads Islamic law and creates pretexts for violence; international security analysts attest that occupation and governance crises exist but contest whether the vanguard reading follows from Islamic texts or represents ideological reinterpretation; victimized populations attest that the constraint's operation causes the very instability it claims to solve.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint transfers substantial compliance, risk of death, and obedience from individuals to vanguard authorities via reframing of obligation. It extracts willingness to fight, material resources, loyalty, and life itself from those categorized as bound by fard 'ayn. Suppression is high (0.78) because alternatives are actively suppressed: classical jurisprudential voices are discredited, non-combatants are pressured into compliance through collective guilt, and exit is constrained by social sanction and physical danger. Theater ratio rises from 0.31 to 0.42 over the interval as the vanguard institutionalizes (claims of scholarly consensus, administrative structures, courts) — as institutions mature, the percentage of activity devoted to legitimacy performance (rather than direct violence) increases, a Goodhart drift indicating that the vanguard's organizational interests may diverge from the founding problem's solution. Accessibility collapse is moderate-high (0.72): once the revolutionary reading is known, alternatives become cognitively available but structurally closed through takfir pressure and occupier-state violence. Resistance is high (0.81): classical scholars actively resist the reading through textual refutation, state security forces resist through counterinsurgency, and many non-combatant Muslims resist through non-participation and public criticism. The measurement grid captures extraction accumulation (0.71→0.89) and suppression intensification (0.58→0.78) over 40 time points, showing a constraint that hardens as the vanguard consolidates.
 *
 * PERSPECTIVAL GAP:
 *   The vanguard leadership and recruited vanguard members perceive the constraint as liberatory obligation freed from institutional gatekeeping — they see coordination (mobilizing decentralized resistance) and justice (targeting illegitimate authority). Apostate rulers and occupier militaries perceive it as terror doctrine designed to destabilize their authority. Orthodox jurists perceive it as textual misreading that violates Islamic law's own safeguards. Non-combatant Muslims caught in conflict zones perceive it as coercive pressure that serves neither their safety nor their religious conviction. Victims who face targeting through takfir categorization perceive it as violence dressed in religious language. The engine computes divergent directionality (d-values) for each seat from the beneficiary/victim structure and exit options: vanguard leadership sits near d=0.0 (full beneficiary, identity-locked), apostate rulers sit near d=1.0 (full target, trapped), non-combatant Muslims sit near d=0.7-0.8 (high target, trapped but not as completely as rulers), orthodox jurists sit near d=0.6 (target of discreditation, constrained exit), donor constituencies sit near d=0.1-0.2 (indirect beneficiaries, mobile). This divergence is not a defect — it is precisely what the constraint's structural asymmetry produces.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard leadership: Declared beneficiary (collects prestige, authority, obedience, and in some cases material resources). Identity-locked exit (professional identity fused with vanguard role, religious conviction that makes exit apostasy). Power atomic: organized (hierarchical movement structure). Directionality: d ≈ 0.0-0.15 (full to near-full beneficiary). Apostate rulers: Declared victim (targeted for overthrow, face existential threat). Trapped exit (cannot leave the position of state leader without relinquishing state). Power atomic: institutional. Directionality: d ≈ 1.0 (full target). Occupier militaries: Declared victim (targeted for removal, face material violence). Constrained exit (military personnel cannot easily leave service). Power atomic: powerful. Directionality: d ≈ 1.0 (full target, though more mobile than apostate rulers). Civilian populations: Declared victim (reclassified as combatants, face violence). Trapped exit (cannot exit war zone, cannot exit religious/ethnic categorization). Power atomic: powerless. Directionality: d ≈ 0.8-0.9 (near-full target). Orthodox jurists: Declared victim (discredited, excluded from authority). Constrained exit (cannot easily abandon Islamic jurisprudence tradition without losing identity and institutional position). Power atomic: institutional. Directionality: d ≈ 0.6-0.7 (significant target, though less complete than civilians). Non-combatant Muslims: Declared victim (pressured into compliance, caught between forces). Trapped exit (cannot leave identity or geography, cannot escape pressure). Power atomic: powerless. Directionality: d ≈ 0.7-0.8 (high target). Recruitment networks: Declared beneficiary (access to prestige, organizational resources, ideological authority). Constrained exit (personal conviction, social position within network). Power atomic: organized. Directionality: d ≈ 0.2-0.3 (beneficiary, but more constrained than leadership). Donor constituencies: Declared beneficiary (geopolitical advantage, destabilization of rivals, sectarian preference). Mobile exit (can redirect funding, change allies). Power atomic: powerful. Directionality: d ≈ 0.0-0.2 (beneficiary, with highest exit). The benignity of directionality values — that vanguard leadership approaches pure beneficiary while civilians approach pure target — reflects the constraint's structural asymmetry: it serves some while harming others, and those it serves have ways to benefit that those it harms do not have ways to escape.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is occupation and governance deemed non-Islamic, which creates collective action failure in mounting resistance without state apparatus. The vanguard reading solves this by decentralizing obligation and overriding state-monopoly constraints. However, the six_questions.founding_problem_status is CONTESTED, and the disappearance_verdict is WORLD_REARRANGES, which surfaces a critical mandatrophy risk: if the founding problem is actually solved (occupation ends, apostate rulers are replaced with Islamic governance), does the constraint's operation cease, or does it persist because the vanguard benefits from the obligation structure independently of whether the original problem still exists? The measurement series shows extraction rising (0.71→0.89) while the founding problem may be contested or solved, a sign the constraint has shifted from solving emergency problems to serving organizational interests. Theater ratio rising (0.31→0.42) compounds this signal: the vanguard is investing more in legitimacy performance (courts, schools, jurisprudential claims) while extraction grows, a classic Piton or Snare pattern where the function atrophies but institutional maintenance persists. However, resistance remains high (0.81), which means the constraint is not yet theater-dominant — victims and orthodox scholars actively contest it. The classification remains SNARE (not yet PITON) because the extraction is still active and the suppression is still forceful; theater has not yet consumed the operative function. Mandatrophy resolution depends on whether the vanguard's interpretation of the kernel is eventually accepted as canonical (in which case the constraint becomes institutionalized as Islamic law and ceases to be extractive) or whether it remains contested and dependent on force (in which case it remains SNARE, and any retreat by the vanguard leads to rapid institutional collapse).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_divergence,
    'Does the revolutionary vanguard reading derive necessarily from the Qur''anic corpus, or does it represent a discretionary interpretation chosen among multiple textually-grounded alternatives?',
    'Comparative Islamic jurisprudential analysis from scholars outside the vanguard tradition, examining which Qur''anic passages and hadith the defensive, legalist, and vanguard readings each cite and how they resolve apparent textual conflicts.',
    'If the reading is discretionary, the constraint''s legitimacy rests on institutional authority and adherent belief rather than textual necessity, which means takfir doctrine is a constructed boundary, not an extracted fact. If the reading is textually necessary, the constraint''s mandates follow from the kernel itself and cannot be overridden by classical interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_divergence, empirical, 'Whether the revolutionary reading is textually entailed or interpretively chosen.').

omega_variable(
    emergency_doctrine_scope_boundary,
    'What counts as an emergency (dharura) sufficient to override classical safeguards like imam authority and non-combatant immunity? Is the boundary stable and textually determined, or is it indefinitely expandable by interpreters?',
    'Document how vanguard authorities have applied the emergency doctrine over time, whether they have ever narrowed its scope, whether any triggering condition is acknowledged as sufficient to lift it, and whether classical jurists specify measurable threshold criteria.',
    'If the boundary is indefinitely expandable, emergency doctrine becomes a blank check for circumventing safeguards, and the constraint is pure extraction with theoretical safeguards that never activate. If the boundary is textually specified and historically stable, emergency doctrine represents a legitimate but bounded override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_doctrine_scope_boundary, empirical, 'Whether emergency jurisdiction is boundable or indefinitely malleable.').

omega_variable(
    takfir_targeting_collective_guilt,
    'On what textual or jurisprudential basis does takfir doctrine extend from individual apostate rulers to entire civilian populations? Is collective guilt a coherent Islamic legal concept or a constructed boundary that maximizes recruitment and targeting scope?',
    'Comparative analysis of how classical Islamic law treats collective responsibility, whether any classical school extends takfir to non-combatants, and whether the vanguard reading cites textual authority for collective-guilt categorization or relies on reframing of classical sources.',
    'If collective guilt is textually authorized, civilian targeting follows from Islamic law and the constraint''s extraction is theoretically bounded. If collective guilt is constructed, civilian targeting is a choice the vanguard makes to expand victim sets and maximize pressure, and the constraint is substantially more extractive than its framing acknowledges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takfir_targeting_collective_guilt, empirical, 'Whether civilian collective guilt is textually grounded or constructively expanded.').

omega_variable(
    kernel_reading_authenticity,
    'This constraint is ONE reading of the jihad kernel. The defensive and legalist readings each cite substantial textual authority and historical jurisprudential lineages. What criteria would establish that one reading is more authentic to the kernel than the others, or is authenticity itself contested at the kernel level?',
    'Examine whether any extant Islamic legal tradition (classical school, contemporary scholarly consensus, institutional authority) endorses one reading as canonical, or whether all three readings remain live positions with coherent textual groundings.',
    'If one reading is canonical, the others are deviations and the constraint''s legitimacy depends on institutional authority, not on textual necessity. If all remain live, the constraint''s operation depends on persuasion and recruitment rather than on inherited interpretive closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_authenticity, conceptual, 'Whether the kernel admits one canonical reading or multiple live readings.').

omega_variable(
    beneficiary_institutional_depth,
    'Do the revolutionary vanguard leadership and recruitment networks derive sustained benefit from the constraint''s operation, or do they depend on perpetual crisis and recruitment churn to maintain position?',
    'Document organizational stability, resource flows, and succession patterns in vanguard-aligned groups over multi-generational timescales. Examine whether organizations that institutionalize the constraint (build schools, courts, administration) differ in longevity and extraction patterns from those that prioritize armed action.',
    'High institutional depth would suggest the constraint creates sustainable positions of authority and benefit, which means the extraction is self-reinforcing and likely to persist. Low institutional depth would suggest the constraint depends on perpetual mobilization pressure and recruitment, which means stable governance by the vanguard may erode the constraint''s operational logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_institutional_depth, empirical, 'Whether vanguard benefit is structurally self-sustaining or dependent on perpetual crisis.').

omega_variable(
    committer_kernel_relation_defensive_vs_revolutionary,
    'The defensive_spiritual_reading and revolutionary_vanguard_reading both claim to ground obligation in the Qur''an, but they differ in how they interpret the *conditions* that activate obligation and *who* may issue the call. Is this a difference in reading the same kernel (same core premise, different application), or a difference in what the kernel SAYS (different core premises that coexist in the text)?',
    'Examine whether classical Islamic jurisprudence recognizes both defensive and offensive/vanguard jihad obligations, whether they are presented as distinct legal categories or as contextual variations on unified doctrine, and whether texts that support one reading exclude the other.',
    'If they are distinct legal categories in classical law, the constraint represents a reading choice between two kernel-grounded options, and the vanguard reading''s authority rests on institutional backing rather than textual necessity. If they are contextual variations on unified doctrine, the constraint may be better understood as a transformation of the kernel''s meaning than as one reading among equipartitioned alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_relation_defensive_vs_revolutionary, conceptual, 'Whether defensive and vanguard jihad are distinct kernel options or a single doctrine read contextually.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement_basis(jiha_tr_t0, observed).
narrative_ontology:measurement(jiha_tr_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement_basis(jiha_tr_t5, observed).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(jiha_tr_t10, observed).
narrative_ontology:measurement(jiha_tr_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(jiha_tr_t15, observed).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(jiha_tr_t20, observed).
narrative_ontology:measurement(jiha_tr_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(jiha_tr_t25, observed).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(jiha_tr_t30, observed).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(jiha_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(jiha_be_t0, observed).
narrative_ontology:measurement(jiha_be_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement_basis(jiha_be_t5, observed).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement_basis(jiha_be_t10, observed).
narrative_ontology:measurement(jiha_be_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement_basis(jiha_be_t15, observed).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(jiha_be_t20, observed).
narrative_ontology:measurement(jiha_be_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement_basis(jiha_be_t25, observed).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(jiha_be_t30, observed).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(jiha_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(jiha_su_t0, observed).
narrative_ontology:measurement(jiha_su_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(jiha_su_t5, observed).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(jiha_su_t10, observed).
narrative_ontology:measurement(jiha_su_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(jiha_su_t15, observed).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(jiha_su_t20, observed).
narrative_ontology:measurement(jiha_su_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(jiha_su_t25, observed).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(jiha_su_t30, observed).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(jiha_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.14).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_doctrine__apostate_categorization).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_jurisprudence__dharura_override).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the jihad_quranic_corpus kernel. The defensive_spiritual_reading prioritizes internal struggle and defensive response with non-combatant protections, emphasizing imam authority and proportionality safeguards — it measures as moderate extractiveness (~0.35-0.45) and low suppression (~0.25-0.35), yielding ROPE classification. The expansionist_legalist_reading permits offensive campaigns under specific jurisprudential conditions (imam authority, invitation, proportionality) — it measures as moderate-high extractiveness (~0.55-0.65) and moderate suppression (~0.45-0.55), yielding TANGLED_ROPE classification. This reading (revolutionary_vanguard) eliminates gatekeeper authority, expands victim sets, and overrides safeguards via emergency doctrine — it measures as very high extractiveness (~0.85-0.89) and high suppression (~0.75-0.78), yielding SNARE classification. The readings are linked through network.affects_constraints because each reading's authority and recruitment depend partly on the textual legitimacy of the competing readings: if the defensive reading gains canonical authority, vanguard recruitment weakens; if the vanguard reading expands territory and resources, other readings lose institutional prestige. Each reading's epsilon is stable within its own interpretation frame; the constraint family documents how different readings of the same kernel instantiate different obligation structures with radically different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, powerless, 0.85).
constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
