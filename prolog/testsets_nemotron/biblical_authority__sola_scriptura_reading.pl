% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Scripture Alone as Sufficient and Self-Interpreting Authority
 *   domain: theology/religious_studies/christianity
 *
 * SUMMARY:
 *   The sola scriptura reading of biblical authority claims that Scripture
 *   alone — without need for ecclesiastical magisterium, tradition, or
 *   conciliar decree — is sufficient and self-interpreting for doctrine and
 *   practice. Originating in the Protestant Reformation (1517), this reading
 *   restructures authority from clerical mediation to individual and
 *   congregational engagement with the text. The constraint's coordination
 *   function is epistemic democratization: any believer with access to
 *   Scripture can verify doctrine. Its transfer function moves interpretive
 *   authority from ordained office to the gathered community. The victim is
 *   cross-community doctrinal coherence: without an adjudicative monopoly,
 *   divergent readings proliferate (currently 40,000+ Protestant
 *   denominations). The beneficiary is lay believer autonomy and
 *   congregational self-governance. Clerical extraction is structurally low
 *   because no sacerdotal class controls access to salvation or doctrine;
 *   sacraments become ordinances (memorial acts) not salvific channels.
 *
 * KEY AGENTS:
 *   - lay_believers: Primary beneficiary (organized/constrained) — gains direct interpretive access, bears fragmentation cost
 *   - congregational_autonomy_movements: Secondary beneficiary (organized/constrained) — uses reading to justify self-governance
 *   - biblical_literacy_advocates: Beneficiary (organized/mobile) — promotes the reading's enabling condition
 *   - doctrinal_coherence_across_communities: Victim (abstract collective) — bears fragmentation cost with no exit
 *   - inter_denominational_unity_efforts: Victim (organized/constrained) — works against structural fragmentation
 *   - historical_continuity_claimants: Victim (institutional/constrained) — reads fragmentation as rupture with apostolic witness
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure including kernel framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.18).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.22).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Scripture Alone as Sufficient and Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "theology/religious_studies/christianity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, 'c585fb26-58ac-4f52-9110-6d54305273a9').
narrative_ontology:cs_kernel_codification('c585fb26-58ac-4f52-9110-6d54305273a9', fixed_text).
narrative_ontology:cs_authority_grounding('c585fb26-58ac-4f52-9110-6d54305273a9', lineage).
narrative_ontology:cs_reading_relation('c585fb26-58ac-4f52-9110-6d54305273a9', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('c585fb26-58ac-4f52-9110-6d54305273a9', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('c585fb26-58ac-4f52-9110-6d54305273a9', foundational, scripture_sufficient_and_perspicuous).
narrative_ontology:cs_axiom_status(scripture_sufficient_and_perspicuous, holdable).
narrative_ontology:cs_axiom_grounding('c585fb26-58ac-4f52-9110-6d54305273a9', scripture_sufficient_and_perspicuous, deontological).
narrative_ontology:cs_axiom('c585fb26-58ac-4f52-9110-6d54305273a9', foundational, priesthood_of_all_believers).
narrative_ontology:cs_axiom_status(priesthood_of_all_believers, holdable).
narrative_ontology:cs_axiom_grounding('c585fb26-58ac-4f52-9110-6d54305273a9', priesthood_of_all_believers, deontological).
narrative_ontology:cs_reference_frame('c585fb26-58ac-4f52-9110-6d54305273a9', apostolic_scripture_alone).
narrative_ontology:cs_drift_state('c585fb26-58ac-4f52-9110-6d54305273a9', contemporary_denominational_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c585fb26-58ac-4f52-9110-6d54305273a9', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, congregational_autonomy_movements).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, biblical_literacy_advocates).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, inter_denominational_unity_efforts).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, historical_continuity_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain direct interpretive access to Scripture without clerical mediation. Can read, study, and apply Scripture personally and communally. Exit from a specific congregation is possible (constrained), but exit from the reading's logic (perspicuity, priesthood of all believers) is identity_locked for committed believers — the reading constitutes their spiritual identity. Bear the cost of doctrinal uncertainty and fragmentation across communities.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    organized, biographical, constrained, global).

% Use the sola scriptura reading to justify congregational self-governance, elder-led polity, and independence from denominational hierarchies. Set local agendas for doctrine and practice. Can move between or plant new congregations (mobile exit). Benefit from the reading's anti-hierarchical structure.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_autonomy_movements, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, congregational_autonomy_movements, agenda_setter).

% Promote translation, distribution, and education so that lay believers can exercise the reading's enabling condition (access to Scripture). Their mission aligns with the reading's coordination function. Mobile exit — can shift focus to other literacy or advocacy work.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, biblical_literacy_advocates, beneficiary,
    organized, generational, mobile, global).

% An abstract collective good — shared doctrinal understanding across the body of believers — that has no agency and no exit. The reading's structure (no adjudicative monopoly) makes coherence structurally impossible to maintain. Bears the fragmentation cost without recourse.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__sola_scriptura_reading, doctrinal_coherence_across_communities).

% Ecumenical organizations and dialogue movements that work for visible unity. Their efforts are structurally opposed by the reading's fragmentation logic — unity requires an adjudicative authority the reading denies. Constrained exit: they can persist in dialogue but cannot achieve their goal within the reading's framework.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, inter_denominational_unity_efforts, payer,
    organized, generational, constrained, global).

% Communions (Orthodox, Roman Catholic, some Anglican/Lutheran) that claim continuity with the apostolic church through tradition and councils. They experience the sola scriptura reading as a rupture that fragments the one church. Constrained exit: they maintain their claim but cannot compel recognition from sola scriptura communities.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, historical_continuity_claimants, payer,
    institutional, civilizational, constrained, global).

% Sees the full kernel structure: three readings with distinct beneficiary/victim profiles, distinct ε values, and distinct coordination-extraction trade-offs. Does not collect from or pay into any reading.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables any believer with access to Scripture to verify doctrine for themselves, solving the coordination problem of epistemic access to divine revelation without a mediating priesthood.
% TRANSFER_FUNCTION: Moves interpretive authority from ordained office (priests, bishops, magisterium) to the gathered community of believers and the individual conscience. The transfer is not of material resources but of epistemic license and governance power.
% ABSENT_VOICES: The voices of those who would be subject to a magisterial authority if one existed — but in this reading, that authority is structurally absent by design. The 'absent' seat is the magisterium itself, which the reading's founding act displaced. Also absent: future generations in fragmented communities who inherit schism without consent.
% DISAPPEARANCE_RATIONALE: If the sola scriptura constraint vanished overnight, millions of congregations would lose their constitutional warrant for self-governance; denominational structures would either collapse or seek new authority sources (charismatic leadership, denominational bureaucracies, or reunion with magisterial communions); the global Protestant ecosystem (800M+ adherents) would reorganize around new authority claims.
% FOUNDING_PROBLEM: The late medieval Western church restricted vernacular Scripture access, tied salvation to sacramental mediation by a priesthood, and claimed magisterial authority to define doctrine beyond Scripture's explicit teaching. The founding problem was clerical mediation as a barrier to the believer's direct access to God's self-revelation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by communities in contexts where clericalism restricts Scripture access (parts of Global South, some high-church traditions). It is attested as dead by historians noting universal vernacular Scripture availability and literacy in the West. Corroboration from outside beneficiaries: Roman Catholic and Orthodox interlocutors acknowledge the historical problem but argue the reading's solution (no magisterium) creates worse fragmentation; secular historians of the Reformation (e.g., Brad Gregory, Carlos Eire) document the problem and its contested resolution.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).
:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the reading structurally disables clerical rent-seeking: no priesthood controls sacramental grace, no magisterium licenses interpretation. Suppression is low-moderate (0.22) because exit from any given community is possible (mobile/constrained), though identity_locked dynamics operate for some believers. Theater ratio is low (0.12) — the reading's practices (personal study, congregational discernment) are its function. Accessibility collapse is moderate (0.35) — alternatives (tradition, councils) remain conceptually available and are used by sibling readings. Resistance is moderate (0.48) — the reading faces persistent challenge from tradition and conciliar readings, and from internal schism.
 *
 * PERSPECTIVAL GAP:
 *   The lay believer seat experiences this as empowering coordination (rope); the doctrinal coherence seat experiences it as extractive fragmentation (tangled_rope toward snare). The engine computes this divergence from power/exit/role data: lay believers are organized/constrained beneficiaries (d ~ 0.25); doctrinal coherence is an abstract collective victim with no exit (d ~ 0.9). The analytical observer sees both simultaneously — the reading IS both a genuine coordination mechanism AND a fragmentation generator, which is exactly the rope/tangled_rope boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: lay_believers (direct access to authority), congregational_autonomy_movements (self-governance warrant), biblical_literacy_advocates (mission alignment). Victims: doctrinal_coherence_across_communities (no adjudicative mechanism to resolve divergence), inter_denominational_unity_efforts (structural barrier to unity), historical_continuity_claimants (reads fragmentation as loss of apostolic deposit). Directionality derives from this structure: beneficiaries have constrained exit (can change denominations but not the reading's logic), victims have no exit (coherence is a collective good that cannot be individually secured). The clerical class is neither beneficiary nor victim in this reading — it is structurally displaced, which is the reading's point.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clerical mediation as barrier to scriptural access) remains live in communities where clericalism persists, but is dead in contexts where vernacular Scripture and literacy are universal. The reading persists not because the founding problem is universally live, but because its coordination function (epistemic democratization) remains valuable even where the original extraction problem is solved. This is not mandatrophy — the constraint continues to perform its coordination function. The fragmentation cost is a structural trade-off, not an atrophied mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the biblical_authority kernel, or does it describe the kernel itself?',
    'Commitment-system mapping: compare this reading''s structural axioms against the sibling readings'' distinct beneficiary/victim profiles. If the axioms directly contradict, the readings are separate constraints instantiating the same kernel.',
    'Confirms this story is a kernel-reading instance (per Rule 1) with its own ε-invariant metrics, not a universal claim about biblical authority simpliciter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this reading instantiates a distinct constraint from tradition_scripture_reading and conciliar_reading').

omega_variable(
    perspicuity_operationalization,
    'Does the perspicuity axiom (scripture is clear enough for any believer to interpret) function as a coordination mechanism or an extraction cover in practice?',
    'Empirical survey of interpretive disputes within sola scriptura communities: if disputes are resolved through mutual study and convergence, coordination; if disputes persist as schism with no resolution mechanism while leadership benefits from fragmentation, extraction cover.',
    'If extraction cover, reclassifies toward tangled_rope or snare; if genuine coordination, supports rope classification with low ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_operationalization, empirical, 'Whether the self-interpreting claim operationally coordinates or extracts').

omega_variable(
    fragmentation_as_feature_or_bug,
    'Is high doctrinal fragmentation a structural cost (victim) or a tolerated byproduct of the coordination function?',
    'Longitudinal study of community stability: if fragmentation correlates with community dissolution or loss of witness, it is a cost; if communities remain stable and functional despite doctrinal divergence, it is a tolerated byproduct.',
    'If cost, the victim declaration is structurally accurate and ε may be higher; if byproduct, the victim declaration may overstate extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_as_feature_or_bug, conceptual, 'Whether doctrinal fragmentation is a genuine victim-outcome or an accepted trade-off').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 1517, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t1517, biblical_authority__sola_scriptura_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t1555, biblical_authority__sola_scriptura_reading, theater_ratio, 1555, 0.1).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t1648, biblical_authority__sola_scriptura_reading, theater_ratio, 1648, 0.11).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t1789, biblical_authority__sola_scriptura_reading, theater_ratio, 1789, 0.12).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t1900, biblical_authority__sola_scriptura_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_tr_t2024, biblical_authority__sola_scriptura_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t1517, biblical_authority__sola_scriptura_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t1555, biblical_authority__sola_scriptura_reading, base_extractiveness, 1555, 0.16).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t1648, biblical_authority__sola_scriptura_reading, base_extractiveness, 1648, 0.17).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t1789, biblical_authority__sola_scriptura_reading, base_extractiveness, 1789, 0.18).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t1900, biblical_authority__sola_scriptura_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_be_t2024, biblical_authority__sola_scriptura_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t1517, biblical_authority__sola_scriptura_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t1555, biblical_authority__sola_scriptura_reading, suppression_requirement, 1555, 0.24).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t1648, biblical_authority__sola_scriptura_reading, suppression_requirement, 1648, 0.23).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t1789, biblical_authority__sola_scriptura_reading, suppression_requirement, 1789, 0.22).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t1900, biblical_authority__sola_scriptura_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement(biblical_authority__sola_scriptura_reading_su_t2024, biblical_authority__sola_scriptura_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__sola_scriptura_reading, 0.08).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_authority kernel. The sola_scriptura_reading claims Scripture is self-sufficient and self-interpreting (low extraction, high fragmentation). The tradition_scripture_reading claims Scripture requires a magisterial interpreter (higher clerical extraction, lower fragmentation). The conciliar_reading claims Scripture is interpreted through conciliar consensus (moderate extraction, moderate fragmentation). Each reading has distinct beneficiary/victim profiles and ε values. They are linked via affects_constraints to form the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
