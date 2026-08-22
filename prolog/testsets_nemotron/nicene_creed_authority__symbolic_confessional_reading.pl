% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed Authority — Symbolic-Confessional Reading
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   This reading treats the Nicene Creed as a historically situated witness
 *   of the early church — a symbolic confession that communities today
 *   receive, interpret, and re-appropriate through collective discernment and
 *   personal faith. Authority does not inhere in the creed's propositions as
 *   metaphysical claims, but in the Spirit-guided process by which
 *   communities discern its meaning for their time. The constraint is the
 *   *practice of symbolic-confessional reception* that mainline Protestant,
 *   progressive Catholic, and ecumenical bodies have consolidated since
 *   Vatican II. It inverts the authority topology of the
 *   strict_orthodox_reading: beneficiaries are the local discernment
 *   communities, victims are the centralized offices that lose gatekeeping
 *   power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.08).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed Authority — Symbolic-Confessional Reading").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "theological/ecclesiological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, 'ba2ede2d-8b46-4d05-9638-96dac2497510').
narrative_ontology:cs_kernel_codification('ba2ede2d-8b46-4d05-9638-96dac2497510', fixed_text).
narrative_ontology:cs_authority_grounding('ba2ede2d-8b46-4d05-9638-96dac2497510', lineage).
narrative_ontology:cs_interpretation_layer_present('ba2ede2d-8b46-4d05-9638-96dac2497510').
narrative_ontology:cs_reading_relation('ba2ede2d-8b46-4d05-9638-96dac2497510', nicene_creed_authority__strict_orthodox_reading, forecloses).
narrative_ontology:cs_reading_relation('ba2ede2d-8b46-4d05-9638-96dac2497510', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('ba2ede2d-8b46-4d05-9638-96dac2497510', foundational, creed_authority_from_consensus_fidelium).
narrative_ontology:cs_axiom_status(creed_authority_from_consensus_fidelium, holdable).
narrative_ontology:cs_axiom_grounding('ba2ede2d-8b46-4d05-9638-96dac2497510', creed_authority_from_consensus_fidelium, deontological).
narrative_ontology:cs_axiom('ba2ede2d-8b46-4d05-9638-96dac2497510', foundational, historical_critical_method_legitimate_for_confession).
narrative_ontology:cs_axiom_status(historical_critical_method_legitimate_for_confession, holdable).
narrative_ontology:cs_axiom_grounding('ba2ede2d-8b46-4d05-9638-96dac2497510', historical_critical_method_legitimate_for_confession, empirically_contingent).
narrative_ontology:cs_reference_frame('ba2ede2d-8b46-4d05-9638-96dac2497510', vatican_ii_reception_ecclesiology).
narrative_ontology:cs_drift_state('ba2ede2d-8b46-4d05-9638-96dac2497510', post_secular_theology_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ba2ede2d-8b46-4d05-9638-96dac2497510', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_teaching_offices).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, institutional_unity_enforcers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, individual_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive interpretive authority over the creed's meaning for their community. They discern collectively how the ancient witness speaks to their context, free from centralized mandate. Exit means affiliating with a different tradition that respects congregational discernment — a real option within mainline Protestant and progressive Catholic ecclesiology.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    moderate, generational, mobile, regional).

% Gain theological freedom to appropriate the creed symbolically rather than propositionally. They pay with the cognitive work of ongoing discernment and the social cost of occupying a contested middle ground. Exit toward stricter or secular frameworks is possible but carries identity disruption.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, individual_believers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__symbolic_confessional_reading, individual_believers, payer).

% Gain a shared symbolic language that enables dialogue across confessional lines without requiring metaphysical convergence. They invest in the creed as a hospitable meeting ground. Their exit options are strong — they can engage or withdraw from bilateral dialogues without institutional penalty.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, ecumenical_dialogue_partners, beneficiary,
    organized, generational, arbitrage, global).

% Lose exclusive interpretive authority over the creed. Their structural position as doctrinal gatekeepers is eroded when the creed becomes a communal discernment tool rather than a magisterial test. They bear the cost of managing schism risk and credibility loss. Exit from this victimhood would require abandoning their self-understanding as guardians of orthodoxy — identity-locked at the institutional level.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_teaching_offices, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__symbolic_confessional_reading, centralized_teaching_offices, payer).

% Lose the creed's utility as a boundary marker for communion discipline. When assent becomes symbolic and contextual, the tool for excluding heretics and maintaining visible unity degrades. They bear enforcement costs without the payoff of clear boundaries. Exit means shifting to other unity mechanisms (canonical, sacramental) — possible but institutionally costly.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, institutional_unity_enforcers, payer,
    organized, generational, constrained, continental).

% Study the creed's reception history and document how this reading emerges from modern critical scholarship and post-Enlightenment ecclesiology. They neither collect nor pay; their analytical seat maps the constraint's genealogy and its structural effects on the other seats.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared symbolic vocabulary that enables diverse Christian communities to recognize each other as participants in a common tradition without requiring doctrinal uniformity. The creed coordinates identity across difference.
% TRANSFER_FUNCTION: Moves interpretive authority from centralized teaching offices to local discernment communities and individual conscience. The arrangement transfers the power to define what the creed *means* from the magisterium to the consensus fidelium.
% ABSENT_VOICES: Traditionalist laity who experience symbolic reading as betrayal of the faith they inherited — they would object that the creed's words have fixed metaphysical content. They are present in parishes but excluded from synodical and academic discourse where this reading is consolidated.
% DISAPPEARANCE_RATIONALE: If this reading vanished, local congregations would lose their primary warrant for theological autonomy; ecumenical dialogues would lose a key convergence text; centralized authorities would regain unchallenged interpretive monopoly. The ecclesial landscape would rearrange toward either stricter confessionalism or fragmentation.
% FOUNDING_PROBLEM: The post-Reformation and post-Enlightenment crisis of authority: how can the ancient creed function as a bond of unity when metaphysical consensus has fractured and historical criticism has problematized its propositions?
% FOUNDING_PROBLEM_CORROBORATION: World Council of Churches Faith and Order documents (1982 Lima text, 2013 The Church: Towards a Common Vision) attest the problem is live from the ecumenical side. Vatican II's Dei Verbum and Lumen Gentium attest it from the Catholic reform side. No corroboration exists from the strict_orthodox_reading's beneficiaries — they reject the premise that a crisis of authority exists.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily *releases* authority downward rather than extracting upward — it dismantles extraction rather than enacting it. The small positive value reflects the cognitive and communal labor of ongoing discernment (the 'cost of freedom'). Suppression is minimal (0.08) because no one is coerced into this reading; communities adopt it voluntarily, and exit to other readings is open. Theater ratio is low (0.12) — the discernment practices (synods, study groups, confessional writing) are functionally integral to the reading, not performative substitutes. Accessibility collapse is low (0.25) because alternative readings (strict_orthodox, liturgical_habituation) remain fully available and actively practiced. Resistance is moderate (0.35) — the reading faces organized opposition from traditionalist movements, but this resistance is discursive and institutional, not coercive.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from the structural data. From the centralized_teaching_offices seat, the constraint may compute as a snare (they are extracted from, exit is trapped, they experience active resistance to their authority). From the local_congregations seat, it computes as a rope (genuine coordination of identity across difference, minimal coercion, net beneficiaries). The reading's claimed_type (rope) reflects the authoring seat's structural judgment — the engine's seat-divergent output is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Local congregations and individual believers are structural beneficiaries (d ~ 0.15-0.25): they gain interpretive authority and theological freedom. Ecumenical partners are beneficiaries with arbitrage-grade exit (d ~ 0.10). Centralized teaching offices are victims (d ~ 0.85): they lose their exclusive interpretive franchise and are identity-locked into the gatekeeper role — they cannot exit without dissolving their self-understanding. Institutional unity enforcers are victims (d ~ 0.70): they lose a disciplinary tool but can partially substitute other mechanisms. Historical theologians are analytical observers (d = 0.5). The beneficiary/victim declarations drive the engine's directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authority crisis after metaphysical consensus fracture) remains live — the reading has not outlived its function. It continues to solve a real coordination problem: how to remain in communion across doctrinal difference. The mandate is not atrophied; the constraint is not a piton. However, the rising theater_ratio (0.08→0.12) and flattening extractiveness after 2005 suggest a potential drift toward performative discernment where the *practice* of discernment substitutes for its *outcome* — an omega tracks this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discernment_performative_drift,
    'Is the discernment practice becoming performative — a ritual of ''having conversations'' that substitutes for actual theological judgment?',
    'Longitudinal study of synodical outcomes: do discernment processes produce revised teaching, or only documents affirming the status quo? Compare decision-rates on contested questions before/after 2000.',
    'If performative, theater_ratio is understated and the constraint drifts toward piton — the coordination function atrophies while the ritual persists. This would also raise effective extraction for individual_believers (they pay discernment labor for no structural change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_performative_drift, empirical, 'Whether symbolic discernment remains functionally generative or has become theatrical maintenance.').

omega_variable(
    committer_structure_ambiguity,
    'Does this reading''s core premise (authority derives from community discernment + personal faith) logically foreclose the strict_orthodox_reading, or do they coexist as competing frameworks?',
    'Test whether a single ecclesial body can simultaneously hold: (a) the creed''s authority is constituted by contemporary discernment, and (b) the creed binds all believers to a fixed metaphysical ontology. If no body stably holds both, the relation is forecloses; if different bodies hold each, coexists_with.',
    'If forecloses, the kernel has a genuine logical fracture — the readings cannot be reconciled within one framework. If coexists_with, the kernel hosts a stable pluralism of authority-claims. This determines the cs_structure.reading_relations declaration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Structural relationship between this reading and the strict_orthodox_reading within the nicene_creed_authority kernel.').

omega_variable(
    ecumenical_convergence_sustainability,
    'Can the creed function as a convergence text for ecumenical dialogue indefinitely without metaphysical consensus, or does symbolic reading eventually dissolve the very unity it coordinates?',
    'Track bilateral dialogue outcomes over 20+ years: do churches using symbolic reading achieve deeper communion (shared Eucharist, mutual recognition of ministries) or does dialogue plateau at ''agreeing on the creed''s symbolic value''?',
    'If symbolic reading enables real convergence, its coordination function is robust (rope confirmed). If it plateaus, the constraint may be a scaffold whose sunset has not been declared — it coordinates only until metaphysical differences become unavoidable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecumenical_convergence_sustainability, empirical, 'Whether the creed''s symbolic coordination function sustains ecumenical progress or masks persistent divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_sym_conf_tr_t1960, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(nicene_sym_conf_tr_t1975, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(nicene_sym_conf_tr_t1990, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(nicene_sym_conf_tr_t2005, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(nicene_sym_conf_tr_t2015, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(nicene_sym_conf_tr_t2025, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(nicene_sym_conf_be_t1960, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(nicene_sym_conf_be_t1975, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(nicene_sym_conf_be_t1990, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(nicene_sym_conf_be_t2005, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement(nicene_sym_conf_be_t2015, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement(nicene_sym_conf_be_t2025, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(nicene_sym_conf_su_t1960, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(nicene_sym_conf_su_t1975, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 1975, 0.12).
narrative_ontology:measurement(nicene_sym_conf_su_t1990, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(nicene_sym_conf_su_t2005, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 2005, 0.08).
narrative_ontology:measurement(nicene_sym_conf_su_t2015, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 2015, 0.07).
narrative_ontology:measurement(nicene_sym_conf_su_t2025, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__symbolic_confessional_reading, 0.06).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the nicene_creed_authority kernel family. The three readings share the same creedal text but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different authority topologies. This reading (symbolic_confessional) has low extractiveness and inverted topology; strict_orthodox_reading has high extractiveness and centralized topology; liturgical_habituation_reading has near-zero extractiveness but high identity-boundary function. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, institutional, 0.85).
constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, organized, 0.7).
constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, moderate, 0.2).
constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, powerless, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
