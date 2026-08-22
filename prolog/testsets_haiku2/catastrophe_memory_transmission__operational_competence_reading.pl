% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission via Operational Competence Encoding
 *   domain: religious/collective_memory/survival
 *
 * SUMMARY:
 *   This constraint story instantiates the operational_competence_reading of
 *   the catastrophe_memory_transmission kernel. The reading evaluates ritual
 *   (Passover's rapid-departure protocols, Tisha B'Av's resource-scarcity
 *   rehearsal, mourning-cycle threat assessment) as an encoding and
 *   transmission mechanism for survival competence — pattern recognition for
 *   threat identification, resource coordination under constraint, rapid
 *   decision-making under pressure. The constraint solves a genuine
 *   coordination problem: how to pass operationally-meaningful knowledge
 *   across generations, through non-literate transmission chains, and across
 *   periods when the catastrophic conditions that motivated the knowledge
 *   fade from living memory. This reading does NOT evaluate ritual primarily
 *   as symbolic meaning or identity preservation — those are the functions of
 *   the symbol_continuity_reading and hybrid_embedded_reading siblings. This
 *   story claims rope classification: a genuine coordination mechanism with
 *   asymmetric enforceability (community youth bear transmission cost; future
 *   generations receive the benefit in catastrophic scenarios). The
 *   measurement series show extractiveness rising slightly in the
 *   mid-interval (t=5 to t=20) as interpretive authority tightens the
 *   fidelity requirement, theater_ratio rising as the crisis scenarios that
 *   motivated the encoding become historically distant, and
 *   suppression_requirement remaining stable as the constraint relies on
 *   identity-fusion rather than external coercion. Across sibling readings,
 *   this one evaluates efficacy by operational yield; the symbol_continuity
 *   reading evaluates efficacy by identity persistence; the hybrid_embedded
 *   reading holds both mechanisms inseparable.
 *
 * KEY AGENTS:
 *   - ritual_community_practitioners: Maintain and transmit precise operational patterns; their identity is constituted through transmission fidelity
 *   - future_generations: Receive encoded survival competence; their survival capacity in catastrophe depends on encoding accuracy
 *   - symbolic_meaning_traditionalists: Emphasize ritual meaning as symbolic and spiritual; structurally excluded from this operational reading
 *   - community_youth: Participate in demanding ritual enactment; bear compliance cost with constrained exit
 *   - interpretation_authority: Maintains authority to declare correct enactment and which elements encode competence; shapes what is transmitted
 *   - skeptical_analysts: Question fidelity of symbolic transmission and whether explicit training would better serve survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission via Operational Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious/collective_memory/survival").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '7e526bf7-1401-48b7-8602-ac8163740a6f').
narrative_ontology:cs_kernel_codification('7e526bf7-1401-48b7-8602-ac8163740a6f', formalized).
narrative_ontology:cs_authority_grounding('7e526bf7-1401-48b7-8602-ac8163740a6f', lineage).
narrative_ontology:cs_interpretation_layer_present('7e526bf7-1401-48b7-8602-ac8163740a6f').
narrative_ontology:cs_reading_relation('7e526bf7-1401-48b7-8602-ac8163740a6f', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e526bf7-1401-48b7-8602-ac8163740a6f', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('7e526bf7-1401-48b7-8602-ac8163740a6f', foundational, ritual_operationally_encodes_survival_competence).
narrative_ontology:cs_axiom_status(ritual_operationally_encodes_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('7e526bf7-1401-48b7-8602-ac8163740a6f', ritual_operationally_encodes_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('7e526bf7-1401-48b7-8602-ac8163740a6f', foundational, symbolic_transmission_reliably_preserves_procedural_knowledge).
narrative_ontology:cs_axiom_status(symbolic_transmission_reliably_preserves_procedural_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('7e526bf7-1401-48b7-8602-ac8163740a6f', symbolic_transmission_reliably_preserves_procedural_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('7e526bf7-1401-48b7-8602-ac8163740a6f', catastrophe_encoded_competence_framework).
narrative_ontology:cs_drift_state('7e526bf7-1401-48b7-8602-ac8163740a6f', contemporary_historical_distance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e526bf7-1401-48b7-8602-ac8163740a6f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_survival_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, community_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit specific ritual practices (e.g., Passover preparation and rapid-departure protocols, Tisha B'Av resource-scarcity simulation, mourning-cycle threat rehearsal). Argue that precise enactment encodes operational survival knowledge: the bitter herbs teach resource recognition under scarcity, the locked doors rehearse escape-route verification, the fast rehearses deprivation tolerance. Their identity is constituted through transmission fidelity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_community_practitioners, agenda_setter,
    organized, generational, identity_locked, local).

% Receive encoded survival competence through ritual participation: pattern recognition for threat identification, resource coordination under constraint, rapid decision-making under pressure. Their survival capacity in catastrophic scenarios depends on the accuracy of the encoding and completeness of the transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, future_generations).

% Emphasize that ritual meaning is primarily symbolic and spiritual — identity continuity, communal mourning, remembrance — rather than operational survival instruction. They argue that reading ritual as operational competence encoding instrumentalizes sacred practice and may distort interpretation of symbols toward literal threat-response when the living function is remembrance itself. They are not at the table of this reading's analytical framing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbolic_meaning_traditionalists, excluded,
    organized, generational, constrained, local).

% Participate in precise, sometimes demanding ritual enactment (fasting, locked-door protocols, resource-scarcity simulations, threat-assessment drills embedded in symbolic form). They bear the compliance cost and the cognitive burden of learning operational procedures through symbolic media. Their exit options are limited by kinship and identity fusion with the practicing community.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_youth, payer,
    powerless, biographical, identity_locked, local).

% In catastrophic conditions (siege, persecution, displacement, resource collapse), individuals who have internalized the ritual-encoded competence have measurably better threat recognition, faster resource-rationing decisions, and coordinated response. The constraint's payoff accrues in scenarios the ritual rehearses.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, crisis_scenario_agents, beneficiary,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, crisis_scenario_agents).

% Maintains the interpretive authority to declare what counts as correct ritual enactment and which elements encode operational competence versus pure symbolic meaning. Holds the genealogy of the tradition and the right to instruct youth. Their reading of the constraint shapes what competence is transmitted.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, interpretation_authority, agenda_setter,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, interpretation_authority, observer).

% Question whether the ritual encoding is sufficiently precise to transmit operational competence reliably, whether symbolic obfuscation introduces noise that degrades survival value, and whether the communities relying on symbolic transmission would fare better with explicit operational training. They measure fidelity from outside the ritual system.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, skeptical_analysts, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, interpretation_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and rehearses threat-response, resource-rationing, and rapid-decision-making procedures through patterned symbolic action. The ritual solves the coordination problem of transmitting operational survival competence across generations, through non-literate or semi-literate periods, and in a form that persists through cultural discontinuity because it is embedded in sacred practice.
% TRANSFER_FUNCTION: Moves operational survival competence from past-experienced catastrophe (embedded in ritual pattern) to future-unexamined generations (who rehearse it through participation). The mechanism is repetition, embodied learning, and identity-fusion: practitioners internalize threat-assessment procedures as habitual response because they are woven into valued communal identity.
% ABSENT_VOICES: Those who would argue that ritual meaning is purely symbolic, that operational reading instrumentalizes and distorts sacred practice, and that explicit operational training would better serve survival. The symbolic-continuity reading is structurally excluded from this operational reading's framing; this reading evaluates the constraint under the measure of survival-competence transmission, not spiritual meaning or identity preservation.
% DISAPPEARANCE_RATIONALE: If this ritual constraint (the precise enactment of operationally-meaningful threat rehearsal) disappeared, survival competence would be lost or degraded unless replaced by explicit operational training. Communities that relied solely on the ritual encoding would face catastrophic scenarios (siege, persecution, resource collapse, displacement) with reduced threat-recognition speed, less-coordinated resource rationing, and slower response initiation. The constraint's disappearance would leave future generations without a transmission mechanism for competence their ancestors deemed essential.
% FOUNDING_PROBLEM: How to encode and transmit survival competence for catastrophic scenarios across generations, across cultural discontinuity, and through non-literate transmission chains, such that the knowledge persists and remains accessible even when the catastrophic conditions that motivated it fade from living memory.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists document that ritual-encoding of threat-response and resource-management procedures occurs across catastrophe-affected cultures (Passover's rapid-departure readiness for repeated persecution and displacement, Tisha B'Av's resource-scarcity and mourning-cycle rehearsal, Navajo and Vietnamese survivor-community protocols embedded in ceremonial cycles). However, whether this encoding remains operationally precise across generational transmission, whether participants consciously recognize the operational content, and whether symbolic transmission is superior to explicit training remain contested. No corroboration exists from communities that have successfully replaced ritual-encoded competence with explicit operational training and maintained equal survival outcomes under catastrophic conditions.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.38 at t=20, projected stable at 0.38 thereafter) because the constraint imposes genuine coordination costs (transmission requires precise enactment, community participation, time investment) but yields legitimate benefits (survival competence in catastrophic scenarios). Suppression is low-to-moderate (0.22 at interval end) because the constraint relies primarily on identity-fusion (youth are locked by identity, not by external coercion) rather than active suppression of alternatives. Theater ratio rises from 0.08 to 0.18 across the interval as the crisis scenarios that originally motivated the encoding become historically distant — the practice becomes increasingly theatrical as its operational trigger scenarios are not immediately present, but the form persists due to identity-fusion and interpretive authority. Accessibility collapse is high (0.72) because once the community commits to the operational reading of ritual, alternatives (explicit training, secular competence transmission) appear as threats to identity preservation rather than equivalent options. Resistance is moderate (0.41) because skeptical analysts and some traditionalists question whether symbolic encoding is operationally reliable and whether explicit training would better serve the community. The measurement trajectory shows the constraint stabilizing in the t=20-40 interval at roughly its historical level — no major drift, no enforcement intensification, no theater explosion, suggesting a stable rope constraint with modest identity-lock suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual community's seat, the constraint is genuinely functional coordination: operationally-meaningful knowledge encoded in sacred form, transmitted reliably through generational cycles, retained through identity-fusion with the practicing community. From the community_youth seat, the constraint is compliance cost born without immediate visible benefit (the catastrophic scenarios are historical) and with locked exit (leaving the community means severing identity). From the skeptical_analyst seat, the constraint is operationally unreliable (symbolic encoding introduces noise) and potentially extractive (the community bears the cost of precise enactment for a speculative future benefit). From the symbolic_meaning_traditionalist seat (excluded), the constraint is instrumentalization of sacred practice that distorts spiritual meaning. The engine will compute per-seat directionality from the structural data: practitioners near d=0.0 (beneficiary, agenda-setter), youth near d=0.7+ (target, identity-locked), analysts near d=0.5 (symmetric), traditionalists systematically excluded from the framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual practitioners are beneficiaries who set the constraint: they preserve tradition, maintain interpretive authority, and benefit from identity-fusion of youth. Their directionality is near 0.0 (full beneficiary). Future generations are labeled beneficiary (receive survival competence) but have no choice about participation; their directionality would compute toward 0.5 or higher depending on how the engine treats non-agent beneficiaries and powerless stakeholders. Community youth are structural targets: they participate without immediate visible benefit, bear compliance cost, and face identity-locked exit (d near 0.7-1.0). Skeptical analysts are observers (d=0.5, symmetric). The spread in directionalities reflects the constraint's asymmetry: the coordination function is real, but it accrues primarily to practitioners (who preserve meaning and authority) and secondarily to future generations (in catastrophic scenarios that may not occur), while youth bear the compliance cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem ('how to encode and transmit survival competence across generations in non-literate chains') had clear salience when the catastrophic scenarios (persecution, displacement, siege, resource collapse) were recurring or imminent. The founding_problem_status is contested because: (1) in communities that have experienced recent catastrophe, the operational content remains live and recognized (practitioners can cite specific procedural knowledge); (2) in communities where catastrophe is historical or hypothetical, the problem may be dead (the practice persists as identity-preservation, not operational preparation); (3) skeptical analysts argue the problem is poorly solved (symbolic encoding is unreliable compared to explicit training). The disappearance_verdict is world_rearranges because the constraint's removal would alter survival capacity and force adoption of explicit training or abandonment of competence transmission. However, the theater_ratio rising across the interval (0.08 → 0.18) and the historical distance of catastrophic scenarios suggest potential mandatrophy: the founding problem may be dead (survivors' children are increasingly unlikely to face the original catastrophe) while the practice persists due to identity-fusion and interpretive authority. The constraint is NOT yet piton (theater_ratio is still low, operational content is still recognized, communities still connect the practice to survival), but it is drifting toward that classification. An omega variable captures this: if the constraint's founding problem is dead and the practice persists primarily as identity-maintenance, the symbol_continuity_reading would better describe the actual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contention,
    'Is ritual''s primary function the transmission of operational survival competence, the preservation of symbolic identity and mourning-practice as intrinsic good, or the inseparable embedding of competence within symbolic form?',
    'The constraint_catastrophe_memory_transmission kernel hosts three distinct readings: operational_competence_reading (this story), symbol_continuity_reading, and hybrid_embedded_reading. Resolution occurs at the constraint-family level when empirical observation of community competence under catastrophic conditions is cross-referenced against the reading''s predicted mechanism (operationally-encoded competence vs. symbolically-embedded vs. hybrid). No single reading forecloses the others within a community that holds multiple interpretations simultaneously.',
    'If operational competence is the primary function, fixing the constraint means explicit operational training and may allow ritual to become primarily symbolic. If symbol is primary, the constraint is a mountain (natural, inseparable from community identity) and cannot be unmade without community dissolution. If hybrid, the constraint is a tangled_rope with both genuine coordination and identity-extraction riding the same structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'The three-reading decomposition of the catastrophe_memory_transmission kernel: operational, symbolic, hybrid.').

omega_variable(
    encoding_fidelity_across_time,
    'Does the ritual encoding of operational survival competence remain precise across generational transmission, or does symbolic drift progressively obscure the operational content until youth learn the form without the function?',
    'Comparative analysis of ritual enactment fidelity across communities: (1) communities where catastrophic conditions recur frequently and selection pressure maintains operational precision; (2) communities where catastrophic conditions are rare or absent, permitting symbolic drift. Empirical test: does communities with recurring threat maintain competence under catastrophic conditions at higher rates than communities with symbolic drift? Post-catastrophe interview cohorts from both groups would establish whether operational knowledge was accessible or lost.',
    'High encoding fidelity supports the rope classification: genuine coordination mechanism sustained over time. Low fidelity and symbolic drift (theater_ratio rising toward 0.5+) would suggest the constraint is drifting toward piton (theatrical maintenance of a form whose function has atrophied). If drift is observed, the constraint may be operationally defunct but symbolically essential — a hybrid_embedded or symbol_continuity reading would better capture the actual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encoding_fidelity_across_time, empirical, 'Whether operational content survives transmission or is progressively replaced by symbolic form.').

omega_variable(
    identity_lock_vs_exit_capacity,
    'Are youth who participate in ritual-encoded competence transmission constrained by identity-fusion with the practicing community, or do they retain genuine exit capacity and choose to participate?',
    'Ethnographic study of youth exit patterns: do young people who were raised in the ritual-practicing community but encounter explicit operational training (military service, disaster-response training, crisis-management courses) show retention of ritual-encoded competence, supplement it with explicit training, or abandon the ritual entirely? Does exit from the community correlate with loss of competence or replacement with equivalent explicit training? Are there examples of communities that transition from ritual-encoded to explicitly-trained competence?',
    'If exit is identity-locked (low exit_options) and youth cannot leave without community rupture, the constraint carries an extraction component: community youth bear transmission cost without choice. If exit is genuinely available and youth choose participation, the extraction is lower and the constraint remains rope. If youth adopt hybrid strategies (ritual + explicit training), the constraint may be rope + piton (some operational function persists, some is theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_exit_capacity, empirical, 'Whether youth participation in ritual transmission is voluntary or structurally constrained by identity-fusion.').

omega_variable(
    symbolic_distortion_mechanism,
    'Does reading ritual as operational competence encoding create a feedback loop where the symbolic interpretation becomes the dominant meaning and actual operational content is lost or distorted?',
    'Historical-linguistic analysis of ritual terminology and interpretation: trace how operational language in ritual description (e.g., ''swiftness of departure,'' ''readiness for flight,'' ''scarcity preparation'') is progressively reinterpreted as metaphor for spiritual readiness, identity commitment, or mourning depth. Does explicit operational reframing (teaching youth that ritual X encodes competence Y) improve survival outcomes under catastrophe, or does it degrade symbolic meaning without improving actual competence?',
    'If symbolic reinterpretation erases operational content (theater_ratio rising, resistance from traditionalists increasing, extractiveness rising as the practice becomes obligation without function), the constraint is drifting toward piton. If operational reframing improves outcomes without degrading symbol, the rope classification is supported. If the two readings coexist stably (hybrid_embedded_reading), both mechanisms function in parallel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_distortion_mechanism, empirical, 'Whether naming the operational function changes the practice or corrupts the symbol.').

omega_variable(
    future_generations_agency,
    'Are future generations (listed as beneficiary, non-agent) genuinely passive recipients of transmitted competence, or do they possess agency to evaluate, adopt, or modify the ritual encoding they receive?',
    'Ethnographic analysis of ritual innovation and modification within practicing communities. Do young people alter ritual procedures to suit contemporary threats? Do they supplement ritual with explicit training? Do they reject the ritual entirely when explicit alternatives become available? Agency shows as modification and contestation; passivity shows as unchanged transmission across generations despite changed threat landscape.',
    'If future generations are truly passive (trapped, non-agent), the constraint is a one-way transmission mechanism and the rope classification is supported: genuine coordination function. If they possess contestation power and modify the constraint (symbolic_continuity reading becomes more salient), the constraint shows cross-generational negotiation and may be better classified as tangled_rope (both coordination and asymmetric obligation). If they abandon ritual entirely when explicit training is available, the constraint''s classification shifts based on what they adopt instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_agency, empirical, 'Whether future generations passively receive or actively contest ritual-encoded competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.19).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 15, 0.19).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel decomposes into three structurally distinct constraint stories: (1) operational_competence_reading (this story) — ritual evaluated by operational yield in catastrophic scenarios; (2) symbol_continuity_reading — ritual evaluated by identity preservation and mourning-practice continuity; (3) hybrid_embedded_reading — ritual evaluated as inseparable embedding of competence within symbolic form. All three evaluate the same practices (Passover protocols, Tisha B'Av rehearsal, mourning cycles) but under different measures and from different analytical seats. No single reading forecloses the others in actual practicing communities, where multiple interpretations coexist. The three stories are linked bidirectionally via network.affects_constraints and share the kernel_id in their cs_structure blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__operational_competence_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
