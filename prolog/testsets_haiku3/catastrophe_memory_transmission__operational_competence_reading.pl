% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Ritual Transmission of Catastrophe Survival Competence (Operational Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel:
 *   catastrophe_memory_transmission. The kernel itself is the standing
 *   arrangement whereby ritual practice carries meaning and obligation across
 *   generational time in communities with historical trauma. Three readings
 *   are in genuine contest: (1) operational_competence_reading (this story)
 *   interprets ritual as encoding practical survival knowledge—threat
 *   recognition, resource coordination, rapid mobilization—that younger
 *   generations inherit through embodied practice and can deploy if
 *   catastrophe recurs; (2) symbol_continuity_reading interprets the same
 *   rituals as preserving identity and mourning practice as intrinsic
 *   communal goods, where the survival mechanism is relational/symbolic, not
 *   practical; (3) hybrid_embedded_reading holds that survival competence is
 *   inseparable from symbolic form and transmitted through non-propositional
 *   knowledge embedded in practice itself. These are not different
 *   observations of the same thing—they are different structural claims about
 *   what the ritual IS FOR and what its beneficiary/victim structure looks
 *   like. This story authors the operational reading only, cleanly, as an
 *   ε-invariant constraint. The kernel contest and alternative readings live
 *   in omega variables and cs_structure fields.
 *
 * KEY AGENTS:
 *   - ritual_practicing_community: Maintains and deploys the ritual practice; from the operational reading, they steward encoded survival knowledge across generations.
 *   - future_generation_survivors: Inherit survival competence through childhood participation and community practice; they have no choice in whether to receive this education.
 *   - community_elders_knowledge_keepers: Hold and transmit the embedded operational knowledge within ritual form; responsible for ensuring competence does not degrade into symbol-only interpretation.
 *   - symbol_only_practitioners: Participate in ritual but interpret it as identity/historical expression, decoupled from operational function; they inherit the obligation but may not develop the survival competence, bearing potential cost if catastrophe occurs.
 *   - catastrophic_event_scenario: The counterfactual test case that would validate or refute the operational reading's core claim about survival advantage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.31).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual Transmission of Catastrophe Survival Competence (Operational Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6').
narrative_ontology:cs_kernel_codification('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', distributed).
narrative_ontology:cs_authority_grounding('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', practice).
narrative_ontology:cs_interpretation_layer_present('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6').
narrative_ontology:cs_reading_relation('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', foundational, survival_competence_transmissible_through_practice).
narrative_ontology:cs_axiom_status(survival_competence_transmissible_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', survival_competence_transmissible_through_practice, empirically_contingent).
narrative_ontology:cs_axiom('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', foundational, operational_yield_primary_justification_for_ritual).
narrative_ontology:cs_axiom_status(operational_yield_primary_justification_for_ritual, holdable).
narrative_ontology:cs_axiom_grounding('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', operational_yield_primary_justification_for_ritual, instrumental).
narrative_ontology:cs_reference_frame('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', pre_catastrophe_competence_state).
narrative_ontology:cs_drift_state('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', contemporary_without_recent_catastrophe, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('0b93e1f4-fa68-45b5-aa9d-7e2fb3855ac6', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_survival_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, ritual_practicing_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generation_survivors).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, symbol_only_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits ritual practice (Passover, Tisha B'Av, others) across generations. From the operational reading, they are stewarding encoded survival knowledge: rapid-departure readiness during Passover enactment, resource-scarcity decision-making rehearsed during fasting and memorial practices, threat-assessment protocols embedded in communal gathering patterns. Their effort is justified by the operational yield — the competence their children inherit through embodied practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_practicing_community, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__operational_competence_reading, ritual_practicing_community, beneficiary).

% Inherit survival competence — pattern recognition of threat signals, resource coordination under scarcity, rapid collective mobilization — transmitted through ritual participation. They have no choice in whether to receive this education; they inherit it as embodied knowledge through childhood ritual participation and community practice continuity. In catastrophic scenarios, this inherited competence may determine survival outcomes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generation_survivors, beneficiary,
    powerless, generational, analytical, regional).

% Hold that ritual's primary function is identity and mourning-practice preservation, not operational competence transmission. They interpret the same rituals but with a different operational map: the survival mechanism is symbolic/relational, not practical/physical. This reading is structurally excluded from the operational reading because the two interpret the same ritual elements with incommensurable outcomes. They would argue the operational frame instrumentalizes sacred practice and misses what is actually being preserved.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbolic_continuity_interpreters, excluded,
    moderate, generational, mobile, regional).

% Hold and transmit the embedded operational knowledge within ritual practice. From the operational reading, their role is to ensure that Passover's rapid-departure elements are not treated as mere reenactment but as living threat-response rehearsal; that fasting practices encode resource-scarcity decision logic; that gathering protocols maintain community cohesion under uncertainty. They bear responsibility for whether the competence actually transmits or is lost to symbolism-only interpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_elders_knowledge_keepers, agenda_setter,
    powerful, generational, trapped, regional).

% Participate in ritual practice but interpret it primarily as identity expression or historical commemoration, decoupled from operational function. From the operational reading, they bear a structural cost: they inherit the ritual obligation but may not develop the survival competence it encodes. They 'mistake symbol for substance' in the reading's frame, meaning they do not extract or practice the threat-assessment, resource-coordination, or rapid-mobilization patterns the ritual is structured to teach. If catastrophe strikes, this interpretive decoupling becomes fatal cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, symbol_only_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% The counterfactual stress-test of the operational reading: does the community's survival rate differ when ritual practice has encoded competence versus when it has been reduced to symbol-only participation? This is not an actor but a structural referent — the test case that would validate or falsify the operational reading's core claim.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, catastrophic_event_scenario, observer,
    analytical, immediate, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__operational_competence_reading, catastrophic_event_scenario).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual practice solves the coordination problem of transmitting survival competence across generations without relying on explicit verbal instruction or text-based training. Pattern recognition, resource-scarcity decision logic, and threat-assessment protocols are embedded in the practice itself—Passover's rapid-departure readiness drill, Tisha B'Av's resource-scarcity rehearsal, communal gathering patterns that maintain cohesion under uncertainty. The coordination it solves is: how does a community encode and preserve practical survival knowledge that may not be needed for decades but becomes critical in catastrophic scenarios?
% TRANSFER_FUNCTION: Moves embodied survival competence from elder generations to younger generations through ritualized participation. The transfer is not monetary or material; it is operative: the younger community members develop pattern recognition for threat signals, decision-making under resource scarcity, and rapid collective mobilization — all rehearsed and reinforced through annual ritual reenactment and practice.
% ABSENT_VOICES: Those who have left ritual practice and do not participate in the transmission are absent. Alternative knowledge-transmission communities (secular survival training, military preparedness instruction, disaster-response protocols) are outside the ritual frame and unheard in the community's justification of ritual. Most importantly, future catastrophe survivors who might validate or refute the operational competence frame are, by definition, absent from present-time justification — we cannot consult them about whether the ritual transmission actually worked until the stress-test occurs.
% DISAPPEARANCE_RATIONALE: If the ritual and its operational-competence transmission mechanism disappeared overnight, the community's short-term social identity would persist (the symbol-continuity reading would claim nothing essential was lost), but the operational reading asserts that survival capacity in future catastrophic scenarios would degrade. The verdict is contested because the operational claim can only be tested under catastrophe conditions, which have not occurred recently enough to validate the mechanism. The community that loses ritual practice does not immediately lose survival; the loss becomes apparent only when catastrophe arrives and the embedded competence is called upon.
% FOUNDING_PROBLEM: Survival in catastrophic scenarios (diaspora, persecution, famine, displacement) requires that communities maintain operational competence across generational cycles—threat recognition, resource coordination, rapid collective response. These competencies cannot be preserved through explicit written instruction alone because they must be updated and validated through practice, and because they include non-propositional, embodied knowledge that text cannot fully capture. Ritual practice solves this by encoding the competence into repeatable, generationally-binding action patterns.
% FOUNDING_PROBLEM_CORROBORATION: Historical trauma scholars and anthropologists who study ritual transmission attest that ritual practice encodes and preserves practical knowledge across generations. However, the specific claim that ritual transmission produces measurable survival advantages in catastrophic scenarios is not independently corroborated by external researchers—it remains the operational reading's own assertion. Evolutionary anthropologists debate whether ritual functions for survival coordination or for identity/bonding (supporting symbol_continuity_reading). No contemporary catastrophic scenario has produced the stress-test data needed to validate the operational reading against alternatives.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The operational reading claims that ritual encodes and transmits practical survival competence: pattern recognition for threat signals, resource-coordination decision logic, rapid collective mobilization. Extractiveness is low (0.31) because the coordination function is genuine—ritual practice solves a real problem (transmitting competence across generations without relying on explicit text or instruction). Suppression is minimal (0.18) because the constraint persists by participant choice and cultural continuity, not by coercion; younger generations are socialized into the practice but are not trapped by external force. Theater ratio is moderate (0.22) because the practice includes both real operational rehearsal (Passover rapid-departure readiness, Tisha B'Av resource-scarcity training) and performative commemoration (historical reenactment, identity expression). The gap reflects the operational reading's own tension: ritual carries both encoded competence AND symbolic weight, and over time without catastrophic validation, the symbolic interpretation can dominate, turning the practice theatrical. Accessibility collapse is high (0.72) because once the ritual is understood as identity-bound practice and generational obligation, alternatives (abandoning tradition, explicit secular survival training) collapse—the practice is identity_locked for most practitioners. Resistance is moderate (0.41) because the practice genuinely serves coordination (some practitioners recognize and value the competence transmission) but also meets resistance from those who see it as burdensome, outdated, or symbol-only. The measurement series is flat across the interval because catastrophe has not recently occurred to stress-test the mechanism; if catastrophe did occur and competence failed to transmit, extractiveness and theater would spike. The flat profile reflects a constraint in steady-state operation during a period where its core justification (survival advantage) is not being validated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (ritual_practicing_community, community_elders) and the symbol_only_practitioners should experience this constraint differently. From the elder/keeper perspective, the constraint is a genuine coordination mechanism—they see ritual as encoding survival knowledge and interpret their role as stewardship of competence transmission. From the symbol_only perspective, the constraint appears as cultural obligation (identity-locked participation) that demands time and observance without clear practical yield. The future_generation_survivors have no perspective until catastrophe occurs; they inherit the constraint without choosing it. The engine should compute different types from these different structural positions: the coordinating/transmitting seats experience rope (genuine coordination function); the symbol-only seat experiences the constraint as more extractive (obligation without understood benefit). The operational reading's core claim is that this perspectival gap reflects different competence-extraction outcomes, not just different interpretations—those who treat ritual as symbol-only will lack the embodied threat-recognition and resource-decision patterns that those who treat it as operational competence will have.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are future generations and the community's survival capacity—abstract, distributed, and future-time dependent. No concrete agent 'collects' from this constraint in the present. The ritual_practicing_community and elders are both agenda-setters (they maintain and transmit the practice) and beneficiaries (they preserve their community's survival capacity), so directionality is complex: they invest effort and time but the payoff is not captured extraction—it is distributed survival advantage. Symbol_only_practitioners are situated as payers because they bear the obligation (time, observance cost, potential opportunity cost) without (in the operational reading's frame) extracting the competence benefit; they mistake symbol for substance and thus fail to develop the embedded knowledge, bearing the cost of non-understanding if catastrophe occurs. This is not extraction in the classic sense—no agent is collecting the cost—but a structural cost nonetheless: the competence that was available in ritual practice is not extracted by those who do not attend to its operational frame. The low extractiveness (0.31) reflects that this is coordination, not exploitation; no seat is systematically enriched by the arrangement. The potential cost to symbol-only practitioners is a future cost (contingent on catastrophe) and an opportunity cost (loss of competence that was available).
 *
 * MANDATROPHY ANALYSIS:
 *   The operational reading's mandate is to transmit survival competence across generational cycles. The founding problem (survival requires transmitted competence across generations; ritual encodes this competence) is CONTESTED—the symbol_continuity_reading rejects the claim that survival transmission is ritual's primary function. If long periods pass without catastrophe, the mandate can become obscured: ritual that was built to rehearse threat recognition and resource scarcity can degrade into pure identity expression and historical commemoration, with the operational mapping lost. This is mandatrophy risk. The theater_ratio (0.22) captures this degradation potential: the practice includes both real rehearsal and performative reenactment, and the proportion of performative activity increases when stress-testing is absent. The measurement series shows slight drift toward higher theater (0.18→0.22 over 40 time-units), suggesting that without recent catastrophe, the operational mandate is slowly obscured by symbolic interpretation. However, mandatrophy is not yet resolved: the elders and some practitioners retain the operational frame, and the transmission still carries the embedded competence. The constraint would require 2–3 generations without catastrophic validation before mandatrophy becomes irreversible (the practice becomes pure theater, the competence is lost, future catastrophe survivors inherit no knowledge).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_reading_boundary,
    'Is ritual''s primary function survival-competence transmission (operational reading) or identity and mourning preservation (symbol_continuity_reading)? Can a single ritual system carry both functions, or are they structurally incommensurable?',
    'Catastrophic scenario stress-test: measure survival outcomes and threat-response coordination between communities that interpret ritual as operational competence versus those that interpret it as symbolic continuity. Post-catastrophe interviews with survivors about whether embedded ritual knowledge was recalled and applied.',
    'If operational competence is validated, the constraint type remains rope (genuine coordination). If symbolic continuity proves primary, the constraint reclassifies to rope with theater-ratio drift toward 1.0 (performative maintenance). If both functions are genuinely present and inseparable, the constraint becomes tangled_rope (coordination + extraction, the symbol-only practitioners bear the cost of non-understanding).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_vs_symbolic_reading_boundary, empirical, 'Whether ritual transmits operational survival competence or primarily encodes identity and continuity.').

omega_variable(
    competence_preservation_without_catastrophe,
    'Can survival competence encoded in ritual remain sharp and transmissible across generational cycles that do not experience catastrophe? Or does non-catastrophic time degrade the operational mapping, turning embodied knowledge into rote reenactment?',
    'Long-term ethnographic study of ritual communities across multiple generational cycles without catastrophic stress. Measure whether younger generations develop same threat-recognition patterns and resource-coordination decision-speed as communities that have recently experienced catastrophe. Compare ritual interpretation in high-stress-history communities versus long-peaceful communities.',
    'If competence degrades without stress-testing, extractiveness increases (the ritual persists but its encoding function fails—becomes theater). If competence is preserved, the rope classification holds. If degradation is predictable and inevitable over generations of peace, the constraint becomes piton (atrophied function, inertial maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_preservation_without_catastrophe, empirical, 'Whether operational competence encoding survives multi-generational periods without catastrophic validation.').

omega_variable(
    symbol_substance_fusion_necessity,
    'Could the survival competence be transmitted through explicit instruction and practice drills, or is fusion with symbolic/identity form structurally necessary for the transmission to persist across generations?',
    'Compare survival-competence transmission outcomes in communities that maintain operational ritual practice versus communities using secular, explicit survival-skills training. Measure retention, adherence, and accuracy of threat-recognition across generational handoff in both groups.',
    'If competence transmits equally through explicit instruction, the ritual constraint is not necessary—it becomes optional, and extractiveness may be higher (cultural constraint without coordination necessity). If fusion with symbol/identity is necessary for intergenerational persistence, the rope classification is strengthened—the coordination function is genuine and non-substitutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_substance_fusion_necessity, empirical, 'Whether operational competence transmission requires symbolic/identity embedding or can be achieved through explicit training.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the operational, symbolic, and hybrid readings of this kernel genuinely incommensurable (foreclosed from each other), or can a single community hold multiple readings simultaneously, interpreting ritual differently at different moments?',
    'Interview ritual practitioners about their interpretation framework during practice. Measure whether individuals report shifting between operational and symbolic interpretations, or whether practitioners hold one coherent reading. Examine whether ritual leaders teach explicitly toward one reading or acknowledge multiple valid interpretations.',
    'If readings are truly incommensurable, the constraint family contains three separate constraints, each with a different ε and type. If practitioners hold multiple readings simultaneously, the constraint is a hybrid_embedded_reading case (the kernel is read plurally within one community). If incommensurability is institutional rather than logical—different communities hold different readings—the reading_relations are coexists_with rather than forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are logically foreclosed or pragmatically coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 8, 0.29).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 32, 0.31).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 8, 0.16).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 16, 0.17).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 32, 0.18).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__operational_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel is decomposed into three constraint stories representing three readings in genuine contest. Each reading instantiates a different structural claim about what ritual IS FOR and produces a different beneficiary structure and constraint type. The operational_competence_reading (this story) interprets ritual as encoding practical survival knowledge and expects rope classification. The symbol_continuity_reading interprets ritual as preserving identity and expects rope or piton. The hybrid_embedded_reading holds that competence and symbol are inseparable. These are not different views of one constraint—they are three different constraints, each with its own ε, beneficiary structure, and justification. The kernel contest is routed through omega variables and cs_structure.reading_relations; each story author has made an interpretive choice about which reading is structurally true. Cross-story comparison should measure whether the engine classifies each reading according to its structural claims or whether structural assumptions in one reading contaminate interpretation of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__operational_competence_reading, powerless, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
