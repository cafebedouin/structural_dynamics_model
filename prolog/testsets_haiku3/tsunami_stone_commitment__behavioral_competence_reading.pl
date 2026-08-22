% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_behavioral_competence, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Warning Inscription: Behavioral Competence Reading
 *   domain: social/institutional
 *
 * SUMMARY:
 *   Stone inscriptions placed in coastal Japanese communities in the Edo
 *   period (1600s–1868) bear warnings against building settlements or
 *   trusting sea-wall defenses in tsunami-prone zones. The behavioral
 *   competence reading instantiates the claim that these inscriptions
 *   retained live prescriptive force through centuries of intergenerational
 *   cultural transmission: shrine keepers, community elders, and educational
 *   practices maintained the knowledge that 'the stone says do not build
 *   here, do not think the wall protects you.' The 2011 Tōhoku earthquake and
 *   tsunami tested this claim empirically. In communities where the stone's
 *   warning was active in collective memory, evacuation decisions aligned
 *   with the stone's embedded instruction (people moved upslope rapidly,
 *   minimizing casualties). In communities where the stone had become a
 *   commemorative relic divorced from behavioral meaning, the same
 *   catastrophe drove different outcomes. This reading does NOT claim the
 *   stone is a natural law or that its warning would persist without active
 *   cultural transmission. Rather, it claims the stone constraint—the
 *   prescriptive rule embedded in its text and maintained through community
 *   practice—functioned as intended because the transmission system worked.
 *   The sibling commemorative_husk_reading claims the stone decayed to inert
 *   monument, and 2011 compliance was coincidental or weakly enforced by fear
 *   of the immediate tsunami danger itself, not by the stone's historical
 *   instruction.
 *
 * KEY AGENTS:
 *   - coastal_communities_in_tsunami_zones: holders and transmitters of the stone's prescriptive meaning across generations; their evacuation decisions in 2011 test whether the constraint retained behavioral force
 *   - shrine_keepers_and_community_elders: active maintainers of the stone's meaning through storytelling, ritual, and educational practice
 *   - seismic_science_observers: external analysts assessing whether observed evacuation behavior in 2011 aligned with the stone's instruction or was driven by other factors
 *   - catastrophe_itself_2011_tōhoku: empirical event providing evidence for or against the behavioral competence reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Warning Inscription: Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "social/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '38735e75-9bba-43a7-ba7a-c5b6d569cbc3').
narrative_ontology:cs_kernel_codification('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', fixed_text).
narrative_ontology:cs_authority_grounding('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', lineage).
narrative_ontology:cs_interpretation_layer_present('38735e75-9bba-43a7-ba7a-c5b6d569cbc3').
narrative_ontology:cs_reading_relation('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', foundational, intergenerational_knowledge_transmission_retains_prescriptive_force).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transmission_retains_prescriptive_force, holdable).
narrative_ontology:cs_axiom_grounding('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', intergenerational_knowledge_transmission_retains_prescriptive_force, empirically_contingent).
narrative_ontology:cs_axiom('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', secondary, cultural_practice_constitutes_institutional_memory).
narrative_ontology:cs_axiom_status(cultural_practice_constitutes_institutional_memory, holdable).
narrative_ontology:cs_axiom_grounding('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', cultural_practice_constitutes_institutional_memory, conventional).
narrative_ontology:cs_reference_frame('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', active_behavioral_transmission).
narrative_ontology:cs_drift_state('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', contemporary_post_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('38735e75-9bba-43a7-ba7a-c5b6d569cbc3', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities_tsunami_zones).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, shrine_keepers_and_elders).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, younger_generations_coastal_residents).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, intergenerational_knowledge_transmission_efficacy).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, cultural_practice_as_distributed_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive and maintain the stone's prescriptive knowledge across generations through shrine practices, community storytelling, and educational integration. In the behavioral competence reading, they actively keep the stone's meaning alive as a guide for settlement and evacuation decisions. The constraint benefits them by embedding survival knowledge in cultural practice, available without active scientific monitoring. Their constraint-specific exit option is limited—abandoning the stone means losing accumulated wisdom about tsunami behavior; geographic exit (moving away from coastal zones) is the only departure from the stone's governance.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities_tsunami_zones, beneficiary,
    moderate, civilizational, constrained, regional).

% Maintain the stone through shrine care, ritual practice, and explicit teaching of the stone's warning to younger generations. Their role is administration and transmission—they are the primary institutional carriers of the constraint. They benefit from the constraint by maintaining cultural authority and continuity of practice; their identity as elders/keepers is constituted through this transmission role. Exit from this role would mean abandoning the professional and relational identity constructed through generations of shrine stewardship.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, shrine_keepers_and_elders, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, shrine_keepers_and_elders, beneficiary).

% Learn the stone's warning through community practice, shrine visits, school education, and family storytelling. They benefit from the intergenerational transmission by inheriting survival knowledge without having to rediscover it through catastrophic experience. Their constraint-specific exit option is limited—they must either accept the transmitted knowledge or actively reject it and risk repeating the errors their ancestors learned from.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, younger_generations_coastal_residents, beneficiary,
    moderate, biographical, constrained, local).

% Analyzes the stone constraint's behavioral efficacy, especially through the 2011 empirical test. They are external analytical seats assessing whether the constraint's transmission system remained functional and whether observed evacuation behavior aligns with the stone's prescriptive instruction. They do not participate in the constraint's operation; they measure it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, seismic_science_community, observer,
    institutional, generational, analytical, national).

% Hold the alternative reading of the stone constraint, arguing that it decayed to symbolic artifact with minimal behavioral force. They are excluded from the behavioral competence reading's stakeholder set because they represent the sibling reading rather than participants in this constraint's operation. They would argue that the stone's observed preservation and cultural reverence reflect historical momentum and commemorative impulse, not active transmission of behavioral meaning.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, commemorative_husk_reading_advocates, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone coordinates intergenerational knowledge transmission about tsunami danger: instead of each generation learning the hard way (through catastrophic experience), knowledge is embedded in cultural practice and physical artifact, available to all community members. The constraint solves the coordination problem of keeping survival wisdom alive across long timescales despite generational turnover.
% TRANSFER_FUNCTION: No transfer function in the extraction sense. The constraint moves knowledge (the warning) from elders to younger generations, and from the shrine (institutional holder) to individual household decision-making. No wealth, status, or resources flow from one party to another as a cost of the constraint's operation.
% ABSENT_VOICES: In the behavioral competence reading, the voices most notably absent are those of community members who rejected the stone's instruction or who actively minimized its behavioral significance. Such voices would argue the stone is ornamental, that people evacuate because of sirens and media warnings, not because they remember the stone's 250-year-old instruction. The commemorative husk reading embodies this absence.
% DISAPPEARANCE_RATIONALE: If the stone constraint vanished (the stone were removed, shrine maintenance ceased, the warning language were forgotten), the community would lose a primary institutional mechanism for maintaining tsunami knowledge across generations. Younger residents would inherit less accumulated wisdom; settlement patterns might gradually shift toward coastal lowlands; evacuation decisions would depend entirely on real-time warnings (sirens, media) rather than pre-positioned cultural knowledge. The 2011 empirical test suggests that loss of the stone constraint would measurably degrade coastal community resilience, particularly in remote areas where media and siren systems are less reliable.
% FOUNDING_PROBLEM: Coastal communities in the Edo period (1600s–1800s) experienced recurring devastating tsunamis. Survivors inscribed warnings on stone monuments (tsumami-iishi) placed near shrines in high-visibility locations, embedding the message: 'When the earthquake is felt, do not rely on walls; move immediately to high ground.' The founding problem was how to keep this hard-won knowledge alive across the 50–300-year intervals between major tsunamis, when generational turnover would otherwise erase the memory of disaster.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem and its persistence are attested by multiple sources outside the beneficiary set: seismic scientists confirm that tsunami recurrence intervals far exceed human lifespans in the region; ethnographers document that intergenerational knowledge transfer remains fragile without institutional mechanisms (shrine, school, family practice); the 2011 Tōhoku tsunami provided empirical evidence that the founding problem remains unsolved—communities that had lost the stone's cultural transmission suffered higher casualties. The problem is live because tsunami risk is permanent and generational turnover is continuous; the stone constraint addresses an enduring structural challenge of human communities in high-risk zones.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at endpoint) because this constraint has no identifiable beneficiary structure—no agent collects rents from the stone's operation. The stone prescribes behavior (stay away from low ground) that benefits everyone equally (survival) and burdens no one (compliance is costless relative to the catastrophic cost of noncompliance). Suppression is minimal (0.12) because cultural transmission is voluntary, not coercive; the stone's force comes from the community's recognition of its truth, not from external enforcement machinery. Theater ratio is low but non-zero (0.15) because some portion of the stone's persistence is historical reverence and symbolic function—shrine maintenance continues partly because it is the traditional thing to do, not solely because the stone's behavioral command is actively enforced. Accessibility collapse is high (0.82) because once you understand the stone's warning and the tsunami risk it addresses, the alternative—dismissing the stone and building on the coast—becomes structurally inaccessible; the physical catastrophe collapses alternatives for anyone operating under the behavioral competence reading. Resistance is minimal (0.08) because the stone's command aligns with survival incentives; no one actively resists the instruction 'move to high ground when the earthquake hits.' The constraint is classified as piton because it is a former coordination mechanism (the stone originally established a new norm in response to experienced tsunamis) that now persists partly through institutional inertia (shrine maintenance, cultural memory) but retains functional integrity in the 2011 empirical test. A true piton would show high theater and minimal actual coordination; this constraint shows moderate theater (historical reverence) alongside demonstrated behavioral efficacy (2011 evacuation alignment), placing it at the functional piton boundary—stabilized, minimally extractive, still working because the transmission system maintained it.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no identifiable victims or agenda-setters in the structural sense. The stone does not extract from anyone; it prescribes behavior that benefits all parties symmetrically (survival). No agent is forced to accept the stone's instruction at a cost—compliance is identical to self-interest under the Buddhist/Shinto framing of natural disaster as inevitable law. The community acts as a collective beneficiary (everyone benefits from intergenerational knowledge transmission) and collective transmitter (everyone participates in keeping the stone's meaning alive). The directionality derivation produces d ≈ 0.5 for all parties because there is no asymmetric cost-bearing or concentrated benefit capture. The constraint is thus symmetric across all stakeholder seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy. The founding problem (tsunami risk to coastal settlements) remains live, and the constraint's founding mandate (warn communities to avoid low-lying construction) remains directly functional. The 2011 empirical test validates that the stone constraint's behavioral meaning was transmitted successfully and operated as prescribed. Mandatrophy would require the constraint to persist in spite of a dead founding problem (the stone outlives its usefulness) or a decayed mandate (the community forgets why the stone matters). Neither condition holds under the behavioral competence reading. The constraint is alive because the transmission system works and the founding problem has not been solved by other means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_behavioral_vs_commemorative,
    'Is the stone inscription a constraint that retained live behavioral force through intergenerational cultural transmission, or a decayed commemorative artifact whose compliance in 2011 was coincidental?',
    'Ethnographic evidence from pre-2011 interviews documenting community understanding of the stone''s prescriptive meaning; post-2011 testimony from evacuation participants about whether the stone''s historical warning was known and operative in decision-making; analysis of settlement patterns and evacuation routes relative to stone placement across tsunami-vulnerable communities.',
    'Behavioral competence reading: the stone constraint persists because knowledge transmission systems (storytelling, shrine maintenance, community memory practice) kept its prescriptive force alive. Commemorative reading: the stone is inert theater, surviving only because no one bothered to remove it; 2011 compliance was unrelated to the stone. The two readings entail opposite structural diagnoses of how cultural knowledge systems work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_behavioral_vs_commemorative, empirical, 'Whether the stone retained prescriptive force or decayed to monument.').

omega_variable(
    intergenerational_transmission_mechanism,
    'What specific institutional or cultural practice maintained the stone''s behavioral meaning across 250+ years and multiple generational turnover cycles?',
    'Documentation of shrine practices, educational curricula, oral history protocols, and community gathering rituals that explicitly referenced the stone''s warning. Tracing of who taught whom the stone''s meaning in the community chain from 1700s inscription through 2011.',
    'If transmission is documented through identifiable institutional channels (shrine care, structured storytelling, school teaching), the behavioral competence reading is strengthened: a constraint persists because active agents maintain it. If transmission was diffuse or implicit, the reading faces the weaker claim that behavior aligned with the stone''s message without anyone clearly maintaining the rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'The institutional mechanisms sustaining behavioral compliance across generations.').

omega_variable(
    sibling_reading_framing_indeterminacy,
    'Does this constraint have a single objective status (the stone either retained behavioral force or it did not), or is the reading choice itself a frame imposed by different analytical traditions?',
    'Meta-analysis of how Japanese scholars, anthropologists, and seismic science communities describe the stone''s role. Historical documentation of how the stone was discussed and referenced before 2011 (versus how it has been retrospectively narrated after).',
    'If the reading is frame-dependent, the two sibling readings coexist in different analytical frameworks and neither forecloses the other. If the reading is empirically resolvable, one reading corresponds to what actually happened in the community knowledge system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_indeterminacy, conceptual, 'Whether the sibling readings are empirically distinguishable or frame-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 312).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tsun_tr_t0, observed).
narrative_ontology:measurement(tsun_tr_t50, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(tsun_tr_t50, observed).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(tsun_tr_t100, observed).
narrative_ontology:measurement(tsun_tr_t150, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 150, 0.13).
narrative_ontology:measurement_basis(tsun_tr_t150, observed).
narrative_ontology:measurement(tsun_tr_t200, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement_basis(tsun_tr_t200, observed).
narrative_ontology:measurement(tsun_tr_t312, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 312, 0.15).
narrative_ontology:measurement_basis(tsun_tr_t312, observed).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(tsun_be_t0, observed).
narrative_ontology:measurement(tsun_be_t50, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 50, 0.06).
narrative_ontology:measurement_basis(tsun_be_t50, observed).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.07).
narrative_ontology:measurement_basis(tsun_be_t100, observed).
narrative_ontology:measurement(tsun_be_t150, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 150, 0.08).
narrative_ontology:measurement_basis(tsun_be_t150, observed).
narrative_ontology:measurement(tsun_be_t200, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement_basis(tsun_be_t200, observed).
narrative_ontology:measurement(tsun_be_t312, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 312, 0.08).
narrative_ontology:measurement_basis(tsun_be_t312, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.06).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into at least two structurally distinct constraint stories: (1) behavioral_competence_reading — the stone retained live behavioral force through intergenerational transmission, minimal extraction, piton-level stability; (2) commemorative_husk_reading — the stone decayed to symbolic artifact, no behavioral force, extraction via commemorative theater. The readings share the same kernel (the physical stone, the cultural practice) but entail opposite εs and opposite mechanisms of persistence. Each reading is ε-invariant within itself; the kernel contest is about which reading corresponds to the actual structure of intergenerational knowledge transmission in Japanese coastal communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
