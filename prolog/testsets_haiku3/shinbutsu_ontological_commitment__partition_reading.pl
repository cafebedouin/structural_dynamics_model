% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinbutsu Partition: Life-Cycle and Afterlife Domains
 *   domain: religious_studies/ontology_of_practice
 *
 * SUMMARY:
 *   Shinto and Buddhism in historical Japan organized around a functional
 *   division: Shinto handled life-cycle events (births, weddings, seasonal
 *   festivals) while Buddhism addressed death, funerary rites, and the
 *   afterlife. This constraint is the PARTITION READING — the assertion that
 *   this division reflects a genuine ontological commitment: kami and buddhas
 *   occupy *separate domains* without metaphysical integration. Under this
 *   reading, practitioners were not experiencing incoherence (the
 *   incoherence_reading) nor holding a syncretic metaphysics (the
 *   syncretic_reading); they were intelligently partitioning two independent
 *   religious systems into their appropriate spheres. The reading claims low
 *   extraction and natural emergence from the distinct functions each
 *   religion served. The engine will compute whether this framing holds
 *   against the structural data.
 *
 * KEY AGENTS:
 *   - Shinto practitioners — those engaged primarily with kami veneration and life-cycle rituals (weddings, festivals, birth purification)
 *   - Buddhist practitioners — those engaged primarily with Buddhist doctrine, funerary rites, and contemplation of impermanence and the afterlife
 *   - Practitioners of both — Japanese people who participated in both systems, treating them as functional complements rather than competing metaphysics
 *   - Shinto and Buddhist institutional hierarchies — temples and shrines that administered the ritual specialists and maintained the canonical texts
 *   - Scholars of Japanese religion (modern) — later interpreters reconstructing the theological framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.12).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinbutsu Partition: Life-Cycle and Afterlife Domains").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '42736f70-5959-44b6-8f44-539afeec7ccf').
narrative_ontology:cs_kernel_codification('42736f70-5959-44b6-8f44-539afeec7ccf', distributed).
narrative_ontology:cs_authority_grounding('42736f70-5959-44b6-8f44-539afeec7ccf', practice).
narrative_ontology:cs_interpretation_layer_present('42736f70-5959-44b6-8f44-539afeec7ccf').
narrative_ontology:cs_reading_relation('42736f70-5959-44b6-8f44-539afeec7ccf', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('42736f70-5959-44b6-8f44-539afeec7ccf', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('42736f70-5959-44b6-8f44-539afeec7ccf', foundational, kami_and_buddhas_ontologically_distinct).
narrative_ontology:cs_axiom_status(kami_and_buddhas_ontologically_distinct, holdable).
narrative_ontology:cs_axiom_grounding('42736f70-5959-44b6-8f44-539afeec7ccf', kami_and_buddhas_ontologically_distinct, deontological).
narrative_ontology:cs_axiom('42736f70-5959-44b6-8f44-539afeec7ccf', foundational, functional_domain_separation_solves_integration_problem).
narrative_ontology:cs_axiom_status(functional_domain_separation_solves_integration_problem, holdable).
narrative_ontology:cs_axiom_grounding('42736f70-5959-44b6-8f44-539afeec7ccf', functional_domain_separation_solves_integration_problem, conventional).
narrative_ontology:cs_reference_frame('42736f70-5959-44b6-8f44-539afeec7ccf', domain_partition_framework).
narrative_ontology:cs_drift_state('42736f70-5959-44b6-8f44-539afeec7ccf', contemporary_post_meiji_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42736f70-5959-44b6-8f44-539afeec7ccf', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, practitioners_both).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_compartmentalization_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, domain_separation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage with Shinto primarily for life-cycle events: birth purification (miya-mairi), weddings, seasonal festivals, and household maintenance. Under the partition reading, they benefit from having a dedicated, non-mortuary system for these rituals. They can choose to engage with Buddhism for funerary preparation, or not; their primary religious practice remains Shinto without contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Engage with Buddhism primarily for soteriological concerns, funerary rites, and memorial services (obon, higan). Under the partition reading, they benefit from having a dedicated metaphysical framework for death and the afterlife without needing to integrate this with Shinto cosmology. They can choose to participate in Shinto life events or not without doctrinal contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Most Japanese practitioners throughout history participated in both systems across their lifespan. Under the partition reading, they benefit from cognitive clarity: knowing that Shinto handles worldly concerns and Buddhism handles karmic consequence and death. The two systems are not competing for the same conceptual space; each has its domain. No contradiction is experienced; no choice is required between systems because both are *appropriate* to different life-contexts.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, practitioners_both, beneficiary,
    moderate, biographical, mobile, national).

% Temples, shrines, and the priestly establishment that administered Shinto doctrine and ritual. Under the partition reading, they maintain authority over the life-domain: shrine priests conduct weddings, birth purifications, and seasonal rites. They do not administer funerary rites (Buddhism does), so they maintain institutional autonomy in their sphere. The partition constraint preserves their institutional role and legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, national).

% Buddhist temples, monastic orders, and the priestly establishment that administered Buddhist doctrine and funerary ritual. Under the partition reading, they maintain authority over death and the afterlife: temple priests conduct funerary rites, maintain ancestral memorials, and teach karmic consequence. They do not administer life-cycle rituals (Shinto does), so they maintain institutional autonomy in their sphere. The partition constraint preserves their institutional role and legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, national).

% Various Japanese governments from the medieval period onward recognized and sometimes enforced the partition (e.g., during certain periods assigning Shinto to state-civic functions and Buddhism to private devotion, or vice versa). The state appears in this story as observer because the partition was institutionally tolerated/endorsed, not imposed by state force alone; the arrangement reflected both state interest and practitioner preference.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, state_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates two powerful religious traditions to non-competing domains: Shinto addresses the life-world (birth, growth, community participation, seasonal participation in kami veneration), Buddhism addresses the transcendent and mortuary concerns (the afterlife, karmic consequence, preparation for death). This allocation eliminates the need for practitioners to choose between competing metaphysics and allows both institutional hierarchies to maintain distinct authority and function.
% TRANSFER_FUNCTION: The partition transfers no material goods or labor asymmetrically. Instead, it transfers *cognitive authority*: Shinto authorities maintain the right to interpret and conduct life-cycle ritual; Buddhist authorities maintain the right to interpret and conduct death and mortuary ritual. Practitioners voluntarily engage both systems in their appropriate contexts.
% ABSENT_VOICES: Exclusive practitioners — those who would prefer to practice only Shinto (without Buddhism) or only Buddhism (without Shinto) — experience the partition as a constraint on their preferred religious monism, but their voices are marginal in the historical record. Reformers and modernizers (particularly from the 19th century onward) who argued for explicit metaphysical integration or for choosing one system exclusively are absent from the traditional partition framework. These voices enter the conversation as the syncretic_reading and incoherence_reading contend with the partition.
% DISAPPEARANCE_RATIONALE: If the partition constraint disappeared overnight, practitioners would face a choice between integration (metaphysically unifying Shinto and Buddhism under honji-suijaku or another syncretic doctrine) and incoherence (tolerating contradiction without resolution). Institutional hierarchies would compete for authority over all life-domains rather than maintaining autonomous spheres. The historical stability of Japanese religious practice for centuries depended on the partition; its disappearance would reorganize the entire field.
% FOUNDING_PROBLEM: How do two major religious traditions coexist in one society without one dominating the other or practitioners experiencing irresolvable metaphysical contradiction?
% FOUNDING_PROBLEM_CORROBORATION: Contemporary ethnographic research on Japanese religious practice (Hardacre, Reader, Blacker, Nakamaki) confirms that practitioners continue to partition Shinto and Buddhism functionally, treating them as non-competing systems. Temple and shrine administrators explicitly endorse the partition in institutional statements and practice. Historical textual sources (especially from the Edo period) show explicit articulation of domain separation in religious writings. No external corroboration from Western religious scholars alone suffices; the reading is grounded in how Japanese practitioners and institutions themselves frame the relationship.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the partition reading claims that the constraint emerges from functional need and practitioner autonomy, not from coercive hierarchy. Suppression is minimal (0.12) because no party needs to prevent alternatives — the separation is mutually intelligible and practitioners voluntarily used Shinto for weddings and Buddhism for funerals. Theater ratio is very low (0.08) because the functional division is genuine; no need for performative maintenance if the arrangement serves real purposes. Accessibility_collapse is high (0.85) because once you accept that kami govern life and buddhas govern the afterlife, alternatives collapse — you cannot have a 'middle ground' between a transcendent Buddhist cosmology and kami-centered life management. Resistance is low (0.15) because the arrangement was stable for centuries and most historical actors supported it. The measurement series is flat to slightly rising: extractiveness creeps up modestly in the modern period as institutional actors (temples and shrines) began to emphasize the partition more explicitly, possibly as a defensive posture against both state modernization and internal-coherence pressures from Western-educated critics.
 *
 * PERSPECTIVAL GAP:
 *   From a Shinto institutional perspective, the partition preserves shrine authority over life and community. From a Buddhist perspective, it preserves temple authority over death and karmic consequence. From a practitioner perspective, it provides cognitive clarity and reduces contradiction. These are compatible perspectives — no seat disagrees about the function. The gap would emerge if we tested against the incoherence_reading: the incoherence reading would say 'practitioners experienced confusion and the partition reading is ex-post rationalization.' The partition reading would say 'practitioners experienced intelligent coordination.' The engine does not adjudicate this; the measurement of whether the reading holds is precisely whether extractiveness and suppression remain low under scrutiny.
 *
 * DIRECTIONALITY LOGIC:
 *   All three beneficiary groups sit near the symmetric or beneficiary end of the directionality scale (d near 0.0-0.3). Shinto practitioners benefit from having rituals for the life-world. Buddhist practitioners benefit from having a dedicated soteriological framework. Practitioners-of-both benefit from the cognitive clarity of knowing which system applies when. No party is structurally trapped (exit_options are constrained but not trapped — one could defect to exclusive Buddhism or Shinto), and no party bears costs disproportionate to benefits. This is why the reading claims rope-type coordination: the constraint allocates spiritual labor efficiently and all parties gain. The engine's per-seat computation will test this by deriving d from power, exit, and beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading's founding problem is: 'How do we integrate two powerful religious traditions without either one dominating or practitioners experiencing incoherence?' The reading asserts that partitioning by function SOLVED this problem. The founding_problem_status in the six_questions is LIVE — Japanese practitioners still partition Shinto and Buddhism in this way in contemporary practice. Mandatrophy would arise if the founding problem had been solved and the partition persisted as theater, or if the partition persisted despite the founding problem being demonstrably unsolved. Neither appears to be the case: the partition is still functional and practitioners still experience it as solving the problem. So no mandatrophy is detected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_kernel_incoherence,
    'Does the partition reading describe a stable ontological commitment that practitioners held, or does it rationalize what was actually institutional toleration of incoherence?',
    'Textual and ethnographic evidence: if explicit partition doctrines appear in religious texts and practitioner interviews articulate the separation as intentional, the reading is grounded; if partitioning emerges only from modern scholarly reconstruction and historical actors treated the domains as ambiguous or contested, the incoherence_reading may be structurally prior.',
    'If the partition is ex-post rationalization of incoherence, the reading''s claimed autonomy and low extraction would be reclassified as cover stories for institutional confusion that benefited no one deliberately. If the partition is genuine doctrine, it remains a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_kernel_incoherence, empirical, 'Whether domain partition was explicit doctrine or scholarly reconstruction of institutional incoherence.').

omega_variable(
    ontological_commitment_vs_pragmatic_adaptation,
    'Is the partition reading an *ontological commitment* (a claim about the nature of kami, buddhas, and the cosmos) or merely a *pragmatic adaptation* (a rule of practice that says ''use Shinto for weddings, Buddhism for funerals'' without asserting anything about whether kami and buddhas are the same or different entities)?',
    'Textual analysis of religious authorities'' written justifications; interviews with practitioners about the metaphysical reasoning (if any) behind the partition; comparison with explicit syncretic doctrines (honji-suijaku) to identify what domain partition *adds* beyond operational division.',
    'If partition is pragmatic adaptation only, it may be better classified as a simple coordination mechanism (rope) with negligible extraction. If it is ontological commitment, it actively excludes the syncretic reading and represents a genuine constraint on belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_commitment_vs_pragmatic_adaptation, conceptual, 'Whether partition is metaphysical doctrine or operational practice rule.').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who benefits from the partition commitment? Is the constraint maintained because all parties gain coordination benefit, or because specific institutional actors (temples, shrines, priestly hierarchies) benefit from the arrangement?',
    'Institutional history: if partition breaks down and practitioners spontaneously re-integrate domains (or vice versa), the beneficiaries are revealed; if partition persists because Buddhist temples and Shinto shrines have structurally incompatible institutional interests that partition satisfies, those institutions are the beneficiaries.',
    'If partition is genuinely mutual benefit (rope), the low extractiveness and resistance hold. If institutional actors benefit differentially and maintain partition against practitioner preference, reclassify as tangled_rope or snare with hidden beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether partition benefits practitioners symmetrically or institutional actors asymmetrically.').

omega_variable(
    kernel_ontological_status,
    'This constraint is ONE READING of a contested kernel (shinbutsu_ontological_commitment). The kernel itself — the claim about how kami, buddhas, and the cosmos relate — is what is contested. This reading asserts domain partition without metaphysical integration. Is the partition reading *consistent with* the founding problem of Japanese religious pluralism, or does it *foreclose* the syncretic reading within a single coherent framework?',
    'Logical analysis: the partition reading (kami govern life, buddhas govern afterlife, separate domains, no shared ontology) is logically compatible with a world where some people believe that (partition) and others believe honji-suijaku (kami and buddhas are manifestations of one cosmic order) — IF people with different beliefs coexist without contradiction. If the partition reading makes the syncretic reading *logically impossible* for a believer to hold, foreclosure applies; if the readings can coexist as different theological positions, they are coexistence-related.',
    'Determines whether reading_relations to syncretic_reading should be ''forecloses'' (rare, strong logical incompatibility) or ''coexists_with'' (different live positions). Foreclosure would strengthen the partition reading''s ontological claims; coexistence would weaken it to a practical convention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_ontological_status, conceptual, 'Whether partition logically forecloses syncretism or they coexist as distinct theological positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(shin_tr_t10, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(shin_tr_t40, observed).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement_basis(shin_tr_t60, observed).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(shin_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(shin_be_t10, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(shin_be_t40, observed).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement_basis(shin_be_t60, observed).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(shin_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu kernel (the claim about kami-buddha ontological relationship in Japanese religion) instantiates in three structurally distinct constraints: partition_reading (this story, claiming domain separation without metaphysical integration), syncretic_reading (claiming honji-suijaku unified metaphysics), and incoherence_reading (claiming no stable commitment existed). Each constraint has its own ε, beneficiary structure, and type. They are linked by shared kernel and competing readings, not by causal dependency. The ε-invariance principle requires separate stories because the three readings have different epistemic bases (textual doctrine, practiced tradition, absence of doctrine) and different measurements would apply to each.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
