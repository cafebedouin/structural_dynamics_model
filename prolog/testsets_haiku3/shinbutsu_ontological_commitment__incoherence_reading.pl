% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-Shugo Ontological Incoherence: Institutionally Tolerated Contradiction
 *   domain: religious/ontological/political
 *
 * SUMMARY:
 *   This reading instantiates the claim that shinbutsu-shugo (the syncretic
 *   arrangement of Shinto and Buddhism in pre-Meiji Japan) rested on
 *   institutionally tolerated ontological incoherence rather than genuine
 *   doctrinal integration or functional separation. Under this reading, no
 *   stable commitment existed to a coherent metaphysical framework — neither
 *   the honji-suijaku doctrine (which claimed unified cosmic order) nor a
 *   partition model (which claimed separate functional domains) was reliably
 *   enforced or universally accepted. Instead, temples and communities
 *   performed both Shinto and Buddhist functions, sustained rituals in both
 *   frames, and navigated the contradiction through pragmatic institutional
 *   practice rather than philosophical resolution. This reading sees
 *   extraction in the cost imposed on anyone seeking ontological coherence;
 *   it sees suppression in the active maintenance of incoherence despite its
 *   philosophical instability; and it claims the Meiji state benefited from
 *   separation precisely because the incoherence reading made separation easy
 *   to justify and execute — the scattered institutions had no unified
 *   doctrine to defend. The three sibling readings (syncretic, partition,
 *   incoherence) constitute a constraint family: the same historical kernel
 *   (the shinbutsu arrangement) read three different ways.
 *
 * KEY AGENTS:
 *   - syncretic_temples_and_monasteries: Maintained dual ritual and institutional roles without resolving the ontological ground; bore the cost of sustained contradiction
 *   - edo_period_intellectual_class: Some sought coherence through honji-suijaku or partition models; faced institutional tolerance of incoherence that left their philosophical work unresolved
 *   - meiji_state_builders: Weaponized the incoherence reading to justify shinbutsu-bunri (separation); extracted legitimacy from critique they did not originate
 *   - syncretic_institution_beneficiaries: Benefit from the practical coordination that ontological incoherence sustained; lose institutional foundation when separation is enforced
 *   - coherence_seekers: Analytical/philosophical agents who experience the constraint as extractive — forced to operate under unstable premises or to suppress recognition of contradiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-Shugo Ontological Incoherence: Institutionally Tolerated Contradiction").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/ontological/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '8e96a92f-91c8-426a-a0bb-a9c99b0ccced').
narrative_ontology:cs_kernel_codification('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', distributed).
narrative_ontology:cs_authority_grounding('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', extraction).
narrative_ontology:cs_reading_relation('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', shinbutsu_ontological_commitment__syncretic_reading, forecloses).
narrative_ontology:cs_reading_relation('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', foundational, no_stable_ontological_commitment_existed).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment_existed, holdable).
narrative_ontology:cs_axiom_grounding('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', no_stable_ontological_commitment_existed, empirically_contingent).
narrative_ontology:cs_axiom('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', secondary, institutional_incoherence_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(institutional_incoherence_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', institutional_incoherence_as_extraction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', pragmatic_institutional_tolerance).
narrative_ontology:cs_drift_state('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', meiji_enforcement_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('8e96a92f-91c8-426a-a0bb-a9c99b0ccced', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, institutional_syncretism_beneficiaries).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, pre_meiji_syncretic_institutions).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, philosophical_coherence_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, pre_meiji_syncretic_beneficiaries).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temples_and_monasteries).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, edo_period_intellectual_class).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, coherence_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintained both Shinto shrines and Buddhist temples, performed rituals in both frameworks, and served communities across the full cycle of life and death. They sustained the practical coordination but were forced to do so without stable ontological justification — the ground beneath their dual function was explicitly incoherent. During the Edo period, this was tolerated pragmatically. In the Meiji period, this same arrangement was rendered untenable by enforced separation. Their exit options were constrained: leaving one function meant institutional collapse; staying meant eventually accepting separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, syncretic_temples_and_monasteries, payer,
    organized, generational, constrained, national).

% Some sought to resolve the ontological contradiction through philosophical frameworks (honji-suijaku, partition models). Their work was tolerated by institutions but never institutionally enforced — the incoherence persisted despite intellectual effort. They experienced extraction in the form of institutional indifference to coherence: their philosophical work did not resolve the practical arrangement. Some eventually migrated to the Meiji state's coherence project (separation), extracting value from the state's adoption of their framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, edo_period_intellectual_class, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, edo_period_intellectual_class, observer).

% Communities and practitioners who benefited from coordinated ritual services, did not require philosophical justification for the dual system, and pragmatically navigated the incoherence without distress. Their benefit was real: a single institution that solved both Shinto and Buddhist needs. They had modest exit options (choosing among temples or moving), but their fundamental benefit (coordinated services) was portable. When separation was enforced, their benefit dissolved — they now required two institutions where one sufficed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, pre_meiji_syncretic_beneficiaries, beneficiary,
    moderate, biographical, mobile, local).

% Consolidated state power by enforcing shinbutsu-bunri (separation). The incoherence reading provided philosophical cover: separation resolves the incoherent arrangement. The state extracted legitimacy from this critique (which they did not originate) and used it to reorganize religious institutions under state authority. They had high exit options: they could enforce separation, tolerate syncretism, or partition differently — their choice. Their benefit was clear: institutional consolidation, elimination of competing religious authorities, and (in the Shinto frame) state ideological alignment with a revived 'pure' Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary).

% Philosophers, theologians, and anyone invested in stable ontological frameworks who experienced the institutional incoherence as a systematic cost. Their analytical position meant they could exit the system (by adopting one reading fully or abandoning both), but their participation in the broader intellectual conversation meant they bore the cost of unresolved contradiction. The constraint extracted coherence from them: their best thinking could not resolve the arrangement.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, coherence_seekers, payer,
    analytical, civilizational, analytical, universal).

% Intellectuals and institutional reformers who argued for coherent separation (Shinto for life-cycle, Buddhism for afterlife) as a resolution to the incoherence. Their voice was excluded from institutional enforcement: institutions tolerated the partition framework intellectually but did not commit to it operationally. When the Meiji state adopted a separation policy, it did not do so from partition-framework grounds — it did so on incoherence-reading grounds (there is no coherence, so we will separate them). The partition advocates' framework was vindicated in outcome but not adopted in reasoning.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, partition_reading_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single integrated institution (syncretic temple/monastery) that serves communities across both Shinto and Buddhist ritual needs: birth and fertility rites, death and memorial services, seasonal festivals, rites of passage — all resolved within one organizational structure without requiring practitioners to navigate two separate institutions or settle which cosmological framework applies.
% TRANSFER_FUNCTION: Moves institutional legitimacy from practitioners and communities TO the syncretic institution, which collects the benefit of providing unified service. Also moves philosophical coherence FROM coherence-seekers (who must accept or suppress recognition of ontological contradiction) TO the institutions that use pragmatic tolerance to avoid resolving it.
% ABSENT_VOICES: Partition-reading advocates (intellectuals arguing for coherent separation) and Meiji state consolidators were structurally excluded from the pre-Meiji institutional consensus — their frameworks were articulated but not enforced. If they had been fully in the conversation, they would have demanded either coherent separation or explicit syncretic doctrine; instead, they were tolerated but peripheral.
% DISAPPEARANCE_RATIONALE: If the incoherence reading disappeared (i.e., if coherent syncretic doctrine were universally accepted or coherent separation were enforced), the institutional arrangement would reorganize entirely. The pre-Meiji version depended on incoherence being tolerated pragmatically; the Meiji version depended on incoherence being named as a problem requiring separation. Either way, the arrangement rearranges when the reading's status changes.
% FOUNDING_PROBLEM: How can Shinto and Buddhist ritual practice coexist in a single institutional and social framework when their cosmological foundations are distinct or even contradictory?
% FOUNDING_PROBLEM_CORROBORATION: The Meiji state attests the problem is dead — separation is the solution. Syncretic institutions, before separation, attested the problem was not pressing — pragmatic tolerance sufficed. Philosophical coherence-seekers attested the problem was live but unsolved. Modern scholarship (outside the benefiting parties of either the pre-Meiji or Meiji arrangements) attests that the Edo-period arrangement DID involve unresolved ontological incoherence, and that the Meiji-era solution was driven by state consolidation interests, not by genuine resolution of philosophical problems.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint imposes the cost of sustained ontological instability on those who seek philosophical coherence or institutional stability. Suppression is high (0.72) because the institutional incoherence must be actively maintained — contradictions must not be resolved, philosophical critiques must be tolerated but not enforced, and the practical arrangement must hold despite its theoretical weakness. Theater is elevated (0.58) because much of the enforcement activity is rhetorical: arguments are advanced and countered, but the institutional arrangement persists by pragmatic tolerance rather than principled resolution. Accessibility of alternatives is low (0.42) because anyone embedded in the syncretic system faces high costs of exit — leaving requires institutional reorganization. Resistance is high (0.71) because philosophical coherence-seekers mounted persistent intellectual resistance, and later state actors (Meiji) mounted institutional resistance by enforcing separation. The measurement series over 450 time units (representing the Edo period through early Meiji) show rising extractiveness and theater: as the Edo period advanced, the incoherence reading became more explicit in intellectual discourse, making the constraint's extractive character more visible. By the Meiji period, the state weaponized this visibility to justify separation.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of syncretic institutions, the constraint appears as pragmatic institutional coordination: managing both Shinto and Buddhist functions served communities and resolved practical allocation problems. From the seat of philosophical coherence-seekers, the same arrangement appears extractive: forced incoherence suppresses legitimate intellectual work. From the Meiji state seat, the reading is weaponized advantage: the incoherence is named as a problem they can solve, extracting political legitimacy and institutional power by enforcing separation. The engine computes this divergence from the base structural data (beneficiary/victim declarations and exit options); the authored metrics describe the actual suppression and theater required to maintain the arrangement despite its theoretical instability.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic institutions (temples, monasteries, ritual practitioners) are primary targets of the constraint — they must sustain incoherent practice and cannot easily exit. Their directionality is high (d near 1.0), victims in the base_properties sense. Philosophical coherence-seekers experience high directionality but occupy a different structural position — they are harmed by forced incoherence but not trapped by institutional dependency; their exit is possible at higher cognitive cost. Meiji state builders benefit (low d, near 0.0) because the incoherence reading legitimizes separation and allows them to consolidate institutional power by dismantling syncretic arrangements. Pre-Meiji beneficiaries of institutional syncretism (those who enjoyed practical coordination without philosophical resolution) are harmed by the reading's rise to salience — their beneficiary position depends on incoherence remaining tolerated, not explicitly named.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the kernel) is to coordinate Shinto and Buddhist ritual practice in pre-Meiji society. The incoherence reading claims the mandate — coherent ontological integration — was never achieved and was substituted with pragmatic institutional tolerance. By the Meiji period, the mandate was formally abandoned: separation replaced coordination as the official goal. Under the incoherence reading, mandatrophy is resolved: the original coordinate-both-systems goal outlives its function (separation is now the rule), but the measure of whether mandatrophy is present is whether the incoherence reading ITSELF becomes institutional dogma. If the state enforces separation while simultaneously enforcing silence about the prior incoherence, no mandatrophy is visible — the past arrangement is simply erased. But if the incoherence reading becomes explicit and is used to justify continued intervention, mandatrophy may be present in the justification layer: the state maintains a reading of past failure to justify ongoing separation, even when separation is now universal and requires no additional justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_incoherence_versus_pragmatic_tolerance,
    'Was the absence of stable ontological commitment a feature (pragmatic institutional tolerance of contradiction) or a failure (unresolved philosophical incoherence)?',
    'Textual analysis of Edo-period institutional writings distinguishing between: (1) explicit acknowledgment of contradiction with methodological tolerance, versus (2) absence of awareness or reconciliation attempts, versus (3) rhetorical cover for unstated power asymmetries.',
    'If feature (pragmatic tolerance): the constraint is institutional coordination that happened to sustain contradiction; extraction is minimal, theater is low — type shifts toward rope. If failure (unresolved incoherence): extraction is the cost paid by coherence-seekers and syncretic institutions forced to operate under unstable premises — type remains tangled_rope or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_incoherence_versus_pragmatic_tolerance, conceptual, 'Whether institutional tolerance of ontological contradiction was deliberate pragmatism or unrecognized philosophical instability.').

omega_variable(
    meiji_state_interest_in_separation,
    'Did the Meiji state benefit from and deliberately weaponize the incoherence reading to justify shinbutsu-bunri, or did separation follow from genuine philosophical critique of the incoherence reading itself?',
    'Historical analysis of Meiji-era policy documents, state correspondence, and legislative debate: trace whether separation was justified as (a) response to discovered incoherence, (b) response to philosophical argument for coherent separation, or (c) state power-consolidation that opportunistically cited incoherence as post-hoc rationale.',
    'If (a) or (b): the state weaponized genuine structural problems; the incoherence reading is validated by state action against it. If (c): state interest is extractive use of the incoherence frame; the constraint shifts toward snare — the Meiji regime extracted legitimacy from a philosophical critique it did not originate and that may not have described the actual problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_state_interest_in_separation, empirical, 'Whether Meiji separation responded to incoherence critique or used it as cover for political consolidation.').

omega_variable(
    institutional_performance_and_boundary_dissolution,
    'When syncretic temples performed both Shinto and Buddhist rituals without stable ontological commitment, did practitioners experience this as coherent functional separation (partition reading dynamics) or as actual incoherence that required suppression or reframing?',
    'Ethnographic reconstruction from temple records, ritual instructions, and community testimony: do pre-Meiji sources show practitioners managing boundaries pragmatically, or show tension, reinterpretation, or hedging language suggesting awareness of contradiction?',
    'If practitioners managed pragmatically: the incoherence reading overstates the problem; the constraint is institutional coordination with minimal extraction. If actual tension is evident: the incoherence reading correctly names the suppressed cost — extraction is real and borne by those forced to sustain contradiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_performance_and_boundary_dissolution, empirical, 'Whether institutional incoherence was pragmatically managed or required active suppression of recognized contradiction.').

omega_variable(
    sibling_reading_coexistence_versus_foreclosure,
    'Can this reading (incoherence) and the syncretic reading (honji-suijaku integration) coexist within a single intellectual framework, or does acknowledging incoherence logically foreclose the syncretic reading?',
    'Philosophical analysis: the honji-suijaku doctrine claims kami and buddhas are expressions of the same underlying cosmic order; the incoherence reading claims no stable ontological commitment existed. These are logically incompatible at the level of doctrine (one asserts coherence, the other denies it), but compatible at the level of institutional practice (the doctrine could have been articulated but not enforced, leaving incoherence tolerated). Resolution hinges on whether we evaluate the kernel at the level of articulated doctrine or practiced arrangement.',
    'If evaluated at doctrine level: the readings foreclose each other. If evaluated at practice level: both can be live (doctrine claimed coherence, practice tolerated incoherence). The engine will compute foreclosure; this omega flags that the classification depends on the level of analysis chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_versus_foreclosure, conceptual, 'Whether ontological incoherence and syncretic doctrine are logically incompatible or compatible at different levels of analysis (doctrine vs. practice).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 150, 0.44).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 300, 0.51).
narrative_ontology:measurement(shin_tr_t450, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 450, 0.58).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 300, 0.61).
narrative_ontology:measurement(shin_be_t450, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 450, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 150, 0.61).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 300, 0.68).
narrative_ontology:measurement(shin_su_t450, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 450, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu-ontological-commitment kernel. The same historical arrangement (Shinto-Buddhist syncretism in pre-Meiji Japan) is instantiated as three distinct constraints, each with its own ε-invariant structure: (1) incoherence_reading: no stable ontological commitment existed; (2) syncretic_reading: kami and buddhas are aspects of one unified cosmological order under honji-suijaku; (3) partition_reading: Shinto and Buddhism occupy separate functional domains without ontological integration. These are NOT three angles on the same constraint — they are three different constraints derived from readings of the contested kernel. Each reading has its own beneficiary/victim structure, its own claimed type, and its own measurement series. The decomposition follows the ε-invariance principle: measuring the constraint via the incoherence frame yields high extraction; measuring it via the syncretic frame yields low extraction (apparent coherence); measuring it via the partition frame yields moderate extraction (coordination cost). These are not measurement-basis differences in a single ε — they are three distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
