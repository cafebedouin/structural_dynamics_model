% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Intergenerational Trauma Encoding in Ritual Practice
 *   domain: religious/collective_memory/psychology
 *
 * SUMMARY:
 *   This constraint models one reading of a catastrophe-memory kernel: the
 *   ritual practice through which a community survives repeated persecution
 *   by encoding threat-recognition heuristics into intergenerational
 *   mourning. The reading frames ritual as a trauma-transmission mechanism —
 *   trauma is deliberately encoded because the group believes it serves as
 *   early-warning system. The cost falls on descendants (heightened anxiety,
 *   complex PTSD markers, identity-locked participation) while the benefit
 *   accrues to collective vigilance (distributed threat-detection capacity).
 *   This reading contests with sibling interpretations that emphasize
 *   boundary maintenance, symbolic continuity, or survival-competence
 *   transmission divorced from trauma focus. The kernel itself
 *   (catastrophe-memory practice) is fixed; the readings differ on what
 *   function the ritual serves and who bears its costs.
 *
 * KEY AGENTS:
 *   - Ritual custodians (institutional agenda-setter): maintain trauma-focused commemoration as essential to group survival
 *   - Descendant bearers (moderate-power payers): inherit and perform rituals whose trauma content they did not directly experience; bear psychological cost
 *   - Younger generation practitioners (payers with excluded voice): resist trauma-focused practice as psychologically damaging; silenced as disloyal
 *   - Diaspora communities (mobile payers): practice ritual in geographically safe contexts where original threat is remote
 *   - Mental health observers (analytical seat): document intergenerational trauma transmission and question adaptive value
 *   - Perpetrator-group representatives (completely excluded): whose presence would shatter the unified threat narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Intergenerational Trauma Encoding in Ritual Practice").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious/collective_memory/psychology").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '38257323-2cd5-4dd7-8e20-89f04a2e3556').
narrative_ontology:cs_kernel_codification('38257323-2cd5-4dd7-8e20-89f04a2e3556', distributed).
narrative_ontology:cs_authority_grounding('38257323-2cd5-4dd7-8e20-89f04a2e3556', lineage).
narrative_ontology:cs_interpretation_layer_present('38257323-2cd5-4dd7-8e20-89f04a2e3556').
narrative_ontology:cs_reading_relation('38257323-2cd5-4dd7-8e20-89f04a2e3556', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('38257323-2cd5-4dd7-8e20-89f04a2e3556', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('38257323-2cd5-4dd7-8e20-89f04a2e3556', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('38257323-2cd5-4dd7-8e20-89f04a2e3556', foundational, trauma_encodes_adaptive_threat_heuristics).
narrative_ontology:cs_axiom_status(trauma_encodes_adaptive_threat_heuristics, holdable).
narrative_ontology:cs_axiom_grounding('38257323-2cd5-4dd7-8e20-89f04a2e3556', trauma_encodes_adaptive_threat_heuristics, empirically_contingent).
narrative_ontology:cs_axiom('38257323-2cd5-4dd7-8e20-89f04a2e3556', foundational, psychological_cost_justified_by_survival_benefit).
narrative_ontology:cs_axiom_status(psychological_cost_justified_by_survival_benefit, holdable).
narrative_ontology:cs_axiom_grounding('38257323-2cd5-4dd7-8e20-89f04a2e3556', psychological_cost_justified_by_survival_benefit, instrumental).
narrative_ontology:cs_reference_frame('38257323-2cd5-4dd7-8e20-89f04a2e3556', post_catastrophe_vigilance_mandate).
narrative_ontology:cs_drift_state('38257323-2cd5-4dd7-8e20-89f04a2e3556', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38257323-2cd5-4dd7-8e20-89f04a2e3556', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The distributed early-warning capacity of the group: heightened alertness to persecution patterns, heuristics for recognizing threat escalation, vigilance infrastructure maintained across generations. Not an actor, but the functional output the constraint produces.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).

% Inheritors and practitioners of mourning rituals carrying trauma-laden commemorations. They perform the ritual duties prescribed by tradition, internalize the encoded threat narratives, and carry the psychological weight of intergenerational grief. Exit requires breaking from collective identity and group membership. The ritual operates through them; they bear its costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_bearers, payer,
    moderate, biographical, identity_locked, global).

% Religious authorities, elders, and institutional keepers who maintain and interpret the ritual forms. They decide what is commemorated, how trauma is encoded into performance, which narratives are transmitted. They justify the practice as essential to group survival and collective memory. They face pressure from younger generations and secular authorities to moderate or abandon trauma-focused commemoration.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians, agenda_setter,
    institutional, generational, constrained, regional).

% Those born outside the immediate catastrophe, practicing rituals whose trauma content they have not directly experienced. They bear the psychological cost of performing and internalizing narratives of persecution and loss. Some resist the trauma focus as outdated or psychologically damaging; their objections are often framed as apostasy or disloyalty rather than legitimate critique.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, younger_generation_practitioners, excluded).

% Clinicians, psychologists, and researchers studying intergenerational trauma transmission. They measure outcomes (anxiety, complex PTSD, moral injury), document mechanisms (narrative internalization, threat hypervigilance), and raise questions about whether trauma encoding through ritual is adaptive early-warning or iatrogenic psychological burden. They occupy an analytical seat outside the commitment structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, mental_health_observers, observer,
    institutional, biographical, analytical, global).

% Those descendants of perpetrator communities or rival groups whose own narratives conflict with trauma-encoding rituals. Their exclusion from the ritual conversation is structural; their presence would directly contest the threat narratives encoded into performance. They are not invited to the conversation; their voice would dissolve the ritual's functional unity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, perpetrator_group_representatives, excluded,
    powerful, generational, trapped, regional).

% Descendants settled in safe locations geographically distant from the catastrophe's site. They practice the same rituals but occupy a different threat environment. For them the ritual's trauma encoding persists as inheritance even though the original threat is geographically remote. They have some mobility (can choose assimilation, partial practice, reinterpretation) but face social pressure to maintain tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, diaspora_communities, payer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective alertness to persecution patterns through embodied, affective encoding of historical trauma: the ritual works as a distributed threat-detection system, keeping destructive-conflict heuristics alive across generations when formal historical training might fade. The practice coordinates vigilance by making threat-recognition a shared emotional and performative practice rather than abstract historical knowledge.
% TRANSFER_FUNCTION: Moves psychological burden from custodians (who hold the authoritative account of catastrophe) to practitioners (who inherit and enact the trauma narrative). Moves threat vigilance from the adult survivors (who experienced the danger directly) to descendants who must construct their threat model from encoded performance rather than direct experience.
% ABSENT_VOICES: Descendants who want to practice the religious tradition WITHOUT trauma-focused commemoration are structurally excluded; their reframing attempts are met as threat to group continuity. Perpetrator-group descendants are completely outside the conversation; their presence would shatter the unified threat narrative. Diaspora members in safe locations who question whether the ritual's trauma intensity remains adaptive are usually voiceless in custodian-controlled interpretation.
% DISAPPEARANCE_RATIONALE: Custodians argue: if the trauma-encoding ritual disappeared, the group would lose the heuristic vigilance that enabled survival through repeated persecution, and future threats would catch descendants unwarned. Critics argue: if the ritual disappeared, descendants would experience reduced anxiety, complex PTSD markers would decline, and group solidarity could reorganize around non-trauma identity claims. The disagreement is empirical (does the ritual's encoding mechanism actually improve threat detection relative to its psychological cost?) and normative (should a group's adaptive capacity ride on internalized trauma if safer alternatives exist?).
% FOUNDING_PROBLEM: After catastrophic persecution, the surviving group faced a paradox: how to transmit the lessons of threat escalation and survival heuristics to descendants who will not experience the original danger directly, so that future persecution is recognized early enough to mount effective response. The ritual encodes this knowledge into affective, embodied, repetitive practice: trauma becomes the vehicle for survival competence transmission.
% FOUNDING_PROBLEM_CORROBORATION: Custodians and older practitioners attest the founding problem is live: persecution remains a recurring risk and vigilance must be maintained. Mental health researchers and diaspora members attest the founding problem has substantially changed: the threat environment differs geographically and historically, and the cost of maintaining trauma vigilance through ritual may now exceed the early-warning benefit. Independent historians and comparative religion scholars support the assessment that the ritual's threat-encoding function was adaptive in high-risk contexts but may have become disproportionate in safer diaspora environments.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 70 time-units, then plateaus — this trajectory reflects a constraint whose function was adaptive in high-threat contexts but whose cost accumulates as the threat environment changes (diaspora settlement, generational distance from original catastrophe, declining actual persecution risk). Theater ratio rises in parallel (0.18 to 0.41) because an increasing share of ritual performance is defensive (asserting its necessity) rather than functional (detecting actual threats). Suppression requirement stays moderate (0.38–0.52) because the constraint operates through internalized identity-lock and affective adoption more than external coercion — descendants 'choose' to participate, but exit is identity-dissolving. The plateau after t70 suggests the constraint has reached a stable extractive equilibrium in diaspora contexts: trauma encoding persists, benefit-to-cost ratio has stabilized, and the rate of resistance has roughly equilibrated with custodian enforcement. Measurements use a single shared time grid (all metrics authored at every examined point: t0, 14, 28, 42, 56, 70, 100) to avoid false type transitions from temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the custodian seat, the ritual is genuine coordination: a successful mechanism for preserving threat-detection capacity across generational discontinuity, justified by the recurring danger. From the descendant-bearer seat in diaspora (geographically distant from original threat), the same structure operates as extraction: psychological burden imposed for benefit that is no longer proportionate to local threat environment. From the mental-health observer seat, the ritual is mechanically a trauma-transmission device with measurable costs (anxiety, hypervigilance, identity rigidity) and empirically contested benefits (whether the threat-detection function matches its psychological price). The engine should compute type divergence across these seats from the power/exit/beneficiary/victim data: custodians occupy institutional power with arbitrage-grade exit (they set the rules and can reinterpret them), while descendant-bearers occupy moderate power with identity-locked exit. This asymmetry should produce custodian-side classification as coordination (low d toward beneficiary pole) and bearer-side classification as extraction (high d toward target pole).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is not an agent — 'collective threat vigilance' is the functional output (early-warning capacity). The victims are the identity-locked descendants who bear the psychological and social cost of trauma-focused commemoration. Custodians occupy institutional power and can reinterpret the ritual or loosen trauma content (arbitrage exit); they are not primary targets. Descendants occupy moderate power in diaspora but are trapped by identity-lock: leaving the ritual means group exit, which is experienced as self-dissolution. This asymmetry drives d divergence: custodians should compute near the beneficiary pole (they benefit from the vigilance function and can reframe it); descendants should compute near the target pole (they bear costs and cannot exit without identity rupture). The boarding-school trauma cases and Holocaust-memorial escalation provide historical evidence: custodian-side reinterpretation (broadening meaning, loosening ritual intensity) occurs when institutional actors choose it; descendant-side resistance (mental health advocacy, partial practice, assimilation) rarely translates to ritual change without custodian permission.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading fits Tangled Rope because: (1) the ritual has a genuine coordination function (threat-detection, heuristic transmission); (2) extraction is asymmetric (descendants bear psychological cost for benefit that is diffuse and institutional); (3) enforcement is active (custodians maintain trauma-focused practice through ritual requirements, social pressure, and interpretation monopoly). The reading explicitly rejects the Rope classification (pure coordination) because extraction is not symmetrically distributed — the costs and benefits flow in opposite directions. It rejects the Snare classification because the coordination function is real and not merely cover story; the ritual did and does transmit valuable survival heuristics. Tangled Rope is the fit: genuine coordination mechanism layered with asymmetric extraction, maintained by custodian enforcement of trauma focus as non-negotiable feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trauma_encoding_adaptive_value,
    'Does encoding trauma into intergenerational ritual actually improve threat-detection performance relative to alternative knowledge-transmission mechanisms (formal history education, secular oral history, institutional documentation)?',
    'Longitudinal study comparing threat-recognition accuracy and response speed in communities with trauma-focused ritual vs. communities that transmit the same historical knowledge through non-affective channels. Post-conflict settings offer natural experiments: do persecuted communities with intergenerational trauma rituals show measurably faster threat escalation recognition than those without?',
    'If trauma encoding provides no measurable detection advantage over secular alternatives, the constraint reclassifies toward pure extraction (snare) — the trauma focus becomes theater masking a transfer mechanism. If trauma encoding provides substantial advantage, the constraint remains tangled_rope with higher justification for the asymmetric cost. If advantage exists only in high-threat contexts and vanishes in diaspora safety, the constraint''s type and extractiveness should vary by threat environment (per-seat classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trauma_encoding_adaptive_value, empirical, 'Whether the trauma-encoding mechanism delivers measurable early-warning benefit.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the suppression maintaining this constraint is internalized (descendants believe trauma focus is essential and necessary) vs. structural (custodians block alternative practices and punish reinterpretation)?',
    'Ethnographic study of resistance narratives, exit outcomes, and post-exit trajectories. If descendants who exit the ritual report that suppression dissolves outside the community (anxiety drops, identity reconstructs), the suppression is primarily internalized. If exit itself is blocked (social shunning, economic penalty, institutional retaliation), the suppression is structural. Most likely: both operate, but the proportion matters for classification.',
    'If suppression is heavily internalized, the constraint is more entrenched than raw suppression metrics suggest — descendants carry the suppression with them and may reproduce it voluntarily even after exit. If suppression is primarily structural, custody-controlled reform (changing custodian interpretation) could reduce extraction quickly. If mixed, directionality diverges: internalized suppression makes descendant-side exit harder than structural suppression alone would predict, raising d and extractiveness on the payer side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized mechanisms of suppression in identity-locked constraints.').

omega_variable(
    reading_incommensurability,
    'Is the trauma_encoding_reading logically foreclosed by any of its siblings (symbol_continuity, survival_competence, boundary_maintenance), or do all four readings coexist as live interpretations held by different practitioners?',
    'Textual and institutional analysis: Can custodians holding the trauma_encoding reading point to passages or practices that explicitly contradict (foreclose) the symbol_continuity reading? Or do practitioners fluidly adopt multiple readings simultaneously? Ethnographic evidence from communities where different sub-groups adopt different readings of the same kernel ritual.',
    'If readings foreclose each other, the kernel resolves into distinct constraints (each reading is incompatible). If readings coexist, they should all be authored as sibling constraints linked via network.affects_constraints, with reading_relations marked coexists_with. If one reading influences others'' legitimacy without foreclosing, use influences. This affects the kernel family''s structure and whether the readings compete (mutually exclusive adoption) or coexist (different seats adopt different readings of the same practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Logical relationship between this reading and its siblings in the kernel contest.').

omega_variable(
    diaspora_vs_homeland_mandate_divergence,
    'Does the founding problem (transmitting threat-detection across generational discontinuity) remain the same for diaspora practitioners as for communities in the homeland or high-threat contexts?',
    'Comparative study: do diaspora communities reinterpret or lighten trauma-focused ritual content, and if so, do custodians object or accept reinterpretation? Does the founding_problem_status split into homeland=live vs. diaspora=dead/contested? Evidence: custodian speeches, ritual observance data, mental-health outcomes by geography.',
    'If the founding problem is genuinely dead in diaspora contexts (threat is remote, persecution unlikely), the constraint''s extracted cost for negligible benefit suggests piton or degraded snare classification for diaspora seats. If the founding problem remains live for custodians even in diaspora (threat could return, vigilance must persist), the classification diverges: custodian seats compute as tangled_rope (genuine coordination), diaspora payer seats compute as snare (extraction without functional benefit). This is a per-seat divergence driven by threat-environment, not a claim divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_vs_homeland_mandate_divergence, empirical, 'Whether the constraint''s founding mandate has died in diaspora contexts while persisting in custodian framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t14, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 14, 0.21).
narrative_ontology:measurement(cata_tr_t28, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 28, 0.26).
narrative_ontology:measurement(cata_tr_t42, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 42, 0.32).
narrative_ontology:measurement(cata_tr_t56, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 56, 0.37).
narrative_ontology:measurement(cata_tr_t70, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 70, 0.41).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t14, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(cata_be_t28, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 28, 0.56).
narrative_ontology:measurement(cata_be_t42, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 42, 0.62).
narrative_ontology:measurement(cata_be_t56, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 56, 0.66).
narrative_ontology:measurement(cata_be_t70, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t14, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 14, 0.41).
narrative_ontology:measurement(cata_su_t28, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(cata_su_t42, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 42, 0.48).
narrative_ontology:measurement(cata_su_t56, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 56, 0.51).
narrative_ontology:measurement(cata_su_t70, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 70, 0.52).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_memory_kernel, which represents intergenerational commemoration practices contested across multiple institutional and community interpretations. The trauma_encoding_reading emphasizes the mechanism of trauma transmission for threat-detection; sibling readings emphasize boundary maintenance, survival-competence transmission decoupled from trauma focus, or symbolic continuity. All four readings share the same kernel practice but carry different ε values and victim/beneficiary assignments. The constraint family models a single contested religious practice read four ways. Decomposition follows the ε-invariance principle: each reading has a distinct referent (what function does the ritual serve?), distinct beneficiary/victim structure (who bears costs for what benefit?), and distinct extractiveness profile. The family is linked via network.affects_constraints and should be cross-indexed in the kernel_context prose of all sibling stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__trauma_encoding_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
