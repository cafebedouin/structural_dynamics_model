% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Power-Maintained Bundle
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-Buddha amalgamation) operated in Japan from roughly
 *   the 8th to the 19th century. This reading — the incoherent_bundle_reading
 *   — argues the system never achieved theological coherence. Instead,
 *   Buddhist institutions, shrine establishments, and the state maintained
 *   deliberate ambiguity (honji suijaku, ryōbu shintō, etc.) as a power
 *   strategy: it allowed resource extraction from a captive populace,
 *   suppressed doctrinal challengers, and gave the state a flexible
 *   legitimating idiom. The Meiji shinbutsu bunri (separation) did not
 *   destroy a living synthesis; it exposed a hollow core. The constraint was
 *   a snare: coordination was the cover story; extraction and suppression
 *   were the operating logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.78).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.82).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Power-Maintained Bundle").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'cd458af6-b8d7-4456-863f-cc86fd40ae27').
narrative_ontology:cs_kernel_codification('cd458af6-b8d7-4456-863f-cc86fd40ae27', implicit).
narrative_ontology:cs_authority_grounding('cd458af6-b8d7-4456-863f-cc86fd40ae27', extraction).
narrative_ontology:cs_interpretation_layer_present('cd458af6-b8d7-4456-863f-cc86fd40ae27').
narrative_ontology:cs_reading_relation('cd458af6-b8d7-4456-863f-cc86fd40ae27', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('cd458af6-b8d7-4456-863f-cc86fd40ae27', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('cd458af6-b8d7-4456-863f-cc86fd40ae27', foundational, ontological_incoherence_as_structural_feature).
narrative_ontology:cs_axiom_status(ontological_incoherence_as_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('cd458af6-b8d7-4456-863f-cc86fd40ae27', ontological_incoherence_as_structural_feature, empirically_contingent).
narrative_ontology:cs_axiom('cd458af6-b8d7-4456-863f-cc86fd40ae27', secondary, meiji_bunri_as_revelation_not_creation).
narrative_ontology:cs_axiom_status(meiji_bunri_as_revelation_not_creation, holdable).
narrative_ontology:cs_axiom_grounding('cd458af6-b8d7-4456-863f-cc86fd40ae27', meiji_bunri_as_revelation_not_creation, empirically_contingent).
narrative_ontology:cs_reference_frame('cd458af6-b8d7-4456-863f-cc86fd40ae27', incoherent_power_maintained_ambiguity).
narrative_ontology:cs_drift_state('cd458af6-b8d7-4456-863f-cc86fd40ae27', meiji_restoration_1868, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('cd458af6-b8d7-4456-863f-cc86fd40ae27', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, imperial_court_state).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, common_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, nativist_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples managed shrine-temple complexes (jingū-ji), received state patronage and land grants, and avoided pure doctrinal scrutiny by maintaining syncretic ambiguity. The honji suijaku framework gave them interpretive authority over kami as 'traces' of Buddhist truth. Their institutional survival depended on the fused system; exit meant losing the protective ambiguity and facing doctrinal accountability.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutions, beneficiary,
    institutional, generational, constrained, national).

% Shrines were often subordinated to temples in jingū-ji complexes but gained ritual legitimacy, Buddhist institutional infrastructure, and state recognition through association. Shrine priests (shinshoku) performed Buddhist rites and relied on temple networks for education and administration. Exit would mean losing the material and organizational substrate that sustained shrine operations for centuries.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_institutions, beneficiary,
    institutional, generational, constrained, national).

% The court and later shogunates used the fused system to integrate local cults, legitimize rule through divine ancestry narratives, and manage religious institutions through the jisha-bugyō (temple-shrine magistrate) system. The ambiguity allowed flexible political deployment of religious symbols. The state could shift between Buddhist and Shinto framings as needed. Exit was always an option for the state — it ultimately exercised it in 1868 — but the system served its interests until then.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, imperial_court_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, imperial_court_state, beneficiary).

% Ordinary people funded both temple and shrine rites through donations, labor, and parishioner obligations (danka system). They navigated contradictory doctrines — Buddhist karma and Shinto purity — with no coherent theological framework. Alternatives (pure Buddhism, pure Shinto, Christianity) were suppressed or inaccessible. Exit meant social ostracism, loss of funeral rites, or persecution.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, common_practitioners, payer,
    powerless, biographical, trapped, local).

% Scholars like Hirata Atsutane, Motoori Norinaga, and the Kokugaku school argued for kami supremacy, Buddhist exclusion, and restoration of 'ancient' Shinto. They were censored, persecuted, or forced into coded discourse throughout the Edo period. Their exclusion was structural — the fused system's ambiguity could not accommodate a clear doctrinal challenger. They gained voice only after the Meiji collapse.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, nativist_reformers, excluded,
    moderate, biographical, constrained, national).

% Historians and religious studies scholars analyze the system from outside the constraint. The field is divided between syncretic_fusion_reading (honji suijaku as genuine theology), domain_partition_reading (separate spheres), and this incoherent_bundle_reading (ambiguity as power strategy). No scholar is subject to the constraint; all occupy the analytical seat.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a minimally stable religious-political order across a fragmented archipelago: local cults could be incorporated without doctrinal resolution, state authority could mediate competing institutions, and social cohesion was maintained through shared ritual calendars rather than shared belief.
% TRANSFER_FUNCTION: Moved material resources (land, labor, donations) and interpretive authority from common practitioners and excluded reformers to Buddhist temples, shrine establishments, and the state apparatus that managed them. The ambiguity functioned as a rent-extraction mechanism: institutions collected support for services whose doctrinal basis was deliberately left undefined.
% ABSENT_VOICES: Common practitioners had no organized voice; their doctrinal confusion was not represented in the institutional record. Nativist reformers (Kokugaku scholars) were structurally excluded — their clear alternative (kami-only ontology) threatened the ambiguity the system depended on. Would-be Christian converts were violently suppressed (Kirishitan persecution) as the ultimate exit from the fused system.
% DISAPPEARANCE_RATIONALE: When the Meiji state imposed shinbutsu bunri (separation) in 1868, the entire institutional landscape reorganized: temples lost shrine complexes, shrine priests were laicized or reconstituted as state functionaries, the danka parishioner system collapsed, and practitioners were forced into exclusive affiliation. The rearrangement was violent (haibutsu kishaku — 'abolish Buddhism, destroy Shakyamuni') and incomplete — residue persists in folk practice — but the constraint's disappearance fundamentally restructured Japanese religious life.
% FOUNDING_PROBLEM: The Yamato court needed to integrate diverse local cults (kami) into a centralized polity without triggering resistance from powerful clan deities or imported Buddhist institutions. The 'solution' was not a theological synthesis but a deliberate non-decision: allow coexistence without resolution, letting ambiguity serve as political glue.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (political integration of fragmented cults) is attested by early chronicles (Kojiki, Nihon Shoki) and the institutional record of the Jingikan (Department of Divinities) — sources outside the Buddhist institutional beneficiaries. The problem's death is corroborated by the Meiji state's own declaration that the 'ancient unity of ritual and government' had been restored, i.e., the integration problem was solved by fiat, not by the fused system. No beneficiary institution claims the original integration problem persists; they claim the system itself was the solution.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because institutions collected resources (land, labor, donations) for doctrinally undefined services. Suppression (0.82) is high because categorical alternatives (pure Buddhism, pure Shinto, Christianity) were actively marginalized — not by persuasion but by institutional control of registration (danka), censorship, and violence. Theater ratio (0.71) is high because the elaborate syncretic theologies (honji suijaku, etc.) were performative: they produced no stable ontology but served to legitimate the arrangement. Accessibility collapse (0.76) reflects that once the ambiguity is seen as structural, the alternatives (clear doctrinal positions) appear retrospectively obvious but were historically inaccessible. Resistance (0.45) is moderate: nativist reformers existed but were contained until the state itself switched sides.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, the system appears as genuine coordination (honji suijaku as profound theology). From the common practitioner seat, it appears as opaque extraction (paying for contradictory rites). From the nativist reformer seat, it appears as active suppression (their clear alternative silenced). From the state seat, it appears as a tool (useful until 1868). The engine computes these divergences from the declared power/exit/role data; the claimed_type (snare) is this reading's structural judgment, not a forced consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist and Shinto institutions are structural beneficiaries (d ~ 0.15-0.25): they collected rents, controlled interpretation, and had constrained but real exit options (reform movements within their traditions). The imperial court/state is the agenda_setter with arbitrage-grade exit (d ~ 0.10): it designed the ambiguity and ultimately dismantled it. Common practitioners are full targets (d ~ 0.90): trapped, no alternatives, bearing all costs. Nativist reformers are excluded targets (d ~ 0.85): they saw the structure clearly but were suppressed. Modern scholars are analytical observers (d = 0.50 symmetric). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (political integration of fragmented cults) died when the Meiji state achieved centralized control by other means (constitution, bureaucracy, nationalism). The arrangement persisted 200+ years past its founding problem's death — classic mandatrophy. The ambiguity was not a bug but the feature that allowed persistence: by never resolving the ontology, the system never had to justify its resource flows. The Meiji collapse was not exogenous shock but endogenous revelation: the state, which had been the primary enforcer, withdrew enforcement because the constraint no longer served its interests. The snare classification captures this: coordination was always the cover; extraction was the function; when the enforcer switched sides, the cover collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of doctrinal alternatives primarily structural (state censorship, institutional control of registration) or internalized (practitioners genuinely unable to conceive alternatives due to lifelong immersion in ambiguity)?',
    'Post-Meiji trajectory: if doctrinal confusion persisted among common practitioners after 1868 despite state-mandated separation, internalized suppression was significant. If practitioners rapidly adopted exclusive affiliation, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than institutional measures suggest — the ambiguity colonized the cognitive space of the targets. This would increase χ for the practitioner seat and strengthen the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the shinbutsu system').

omega_variable(
    coordination_extraction_boundary,
    'Did the fused system provide ANY genuine coordination function (shared ritual calendar, disaster response, social welfare) that was not purely extractive cover, or was every coordination claim a rationalization for resource extraction?',
    'Comparative analysis: did communities with stronger shinbutsu integration show better outcomes (famine survival, conflict resolution) than those with weaker integration, controlling for state capacity? If yes, some coordination was real; if no, pure extraction.',
    'If genuine coordination existed, the constraint is tangled_rope (hybrid). If zero genuine coordination, pure snare. This reading claims the latter but the boundary is empirically contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether any non-extractive coordination function existed in shinbutsu-shugo').

omega_variable(
    meiji_collapse_causality,
    'Did the Meiji state''s shinbutsu bunri policy CREATE the incoherence (by forcibly separating what had been a functional synthesis) or REVEAL pre-existing incoherence (by removing the enforcement that maintained the ambiguity)?',
    'Counterfactual: if the Meiji state had not intervened, would the system have continued indefinitely, or was it already fragmenting (declining temple revenues, rising nativist sentiment, practitioner indifference)? The latter supports revelation; the former supports creation.',
    'If revelation, this reading''s core claim (ambiguity as structural feature) is vindicated and the snare classification holds. If creation, the system may have been a degraded rope (piton) and the Meiji policy was the extraction event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_collapse_causality, conceptual, 'Whether Meiji bunri revealed or created the system''s incoherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 40, 0.59).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 60, 0.68).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 80, 0.73).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_tr_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 80, 0.79).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_be_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(shinbutsu_incoherent_bundle_su_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_shinbutsu_bunri_policy).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, state_shinto_formation).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, japanese_religious_freedom_1889).

% DUAL FORMULATION NOTE:
% Part of the shinbutsu_coexistence_commitment kernel family. This reading (incoherent_bundle) claims no stable ontology; syncretic_fusion_reading claims honji suijaku unification; domain_partition_reading claims separate spheres. The three readings have different ε values (this: 0.78 high extraction; syncretic_fusion: lower ε if genuine theology; domain_partition: moderate ε if stable partition). Linked via affects_constraints to downstream Meiji constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, powerless, 0.9).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, moderate, 0.85).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
