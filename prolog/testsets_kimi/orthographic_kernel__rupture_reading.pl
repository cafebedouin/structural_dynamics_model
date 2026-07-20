% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Turkish Script Reform as Deliberate Cultural Rupture
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   The 1928 Turkish script reform replaced the Ottoman Turkish alphabet
 *   (Arabic script) with a Latin-based script. The RUPTURE READING interprets
 *   this not as incidental modernization but as a deliberate act of cultural
 *   severance: the post-reform state apparatus used orthographic change to
 *   extract cultural authority from the Ottoman/Islamic literate class and to
 *   manufacture a ruptured, post-imperial national identity. The old literate
 *   populationâreligious scholars, poets, bureaucratsâwere rendered
 *   structurally illiterate in the new order, their textual capital
 *   annihilated. The constraint persists through active enforcement in
 *   education, law, and publishing, and through the stigmatization of the
 *   Ottoman past.
 *
 * KEY AGENTS:
 *   - post_reform_state_apparatus: Agenda-setter and primary beneficiary (institutional/analytical) â administers the script and derives legitimacy from the rupture narrative.
 *   - pre_reform_literate_population: Primary payer (moderate/identity_locked) â bears the cost of cultural dispossession as their script is banned and their textual capital devalued.
 *   - republican_citizenry: Coordinated beneficiary (organized/constrained) â receives a unified national identity at the cost of severed textual heritage.
 *   - ottoman_religious_scholars: Excluded voice (moderate/trapped) â would object on theological grounds, structurally silenced by the reform process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.88).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.78).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Turkish Script Reform as Deliberate Cultural Rupture").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, 'd638d8e2-f093-49c4-a68a-db6a9fa4e1e9').
narrative_ontology:cs_kernel_codification('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', formalized).
narrative_ontology:cs_authority_grounding('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', lineage).
narrative_ontology:cs_interpretation_layer_present('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9').
narrative_ontology:cs_reading_relation('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', foundational, script_as_sovereign_instrument).
narrative_ontology:cs_axiom_status(script_as_sovereign_instrument, holdable).
narrative_ontology:cs_axiom_grounding('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', script_as_sovereign_instrument, conventional).
narrative_ontology:cs_axiom('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', foundational, ottoman_past_is_legitimacy_threat).
narrative_ontology:cs_axiom_status(ottoman_past_is_legitimacy_threat, holdable).
narrative_ontology:cs_axiom_grounding('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', ottoman_past_is_legitimacy_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', republican_nation_state_sovereignty).
narrative_ontology:cs_drift_state('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', contemporary_turkey, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d638d8e2-f093-49c4-a68a-db6a9fa4e1e9', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, republican_citizenry).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets, administers, and enforces the new Latin script through national education curricula, publishing law, and state media. Derives consolidated legitimacy and sovereign authority from the claim of having ruptured the Ottoman/Islamic past and founded a modern nation. Could theoretically amend the alphabet but is structurally incentivized to maintain the rupture narrative as a foundation of its own continuity.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Comprises religious scholars, poets, bureaucrats, and merchants whose literacy and social authority were vested in the Arabic script. The 1928 reform rendered their textual capital obsolete overnight; they were excluded from the new educational and publishing order unless they relearned from scratch. Their cultural self-concept is fused with the abandoned script, making exit equivalent to self-erasure.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    moderate, biographical, identity_locked, national).

% Receives a standardized national script and the symbolic materials of a post-Ottoman identity. Participates in literacy and nation-building campaigns. Bears the cost of lost access to pre-republican textual heritage and the constrained choice to adopt the state-defined identity; departure from the new script is socially and economically penalized.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, republican_citizenry, beneficiary,
    organized, generational, constrained, national).

% Would object on theological and legal grounds, asserting that Arabic script is inseparable from Islamic textual authority. Were structurally excluded from the reform commission and subsequently marginalized from the new state's educational, juridical, and publishing institutions. Their objections were dismissed as reactionary.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_religious_scholars, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified national literate public under a single state-controlled script, replacing imperial particularism with a territorially-defined Republican identity.
% TRANSFER_FUNCTION: Moves cultural capital, textual authority, and symbolic legitimacy from the Ottoman/Islamic literate class to the post-reform state apparatus and its Republican citizenry.
% ABSENT_VOICES: Ottoman religious scholars and the pre-reform literate elderly were excluded from the reform conversation; they would argue that the script is inseparable from Islamic law and Ottoman cultural memory. Their absence from the decision chamber was structural, not accidental.
% DISAPPEARANCE_RATIONALE: If the Latin script enforcement and its rupture narrative vanished overnight, the state's claim to a unique post-Ottoman founding would lose its primary symbolic infrastructure; the old literate class would regain cultural standing, publishing and education would face a generational divide, and the citizenry would confront an unresolved continuity with the imperial past.
% FOUNDING_PROBLEM: The collapse of the Ottoman Empire and the imperative to consolidate a territorially-defined Turkish nation-state distinct from its multi-ethnic, Islamic-caliphate predecessor.
% FOUNDING_PROBLEM_CORROBORATION: Nationalist historians and the state apparatus attest the problem remains live (defense against reaction). Ottomanist historians, marginalized religious communities, and several independent historians of modern Turkey attest the founding crisis is resolved and the arrangement now serves state authority maintenance; these external corroborations support the shifted-function reading.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint deliberately severs an entire population from its textual heritage and cultural memory. Suppression is high (0.78) because the old script was actively banned in education, press, and law; the measurement series shows enforcement intensifying in the early republic and then slowly declining as normalization set in. Accessibility collapse is high (0.82): within one generation, the Arabic script became practically inaccessible in official and educational contexts. Theater ratio rises above the 0.5 threshold (0.52) because the functional need for rupture has faded while the state continues to perform the narrative of radical modernity. Resistance (0.58) reflects significant but ultimately overcome opposition from religious scholars and old elites during the 1928â1930s transition.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat, the constraint is nation-building coordinationâcreating a unified, literate, modern citizenry out of imperial fragments. From the pre-reform literate seat, the same constraint is violent cultural extractionârendering centuries of textual capital obsolete and stigmatizing its possessors as backward. The engine computes this divergence from the same structural data: the state is a beneficiary with analytical exit (it could in principle change the script), while the old literate are identity-locked targets whose cultural self-concept is fused to the banned script.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-reform state apparatus sits at the beneficiary end: it subsidizes its own authority and legitimacy through the constraint (low d). The pre-reform literate population sits at the target end: the constraint extracts their cultural capital, social standing, and political relevance (high d, amplified by identity_locked exit). The Republican citizenry sits near the middleâgaining a national script while losing access to the pastâthough their constrained exit tilts them slightly toward the target side. The religious scholars are excluded rather than directly coordinated; their trapped status gives them a high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe collapse of the Ottoman Empire and the need to consolidate a new nation-stateâwas substantially resolved by the mid-twentieth century. However, the constraint persists because it has become constitutive of the state's identity and authority. Without the rupture narrative, the foundational legitimacy of the republican order weakens. This prevents mislabeling the constraint as a Scaffold (it carries no sunset clause) or as a Rope (the extraction is asymmetric, identity-locked, and actively enforced). The persistence beyond the founding problem's resolution suggests accumulated extraction rather than transitional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the script change best understood as a deliberate rupture, a pragmatic modernization with incidental costs, or a continuity-preserving adaptation?',
    'Comparative analysis of the three kernel readings (rupture, modernization, continuity) against archival state deliberations and structural outcome data.',
    'If the rupture framing is underdetermined, classification may shift toward modernization_reading (lower epsilon, coordination-dominant) or continuity_reading (mountain-like persistence of textual tradition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Commitment-system under-determination: which reading captures the structural truth of the reform').

omega_variable(
    cultural_rupture_intentionality,
    'Was the cultural rupture an intended mechanism of state consolidation or an unavoidable side effect of modernization and mass literacy?',
    'Archival recovery of 1928 reform commission minutes and private correspondence among the Kemalist elite regarding the Arabic script''s symbolic load and the political utility of severing access to Ottoman texts.',
    'If rupture was intentional, the constraint remains extraction-dominant (snare/tangled-rope); if side effect, it may read as scaffold or rope with unintended costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_rupture_intentionality, empirical, 'Whether script reform was designed to sever cultural continuity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal ban on Arabic script, educational exclusion, archival restrictions) or internalized (intergenerational shame attached to Ottoman identity, self-censorship among descendants)?',
    'Post-exit suppression trajectory: observe whether Turkish diaspora communities and private religious schools free of state script enforcement still experience Arabic-script literacy as politically suspect or identity-threatening.',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates even where legal enforcement is absent, raising the true extraction and theater ratios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orth_tr_t19, orthographic_kernel__rupture_reading, theater_ratio, 19, 0.3).
narrative_ontology:measurement(orth_tr_t38, orthographic_kernel__rupture_reading, theater_ratio, 38, 0.4).
narrative_ontology:measurement(orth_tr_t57, orthographic_kernel__rupture_reading, theater_ratio, 57, 0.48).
narrative_ontology:measurement(orth_tr_t76, orthographic_kernel__rupture_reading, theater_ratio, 76, 0.52).
narrative_ontology:measurement(orth_tr_t95, orthographic_kernel__rupture_reading, theater_ratio, 95, 0.52).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(orth_be_t19, orthographic_kernel__rupture_reading, base_extractiveness, 19, 0.86).
narrative_ontology:measurement(orth_be_t38, orthographic_kernel__rupture_reading, base_extractiveness, 38, 0.88).
narrative_ontology:measurement(orth_be_t57, orthographic_kernel__rupture_reading, base_extractiveness, 57, 0.89).
narrative_ontology:measurement(orth_be_t76, orthographic_kernel__rupture_reading, base_extractiveness, 76, 0.88).
narrative_ontology:measurement(orth_be_t95, orthographic_kernel__rupture_reading, base_extractiveness, 95, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(orth_su_t19, orthographic_kernel__rupture_reading, suppression_requirement, 19, 0.9).
narrative_ontology:measurement(orth_su_t38, orthographic_kernel__rupture_reading, suppression_requirement, 38, 0.88).
narrative_ontology:measurement(orth_su_t57, orthographic_kernel__rupture_reading, suppression_requirement, 57, 0.84).
narrative_ontology:measurement(orth_su_t76, orthographic_kernel__rupture_reading, suppression_requirement, 76, 0.8).
narrative_ontology:measurement(orth_su_t95, orthographic_kernel__rupture_reading, suppression_requirement, 95, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_kernel. The continuity_reading and modernization_reading instantiate structurally distinct constraints from the same historical event. This reading isolates the deliberate rupture/extraction dimension; the siblings isolate preservation and modernization dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
