% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Breakthrough (Theological Climb Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'theological climb' reading of the
 *   Reformation, which posits that Luther's rediscovery of justification by
 *   faith alone was a genuine doctrinal breakthrough. This theological
 *   innovation, rather than political or economic factors, was the primary
 *   driver for the institutional separation from the Catholic Church. From
 *   this reading's perspective, the new theological framework offered
 *   liberation and true coordination for believers, even as it necessitated a
 *   disruptive break from the existing religious order.
 *
 * KEY AGENTS:
 *   - Martin Luther: Agenda setter (initiated the theological challenge)
 *   - Believers Justified by Faith: Primary beneficiary (spiritual liberation)
 *   - Protestant Princes: Secondary beneficiary (political/economic autonomy)
 *   - Catholic Church Hierarchy: Primary payer/victim (loss of authority/wealth)
 *   - Papal Authority: Primary payer/victim (challenge to universal claims)
 *   - Secular Rulers Loyal to Rome: Payer (cost of defending old order)
 *   - Theologians of Scholasticism: Excluded (their frameworks challenged)
 *   - Historical Epistemologists: Observer (analytical perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.45).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.55).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Breakthrough (Theological Climb Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '2f4a3e85-dc60-4338-b3d3-eb0bc6193d23').
narrative_ontology:cs_kernel_codification('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', fixed_text).
narrative_ontology:cs_authority_grounding('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', expertise).
narrative_ontology:cs_interpretation_layer_present('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23').
narrative_ontology:cs_reading_relation('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', foundational, sola_fide_centrality).
narrative_ontology:cs_axiom_status(sola_fide_centrality, holdable).
narrative_ontology:cs_axiom_grounding('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', sola_fide_centrality, theological).
narrative_ontology:cs_axiom('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', foundational, scripture_as_sole_authority).
narrative_ontology:cs_axiom_status(scripture_as_sole_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', scripture_as_sole_authority, theological).
narrative_ontology:cs_reference_frame('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', scriptural_truth_revelation).
narrative_ontology:cs_drift_state('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f4a3e85-dc60-4338-b3d3-eb0bc6193d23', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_justified_by_faith).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_princes).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, papal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, secular_rulers_loyal_to_rome).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiated the theological challenge, articulated the core doctrines, and became the central figure in the movement. Faced excommunication and imperial ban, but gained protection from sympathetic princes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther, agenda_setter,
    powerful, biographical, constrained, regional).

% Embraced the new doctrine, finding spiritual liberation and a direct relationship with God, free from perceived intermediaries and burdensome rituals. Formed new communities of worship.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_justified_by_faith, beneficiary,
    moderate, biographical, mobile, local).

% Gained significant political and economic autonomy by breaking from papal authority, seizing church lands, and establishing state-controlled churches. Benefited from the theological justification for their actions.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_princes, beneficiary,
    institutional, generational, arbitrage, regional).

% Lost significant spiritual authority, political influence, and material wealth (tithes, land) in regions that adopted Protestantism. Faced internal schism and external military challenge.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Suffered a profound challenge to its universal spiritual and temporal claims, leading to a permanent division of Christendom and a reduction in its effective power and revenue streams.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Were compelled to defend the existing Catholic order, often at significant political and military cost, against the spread of Protestantism within their territories and among their peers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers_loyal_to_rome, payer,
    powerful, biographical, constrained, regional).

% Their established theological frameworks and interpretive methods were directly challenged and often rejected by the new Protestant doctrines. Many found their intellectual authority undermined and their careers threatened.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, theologians_of_scholasticism, excluded,
    powerful, biographical, identity_locked, continental).

% Analyze the historical evidence and interpretive frameworks to understand the causes and consequences of the Reformation, including the role of theological innovation versus other factors.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_epistemologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a new framework for individual salvation and communal worship, coordinating believers around a direct relationship with God and scriptural authority, thereby solving perceived spiritual anxieties of the late medieval period.
% TRANSFER_FUNCTION: Transfers spiritual authority from the institutional Catholic Church to individual conscience and scripture; transfers political legitimacy and material resources (church lands, tithes) from papal authority to secular rulers who embrace the new doctrine.
% ABSENT_VOICES: The voices of those who maintained the pre-Reformation theological consensus (e.g., scholastic theologians, defenders of papal supremacy) were actively suppressed or marginalized within the emerging Protestant spheres. Their arguments for tradition and institutional authority were dismissed as unscriptural or corrupt.
% DISAPPEARANCE_RATIONALE: If Luther's theological breakthrough and the subsequent institutional separation vanished, the entire trajectory of Western history, religious practice, political organization, and the development of modern nation-states would be fundamentally different. The religious landscape of Europe would be unrecognizable.
% FOUNDING_PROBLEM: The perceived corruption and theological errors of the late medieval Catholic Church, particularly the sale of indulgences, the perceived lack of assurance of salvation, and the perceived distance between God and the common believer.
% FOUNDING_PROBLEM_CORROBORATION: Protestant theologians and historians attest to the problem's live status, arguing that the theological truths rediscovered by Luther remain eternally relevant. Catholic historians acknowledge the historical context of corruption but dispute the theological necessity of separation, offering alternative reforms. The corroboration for the 'live' status of the *theological* problem comes from the ongoing adherence to these doctrines by millions of believers.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).
:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it simultaneously provides a genuine coordination function (spiritual liberation and a new framework for believers) and involves asymmetric extraction (from the Catholic Church hierarchy and papal authority). The base extractiveness (0.45) reflects this hybrid nature: low for adherents, but significant for the old order. Suppression (0.55) is moderate, as the new theological order actively defended itself and enforced its own doctrines, leading to conflict. Theater ratio is low (0.15) as the core function is genuine theological conviction, not mere performance. Resistance is high (0.7) due to the intense opposition from the established Catholic powers. The temporal measurements show an initial period of lower extractiveness and suppression (as the new ideas were liberating), followed by an increase as the new institutions formed and actively enforced their separation and doctrines.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'believers_justified_by_faith' and 'protestant_princes', the constraint operates as a liberating Rope, offering genuine coordination and reducing prior extraction. From the perspective of the 'catholic_church_hierarchy' and 'papal_authority', it is a Snare, actively extracting authority and resources through coercion. The engine's computation of per-seat classification will reflect this divergence based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   For 'believers_justified_by_faith', the constraint is highly beneficial (d near 0.0) as it offers spiritual liberation. For 'protestant_princes', it's also beneficial (d near 0.0-0.1) due to increased autonomy and resources. Conversely, 'catholic_church_hierarchy' and 'papal_authority' are full targets (d near 1.0) as they bear the brunt of the institutional separation and loss of authority. 'Secular_rulers_loyal_to_rome' are also targets (d near 0.8) due to the costs of defending the old order. 'Martin_luther' as the agenda-setter sits near the beneficiary end (d near 0.15) as he drives the new framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Reformation as purely extractive (Snare) or purely coordinative (Rope). By identifying it as a Tangled Rope, the framework acknowledges both the genuine theological breakthrough and coordination function for its adherents, as well as the significant, enforced extraction from the established Catholic order. The 'founding_problem_status' being 'live' from this reading's perspective indicates that the theological mandate is still considered valid, preventing a Piton classification based on obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformation_kernel_reading_identity,
    'Is this constraint accurately identified as the ''theological_climb_reading'' of the ''reformation_event_boundary'' kernel?',
    'Comparative analysis with other historical readings and their structural implications. If this reading''s core claims are found to be structurally indistinguishable from another, the kernel decomposition is flawed.',
    'If misidentified, the classification of this constraint and its relations to other readings would be inaccurate, potentially leading to incorrect conclusions about the nature of historical change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_kernel_reading_identity, conceptual, 'Confirms this constraint is one specific reading of the Reformation kernel.').

omega_variable(
    theological_vs_political_causation,
    'To what extent was Luther''s theological breakthrough the primary cause of the Reformation, versus political and economic factors?',
    'Further historical research and counterfactual analysis, examining the relative weight of theological arguments, princely ambitions, and economic grievances in driving institutional change.',
    'If political/economic factors are found to be dominant, this reading''s extractiveness and suppression metrics might be re-evaluated as higher (more Snare-like), and its coordination function as more of a cover story, shifting its classification towards a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_causation, empirical, 'Assesses the causal primacy of theological innovation.').

omega_variable(
    legitimacy_of_institutional_separation,
    'Was the institutional separation from the Catholic Church a necessary consequence of the theological breakthrough, or an avoidable outcome driven by other factors?',
    'Theological and historical analysis of alternative reform movements (e.g., Catholic Reformation) and their ability to address similar theological concerns without schism.',
    'If separation was not strictly necessary, the ''requires_active_enforcement'' and ''suppression'' metrics might be seen as higher than justified by the theological claims alone, potentially increasing the perceived extractiveness of the new Protestant institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_institutional_separation, conceptual, 'Examines the necessity of institutional schism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.12).
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.15).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.18).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.15).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.4).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.45).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.48).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.45).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.55).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.6).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_event_boundary' kernel. The other readings are 'political_swap_reading' and 'composite_overdetermination_reading', each offering a distinct structural interpretation of the Reformation's primary drivers and consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
