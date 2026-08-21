% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological determinism'
 *   reading of the 'press_reformation_causation' kernel. From this
 *   perspective, the printing press is viewed as an autonomous, mountain-like
 *   force whose inherent properties (rapid, cheap reproduction) inevitably
 *   led to the collapse of traditional censorship and the rise of vernacular
 *   scripture, thereby causing the Protestant Reformation. The Catholic
 *   Church's attempts to resist these changes are seen as futile,
 *   increasingly performative, and ultimately overwhelmed by the technology's
 *   structural power. Reformers and vernacular readers are direct
 *   beneficiaries of this exogenous technological capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.85).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.9).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.85).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '85024913-0234-415f-80b1-922c7d7a97ce').
narrative_ontology:cs_kernel_codification('85024913-0234-415f-80b1-922c7d7a97ce', formalized).
narrative_ontology:cs_authority_grounding('85024913-0234-415f-80b1-922c7d7a97ce', self_enforcing).
narrative_ontology:cs_reading_relation('85024913-0234-415f-80b1-922c7d7a97ce', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('85024913-0234-415f-80b1-922c7d7a97ce', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('85024913-0234-415f-80b1-922c7d7a97ce', foundational, technology_as_exogenous_force).
narrative_ontology:cs_axiom_status(technology_as_exogenous_force, holdable).
narrative_ontology:cs_axiom_grounding('85024913-0234-415f-80b1-922c7d7a97ce', technology_as_exogenous_force, empirically_contingent).
narrative_ontology:cs_axiom('85024913-0234-415f-80b1-922c7d7a97ce', foundational, censorship_futile_against_print).
narrative_ontology:cs_axiom_status(censorship_futile_against_print, holdable).
narrative_ontology:cs_axiom_grounding('85024913-0234-415f-80b1-922c7d7a97ce', censorship_futile_against_print, empirically_contingent).
narrative_ontology:cs_reference_frame('85024913-0234-415f-80b1-922c7d7a97ce', pre_print_information_control).
narrative_ontology:cs_drift_state('85024913-0234-415f-80b1-922c7d7a97ce', post_gutenberg_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('85024913-0234-415f-80b1-922c7d7a97ce', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, traditional_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The dominant religious institution prior to the Reformation, whose authority rested on control over scripture and interpretation. The printing press undermined this control, making their traditional methods of censorship and doctrinal dissemination ineffective. They bore the cost of losing their information monopoly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, generational, trapped, global).

% Religious leaders and movements who advocated for reforms, particularly the translation of scripture into vernacular languages and direct access to religious texts. The printing press provided an unprecedented means to disseminate their ideas and texts, benefiting from the technology's inherent capacity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, biographical, arbitrage, regional).

% Individuals who gained access to printed materials, especially Bibles and theological tracts, in their native languages. This direct access bypassed traditional clerical mediation, empowering them with personal interpretation and fostering new religious communities.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Local priests and religious figures whose authority and role were diminished by the widespread availability of printed vernacular scripture. Their traditional function as sole interpreters and custodians of religious knowledge became increasingly redundant or challenged.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, traditional_clergy, payer,
    moderate, biographical, identity_locked, local).

% Monarchs and princes who often benefited from the weakening of the Catholic Church's centralized authority, gaining more control over religious affairs within their territories and sometimes seizing Church lands. The press facilitated the spread of ideas that challenged papal power.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, secular_rulers, beneficiary,
    institutional, generational, arbitrage, national).

% The technological artifact itself, viewed as an autonomous force whose inherent properties (speed, reproducibility, cost-effectiveness) inevitably led to the outcomes described, regardless of human intent or resistance.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread, and standardized dissemination of information, particularly text, across geographical and social boundaries, enabling a new form of collective understanding and action.
% TRANSFER_FUNCTION: Transferred control over information production and distribution from centralized, elite institutions (like the Catholic Church) to a more decentralized network of printers, authors, and readers, effectively democratizing access to knowledge.
% ABSENT_VOICES: Those who would have preferred a slower, more controlled evolution of religious thought and social structures, or those who benefited from the pre-print information hierarchy, were effectively silenced by the overwhelming and inevitable force of the new technology.
% DISAPPEARANCE_RATIONALE: If the printing press had never been invented or had vanished, the Reformation as we know it would not have occurred. The rapid spread of new theological ideas, vernacular Bibles, and polemical tracts was entirely dependent on print technology. The entire course of European religious, political, and social history would have been fundamentally different.
% FOUNDING_PROBLEM: The Catholic Church's problem was maintaining its exclusive authority over religious doctrine and practice, which relied on controlling the production and dissemination of texts and interpretations.
% FOUNDING_PROBLEM_CORROBORATION: Historians of media, technology, and the Reformation, as well as scholars of religious studies, widely corroborate that the printing press fundamentally altered the landscape of information control, rendering the Church's original problem of maintaining an exclusive information monopoly effectively 'dead' in its original form.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the degree to which the printing press 'extracted' the Catholic Church's monopoly on information and authority. Suppression (0.90) is high because the technology itself suppressed alternatives to widespread dissemination, making traditional control mechanisms ineffective. The theater ratio (0.70) rises significantly as the Church's efforts at censorship became increasingly symbolic and less functional against the tide of print. Accessibility collapse (0.95) is near total for the old order's control mechanisms, while resistance (0.80) from the Church was strong but ultimately unsuccessful against the perceived inevitability of the press's effects. The claimed type is 'mountain' because this reading frames the technology's impact as an unchangeable, natural-law-like force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Catholic Church, the printing press was a disruptive force that imposed immense costs and threatened their institutional survival. From the perspective of the reformers, it was a providential tool that enabled the spread of truth. This story captures the deterministic reading, where the technology's inherent properties dictated these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The Catholic Church and traditional clergy are positioned as targets (payers/victims) because the press's effects directly undermined their authority and control. Protestant reformers, vernacular readers, and secular rulers are beneficiaries, as the press provided them with the means to challenge existing power structures and disseminate new ideas. The 'printing_press_technology' itself is an analytical observer, representing the deterministic force in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'Is the printing press truly an autonomous, mountain-like force whose effects were inevitable, or were its impacts mediated and shaped by human agency, strategic choices, and social contexts?',
    'Historical analysis focusing on specific instances of resistance, adaptation, and strategic deployment of print by various actors, and counterfactual histories exploring alternative outcomes.',
    'If agency and context are found to be highly influential, the ''mountain'' classification would be challenged, potentially reclassifying the constraint as a ''rope'' or ''tangled_rope'' where human coordination and extraction played a more active role.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Ambiguity between technological inevitability and human mediation in the press''s impact.').

omega_variable(
    false_summit_of_technological_mountain,
    'Is the claim that the printing press ''naturally'' caused the Reformation a genuine reflection of technological inevitability, or a narrative constructed to legitimize the outcomes and obscure the active roles of beneficiaries?',
    'Examination of historical narratives and their ideological functions: who promoted the deterministic view, and what interests did it serve? Analysis of the ''naturalness'' claim against evidence of active suppression of alternatives.',
    'If the narrative is found to be a constructed legitimation, the constraint would be reclassified from ''mountain'' to a ''tangled_rope'' or ''snare'', reflecting the active extraction and coordination involved in shaping the historical outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_technological_mountain, conceptual, 'Whether the ''mountain'' claim for the press''s effects is a false summit.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of the Catholic Church''s information monopoly purely structural (the inherent properties of print), or did it also involve internalized acceptance of the new media landscape by some actors, or a failure of imagination regarding alternative control mechanisms?',
    'Detailed historical sociological analysis of how different segments of society (clergy, intellectuals, rulers) adapted to or resisted print, and the cognitive shifts involved in accepting new forms of information authority.',
    'If internalized or cognitive factors played a significant role, the effective suppression might be higher than a purely structural measure suggests, as the ''target'' (old order) might have contributed to its own decline through cognitive lock-in or strategic miscalculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the face of print technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.15).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.3).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__technological_determinism, theater_ratio, 1510, 0.5).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__technological_determinism, theater_ratio, 1530, 0.65).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.7).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.3).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.55).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__technological_determinism, base_extractiveness, 1510, 0.7).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__technological_determinism, base_extractiveness, 1530, 0.8).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causation__technological_determinism, suppression_requirement, 1470, 0.25).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causation__technological_determinism, suppression_requirement, 1490, 0.45).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causation__technological_determinism, suppression_requirement, 1510, 0.65).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__technological_determinism, suppression_requirement, 1530, 0.8).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'press_reformation_causation' kernel. This 'technological_determinism' reading emphasizes the inevitable, mountain-like impact of the printing press, contrasting with 'strategic_deployment' (agent-driven use) and 'mutual_shaping' (co-evolution of technology and society).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
