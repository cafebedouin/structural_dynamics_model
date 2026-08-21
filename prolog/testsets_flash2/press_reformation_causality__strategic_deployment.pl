% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press in the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story analyzes the strategic deployment of the printing
 *   press during the Reformation as a Tangled Rope. Reformers and printers
 *   actively leveraged the technology to achieve their religious and economic
 *   goals, while the Catholic Church and traditional clergy bore the costs of
 *   this disruption. The constraint is framed as a tool that was consciously
 *   'weaponized' by agents, rather than an autonomous force. This is one
 *   reading of the 'press_reformation_causality' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.65).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.7).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of the Printing Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'a0905c30-b21f-45a3-8654-5f8e69958042').
narrative_ontology:cs_kernel_codification('a0905c30-b21f-45a3-8654-5f8e69958042', implicit).
narrative_ontology:cs_authority_grounding('a0905c30-b21f-45a3-8654-5f8e69958042', extraction).
narrative_ontology:cs_interpretation_layer_present('a0905c30-b21f-45a3-8654-5f8e69958042').
narrative_ontology:cs_reading_relation('a0905c30-b21f-45a3-8654-5f8e69958042', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('a0905c30-b21f-45a3-8654-5f8e69958042', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('a0905c30-b21f-45a3-8654-5f8e69958042', foundational, technology_is_a_tool).
narrative_ontology:cs_axiom_status(technology_is_a_tool, holdable).
narrative_ontology:cs_axiom_grounding('a0905c30-b21f-45a3-8654-5f8e69958042', technology_is_a_tool, conventional).
narrative_ontology:cs_axiom('a0905c30-b21f-45a3-8654-5f8e69958042', foundational, human_agency_is_primary_driver).
narrative_ontology:cs_axiom_status(human_agency_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('a0905c30-b21f-45a3-8654-5f8e69958042', human_agency_is_primary_driver, deontological).
narrative_ontology:cs_reference_frame('a0905c30-b21f-45a3-8654-5f8e69958042', agentic_deployment_framework).
narrative_ontology:cs_drift_state('a0905c30-b21f-45a3-8654-5f8e69958042', contemporary_media_theory, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a0905c30-b21f-45a3-8654-5f8e69958042', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printers_publishers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_authority).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, traditional_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their theological arguments, vernacular Bibles, and polemics, challenging Catholic doctrine and authority. They benefited from the press's ability to rapidly multiply their message and reach a wider audience, enabling the spread of the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, constrained, regional).

% Experienced significant economic gains from printing Reformation texts, which were in high demand. They often aligned with reformers for commercial reasons, leveraging the new technology to create a profitable market for religious literature and news. Their business model was directly subsidized by the demand for printed materials.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printers_publishers, beneficiary,
    powerful, biographical, mobile, regional).

% Suffered a significant loss of control over religious discourse and a challenge to its hierarchical authority. The press enabled the rapid spread of dissenting ideas that undermined its traditional role as the sole interpreter of scripture. They attempted to suppress printed materials through censorship and excommunication, but with limited success.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_authority, payer,
    institutional, civilizational, constrained, global).

% Lost influence and authority as direct intermediaries between congregants and scripture. Their role in interpreting and transmitting religious knowledge was diminished by the availability of vernacular Bibles and printed sermons. Many faced economic hardship and social displacement as the religious landscape shifted.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, traditional_clergy, payer,
    moderate, biographical, trapped, local).

% While the press facilitated the spread of ideas, a significant portion of the population remained illiterate and accessed information through oral means or intermediaries. They were excluded from direct engagement with printed texts, relying on others to interpret the new religious messages for them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, illiterate_populace, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the rapid, widespread dissemination of religious and political ideas across diverse geographic regions, enabling reformers to build a coherent movement and printers to establish a new commercial enterprise.
% TRANSFER_FUNCTION: Transferred intellectual and religious authority from centralized ecclesiastical institutions to decentralized networks of reformers and printers. It also transferred economic value from traditional scribal production to industrial printing, and from the Catholic Church to Protestant movements and commercial printers.
% ABSENT_VOICES: Those who valued the traditional, oral, and visual forms of religious transmission, or who lacked literacy, were marginalized. They would have argued for the preservation of established religious practices and the authority of the Church, but their voices were drowned out by the print-driven discourse.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the printing press had not occurred, the Reformation would likely have remained a localized theological dispute, lacking the means to rapidly spread its message and challenge established authority on a continental scale. The religious and political landscape of Europe would have been fundamentally different.
% FOUNDING_PROBLEM: The problem was the Catholic Church's centralized control over religious doctrine and its perceived corruption, coupled with a desire among reformers for direct access to scripture and a more personal faith.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation widely corroborate the existence and persistence of these problems, citing numerous primary sources from the period, including the writings of reformers, papal bulls, and contemporary accounts. The problem's status is considered live by historical consensus, as the issues of religious authority and access to scripture were central to the era.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the strategic use of the press fundamentally undermined the Catholic Church's authority and economic model, transferring significant power and wealth to reformers and printers. Suppression (0.70) was also high, as the Church actively tried to suppress dissenting printed materials, but its efforts were increasingly ineffective against the decentralized nature of print. The 'claimed_type' is 'tangled_rope' because it served a genuine coordination function (disseminating ideas, organizing a movement) but simultaneously extracted heavily from the established religious order through active enforcement (censorship, excommunication) that ultimately failed to hold. The resistance (0.80) from the Catholic Church was substantial, reflecting the direct threat posed by the press.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Protestant reformers and printers, the printing press was a powerful tool for coordination and liberation, enabling the spread of truth and economic opportunity. From the perspective of the Catholic Church and traditional clergy, it was a destructive force that undermined sacred authority and social order. The engine's per-seat classification would reflect this divergence, with reformers/printers experiencing it as a Rope or Scaffold, and the Church/clergy as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers and printers are clear beneficiaries (d near 0.0) as the press enabled their goals and generated profit. The Catholic Church and traditional clergy are clear victims/targets (d near 1.0) as their authority and economic base were directly challenged and eroded by the strategic deployment of the press. The 'requires_active_enforcement' is true because the Church actively tried to enforce its monopoly on religious discourse through censorship, even if ultimately unsuccessful.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the press's role as either purely coordinative (ignoring the profound disruption and extraction from the Church) or purely deterministic (ignoring the agency of reformers and printers). By identifying it as a Tangled Rope, it acknowledges both the genuine coordination function (spreading ideas, organizing a movement) and the asymmetric extraction from the established power structure, which required active, though ultimately failing, enforcement to resist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_structure_balance,
    'To what extent was the Reformation driven by the strategic agency of reformers and printers, versus the inherent structural properties of the printing press itself?',
    'Comparative historical analysis of other regions or periods where printing technology was present but similar social/religious movements did not emerge, or where different agents deployed it for different ends.',
    'If agency is found to be dominant, this ''strategic_deployment'' reading is strengthened, reinforcing the classification of the press as a ''tangled_rope'' (a tool wielded by agents). If structural properties are found to be more determinative, the ''technological_determinism'' reading gains ground, potentially reclassifying the press as a ''mountain'' or ''rope'' (an enabling force rather than a weapon).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_vs_structure_balance, empirical, 'Ambiguity in the balance between human agency and technological determinism in historical outcomes.').

omega_variable(
    economic_vs_religious_motivation,
    'What was the primary driver for printers to align with reformers: genuine religious conviction or commercial opportunity?',
    'Analysis of printers'' personal correspondence, financial records, and patronage networks, comparing their output of religious texts with other genres and their stated motivations.',
    'If commercial opportunity was the dominant driver, it strengthens the ''extraction'' aspect of the Tangled Rope, highlighting the self-interested nature of the alliance. If religious conviction was primary, it emphasizes the ''coordination'' aspect, suggesting a more ideologically aligned partnership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_religious_motivation, empirical, 'Uncertainty regarding the primary motivations of printers in supporting the Reformation.').

omega_variable(
    suppression_mechanism_effectiveness,
    'Was the Catholic Church''s suppression of printed materials genuinely ineffective, or did it merely shift the forms and channels of dissemination?',
    'Detailed bibliometric analysis of clandestine printing, smuggling routes, and the survival rates of banned texts, compared to the official output and reach of approved materials.',
    'If suppression was truly ineffective, the ''suppression'' metric remains high, indicating a failed attempt at control. If it merely forced materials underground, the ''effective suppression'' might be lower, as the ideas still spread, albeit through different means, suggesting a more complex interplay of control and evasion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_effectiveness, empirical, 'The true effectiveness of censorship against the decentralized nature of print.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.55).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causality__strategic_deployment, base_extractiveness, 1570, 0.65).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__strategic_deployment, base_extractiveness, 1600, 0.68).
narrative_ontology:measurement(pres_be_t1648, press_reformation_causality__strategic_deployment, base_extractiveness, 1648, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.5).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causality__strategic_deployment, suppression_requirement, 1570, 0.7).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__strategic_deployment, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement(pres_su_t1648, press_reformation_causality__strategic_deployment, suppression_requirement, 1648, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel. This 'strategic_deployment' reading emphasizes agentic use of technology, while 'technological_determinism' focuses on the press's inherent properties, and 'co_constitution' on feedback loops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
