% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Reformation Co-Constitution Feedback Loop
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the co-constitutive relationship between the
 *   printing press and the Reformation. It argues that neither technology nor
 *   human agency was solely determinant; instead, they formed a feedback loop
 *   where the availability of print shaped religious controversy, and
 *   religious controversy, in turn, drove the demand for print. This reading
 *   classifies the dynamic as a Tangled Rope, acknowledging both its
 *   coordination function (mass dissemination of ideas) and its extractive
 *   aspects (displacing traditional authority, creating new forms of control
 *   and profit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.45).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.6).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Reformation Co-Constitution Feedback Loop").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '8a31762b-fea8-4054-8073-7e1a95dbf379').
narrative_ontology:cs_kernel_codification('8a31762b-fea8-4054-8073-7e1a95dbf379', distributed).
narrative_ontology:cs_authority_grounding('8a31762b-fea8-4054-8073-7e1a95dbf379', practice).
narrative_ontology:cs_interpretation_layer_present('8a31762b-fea8-4054-8073-7e1a95dbf379').
narrative_ontology:cs_reading_relation('8a31762b-fea8-4054-8073-7e1a95dbf379', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('8a31762b-fea8-4054-8073-7e1a95dbf379', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('8a31762b-fea8-4054-8073-7e1a95dbf379', foundational, technology_and_agency_interdependent).
narrative_ontology:cs_axiom_status(technology_and_agency_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('8a31762b-fea8-4054-8073-7e1a95dbf379', technology_and_agency_interdependent, empirically_contingent).
narrative_ontology:cs_axiom('8a31762b-fea8-4054-8073-7e1a95dbf379', foundational, feedback_loops_drive_historical_change).
narrative_ontology:cs_axiom_status(feedback_loops_drive_historical_change, holdable).
narrative_ontology:cs_axiom_grounding('8a31762b-fea8-4054-8073-7e1a95dbf379', feedback_loops_drive_historical_change, empirically_contingent).
narrative_ontology:cs_reference_frame('8a31762b-fea8-4054-8073-7e1a95dbf379', dynamic_co_evolution).
narrative_ontology:cs_drift_state('8a31762b-fea8-4054-8073-7e1a95dbf379', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8a31762b-fea8-4054-8073-7e1a95dbf379', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, traditional_scribal_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from increased demand for printed materials, especially controversial religious texts. They invested in presses and distribution networks, shaping the content that became widely available. Their economic success was intertwined with the spread of Reformation ideas.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers_publishers, beneficiary,
    organized, biographical, mobile, regional).

% Utilized the printing press to disseminate their theological arguments, pamphlets, and vernacular Bibles, reaching a mass audience. Their ideas fueled the demand for print, creating a feedback loop. They faced suppression from the Catholic Church but gained popular support through print.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_leaders, agenda_setter,
    powerful, generational, constrained, regional).

% Suffered a loss of control over information dissemination and religious authority. Their attempts to suppress dissenting texts through censorship and the Index Librorum Prohibitorum were often outpaced by the speed and volume of print. They bore the cost of a fragmented religious landscape.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to religious texts and theological debates in their own languages, fostering individual interpretation and engagement with scripture. This empowered them but also exposed them to diverse, sometimes conflicting, religious views.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Displaced by the efficiency and lower cost of printing. Scribes, illuminators, and manuscript producers saw their livelihoods diminish as printed books became dominant. They had few alternatives within the existing economic structure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, traditional_scribal_economy, payer,
    powerless, immediate, trapped, local).

% Navigated the religious and political turmoil, often using the printing press to consolidate their own power by supporting either the Reformation or the Catholic Church, depending on their strategic interests. They sought to control the flow of information within their territories.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_rulers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the rapid, widespread, and relatively inexpensive dissemination of complex religious and political ideas across diverse populations, enabling the formation of new intellectual and religious communities.
% TRANSFER_FUNCTION: Transferred religious authority and interpretive power from the centralized Catholic Church hierarchy to a more distributed network of reformers, printers, and vernacular readers, alongside economic gains for printers and publishers.
% ABSENT_VOICES: Those who preferred a unified, traditional religious authority and feared the social fragmentation caused by widespread religious debate were often marginalized or silenced by the very forces of print they sought to control.
% DISAPPEARANCE_RATIONALE: If the co-constitutive feedback loop between print and religious controversy had not emerged, the Reformation would have been a localized, academic dispute rather than a mass movement. The political, social, and religious landscape of early modern Europe would be fundamentally different, with a much stronger, unified Catholic Church and a slower pace of intellectual change.
% FOUNDING_PROBLEM: The problem of disseminating complex theological arguments and religious texts to a broad, non-Latin-speaking audience, and the desire for greater individual engagement with scripture.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the initial problem of mass dissemination was solved by the printing press. The 'co-constitution' reading argues that the problem's 'solution' then created new problems and dynamics, making the original problem obsolete in its initial form, but the feedback loop itself became the new constraint. No single party benefits from claiming the original problem is still live, as the nature of religious communication has fundamentally changed.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by the Catholic Church and the traditional scribal economy, as well as the new forms of control and profit generated by the print industry. Suppression (0.60) is high due to the Church's active, though ultimately unsuccessful, attempts at censorship and the enforcement of new orthodoxies by both Catholic and Protestant authorities. The theater ratio is low (0.10) because the constraint was highly functional and consequential, not performative. The metrics show a rise in extractiveness and suppression during the peak of the Reformation, followed by a slight decline as new equilibria were established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Catholic Church, the print-Reformation dynamic was a Snare, actively undermining its authority and extracting its power. From the perspective of printers and reformers, it was a Rope or even a Scaffold, enabling new forms of coordination and societal transformation. This 'co-constitution' reading attempts to capture the hybrid nature of this dynamic, acknowledging both the genuine coordination and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers/publishers and Reformation leaders were primary beneficiaries, leveraging the new medium for economic gain and ideological dissemination, respectively. Vernacular readers also benefited from increased access to information. The Catholic Church hierarchy and the traditional scribal economy were victims, losing authority and livelihoods. Secular rulers acted as agenda-setters, attempting to control the dynamic for their own political ends.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_ambiguity,
    'To what extent did the inherent properties of print technology (e.g., reproducibility, cost-effectiveness) drive the Reformation, versus the intentional choices and strategies of human agents (e.g., Luther''s use of pamphlets, printers'' profit motives)?',
    'Comparative historical analysis of other regions or periods where print existed but similar religious upheavals did not occur, or where religious upheavals occurred without widespread print technology.',
    'If technological properties were more determinant, the constraint leans towards a Mountain or Rope; if human agency was primary, it leans towards a Snare or Tangled Rope. This reading asserts co-constitution, making the causal arrow bidirectional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, empirical, 'Ambiguity regarding the primary causal driver in the print-Reformation dynamic.').

omega_variable(
    long_term_impact_on_authority,
    'Did the co-constitutive dynamic permanently decentralize religious authority, or did new forms of centralized control (e.g., state churches, new orthodoxies) eventually re-establish a similar extractive structure?',
    'Longitudinal historical study extending beyond the immediate post-Reformation period into the era of confessionalization and the rise of state-controlled media.',
    'If authority remained decentralized, the constraint''s long-term extractiveness might be lower; if new centralizations emerged, the constraint might have evolved into a different form of Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_authority, empirical, 'Uncertainty about the long-term impact on the centralization of religious authority.').

omega_variable(
    framing_of_technological_role,
    'Is the printing press best understood as a neutral ''scaffold'' that enabled new dynamics, or as an active ''agent'' with its own inherent biases and influences?',
    'Conceptual analysis of media theory and historical case studies where media technologies have demonstrably shaped content and social structures beyond mere enablement.',
    'Framing the press as a neutral scaffold supports a lower extractiveness for the technology itself, while framing it as an active agent implies a more inherent, possibly extractive, influence on the social system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_technological_role, conceptual, 'Conceptual framing of the printing press''s role in the co-constitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__co_constitution, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__co_constitution, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__co_constitution, theater_ratio, 1550, 0.1).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__co_constitution, theater_ratio, 1650, 0.08).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__co_constitution, base_extractiveness, 1450, 0.2).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__co_constitution, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__co_constitution, base_extractiveness, 1550, 0.45).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__co_constitution, base_extractiveness, 1650, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__co_constitution, suppression_requirement, 1450, 0.3).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__co_constitution, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__co_constitution, suppression_requirement, 1550, 0.6).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__co_constitution, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__co_constitution, suppression_requirement, 1650, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel. It emphasizes the co-constitutive feedback loop, contrasting with 'technological_determinism' (press as autonomous cause) and 'strategic_deployment' (press as tool of agents).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
