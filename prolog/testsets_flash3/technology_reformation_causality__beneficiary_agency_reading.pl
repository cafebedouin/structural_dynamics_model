% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformation-Era Printing Press as Tool of Beneficiary Agency
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the printing press during the Reformation from the
 *   perspective of 'beneficiary agency' – that is, how Protestant reformers
 *   and independent printers strategically deployed the technology as a tool
 *   to bypass the authority of the Catholic Church. The technology itself is
 *   viewed as a 'scaffold' that enabled a 'tangled rope' of mutual extraction
 *   between reformers and printers, where both parties benefited from
 *   challenging the established order, while the Church and traditional
 *   scribal guilds bore the costs. The constraint's extractiveness derives
 *   from the value of authority bypass and the disruption of existing
 *   information monopolies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.65).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.7).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformation-Era Printing Press as Tool of Beneficiary Agency").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'bb70b8bf-9866-4031-bc20-dc93bdb54c07').
narrative_ontology:cs_kernel_codification('bb70b8bf-9866-4031-bc20-dc93bdb54c07', implicit).
narrative_ontology:cs_authority_grounding('bb70b8bf-9866-4031-bc20-dc93bdb54c07', practice).
narrative_ontology:cs_reading_relation('bb70b8bf-9866-4031-bc20-dc93bdb54c07', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb70b8bf-9866-4031-bc20-dc93bdb54c07', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('bb70b8bf-9866-4031-bc20-dc93bdb54c07', foundational, technology_is_a_tool).
narrative_ontology:cs_axiom_status(technology_is_a_tool, holdable).
narrative_ontology:cs_axiom_grounding('bb70b8bf-9866-4031-bc20-dc93bdb54c07', technology_is_a_tool, conventional).
narrative_ontology:cs_axiom('bb70b8bf-9866-4031-bc20-dc93bdb54c07', foundational, human_agency_drives_change).
narrative_ontology:cs_axiom_status(human_agency_drives_change, holdable).
narrative_ontology:cs_axiom_grounding('bb70b8bf-9866-4031-bc20-dc93bdb54c07', human_agency_drives_change, deontological).
narrative_ontology:cs_reference_frame('bb70b8bf-9866-4031-bc20-dc93bdb54c07', strategic_deployment_of_technology).
narrative_ontology:cs_drift_state('bb70b8bf-9866-4031-bc20-dc93bdb54c07', contemporary_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bb70b8bf-9866-4031-bc20-dc93bdb54c07', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, independent_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, traditional_scribal_guilds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Strategically used the printing press to disseminate their theological arguments and vernacular Bibles, bypassing the Church's control over information. They benefited from the speed and reach of print, but were constrained by censorship and persecution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, agenda_setter,
    organized, generational, constrained, regional).

% Gained economic opportunity and influence by printing and distributing Reformation texts. They were often aligned with reformers due to shared interests in challenging established authority, but faced risks of confiscation and imprisonment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, independent_printers, beneficiary,
    moderate, biographical, constrained, local).

% Suffered a loss of authority and control over religious discourse as printing enabled the rapid spread of dissenting ideas. They attempted to suppress printing through censorship and the Index Librorum Prohibitorum, but their efforts were largely ineffective against the distributed nature of print.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Saw their livelihood and craft undermined by the efficiency and lower cost of printed books. Their skills became less relevant, and they had few options to adapt or resist the technological shift.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, traditional_scribal_guilds, payer,
    powerless, biographical, trapped, local).

% Largely unaffected by the direct content of printed materials due to illiteracy, but their social and religious lives were indirectly shaped by the broader shifts in authority and access to religious texts. They would have benefited from direct access to information but were excluded by literacy barriers.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, illiterate_peasantry, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled reformers and printers to coordinate the rapid, widespread dissemination of new theological ideas and vernacular texts, bypassing the traditional, centralized control mechanisms of the Church.
% TRANSFER_FUNCTION: Transferred control over religious narrative and information dissemination from the Catholic Church hierarchy to a decentralized network of reformers and independent printers, extracting authority and influence from the former.
% ABSENT_VOICES: The illiterate masses, who were the ultimate target audience for vernacular Bibles but lacked the means to directly engage with the printed word, and those who benefited from the pre-print information monopoly, whose voices were actively suppressed by the new media landscape.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the printing press by reformers and printers had not occurred, the Reformation's trajectory would have been fundamentally different, likely slower and more localized, with the Church retaining greater control over religious discourse. The entire socio-religious landscape of early modern Europe would have rearranged.
% FOUNDING_PROBLEM: The Catholic Church held a near-monopoly on religious interpretation and information dissemination, limiting access to scripture and theological debate to a select few, hindering reform efforts.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the Church's information monopoly was a key problem for reformers, and that the printing press effectively broke this monopoly. The problem is 'dead' in the sense that the specific form of Church control over information was fundamentally altered by print, though new forms of information control emerged.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the strategic use of the press fundamentally undermined the Church's control over religious discourse, extracting its authority and influence. Suppression (0.7) is also high, reflecting the Church's active but ultimately unsuccessful attempts to suppress dissenting publications through censorship and persecution. The theater ratio is low (0.1) because the deployment of the press was highly functional and effective in achieving its goals, with little performative maintenance. Accessibility collapse is moderate (0.4) as the press opened new avenues for information but still faced barriers like literacy and censorship. Resistance is high (0.8) from the Church, but it was largely overcome by the distributed nature of print.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reformers and printers, the press was a tool of liberation and progress, enabling the spread of truth. From the Church's perspective, it was a destructive force undermining sacred authority. This reading emphasizes the agency of the beneficiaries in shaping the technology's impact, rather than the technology's inherent determinism.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers and independent printers are the primary beneficiaries, actively using the press to their advantage (low directionality). The Catholic Church hierarchy and traditional scribal guilds are the victims, experiencing a loss of authority and livelihood (high directionality). The press itself, as a technology, is a scaffold, providing temporary support for this shift in power dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to bypass Church authority and disseminate new ideas. This mandate was successfully fulfilled, leading to a 'dead' founding problem status. The persistence of the printing press as a technology, however, led to new forms of information control and coordination, evolving beyond this specific 'tangled rope' dynamic. The classification prevents mislabeling the strategic deployment as pure technological determinism, highlighting the active choices of agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'To what extent was the Reformation an inevitable outcome of the printing press''s existence (technological determinism), versus a result of strategic choices by reformers and printers (beneficiary agency)?',
    'Comparative historical analysis of other regions/periods where printing existed but similar religious upheavals did not occur, or where other technologies had different social impacts.',
    'If technological determinism is stronger, the constraint would lean more towards a ''mountain'' or ''rope'' (inherent properties of the technology); if agency is stronger, it remains a ''tangled rope'' (human-driven extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Ambiguity between technological determinism and human agency in historical causality.').

omega_variable(
    scaffold_or_permanent_fixture,
    'Was the printing press truly a ''scaffold'' for the Reformation, meant to be transitional, or did it become a permanent fixture that fundamentally reshaped information control in a non-transitional way?',
    'Longitudinal study of media evolution post-Reformation: if new, stable information monopolies emerged that relied on print, it suggests a permanent fixture rather than a temporary scaffold.',
    'If it became a permanent fixture, the ''scaffold'' classification for the technology itself would be inaccurate, potentially reclassifying it as a ''rope'' or ''tangled rope'' in its own right, beyond its role in the Reformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_or_permanent_fixture, empirical, 'Whether the technology''s role was temporary support or permanent transformation.').

omega_variable(
    extraction_source_ambiguity,
    'Is the measured extraction primarily from the bypass of Church authority, or from the economic disruption of traditional scribal guilds?',
    'Detailed economic modeling separating the value of religious authority from the market value of manuscript production.',
    'If primarily from authority bypass, the constraint''s core function is political/religious. If primarily from economic disruption, it highlights a different, more purely economic ''snare'' aspect for the scribal guilds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_source_ambiguity, empirical, 'Ambiguity in the primary source of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1510, 0.1).
narrative_ontology:measurement(tech_tr_t1540, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1540, 0.1).
narrative_ontology:measurement(tech_tr_t1570, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1570, 0.1).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1600, 0.1).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1480, 0.3).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1510, 0.5).
narrative_ontology:measurement(tech_be_t1540, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1540, 0.6).
narrative_ontology:measurement(tech_be_t1570, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1570, 0.65).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1600, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1450, 0.2).
narrative_ontology:measurement(tech_su_t1480, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1480, 0.4).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1510, 0.6).
narrative_ontology:measurement(tech_su_t1540, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1540, 0.7).
narrative_ontology:measurement(tech_su_t1570, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1570, 0.7).
narrative_ontology:measurement(tech_su_t1600, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1600, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the 'beneficiary_agency_reading' of the 'technology_reformation_causality' kernel. It emphasizes the strategic deployment of the printing press by reformers and printers to bypass Church authority. Sibling readings include 'technological_determinism_reading' and 'co_constitution_reading', which offer alternative causal explanations for the Reformation's relationship with technology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
