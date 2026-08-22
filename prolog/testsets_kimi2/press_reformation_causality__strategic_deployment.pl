% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Strategic Deployment of Printing in the Reformation
 *   domain: history/religious/media
 *
 * SUMMARY:
 *   This constraint instantiates the strategic_deployment reading of the
 *   press_reformation_causality kernel. Against technological determinism
 *   (which treats the press as an autonomous cause) and co_constitution
 *   (which treats technology and human agency as symmetrically constitutive),
 *   this reading holds that reformers and printers strategically weaponized
 *   printing technology to achieve religious and economic goals. The
 *   constraint describes the standing arrangement of this deployment: a
 *   coordination network among reformers and printers that asymmetrically
 *   extracts interpretive authority and economic surplus from the Catholic
 *   Church hierarchy.
 *
 * KEY AGENTS:
 *   - reform_leaders: Primary agenda_setter (organized/mobile/continental) â coordinates the weaponization of print across jurisdictions
 *   - print_entrepreneurs: Primary beneficiary (moderate/mobile/continental) â captures commercial surplus from controversial religious publishing
 *   - church_hierarchy: Primary target (institutional/constrained/continental) â bears the extraction of its information monopoly and sacramental revenue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.78).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.72).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of Printing in the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history/religious/media").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'd7eaaa46-f255-494f-8f61-c7e97a81bf16').
narrative_ontology:cs_kernel_codification('d7eaaa46-f255-494f-8f61-c7e97a81bf16', distributed).
narrative_ontology:cs_authority_grounding('d7eaaa46-f255-494f-8f61-c7e97a81bf16', distributed).
narrative_ontology:cs_reading_relation('d7eaaa46-f255-494f-8f61-c7e97a81bf16', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('d7eaaa46-f255-494f-8f61-c7e97a81bf16', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('d7eaaa46-f255-494f-8f61-c7e97a81bf16', foundational, human_agency_directs_technological_change).
narrative_ontology:cs_axiom_status(human_agency_directs_technological_change, holdable).
narrative_ontology:cs_axiom_grounding('d7eaaa46-f255-494f-8f61-c7e97a81bf16', human_agency_directs_technological_change, empirically_contingent).
narrative_ontology:cs_axiom('d7eaaa46-f255-494f-8f61-c7e97a81bf16', foundational, instrumental_use_of_media_for_political_ends).
narrative_ontology:cs_axiom_status(instrumental_use_of_media_for_political_ends, holdable).
narrative_ontology:cs_axiom_grounding('d7eaaa46-f255-494f-8f61-c7e97a81bf16', instrumental_use_of_media_for_political_ends, empirically_contingent).
narrative_ontology:cs_reference_frame('d7eaaa46-f255-494f-8f61-c7e97a81bf16', strategic_instrumentality).
narrative_ontology:cs_drift_state('d7eaaa46-f255-494f-8f61-c7e97a81bf16', contemporary_historiography, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d7eaaa46-f255-494f-8f61-c7e97a81bf16', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reform_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, print_entrepreneurs).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Strategically commissioned, edited, and disseminated vernacular theological texts to challenge Church doctrine. Traveled to coordinate with printers and sympathetic magistrates. Could relocate to protective jurisdictions when banned, using the print network to maintain continuity across borders.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reform_leaders, agenda_setter,
    organized, generational, mobile, continental).

% Invested in presses, type, and paper to produce high-demand reformist pamphlets and Bibles. Profited from the surge in controversial religious literature. Moved shops between cities to exploit favorable local rulers or avoid Catholic censorship regimes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, print_entrepreneurs, beneficiary,
    moderate, biographical, mobile, continental).

% Held a monopoly on sacred text interpretation, sacramental authority, and theological education. Funded manuscript production and controlled the parish pulpits. Could not exit its own doctrinal and institutional structure; responded with the Index, censorship, and the Counter-Reformation but lost control over information flows.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, church_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized networks of reformers, printers, and sympathetic magistrates to produce, distribute, and protect vernacular religious texts across jurisdictional boundaries without centralized Church approval.
% TRANSFER_FUNCTION: Moves religious interpretive authority and economic surplus from the Catholic Church hierarchy to reform movements and the commercial printing industry.
% ABSENT_VOICES: Monastic copyists and illuminators whose livelihoods were destroyed by the shift to print; illiterate laypeople structurally excluded from the text-based public sphere; Catholic vernacular theologians who advocated internal reform without schism but were drowned out by polarizing polemic.
% DISAPPEARANCE_RATIONALE: Without the strategic deployment of printing, reformers would lack the infrastructure to sustain a mass movement across regions; the Church's information monopoly would likely have persisted for decades; the confessional map of Europe and the economic structure of the book trade would not have reorganized around competing religious publics.
% FOUNDING_PROBLEM: The Catholic Church's monopoly on sacred text production, interpretation, and sacramental mediation created a bottleneck in religious communication, limiting lay access to scripture and concentrating interpretive authority in the clerical hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Protestant reformers attest to the bottleneck as a central grievance; secular book historians and sociologists of religion corroborate the structural monopoly from outside the benefiting parties; Catholic historians acknowledge the monopoly's existence but dispute that its dissolution justified schismatic violence.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the press directly dismantled the Church's information monopoly and redirected theological authority and revenue. Suppression (0.72) is high because the constraint's persistence depended on actively suppressing the Church's alternative dissemination channels (manuscript culture, controlled pulpit, interpretive gatekeeping). Theater ratio (0.32) is moderate because polemical pamphleteering contained performative and ritual elements, but the core text production was functionally effective. Accessibility collapse (0.80) reflects the near-total displacement of manuscript alternatives for mass communication once print was strategically deployed. Resistance (0.75) captures the Church's vigorous counter-measures including the Index, censorship tribunals, and the Counter-Reformation.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and printer seats, the constraint is experienced as a rope â a coordination tool enabling religious renewal, lay literacy, and commercial opportunity. From the Church hierarchy's seat, the same arrangement operates as a snare â a weaponized media system designed to destroy its legitimacy and capture its economic base. The tangled_rope classification captures this structural divergence: the engine computes different per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform_leaders and print_entrepreneurs are structurally positioned as beneficiaries of the arrangement: they gain religious authority and commercial profit respectively, giving them low directionality (d near 0.0). The church_hierarchy is the target: it loses its monopoly on interpretation and faces an existential threat to its revenue and authority, giving it high directionality (d near 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution prevents classifying this as a pure rope (which would ignore the Church's catastrophic loss of authority and revenue) or a pure snare (which would ignore the genuine coordination problem solved for reformers and the real economic function for printers). The reading is committed to the claim that coordination and extraction are structurally coupled through the same mechanism â print dissemination â requiring active human enforcement through reformer networks, printer contracts, and magistrate protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_instrumentality,
    'Is the printing press in the Reformation best understood as a passive instrument of human strategy, or does it possess autonomous causal efficacy that reshaped the actors who used it?',
    'Comparative historiography across regions with differential print adoption rates; if Reformation success tracks press density more than reformer strategy, autonomy is supported.',
    'If autonomous, this reading overstates human agency and the constraint''s classification tilts toward technological determinism; if purely instrumental, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_instrumentality, conceptual, 'Whether the press is autonomous or instrumental to human strategy').

omega_variable(
    printer_profit_motive,
    'To what extent did printers select and disseminate reformist texts based on economic profit versus ideological alignment with reform theology?',
    'Archival analysis of printer contracts, inventory records, and price data; correlation between theological controversy and print-run profitability.',
    'If profit motive dominates, the coordination function is secondary to market extraction and the constraint approaches snare; if ideological alignment dominates, coordination among reformers is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_profit_motive, empirical, 'Economic versus theological motivation of printers').

omega_variable(
    church_resistance_capacity,
    'Did the Church hierarchy possess the institutional capacity to neutralize the press threat through alternative media strategies, or was its information monopoly structurally obsolete?',
    'Counterfactual analysis of Church-sponsored vernacular print initiatives and their uptake rates relative to reformist output.',
    'If the Church could have competed but failed strategically, extraction was contingent on reformer skill; if structural obsolescence, the constraint''s extractiveness reflects a natural technological transition rather than strategic weaponization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_resistance_capacity, empirical, 'Whether Church media monopoly was strategically or structurally defeated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pr_strat_dep_tr_t0, press_reformation_causality__strategic_deployment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pr_strat_dep_tr_t8, press_reformation_causality__strategic_deployment, theater_ratio, 8, 0.2).
narrative_ontology:measurement(pr_strat_dep_tr_t16, press_reformation_causality__strategic_deployment, theater_ratio, 16, 0.25).
narrative_ontology:measurement(pr_strat_dep_tr_t24, press_reformation_causality__strategic_deployment, theater_ratio, 24, 0.28).
narrative_ontology:measurement(pr_strat_dep_tr_t32, press_reformation_causality__strategic_deployment, theater_ratio, 32, 0.3).
narrative_ontology:measurement(pr_strat_dep_tr_t40, press_reformation_causality__strategic_deployment, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(pr_strat_dep_be_t0, press_reformation_causality__strategic_deployment, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pr_strat_dep_be_t8, press_reformation_causality__strategic_deployment, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(pr_strat_dep_be_t16, press_reformation_causality__strategic_deployment, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(pr_strat_dep_be_t24, press_reformation_causality__strategic_deployment, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(pr_strat_dep_be_t32, press_reformation_causality__strategic_deployment, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(pr_strat_dep_be_t40, press_reformation_causality__strategic_deployment, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pr_strat_dep_su_t0, press_reformation_causality__strategic_deployment, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pr_strat_dep_su_t8, press_reformation_causality__strategic_deployment, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(pr_strat_dep_su_t16, press_reformation_causality__strategic_deployment, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(pr_strat_dep_su_t24, press_reformation_causality__strategic_deployment, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(pr_strat_dep_su_t32, press_reformation_causality__strategic_deployment, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(pr_strat_dep_su_t40, press_reformation_causality__strategic_deployment, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the press_reformation_causality kernel, which decomposes the colloquial claim 'the printing press caused the Reformation' into three structurally distinct constraints: technological_determinism (press as autonomous cause), strategic_deployment (press as instrument of human agency), and co_constitution (press and agency as mutually constitutive). Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
