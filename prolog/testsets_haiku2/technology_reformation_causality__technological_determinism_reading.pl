% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press Enabled Mass Vernacular Scripture Distribution (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The printing press emerged in mid-15th century Europe (Gutenberg, c.
 *   1440) as a mechanical reproduction technology that reduced the per-unit
 *   cost of copying text from labor-intensive hand production to
 *   capital-intensive bulk manufacturing. Within a century, printing had
 *   displaced manuscript production as the dominant reproduction method.
 *   Religious reform movements of the 16th century (Luther, Calvin, Zwingli)
 *   used the press to distribute their theological arguments, translated
 *   scripture, and polemics at unprecedented scale and speed. The
 *   technological determinism reading frames the press as a structural
 *   constraint — a mountain — whose operation made religious fragmentation
 *   inevitable by enabling mass distribution of vernacular scripture. In this
 *   reading, the Reformation was not primarily the outcome of reformers'
 *   agency or strategy; it was the structural consequence of a technology
 *   that made textual gatekeeping impossible. The constraint is CLAIMED as
 *   mountain (emerges_naturally: true) because the cost-reduction benefit of
 *   mechanical reproduction is presented as a physical/mechanical property,
 *   not a social construction. The reading is one of three sibling readings
 *   of the same kernel: beneficiary_agency_reading (reformers deployed the
 *   press strategically); co_constitution_reading (technology and agency
 *   co-evolved); technological_determinism_reading (this one — the press made
 *   the outcome inevitable).
 *
 * KEY AGENTS:
 *   - Printing technology: the structural agent, a mountain constraining distribution costs
 *   - Manuscript copyists: lose employment to mechanical reproduction (payers)
 *   - Printing-press adopters and owners: capture labor-cost reductions (beneficiaries)
 *   - Church hierarchy: loses gatekeeping power via textual abundance (payers)
 *   - Reformation reformers: exploit the constraint's operation for theological distribution (observers in this reading, not architects)
 *   - European reading publics: gain access to vernacular scripture (beneficiaries, powerless)
 *   - Technology historians: corroborate the founding problem (production bottleneck) from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.15).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.08).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press Enabled Mass Vernacular Scripture Distribution (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '1b971d0c-163c-4084-bab0-be00348241ea').
narrative_ontology:cs_kernel_codification('1b971d0c-163c-4084-bab0-be00348241ea', distributed).
narrative_ontology:cs_authority_grounding('1b971d0c-163c-4084-bab0-be00348241ea', distributed).
narrative_ontology:cs_reading_relation('1b971d0c-163c-4084-bab0-be00348241ea', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('1b971d0c-163c-4084-bab0-be00348241ea', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('1b971d0c-163c-4084-bab0-be00348241ea', foundational, technological_causality_sufficient).
narrative_ontology:cs_axiom_status(technological_causality_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('1b971d0c-163c-4084-bab0-be00348241ea', technological_causality_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('1b971d0c-163c-4084-bab0-be00348241ea', secondary, reformer_agency_contingent).
narrative_ontology:cs_axiom_status(reformer_agency_contingent, holdable).
narrative_ontology:cs_axiom_grounding('1b971d0c-163c-4084-bab0-be00348241ea', reformer_agency_contingent, empirically_contingent).
narrative_ontology:cs_created_at('1b971d0c-163c-4084-bab0-be00348241ea', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, printing_technology_adopters).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_literacy_expansion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, european_reading_public).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, manuscript_copyists).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, established_church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Printers, scholars, and entrepreneurs who leverage the press to produce multiple copies of vernacular scripture at dramatically reduced per-unit cost compared to manuscript production. They benefit from the technology's inherent efficiency gain — the cost reduction is structural, not negotiated.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printing_technology_adopters, beneficiary,
    moderate, generational, mobile, continental).

% The broadening of reading populations across European language groups. Not an actor, but a consequence structure: printing's cost reduction enabled distribution to markets previously unprofitable under manuscript economics.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_literacy_expansion, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, vernacular_literacy_expansion).

% Professional scribes who lose labor-intensive employment as printing replaces hand-copying. Their exit from the constraint (transition to other work) is available but costly — a generation of retraining or career abandonment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, manuscript_copyists, payer,
    moderate, biographical, constrained, regional).

% Roman Catholic Church authority structures that benefited from scarcity of scripture (limited copies meant limited unmediated access and interpretive monopoly). The press mechanically undermines their gatekeeping by making vernacular scripture abundant. They cannot exit the constraint; they can only resist its operation or adapt doctrine.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, established_church_hierarchy, payer,
    institutional, generational, constrained, continental).

% Religious reform advocates (Luther, Calvin, etc.) who exploit the press as a distribution channel. In the technological determinism reading, they are DOWNSTREAM adapters to the constraint's operation, not its architects — the press creates the possibility; they execute the strategy it enables.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformation_movement, observer,
    organized, generational, analytical, continental).

% Literate and semi-literate populations in vernacular-language regions who gain access to scripture text. This access was economically impossible under manuscript production; printing makes it feasible. They benefit without organizing or negotiating the benefit.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, european_reading_public, beneficiary,
    powerless, biographical, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printing press coordinates a transition from hand-copying to mechanical reproduction: solves the production bottleneck that had constrained scripture availability to high-cost, low-volume manuscript supply. The coordination problem is technical — how to produce many copies reliably — not social.
% TRANSFER_FUNCTION: Transfers labor-intensive copying work from individual scribes to capital-intensive press operations; transfers gatekeeping power from manuscript-scarcity-dependent hierarchies to distribution networks; transfers interpretive authority from monopoly keepers to distributed readers accessing the same text simultaneously.
% ABSENT_VOICES: Manuscript copyists were economically displaced but had no collective voice in the technological adoption; the press operated regardless of their interests. Their objections, had they been centered, would have argued for protected labor markets — a concern the determinism reading treats as economically futile against mechanical cost reduction.
% DISAPPEARANCE_RATIONALE: If printing press technology had never been invented and manuscript production remained the only reproduction method, vernacular scripture distribution would have remained scarce, expensive, and Church-gatekept for centuries longer. The Reformation as it happened — driven by mass distribution of reformist theology — would not have occurred. The constraint's disappearance rewrites religious history.
% FOUNDING_PROBLEM: Mechanical reproduction of text at scale — solving the production bottleneck that made copying labor-intensive and expensive, limiting the number of scripture copies available in a given region to whatever scribes could produce by hand.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and media historians (Eisenstein, Febvre & Martin, Pettegree) attest that manuscript production was the binding constraint on text distribution before printing. The founding problem is corroborated from outside the benefiting parties — technology historians and economic analysis of pre-print book economics document the constraint.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint operates as a cost reduction, not as a transfer from identifiable agents. No seat is coercing another; the press simply makes bulk reproduction cheaper than hand-copying. Suppression is minimal (0.08) because the constraint's operation requires no active enforcement — copying economically cannot compete with printing once the press exists. Theater is near-zero (0.02) because the constraint's function is purely mechanical; there is no performative maintenance or cover-story maintenance — the press reproduces text reliably without theatrical justification. Accessibility collapse is very high (0.92) because once the press exists and the cost advantage is understood, manuscript-copying is economically unfeasible as a primary technology — alternatives collapse not due to coercion but due to cost. Resistance is minimal (0.05) because the constraint operates through economic advantage, not through suppression; people resist losing employment (scribes), but the technology itself meets no organized technical opposition. The measurements span 1440–1550 (Gutenberg through Reformation peak) on a shared time grid. Extractiveness shows slight rise through the 1500s as printing becomes dominant, then stabilizes — the constraint's operation reaches mature form by 1520. Theater and suppression remain flat near zero throughout — this is the signature of a genuine mountain, not a manufactured constraint requiring maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the press-adopter seat: the technology is a blessing, a cost-reduction that makes their business profitable and scalable. From the scribe seat: the technology is a catastrophe, a displacement of their labor with no cushion. From the Church seat: the technology is a threat to institutional authority. From the reform-movement seat (observer in this reading): the technology is a tool whose operation they can exploit, but whose existence and operation they did not engineer. The engine computes these divergences from the structural data (power, exit, beneficiary/victim status) without requiring the commentary to predict them — the claim and the metrics remain independent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiary/victim declarations and exit options. Printing-press adopters (beneficiaries, moderate power, mobile exit) sit near d=0.2 — they benefit from cost reduction and can exit by choosing not to print. Manuscript copyists (victims, moderate power, constrained exit) sit near d=0.75 — they bear employment loss and cannot easily exit (retrain into different labor markets, wait for the technology to fail). Church hierarchy (victims, institutional power, constrained exit) sits near d=0.80 — they bear the loss of textual gatekeeping; their only exit is theological adaptation or organized suppression of the technology (constrained options). The European reading public (beneficiaries, powerless, mobile exit) sits near d=0.1 — they benefit from access, and their exit is default (they cannot choose to use the press, only to read or not read texts printed by others). The measurement series shows slight extraction rise as printing matures (1440–1540) then stabilizes — the constraint's operation reaches equilibrium once printing dominates.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the decay of a constraint's original mandate) is low here because the press's founding problem (mechanical reproduction bottleneck) remains live: the constraint's primary function (producing many copies reliably) is still the function the technology performs. No theater_ratio rise is observable — no gap between claimed function and actual maintenance. The measurement series shows theater near zero throughout, confirming that the constraint is doing what it was 'designed' to do (produce text at scale) without performative maintenance. The constraint is structurally live, not zombie-like. However, one axis of mandatrophy exists: by 1550, printing had solved its original problem so thoroughly that textual scarcity was not the binding constraint anymore — distribution networks, literacy, and access had become the new bottlenecks. In this sense the press's founding problem (production capacity) had been solved and displaced by downstream problems (who gets to read, what gets printed, who controls publication). The constraint itself persists, but its role as the primary limit on religious communication had shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'Is the printing press a structural constraint that made the Reformation inevitable, or did it merely enable reformers who chose to use it?',
    'Comparative historical analysis of printing adoption in non-Reformation regions (Spain, Portugal, Italy) and examination of pre-print reform movements (Lollardy, Hussites, Waldensians) to determine whether printing alone was sufficient for religious fragmentation or required active reformist agency.',
    'If deterministic: the constraint is a mountain (production cost reduction is structural). If agency-dependent: the constraint is rope or tangled_rope (the press enabled but did not compel). The reading''s core claim rides on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, empirical, 'Whether printing technology was sufficient for Reformation outcomes or only necessary.').

omega_variable(
    natural_law_vs_contingent_artifact,
    'Is the printing press a natural, inevitable invention once technological prerequisites were met, or a contingent artifact shaped by Gutenberg''s specific choices and cultural context?',
    'History of technology analysis: trace whether the press was convergent evolution toward an inevitable solution or one path among feasible alternatives. Examine Chinese/Korean printing precedents and whether they constrain or parallel European innovation.',
    'If inevitable: the press is a mountain. If contingent: the press is constructed; beneficiaries become suspicious (who chose it, why), and false-summit detection triggers. This determines whether the reading claims genuine naturality or disguised artifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_artifact, conceptual, 'Whether the printing press is a natural technological inevitability or a contingent human artifact.').

omega_variable(
    reformation_counterfactual_dependency,
    'Could reformist theology have spread via alternative distribution mechanisms (oral preaching, manuscript networks, word-of-mouth) without printing, or was the Reformation structurally dependent on the press?',
    'Counterfactual modeling of pre-print reform movements and their capacity to spread; examination of 14th–15th century reform theology distribution under manuscript constraints; identification of the specific distribution bottleneck the press solved.',
    'If Reformation was feasible without printing: the press was an accelerant, not determinant, and co-constitutional/agency readings gain power. If structurally dependent on printing: the determinism reading''s causal claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformation_counterfactual_dependency, empirical, 'Whether Reformation could have succeeded via pre-print distribution mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1440, 0.01).
narrative_ontology:measurement(tech_tr_t1470, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1470, 0.01).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1500, 0.02).
narrative_ontology:measurement(tech_tr_t1520, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1520, 0.02).
narrative_ontology:measurement(tech_tr_t1540, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1540, 0.02).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1550, 0.02).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1440, 0.12).
narrative_ontology:measurement(tech_be_t1470, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1470, 0.14).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(tech_be_t1520, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1520, 0.16).
narrative_ontology:measurement(tech_be_t1540, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1540, 0.17).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1550, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1440, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1440, 0.05).
narrative_ontology:measurement(tech_su_t1470, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1470, 0.06).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1500, 0.07).
narrative_ontology:measurement(tech_su_t1520, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1520, 0.08).
narrative_ontology:measurement(tech_su_t1540, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1540, 0.09).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1550, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__technological_determinism_reading, 0.1).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (technology_reformation_causality). The kernel represents the causal role of printing press technology in the Protestant Reformation. Three structurally distinct readings exist: (1) technological_determinism_reading (this file) — the press made Reformation inevitable via mechanical cost reduction; (2) beneficiary_agency_reading — reformers deployed the press strategically as a tool; (3) co_constitution_reading — technology and agency co-evolved, neither individually sufficient. Each reading instantiates a different constraint with different ε values and different beneficiary/victim structures. The readings are siblings linked via network.affects_constraints — they share a kernel but diverge on causal attribution and beneficiary positioning. The technological_determinism_reading claims the constraint is a mountain (emerges_naturally: true); the other readings claim ropes or tangled_ropes. The ε-invariance principle requires separate stories for each reading — they have different referents (press-as-determinant vs. press-as-instrument vs. press-as-co-evolving-system) and would yield different ε values even assessing the same historical facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
