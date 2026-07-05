% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Printing Press / Reformation Co-Constitution (Interaction-Term Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the co-constitution reading of the
 *   technology_reformation_causality kernel: the printing press and the
 *   Reformation are treated as mutually constituting each other through
 *   repeated interaction, rather than press causing reform (determinism) or
 *   reformers merely instrumentalizing a neutral tool (beneficiary agency).
 *   Under this reading, the press functions largely as a coordination
 *   mechanism (rope) — it let dispersed reform-minded actors converge on
 *   shared vernacular texts — while the traditional ecclesiastical mechanisms
 *   of doctrinal control that failed to adapt to the print market show
 *   piton-like atrophy: not actively suppressed by any single agent, but
 *   structurally out-competed and left as diminished, largely ceremonial
 *   authority. Extraction in this reading is attributed to the INTERACTION
 *   TERM itself — the compounding advantage that accrued to actors positioned
 *   at the intersection of print-market logic and reform rhetoric — rather
 *   than to either press or reformers unilaterally. This is a genuinely
 *   different ε-claim from the sibling readings and is authored as its own
 *   constraint per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - printer_guilds: primary beneficiary/agenda_setter (organized/mobile) — shape and are shaped by demand for reform pamphlets
 *   - reform_minded_clergy: beneficiary/agenda_setter (organized/constrained) — doctrinal content co-evolves with print-market rhetoric
 *   - urban_literate_laity: beneficiary (moderate/mobile) — market demand feeds back into content production
 *   - traditionalist_clergy_networks: payer (institutional/constrained) — slower-adapting channels lose relative reach, not by direct suppression but by competitive atrophy
 *   - unlicensed_itinerant_preachers: payer (powerless/trapped) — oral dissent modes marginalized as a side effect of the interaction, absent from the pamphlet record
 *   - print_technology_itself: non-agent observer — sets affordances, does not determine outcomes alone
 *   - historians_of_technology: analytical observer — chooses among competing causal framings with real historiographical stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.31).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.24).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Printing Press / Reformation Co-Constitution (Interaction-Term Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, 'd89a2dac-9efd-4925-9ce4-9245cae905b4').
narrative_ontology:cs_kernel_codification('d89a2dac-9efd-4925-9ce4-9245cae905b4', distributed).
narrative_ontology:cs_authority_grounding('d89a2dac-9efd-4925-9ce4-9245cae905b4', expertise).
narrative_ontology:cs_interpretation_layer_present('d89a2dac-9efd-4925-9ce4-9245cae905b4').
narrative_ontology:cs_reading_relation('d89a2dac-9efd-4925-9ce4-9245cae905b4', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('d89a2dac-9efd-4925-9ce4-9245cae905b4', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('d89a2dac-9efd-4925-9ce4-9245cae905b4', foundational, causality_is_bidirectional_and_emergent).
narrative_ontology:cs_axiom_status(causality_is_bidirectional_and_emergent, holdable).
narrative_ontology:cs_axiom_grounding('d89a2dac-9efd-4925-9ce4-9245cae905b4', causality_is_bidirectional_and_emergent, empirically_contingent).
narrative_ontology:cs_axiom('d89a2dac-9efd-4925-9ce4-9245cae905b4', foundational, neither_artifact_nor_agent_alone_is_sufficient_cause).
narrative_ontology:cs_axiom_status(neither_artifact_nor_agent_alone_is_sufficient_cause, holdable).
narrative_ontology:cs_axiom_grounding('d89a2dac-9efd-4925-9ce4-9245cae905b4', neither_artifact_nor_agent_alone_is_sufficient_cause, conventional).
narrative_ontology:cs_reference_frame('d89a2dac-9efd-4925-9ce4-9245cae905b4', eisenstein_communications_revolution_thesis).
narrative_ontology:cs_drift_state('d89a2dac-9efd-4925-9ce4-9245cae905b4', post_adrian_johns_critique_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d89a2dac-9efd-4925-9ce4-9245cae905b4', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printer_guilds).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reform_minded_clergy).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, urban_literate_laity).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, traditionalist_clergy_networks).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, unlicensed_itinerant_preachers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, co_evolutionary_causality_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, media_ecology_bidirectionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print shops in cities like Wittenberg, Basel, and Strasbourg selected which pamphlets to run based on what sold; they were not passive conduits for reformer intent nor autonomous technological determinants. They shaped print runs, formats, and pricing, which in turn shaped which reform arguments reached which audiences. Their commercial incentives fed back into what reformers wrote and how.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printer_guilds, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printer_guilds, agenda_setter).

% Reformers like Luther adapted their rhetorical style, pamphlet length, and vernacular register to what the medium and market rewarded — short, polemical, illustrated tracts outcompeted long theological treatises. Their doctrinal content was not simply transmitted by print; it was partly formed by what print made rhetorically effective.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reform_minded_clergy, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reform_minded_clergy, agenda_setter).

% Merchants, guild members, and literate townspeople gained access to vernacular religious argument and could compare positions across pamphlets. Their reading preferences and willingness to pay for certain genres of tract exerted upward pressure on what printers commissioned and reformers produced — a genuine two-way market in ideas, not simple reception.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, urban_literate_laity, beneficiary,
    moderate, biographical, mobile, regional).

% Established Church institutions found their traditional channels of doctrinal control — pulpit, manuscript, conciliar pronouncement — outcompeted in speed and reach by the co-evolving print-reform system, without press alone being the cause; their own slower adaptation to print as a genre also mattered. They bore reputational and authority costs as the interaction accelerated beyond what either printers or reformers alone intended.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, traditionalist_clergy_networks, payer,
    institutional, generational, constrained, continental).

% Oral, itinerant modes of doctrinal dissent that had operated for centuries before print found their audiences and legitimacy siphoned toward printed, urban-centered reform voices whose scale the co-evolving system favored. They had no equivalent capital to enter the print-market feedback loop and their alternative mode of dissent atrophied as an institutional pathway, independent of anyone's deliberate suppression.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, unlicensed_itinerant_preachers, payer,
    powerless, biographical, trapped, regional).

% The press as artifact set real affordances (movable type, reproducibility, falling per-copy cost) that constrained what was cheaply producible, but did not by itself select reform content, audience, or outcome — those emerged from repeated interaction between the artifact's affordances and the choices of printers, reformers, and readers.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, print_technology_itself, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, print_technology_itself).

% Retrospectively model the causal structure, choosing between deterministic, instrumentalist, and co-constitutive framings; their choice of framing has stakes for how later media-and-social-change arguments are evaluated, without themselves being party to the historical events.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press and the reform movement jointly solved a genuine coordination problem: standardizing and distributing a shared vernacular doctrinal vocabulary at a speed and scale no prior medium allowed, letting geographically dispersed reformers, printers, and lay readers converge on common texts and arguments without a central directing authority.
% TRANSFER_FUNCTION: The interaction moved authority and audience attention away from oral, manuscript, and conciliar channels of doctrinal control toward print-market-mediated channels; it moved commercial gain toward printer guilds who could scale production, and it moved rhetorical prestige toward reformers whose style fit the medium's affordances, at the expense of clergy and preachers whose modes did not translate into print-market success.
% ABSENT_VOICES: Unlicensed itinerant preachers and oral-tradition dissenters left almost no printed trace of their objection to the print-reform system's dominance, precisely because their mode of dissent was the one the co-evolving system marginalized; their absence from the surviving pamphlet record is itself part of what the interaction-term reading has to explain rather than take for granted.
% DISAPPEARANCE_RATIONALE: If print technology had not existed, historians disagree sharply on whether reform ideas would have propagated via oral and manuscript networks at a much slower but still consequential pace (world adjusts, more slowly), or whether the specific scale and speed of the print-reform interaction was itself load-bearing for outcomes like the Peasants' War pamphlet wars and the permanence of doctrinal fragmentation (world rearranges substantially). The co-constitution reading itself predicts genuine indeterminacy here, since it denies either factor alone was sufficient.
% FOUNDING_PROBLEM: Neither reformers nor printers set out to build a 'co-constitutive system'; reformers sought to challenge Church doctrine and printers sought commercial viability. The interaction pattern this constraint names emerged from the historical record, not from a designed arrangement any party intended to build.
% FOUNDING_PROBLEM_CORROBORATION: This is a historiographical characterization rather than a persisting institution with a founding problem in the ordinary sense; media ecology scholars (outside both the printing trades' and the reform movement's own self-narratives) attest that the co-constitutive framing was retrospectively constructed in the 20th century (Eisenstein, and later critics of Eisenstein such as Adrian Johns) to correct both over-deterministic and over-instrumentalist prior histories. No party from the 16th century itself corroborates or denies the framing, since the framing is an analytical artifact of later historiography, not a claim any 16th-century actor made about their own situation.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.31 at end) and attributed specifically to the interaction term — the compounding advantage of being positioned at the press/reform intersection — not to the press as artifact (which would push toward a mountain-like, non-agentic reading) nor to reformer strategy alone (which would push toward pure instrumentalism). Suppression is comparatively low (0.24) because the marginalization of traditionalist and oral-preaching channels was substantially a competitive/structural byproduct of the co-evolving system rather than an actively enforced exclusion by any single party — this is the key structural difference from a snare or tangled_rope reading, and is why requires_active_enforcement is not declared. Theater ratio rises across the interval (0.10 to 0.42) tracking the growing gap between the press's genuine coordination function (real in the early decades) and its later use as a ceremonial marker of 'reform legitimacy' independent of continued functional necessity, as print infrastructure matured and became background rather than novel coordination technology. Accessibility collapse is moderate (0.38): alternative channels (oral preaching, manuscript circulation, conciliar authority) did not vanish, they persisted but at reduced relative reach — consistent with a piton-style atrophy of the traditionalist alternative rather than its active suppression. Resistance (0.45) reflects genuine, documented ecclesiastical and academic resistance to both the print-reform interaction as it happened and to this later co-constitutive historiographical framing itself.
 *
 * PERSPECTIVAL GAP:
 *   From the print_technology_itself seat (non-agent, analytical), no causal privilege attaches to either press or reformers — the interaction is symmetric by construction. From the traditionalist_clergy_networks seat, the same historical process reads as an unplanned but real loss of relative doctrinal authority — a piton-style atrophy of their own institutional channels, without any single identifiable extractive agent to blame, which is precisely the structural signature this reading claims (as against the beneficiary_agency reading's claim that reformers deliberately engineered the bypass).
 *
 * DIRECTIONALITY LOGIC:
 *   Printer guilds and reform-minded clergy sit near the beneficiary end because they occupy the interaction-term position and capture the coordination gains (reach, doctrinal standardization, market revenue). Urban literate laity sit closer to symmetric — real access gained, but also exposed to propaganda dynamics and market-driven content selection they did not control. Traditionalist clergy networks and unlicensed itinerant preachers sit toward the target end not because anyone extracted from them directly, but because the co-evolving system structurally reduced their relative institutional reach — a diffuse, non-agentic cost that the directionality derivation should register as moderate rather than severe extraction, consistent with a rope/piton profile rather than snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The co-constitution reading guards against two mandatrophy-adjacent errors: (1) treating the press's ORIGINAL coordination function (letting geographically dispersed reformers converge on shared vernacular texts) as if it still operated identically at the interval's end, when by 1550-1600 the same coordination infrastructure had become background and partly ceremonial (rising theater_ratio) — a scaffold-turned-piton dynamic on the technology side; and (2) treating the traditionalist clergy networks' loss of relative authority as evidence of active extraction requiring a snare classification, when the structural data support competitive atrophy without an identifiable extracting agent — consistent with piton, not snare, on the losing-institution side. Declaring this reading distinctly from its siblings prevents conflating three different ε claims into one number.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_term_separability,
    'Can the extraction attributed to the co-constitution interaction term be meaningfully separated from (a) extraction attributable to press affordances alone, and (b) extraction attributable to reformer/printer strategic agency alone — or is the interaction term an artifact of this reading''s own framing rather than a discoverable historical quantity?',
    'Comparative case study: regions/periods where print technology diffused without a coincident reform movement (e.g. print in Italy without an equivalent doctrinal rupture) versus regions with reform movements but slower print penetration, to see whether the interaction term''s effects appear, disappear, or shift proportionally.',
    'If the interaction term dissolves under comparative analysis (extraction fully explained by press-alone or agency-alone factors), the co_constitution_reading loses its distinct ε claim and collapses into one of the sibling readings; if it persists as an independent residual, the co-constitution reading is empirically distinguishable from its siblings, not merely a rhetorical synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_separability, empirical, 'Whether the interaction-term extraction claim is empirically distinguishable from its sibling readings'' claims.').

omega_variable(
    piton_vs_active_suppression_traditionalist_channels,
    'Did traditionalist clergy networks and itinerant preachers lose relative reach purely through competitive/structural atrophy (piton), or did reform-aligned printers and clergy also actively suppress rival channels (which would push the traditionalist-facing seat toward tangled_rope or snare)?',
    'Archival review of guild licensing records, printer commissioning refusals, and documented instances of reform-aligned authorities blocking traditionalist pamphlet production or preaching licenses, versus evidence of purely market-driven content selection.',
    'Evidence of deliberate suppression would require reclassifying the traditionalist-facing side of this constraint toward tangled_rope (adding requires_active_enforcement and reframing the coordination/extraction split), rather than sustaining the piton characterization used here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_vs_active_suppression_traditionalist_channels, empirical, 'Whether traditionalist channel decline was competitive atrophy or active suppression.').

omega_variable(
    co_constitution_framing_underdetermination,
    'Is the choice of co-constitution as the correct kernel reading itself underdetermined by the historical evidence, such that determinism and beneficiary-agency readings remain equally defensible given the same primary sources — or does the co-constitution reading have independent evidentiary support (e.g. documented feedback loops between print-market data and pamphlet content) that the sibling readings lack?',
    'Systematic review of printer account books, pamphlet print-run revisions, and reformer correspondence discussing market reception, to establish whether bidirectional feedback is directly evidenced or inferred from the outcome alone.',
    'If bidirectional feedback is directly evidenced (printers documented adjusting content to sales, reformers documented adjusting rhetoric to print reception), the co-constitution reading has stronger evidentiary grounding than the sibling readings; if it is inferred post hoc from outcomes, all three readings remain comparably underdetermined by the same evidence base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_framing_underdetermination, conceptual, 'Whether the co-constitution framing has independent evidentiary support or is equally underdetermined as its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.1).
narrative_ontology:measurement_basis(tech_tr_t1450, observed).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__co_constitution_reading, theater_ratio, 1480, 0.15).
narrative_ontology:measurement_basis(tech_tr_t1480, observed).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.28).
narrative_ontology:measurement_basis(tech_tr_t1510, observed).
narrative_ontology:measurement(tech_tr_t1525, technology_reformation_causality__co_constitution_reading, theater_ratio, 1525, 0.4).
narrative_ontology:measurement_basis(tech_tr_t1525, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__co_constitution_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement_basis(tech_be_t1450, observed).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1480, 0.12).
narrative_ontology:measurement_basis(tech_be_t1480, observed).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.22).
narrative_ontology:measurement_basis(tech_be_t1510, observed).
narrative_ontology:measurement(tech_be_t1525, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1525, 0.34).
narrative_ontology:measurement_basis(tech_be_t1525, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.31).
narrative_ontology:measurement_basis(tech_be_t1550, observed).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.31).
narrative_ontology:measurement_basis(tech_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__co_constitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.05).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the technology_reformation_causality kernel. technological_determinism_reading locates ε in the press's inherent affordances treated as near-inevitable causal force (mountain-leaning). beneficiary_agency_reading locates ε entirely in reformer/printer strategic deployment of an inert tool (agency-leaning, tangled_rope-leaning given active strategic exclusion of Church channels). This co_constitution_reading locates ε in the interaction term between technology and social actors, treating the press as substantially rope-like coordination and the out-competed traditionalist channels as piton-like atrophy. The three are linked via affects_constraints because each reading's popularity in historiography exerts real downstream pressure on the others' scholarly resources and legitimacy (a shift in academic consensus toward one reading structurally starves grant funding, dissertation topics, and citation networks from the others), even though the readings coexist as live positions rather than one foreclosing the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
