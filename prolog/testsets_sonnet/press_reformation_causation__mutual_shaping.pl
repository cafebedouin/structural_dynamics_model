% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press as Co-Evolving Scaffold of Reformation Agency
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the 'mutual_shaping' reading of the
 *   press-Reformation causation kernel: neither the printing press caused the
 *   Reformation as an inert deterministic mechanism, nor did reformers simply
 *   pick up a neutral tool and deploy it strategically. Instead, printers
 *   adapted their craft, financing, and output around reform demand, while
 *   reformers adapted their theological rhetoric and organizational strategy
 *   around print's affordances, and this two-way adaptation is what produced
 *   both the historical Reformation's specific shape and print culture's
 *   specific commercial and technical trajectory. The constraint is authored
 *   as scaffold rather than mountain or pure rope: press capacity functioned
 *   as an enabling structure reformers reinforced through use, and that
 *   reinforcement had a use-by character — once vernacular print became the
 *   settled default medium (by roughly 1600), the co-evolutionary discovery
 *   process that generated the loop was substantially complete, even though
 *   its institutional residue (confessional publishing houses, censorship
 *   regimes) persisted independently as separate constraints.
 *
 * KEY AGENTS:
 *   - protestant_printer_publishers: adapted craft workflows around reform demand, and reform success drove further craft investment
 *   - reforming_clergy: adapted rhetorical register to print affordances, and that adaptation fed reader demand back to printers
 *   - literate_urban_laity: reading practices and demand patterns shaped what got printed next
 *   - catholic_censorship_authorities: built new institutions specifically in reaction to the co-evolving technology-agency loop
 *   - displaced_scribal_copyists and unlicensed_dissenting_printers: bore the externalized costs of a coordination they did not negotiate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.32).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.28).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.32).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press as Co-Evolving Scaffold of Reformation Agency").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, '53925489-f3cb-42a5-b296-48596b43b891').
narrative_ontology:cs_kernel_codification('53925489-f3cb-42a5-b296-48596b43b891', distributed).
narrative_ontology:cs_authority_grounding('53925489-f3cb-42a5-b296-48596b43b891', distributed).
narrative_ontology:cs_reading_relation('53925489-f3cb-42a5-b296-48596b43b891', press_reformation_causation__technological_determinism, influences).
narrative_ontology:cs_reading_relation('53925489-f3cb-42a5-b296-48596b43b891', press_reformation_causation__strategic_deployment, influences).
narrative_ontology:cs_axiom('53925489-f3cb-42a5-b296-48596b43b891', foundational, causation_is_bidirectional_and_iterative).
narrative_ontology:cs_axiom_status(causation_is_bidirectional_and_iterative, holdable).
narrative_ontology:cs_axiom_grounding('53925489-f3cb-42a5-b296-48596b43b891', causation_is_bidirectional_and_iterative, empirically_contingent).
narrative_ontology:cs_axiom('53925489-f3cb-42a5-b296-48596b43b891', foundational, technology_capability_is_use_shaped_not_fixed_ex_ante).
narrative_ontology:cs_axiom_status(technology_capability_is_use_shaped_not_fixed_ex_ante, holdable).
narrative_ontology:cs_axiom_grounding('53925489-f3cb-42a5-b296-48596b43b891', technology_capability_is_use_shaped_not_fixed_ex_ante, empirically_contingent).
narrative_ontology:cs_reference_frame('53925489-f3cb-42a5-b296-48596b43b891', co_evolutionary_discovery_process_1450_1600).
narrative_ontology:cs_drift_state('53925489-f3cb-42a5-b296-48596b43b891', post_settlement_print_culture, gap(stable, minor, true)).
narrative_ontology:cs_created_at('53925489-f3cb-42a5-b296-48596b43b891', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, protestant_printer_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reforming_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, literate_urban_laity).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, territorial_princes_adopting_reform).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_censorship_authorities).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, displaced_scribal_copyists).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, unlicensed_dissenting_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Printers in cities like Wittenberg, Basel, and Strasbourg adapted press capacity to serialize pamphlets, broadsheets, and vernacular Bibles at a pace and price scriptoria could not match. They did not merely use a fixed tool: they reconfigured typesetting workflows, financing, and distribution networks around reform demand, and that adaptation fed back into what the press became capable of producing next.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, protestant_printer_publishers, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, protestant_printer_publishers, beneficiary).

% Luther, and reformers who followed, wrote for print in ways earlier theologians had not needed to: shorter, punchier, argumentative registers suited to pamphlet circulation. Their rhetorical practice was shaped by what the press rewarded (speed, repeatability, vernacular reach), and their success in turn drove printers to invest further in reform-oriented output — a genuine feedback loop, not one-directional exploitation of a neutral tool.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reforming_clergy, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, reforming_clergy, agenda_setter).

% Urban readers gained access to vernacular scripture and polemic they could not previously obtain or afford, and their demand for this material (measured in print runs and reprints) directly shaped which texts printers commissioned next. Their reading practices — communal reading aloud, pamphlet-sharing networks — were themselves new social forms enabled by, and reinforcing, print's expansion.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, literate_urban_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Princes who adopted reform used printed decrees, catechisms, and vernacular liturgy to consolidate territorial religious authority against Rome. Their patronage of print shops shaped which technical improvements (type fonts, format standardization) were funded, while the propaganda capacity print offered shaped their calculation of reform's political utility.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, territorial_princes_adopting_reform, beneficiary,
    powerful, generational, mobile, regional).

% Church and allied civil authorities built licensing regimes, indices of prohibited books, and pre-publication review specifically in reaction to print's changed distribution economics. Their enforcement apparatus was itself a co-evolutionary response — new institutions built to govern a technology whose social use kept outrunning existing control structures.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_censorship_authorities, payer,
    institutional, generational, constrained, continental).

% Monastic and guild scribes lost commissioned work as print absorbed demand for both religious and secular texts. They had no coordinated say in how the technology's uptake was shaped by reformers' rhetorical and commercial choices; their displacement was a byproduct of the coordination between printers and reform demand, not a negotiated transition.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, displaced_scribal_copyists, payer,
    powerless, biographical, trapped, local).

% Printers who set type for radical or heterodox material outside the emerging reform-mainstream faced prosecution, press seizure, and exile. Their exploitation of the press's possibilities went further than what territorial reform settlements would tolerate, and enforcement against them was shaped by which reform currents had achieved political protection — the co-evolution had insiders and outsiders.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, unlicensed_dissenting_printers, payer,
    moderate, biographical, trapped, regional).

% Study print runs, format shifts, and rhetorical adaptation across decades to trace how the technology and the movement changed each other's trajectory, rather than treating either as a fixed independent variable acting on an inert dependent one.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the technical capacity of movable-type printing with the rhetorical, financial, and organizational adaptations reformers made to exploit it — neither exists in the form observed without the other's reciprocal adjustment.
% TRANSFER_FUNCTION: Moves religious authority and interpretive capacity from centralized scribal/clerical control toward decentralized vernacular-literate networks, while moving craft income from scribal copyists toward printing capital, and moves enforcement burden onto censorship institutions built specifically to react to the technology's changed distribution economics.
% ABSENT_VOICES: Scribal copyists and unlicensed dissenting printers had no organized channel to shape how the press-reform feedback loop developed; they bore its displacement and enforcement costs as externalities of a coordination they were not party to negotiating.
% DISAPPEARANCE_RATIONALE: If the co-evolutionary feedback loop were removed — say, if either printers had never adapted workflows to reform demand or reformers had never adapted rhetoric to print's affordances — the pace, geography, and durability of the Reformation would have been materially different: slower diffusion, more centralized control retained by scribal/clerical networks, and a different balance between vernacular and Latin religious culture.
% FOUNDING_PROBLEM: Neither the press nor the reform movement was 'built to solve' the other's problem in the way an institution is built to solve a coordination problem; rather, printers seeking viable commercial output and reformers seeking wider theological reach discovered, through iterative use, that each could solve a problem for the other — cheap serial reproduction for printers, mass vernacular reach for reformers — and that discovery process itself became the arrangement.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the book (outside both the printing trade and confessional reform traditions) attest, via print-run and format analysis, that the original commercial-and-theological discovery process that generated the feedback loop concluded once print became the default medium for both religious and secular text by the mid-16th century; the mutual-shaping dynamic itself is now a settled historical pattern rather than a live discovery process, though its downstream institutional residue (censorship regimes, confessional publishing houses) persisted independently.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly (0.10 to 0.32) across the interval as the feedback loop matures from experimental adaptation into an entrenched commercial-confessional print economy that displaced scribal labor and marginalized unlicensed dissent. Suppression requirement rises sharply through the crisis period (1517-1540, peaking near 0.30) as censorship institutions scrambled to build capacity to match the technology-agency loop's pace, then eases slightly as licensing regimes stabilized into routine administration rather than crisis response. Theater ratio stays low throughout (0.05 to 0.15) because both the printing adaptations and the censorship reactions were substantially functional responses to a genuine, actively-changing situation, not performative maintenance of an already-settled arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and reformers occupy dual beneficiary/agenda-setter positions because the coordination they built was mutual and reinforcing — neither party simply received a subsidy from a pre-existing structure; both actively constructed the loop's terms through iterative adaptation. Scribal copyists and unlicensed dissenting printers sit at the target end because the costs of the loop's operation (displaced craft income, prosecution risk) were externalized onto them without their having shaped the loop's direction. Censorship authorities are payers in a structural sense distinct from the craft-displacement payers: they bear the cost of building new institutional capacity to govern a technology whose reformist uptake kept outrunning existing control mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing with a declared sunset prevents mislabeling the press-reform loop as either permanent natural law (which would obscure that specific historical actors built and rebuilt the arrangement through choices) or as pure extraction (which would obscure the genuine, mutual coordination function that made vernacular religious literacy and rapid theological communication possible for the first time). The founding_problem_status of 'dead' registers that the discovery process itself concluded once print became the default medium; treating the mutual-shaping dynamic as still-live past 1600 would be the mandatrophy this classification is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    loop_directionality_priority,
    'Within the mutual-shaping loop, did print-side technical/commercial adaptation more often lead reform-side rhetorical adaptation, or vice versa, at the fine-grained (decade-by-decade, region-by-region) level — or is the loop genuinely symmetric with no consistent lead?',
    'Fine-grained print-run and format-change dating cross-referenced against dating of specific reform rhetorical innovations (pamphlet genres, catechism formats) region by region, looking for consistent lag structure.',
    'A consistent lead by print-side adaptation would push this reading toward the technological_determinism sibling; a consistent lead by reform-side strategy would push it toward strategic_deployment; genuine symmetry with no consistent lag supports the mutual_shaping reading as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(loop_directionality_priority, empirical, 'Whether fine-grained lag analysis would reveal an asymmetric lead hidden within the aggregate mutual-shaping pattern.').

omega_variable(
    counterfactual_press_without_reform,
    'Would movable-type printing have developed the same technical and commercial trajectory (font standardization, serial pamphlet formats, distribution networks) absent reform demand — i.e., is the co-evolution specific to this historical instance or would print technology have converged on the same forms via secular/commercial demand alone?',
    'Comparative analysis of print technology development in regions/periods with minimal reform uptake but comparable secular commercial print demand (e.g., certain Italian city-states), checking whether format and distribution innovations proceeded on a similar trajectory without reform pressure.',
    'If print would have developed similarly without reform demand, the ''mutual'' in mutual_shaping is weaker than authored and this reading drifts toward strategic_deployment (press as pre-formed neutral tool). If print''s trajectory diverges sharply without reform demand, the mutual_shaping reading is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_press_without_reform, empirical, 'Whether print technology''s developmental trajectory was contingent on reform demand specifically, or would have converged similarly via secular commercial pressure.').

omega_variable(
    founding_problem_dead_vs_residue_live,
    'Is it correct to say the founding co-evolutionary discovery process is ''dead'' by 1600, given that the institutional residue (confessional publishing infrastructures, censorship regimes) it generated continued actively shaping later print culture and religious conflict for centuries afterward?',
    'Trace whether post-1600 confessional publishing and censorship institutions continued to exhibit genuine bidirectional co-evolution with technology, or had settled into a stable, non-evolving institutional pattern by that point.',
    'If post-1600 institutions still exhibited active co-evolution, the founding_problem_status should be ''contested'' rather than ''dead'', and this constraint''s temporal boundary (interval end at 1600) understates the loop''s duration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_dead_vs_residue_live, conceptual, 'Whether the mutual-shaping dynamic''s conclusion date is a defensible periodization or an artifact of this story''s chosen interval boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.07).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__mutual_shaping, theater_ratio, 1540, 0.13).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causation__mutual_shaping, theater_ratio, 1570, 0.15).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.15).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.15).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__mutual_shaping, base_extractiveness, 1540, 0.3).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causation__mutual_shaping, base_extractiveness, 1570, 0.32).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.1).
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__mutual_shaping, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__mutual_shaping, suppression_requirement, 1540, 0.3).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causation__mutual_shaping, suppression_requirement, 1570, 0.28).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.05).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the press_reformation_causation kernel, each with its own ε, stakeholder structure, and classification: technological_determinism reads the press as a mountain-like enabling condition with reform as near-inevitable consequence; strategic_deployment reads the press as a neutral rope-like tool reformers instrumentally exploited; mutual_shaping (this story) reads both as co-evolving, authored here as a scaffold with a declared sunset because the discovery process that generated the loop concluded once print became the settled default medium. All three share the same underlying historical events but differ in causal structure, beneficiary/victim framing, and persistence claims — they are linked here rather than merged because attempting to average or hedge across them would violate the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
