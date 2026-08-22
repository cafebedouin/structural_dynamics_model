% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Technological Enabler of Reformation
 *   domain: history/technology/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinism reading of the
 *   printing press and the Reformation. The claim is that the mechanical
 *   invention of movable-type printing created an autonomous enabling
 *   technology whose capabilities made vernacular scripture dissemination and
 *   the Reformation's mass reach inevitable. Human actors (reformers,
 *   printers, readers) are responders to the technological substrate, not
 *   primary agents steering outcomes. The constraint is structured as a
 *   Mountain: it names a physical capability (rapid text reproduction) that
 *   overcomes a material bottleneck (scribal gatekeeping of texts) and makes
 *   certain outcomes (wide access to vernacular Scripture, rapid theological
 *   debate across distances) physically inescapable once the technology
 *   exists. The beneficiary structure is obscured by the technological
 *   framing: what appears as a natural enablement is analyzed here as itself
 *   a constraint that benefits vernacular readers and reform-minded clergy
 *   while displacing scribal authority and Church gatekeeping. The
 *   measurement series tracks how suppression rises with the Church's
 *   counterreformation printing response (1520s peak) but cannot overcome the
 *   underlying technological capability. Theater ratio remains near-zero
 *   because the press, in this reading, does not require theatrical
 *   maintenance — it is a self-maintaining physical fact.
 *
 * KEY AGENTS:
 *   - Printing press technology — the autonomous enabler; physical apparatus that makes rapid reproduction of vernacular texts possible
 *   - Reformers (Luther, Zwingli, Calvin) — institutional actors whose message is amplified by printing but not strategically orchestrated by them in this reading
 *   - Church hierarchy — institutional opponent whose suppressive efforts (index, censorship, Latin theological monopoly) cannot overcome the material fact of the press
 *   - Urban literate population — beneficiaries of access enabled by the technology, not targets of strategic outreach
 *   - Printers — economic responders to market demand for texts, not primary architects of religious change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Technological Enabler of Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history/technology/religious_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'ae005d04-778d-4b95-88a8-b67af50f3ac4').
narrative_ontology:cs_kernel_codification('ae005d04-778d-4b95-88a8-b67af50f3ac4', fixed_text).
narrative_ontology:cs_authority_grounding('ae005d04-778d-4b95-88a8-b67af50f3ac4', distributed).
narrative_ontology:cs_reading_relation('ae005d04-778d-4b95-88a8-b67af50f3ac4', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('ae005d04-778d-4b95-88a8-b67af50f3ac4', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('ae005d04-778d-4b95-88a8-b67af50f3ac4', foundational, technology_autonomous_constraint).
narrative_ontology:cs_axiom_status(technology_autonomous_constraint, holdable).
narrative_ontology:cs_axiom_grounding('ae005d04-778d-4b95-88a8-b67af50f3ac4', technology_autonomous_constraint, empirically_contingent).
narrative_ontology:cs_axiom('ae005d04-778d-4b95-88a8-b67af50f3ac4', foundational, human_agency_downstream_response).
narrative_ontology:cs_axiom_status(human_agency_downstream_response, holdable).
narrative_ontology:cs_axiom_grounding('ae005d04-778d-4b95-88a8-b67af50f3ac4', human_agency_downstream_response, deontological).
narrative_ontology:cs_reference_frame('ae005d04-778d-4b95-88a8-b67af50f3ac4', manuscript_textual_scarcity).
narrative_ontology:cs_drift_state('ae005d04-778d-4b95-88a8-b67af50f3ac4', post_1600_print_normalization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ae005d04-778d-4b95-88a8-b67af50f3ac4', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_reading_access).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, rapid_idea_dissemination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The mechanical apparatus for rapid reproduction of text in movable type. In this reading, it is an autonomous enabler whose capabilities make certain outcomes (vernacular scripture, rapid dissemination) inevitable once the physical mechanism exists, independent of human intention or strategic deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Religious actors (Luther, Zwingli, Calvin) whose goals are enabled by printing capability but not, in this reading, primarily chosen or strategically orchestrated. Their agency rides on the technological substrate; printing makes their message spread regardless of their particular tactical choices.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, reformers_and_clergy, observer,
    institutional, generational, analytical, continental).

% Catholic institutional authority whose opposition to vernacular translation and reform doctrine is overwhelmed by the press's capacity for cheap reproduction and distribution. In this reading, they face a technological constraint they cannot overcome by argument or suppression alone.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, church_hierarchy, observer,
    institutional, generational, analytical, continental).

% Readers in urban centers whose access to texts in their own languages becomes possible through mechanical reproduction. In this reading, their access is determined by the existence of the technology, not by strategic decisions to educate them or deny them texts.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, literate_urban_population, observer,
    moderate, biographical, analytical, regional).

% Commercial operators (Gutenberg, the Wechel family, others) who reproduce texts to meet market demand. In this reading, they are economic responders to the technological possibility, not primary agents steering religious change.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printers_as_economic_actors, observer,
    moderate, biographical, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the strict sense — this reading presents a technical capability, not a coordination problem. The press enables rapid text reproduction; it does not coordinate actors toward a shared goal except consequentially.
% TRANSFER_FUNCTION: The press transfers control over text production from the Church's scribal gatekeepers to distributed printers. Printed books move from presses to readers; religious authority moves from institutional monopoly over textual interpretation toward pluralism (consequence of the technology, not of deliberate transfer).
% ABSENT_VOICES: Scribes whose professional monopoly is displaced by the press; manuscript culture advocates who would defend hand-copying as preserving sacred intentionality; oral-tradition communities whose knowledge systems are bypassed by print's emphasis on written authority.
% DISAPPEARANCE_RATIONALE: In this reading, the printing press is a physical/mechanical fact. If it had never been invented, the Reformation would not have occurred in its historical form — or it would have required entirely different mechanisms (manuscript networks, institutional decay accelerated by other means, theological evolution without mass-distributed vernacular scripture). The world does not rearrange if the press disappears; the world never had the press to begin with, and the Reformation as a mass religious movement never gains traction. This is the technological determinist claim: the press did not activate an arrangement people already wanted — it made the arrangement possible by overcoming a material constraint.
% FOUNDING_PROBLEM: The concentration of textual authority in the hands of a scribal clergy and Latin-bound institutional Church created a bottleneck in scripture access and theological debate. Manuscripts were expensive, slow to copy, and gatekept. Vernacular understanding of doctrine was limited to what priests transmitted orally.
% FOUNDING_PROBLEM_CORROBORATION: Historians specializing in pre-print religious culture (Brian Richardson, Andrew Pettegree, others working from manuscript evidence) confirm the scarcity and gatekeeping of texts before printing. The scale of text production after 1450 is documented in print history databases (Short-Title Catalogue, Incunabula Short-Title Catalogue) showing orders-of-magnitude increases in title diversity and print run sizes. The technological reading is corroborated by quantitative book history and material evidence of production capacity.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.15 baseline) reflects the fact that in the determinist reading, the press itself does not extract: it is a neutral capability. The slight non-zero extractiveness enters because once the constraint is named as a 'capability that displaces gatekeepers,' it becomes visible as a structure that benefits some (vernacular readers) and harms others (scribal monopoly, Church institutional control). This is the false-summit candidate: the constraint is presented as natural/inevitable, but naming beneficiaries (vernacular_reading_access, rapid_idea_dissemination) flags it for FSM evaluation. Suppression is low (0.08) because the constraint is the technological capability itself, not an enforced rule — though Church counter-measures (Index, censorship, printing bans) show real suppression effort (peak at 0.12 in 1520). Accessibility collapse is very high (0.92) because once the press exists, alternatives to printed text reproduction are foreclosed by economic efficiency — hand-copying cannot compete. Resistance is near-zero (0.04) because the physical fact of the press faces no meaningful active opposition; the Church's efforts are symptom-level responses to an underlying material fact. Theater ratio is minimal because there is no performative component — the press does not require theatrical maintenance. The measurement trajectory shows suppression rising through the 1520s (Church's maximum counter-effort) then falling by 1600 as the technological victory becomes complete and institutional accommodation occurs.
 *
 * PERSPECTIVAL GAP:
 *   The technological determinism reading does not authorize a significant perspectival gap among seats — it evacuates perspective altogether by treating the press as an autonomous fact. All seats are 'observers' because none is the agent of the constraint; all are responders to it. This is the reading's structural claim: human choices and institutional strategies are downstream of the physical capability. If this reading were true, the payer seats (Church hierarchy, scribal culture) and beneficiary seats (vernacular readers, reformers) would both compute as 'observing' a constraint imposed by material reality, not by any human actor. This differs sharply from the strategic_deployment and co_constitution readings, where agency and intentionality are redistributed among human actors, creating divergent seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   In the determinist reading, directionality derivation breaks down because no human actor is the source of the constraint — it is autonomous technology. The beneficiary/victim declarations (vernacular_reading_access, rapid_idea_dissemination as beneficiaries; the Church's gatekeeping monopoly as implicitly harmed) mark where the technological output lands, not where human agency directs it. The Church and scribal culture face displacement (victim-like), but not from a human actor exercising power — from a material fact. Reformers benefit from the press's capabilities (beneficiary-like), but they did not cause the benefit. This is the reading's structural paradox: the classical directionality chain (agent → beneficiary/victim) assumes human authorship of the constraint. The determinist reading insists the constraint is authored by physics and mechanics, not by human strategy. The engine computes d from structural data anyway, deriving that the Church faces a target-like situation (beneficiary benefits from the technological shift, Church loses institutional function) — but the commentary must clarify that this is emergent directionality from an autonomous source, not chosen directionality from an actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual gatekeeping by scribal clergy) is declared as 'live' at t0 (1440), and the press is presented as solving it. By t1 (1600), the problem is substantially dead: vernacular scripture is widely available, printing replaces manuscript copying at scale, Church institutional control over textual authority has eroded. If the analysis stopped there, the founding problem is resolved and the constraint should be functional, not extractive. But the measurement series and the false-summit flagging reveal a subtlety: the extractiveness metric is non-zero (0.15) and remains stable through t1. This suggests that while the stated founding problem (textual access) is solved, the constraint has become a structure that extracts from some seats (scribal communities lose professional monopoly, Church loses institutional gatekeeping authority) while benefiting others (printers gain economic opportunity, vernacular readers gain access). The constraint's mandate (overcome textual scarcity) is achieved, but the constraint persists because it now embeds asymmetric benefits/harms. The false-summit framing captures this: the press is presented as a natural, inevitable technological fact, but it is also a structure that benefits identifiable parties and displaces others. Mandatrophy is not resolved — the founding problem is dead, but the constraint persists. The technological determinism reading does not allow the classical mandatrophy analysis (an actor chose poorly and now cannot unmake their choice) because the determinist frame denies human authorship. Instead, mandatrophy appears as a side effect of technological inevitability: the founding problem gets solved, but the solution creates new asymmetries that persist because no actor 'chose' the constraint and thus no actor can 'unchose' it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomous_vs_embedded_technology,
    'Is the printing press an autonomous enabling technology whose capabilities make outcomes inevitable, or is it always already embedded in human intentionality, economic incentive, and strategic choice?',
    'Comparative historical analysis: (a) Cases where printing technology existed but Reformation-like religious movements did not occur (e.g., China, Orthodox Christianity) would suggest the technology alone is not sufficient. (b) Cases where similar religious challenges to authority arose without printing (e.g., Hussites before widespread printing) would suggest human agency can produce reform outcomes without the press. (c) Analysis of early print content: if printers chose deliberately to publish reform texts and avoided Church-censored content, it shows strategic deployment rather than autonomous technological outcome.',
    'If the press is proven embedded in human choice, the reading collapses toward strategic_deployment or co_constitution, and the constraint''s beneficiary structure shifts from obscured (natural fact) to explicit (human actors benefit from their choices). If the technology is proven autonomous, the determinist reading holds and the constraint remains a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_vs_embedded_technology, empirical, 'Whether printing press outcomes derive from technological capability alone or require human strategic intention.').

omega_variable(
    technological_determinism_vs_contingency,
    'Does the printing press represent technological determinism (outcomes inevitable given the technology) or technological contingency (the technology enabled outcomes but did not determine them)?',
    'Counterfactual analysis: historians of technology debate whether the Reformation would have occurred without printing or merely in a different form/timeline. If plausible alternative Reformation scenarios exist that do not depend on printing (e.g., through manuscript networks, itinerant preachers, institutional decay), contingency is supported. If no plausible alternative paths exist to Reformation success without the press, determinism is supported.',
    'Determinism classification as Mountain stands if the technology is truly generative of outcomes. Contingency would reclassify the constraint toward Rope (genuine enabling coordination) or even Snare (if printers are shown to benefit asymmetrically from the technology''s application to religious texts). The distinction is whether the constraint is a physical fact (mountain) or a choice structure that could have been different (rope/snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_determinism_vs_contingency, conceptual, 'Whether the printing press determines Reformation outcomes or merely enables contingent possibilities.').

omega_variable(
    natural_law_vs_beneficiary_construction,
    'If the printing press is declared as benefiting ''vernacular reading access'' and ''rapid idea dissemination,'' are these genuine beneficiaries (real actors who gain) or abstract goods that obscure a more fine-grained beneficiary structure (printers as economic winners, reformers as religious winners, Church hierarchy as losers)?',
    'Unpacking the abstract beneficiaries: (a) Who materially benefits from ''vernacular reading access''? Literate urban populations, yes, but also merchants seeking to standardize communication, printers seeking profitable markets, reformers seeking wider audience. (b) Who bears costs? Scribal communities lose employment, Church loses textual authority monopoly, Latin-trained clergy lose interpretive privilege. (c) Examining who invested in printing technology and why: if printers made deliberate choices about what to print (market demand, profit motive), the constraint''s beneficiary structure is human and strategic. If the constraint is truly autonomous, beneficiaries are the technological capabilities themselves (abstract), not actors.',
    'Naming concrete human beneficiaries (printers, reformers, urban merchants) would trigger false-summit evaluation and likely reclassification toward Tangled Rope (coordination + extraction) or Snare (extraction riding on coordination). Keeping beneficiaries abstract (technological capabilities) preserves the mountain framing but strains credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_beneficiary_construction, empirical, 'Whether the constraint benefits abstract technological capabilities or concrete human actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__technological_determinism, theater_ratio, 1440, 0.0).
narrative_ontology:measurement_basis(pres_tr_t1440, projected).
narrative_ontology:measurement(pres_tr_t1460, press_reformation_causality__technological_determinism, theater_ratio, 1460, 0.01).
narrative_ontology:measurement_basis(pres_tr_t1460, observed).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causality__technological_determinism, theater_ratio, 1480, 0.01).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causality__technological_determinism, theater_ratio, 1520, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1520, observed).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causality__technological_determinism, theater_ratio, 1560, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1560, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__technological_determinism, base_extractiveness, 1440, 0.0).
narrative_ontology:measurement_basis(pres_be_t1440, projected).
narrative_ontology:measurement(pres_be_t1460, press_reformation_causality__technological_determinism, base_extractiveness, 1460, 0.02).
narrative_ontology:measurement_basis(pres_be_t1460, observed).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causality__technological_determinism, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causality__technological_determinism, base_extractiveness, 1520, 0.18).
narrative_ontology:measurement_basis(pres_be_t1520, observed).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causality__technological_determinism, base_extractiveness, 1560, 0.16).
narrative_ontology:measurement_basis(pres_be_t1560, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causality__technological_determinism, suppression_requirement, 1440, 0.0).
narrative_ontology:measurement_basis(pres_su_t1440, projected).
narrative_ontology:measurement(pres_su_t1460, press_reformation_causality__technological_determinism, suppression_requirement, 1460, 0.02).
narrative_ontology:measurement_basis(pres_su_t1460, observed).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causality__technological_determinism, suppression_requirement, 1480, 0.04).
narrative_ontology:measurement_basis(pres_su_t1480, observed).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causality__technological_determinism, suppression_requirement, 1520, 0.12).
narrative_ontology:measurement_basis(pres_su_t1520, observed).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causality__technological_determinism, suppression_requirement, 1560, 0.09).
narrative_ontology:measurement_basis(pres_su_t1560, observed).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.08).
narrative_ontology:measurement_basis(pres_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__technological_determinism, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The 'press_reformation_causality' kernel decomposes into three constraint stories, each authoring a different ε and beneficiary structure for the same historical event. The technological_determinism reading (this file) treats the press as an autonomous physical capability; the strategic_deployment reading treats it as a tool deliberately weaponized by human actors; the co_constitution reading treats it as a partner in recursive feedback with human agency. They are not three measurements of one constraint — they are three different constraints instantiated from one kernel, each with different implications for classification and causality. This story links to both siblings via network.affects_constraints and omega variables documenting the reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
