% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press Technological Determinism: Reformation Causation
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   The technological determinism reading of the printing press and
 *   Reformation argues that Gutenberg's invention created a capacity for mass
 *   reproduction and cost-reduction that made censorship practically
 *   impossible and vernacular scripture economically inevitable. Under this
 *   reading, the Reformation is not a product of reformer agency, strategy,
 *   or theological innovation, but of exogenous technological displacement of
 *   the material basis of the Church's doctrinal monopoly. The press made it
 *   technically infeasible to suppress all copies of heretical texts; profit
 *   incentives aligned printer behavior with reformation-enabling production;
 *   literate populations gained access to scripture independent of any
 *   deliberate democratization effort. Reformers and the Church are
 *   structural beneficiaries and victims respectively, not protagonists. This
 *   is ONE of three distinct readings of the contested kernel
 *   'press_reformation_causation'; the other readings (mutual_shaping,
 *   strategic_deployment) reject technological determinism and emphasize
 *   agency, co-evolution, or strategic deployment. This constraint story
 *   instantiates only the technological determinism reading.
 *
 * KEY AGENTS:
 *   - Printing press technology: the exogenous technological capacity (claimed as causally sufficient; non-agent, analytical seat)
 *   - Reformation advocates (Luther, Calvin, radical sects): downstream beneficiaries of the press's mass-reproduction capability, not primary drivers
 *   - Catholic Church institutional authority: structural payer, suffering loss of monopoly on scriptural gatekeeping
 *   - Vernacular literate populations: beneficiaries gaining direct textual access (not through reformer strategy but through printer profit motive)
 *   - Printers and publishers: beneficiaries responding to market demand the press created (theological texts were profitable, not strategically recruited)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press Technological Determinism: Reformation Causation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history/technology/religion").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'e6ccd1ed-987c-4640-8f4f-0c061072159a').
narrative_ontology:cs_kernel_codification('e6ccd1ed-987c-4640-8f4f-0c061072159a', distributed).
narrative_ontology:cs_authority_grounding('e6ccd1ed-987c-4640-8f4f-0c061072159a', distributed).
narrative_ontology:cs_reading_relation('e6ccd1ed-987c-4640-8f4f-0c061072159a', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('e6ccd1ed-987c-4640-8f4f-0c061072159a', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('e6ccd1ed-987c-4640-8f4f-0c061072159a', foundational, technological_determinism_printing_inevitability).
narrative_ontology:cs_axiom_status(technological_determinism_printing_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('e6ccd1ed-987c-4640-8f4f-0c061072159a', technological_determinism_printing_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('e6ccd1ed-987c-4640-8f4f-0c061072159a', foundational, agency_causally_insufficient_reformation_outcome).
narrative_ontology:cs_axiom_status(agency_causally_insufficient_reformation_outcome, holdable).
narrative_ontology:cs_axiom_grounding('e6ccd1ed-987c-4640-8f4f-0c061072159a', agency_causally_insufficient_reformation_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('e6ccd1ed-987c-4640-8f4f-0c061072159a', manuscript_doctrinal_monopoly_sustainability).
narrative_ontology:cs_drift_state('e6ccd1ed-987c-4640-8f4f-0c061072159a', post_gutenberg_printing_maturation, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('e6ccd1ed-987c-4640-8f4f-0c061072159a', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, reformation_advocates).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literacy_expansion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, literate_vernacular_populations).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printers_and_publishers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The exogenous technological capacity itself: mechanical reproduction at scale, cost decline, speed of dissemination. Not an agent with intentions, but the enabling constraint whose properties the reading treats as causally sufficient for the Reformation's occurrence.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Reformers (Luther, Calvin, radical sectarians) gained the ability to mass-produce vernacular scripture and theological polemic in ways that made censorship practically impossible. Under this reading, they are downstream beneficiaries of the press's exogenous capacity, not the primary drivers of adoption. Their ideas found an audience not because they persuaded strategically but because the technology made them unavoidable.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, reformation_advocates, beneficiary,
    moderate, generational, mobile, continental).

% Previously maintained doctrinal monopoly through manuscript scarcity and clerical gatekeeping. The printing press eliminated the material basis for that monopoly — once texts could be reproduced at scale, suppressing all copies became technically infeasible. Institutional control eroded not from strategic failure but from technological bypass.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_authority, payer,
    institutional, civilizational, trapped, continental).

% Gained direct access to scripture and theological argument in native languages, rather than through clerical mediation. This accessibility was a consequence of printing's economic logic (vernacular texts had larger addressable markets than Latin texts) rather than an outcome of advocacy for democratic theology.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, literate_vernacular_populations, beneficiary,
    organized, generational, mobile, continental).

% Discovered that controversial theological texts sold better than devotional or liturgical texts — not by strategic alliance with reformers but by profit motive responding to market demand the press itself created. Their incentive structure aligned with reformation-enabling production.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers_and_publishers, beneficiary,
    organized, biographical, mobile, continental).

% Radical sects, Hussite movements, and other pre-Gutenberg reform attempts had existed but failed to gain lasting institutional foothold or geographical spread. They were excluded from the printing revolution's benefits because the technology arrived after their suppression. The reading notes this asymmetry: had the press arrived earlier, different movements might have succeeded.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, competing_reform_movements, excluded,
    moderate, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The constraint in this reading is a technological property (reproducibility at scale), not a stabilized arrangement solving a cooperation problem.
% TRANSFER_FUNCTION: The constraint transfers religious authority from the Latin-literate clerical class to lay populations gaining direct scriptural access. Under the technological determinism reading, this transfer is inevitable rather than strategically negotiated.
% ABSENT_VOICES: Those who would defend manuscript-era religious authority — prelates invested in doctrinal gatekeeping — are not absent but rendered powerless by technological fait accompli. Their resistance is futile not because they lack a voice but because the mechanism they relied on (manuscript scarcity) is gone.
% DISAPPEARANCE_RATIONALE: If printing technology never arrived or had been suppressed by unified force, the Catholic Church's institutional monopoly on scriptural interpretation would have persisted indefinitely. The Reformation depended on this technological shift; without it, reform movements would have remained localized and suppressible.
% FOUNDING_PROBLEM: This reading does not posit a founding problem in the traditional sense. It asserts a technological inevitability: once printing at scale became economically viable, mass production of vernacular texts followed as market logic, not as response to an articulated demand for religious reform.
% FOUNDING_PROBLEM_CORROBORATION: The corroboration is indirect: printing historian Elizabeth Eisenstein documents the speed and scale of text dissemination after Gutenberg; economic historians (e.g., Mokyr, Crafts) note the profit incentives driving printer decisions. The claim that the press 'caused' the Reformation is contested—most contemporary religious historians (Gregory, Cameron) argue the reading understates agency and mutual shaping. No independent voice outside the technological determinism school supports the full causal claim, which is why this reading's corroboration is weak.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

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
 *   The technological determinism reading claims the printing press as a MOUNTAIN: a constraint whose operation is not suspended, whose alternatives collapse nearly completely (once printing exists, reverting to manuscript-only gatekeeping becomes practically impossible), and which meets minimal real resistance (the Church tried censorship and failed). Extractiveness is low (0.15 end-interval) because the constraint is not presented as extractive of anything from anyone — it is purely a technological displacement of one capacity with another. Suppression is near-zero (0.08) because the constraint does not require active enforcement; it is an exogenous technological fact. Theater is negligible (0.02) because there is no performative maintenance — the press operates as described. Accessibility collapse is high (0.92): once printing technology exists, the alternatives (censorship, manuscript gatekeeping, clerical monopoly) collapse as practically infeasible. Resistance is minimal (0.05): the Church's Counter-Reformation censorship effort failed to stop printing or reformation texts in Protestant regions; resistance was futile against technological inevitability. The measurement series shows extractiveness and suppression rising slightly over the interval (1440–1600) as printing's scope expands and institutional suppression attempts intensify, but remain low throughout because the reading treats the constraint as technology, not as an extractive arrangement. The beneficiary declarations (reformation_advocates, vernacular_literacy_expansion) trigger false-summit evaluation: a mountain with declared beneficiaries requires omegas documenting the natural-law vs. constructed ambiguity. This is precisely the ambiguity the technological determinism reading stakes its existence on — hence the four omegas addressing causation direction, determinism vs. agency, economic necessity, and natural inevitability.
 *
 * PERSPECTIVAL GAP:
 *   Under the technological determinism reading, there is minimal perspectival divergence because the reading denies that agentive perspective is causally relevant. The Church's institutional seat would report trying to suppress reformation through censorship and failing due to technological factors beyond institutional control. Reformer seats would report benefiting from printing without having strategically deployed it — they exploited an exogenous capacity. Printer seats would report responding to market incentives, not to theological recruitment. The reading does not produce per-seat type divergence because it treats the constraint as a mountain experienced uniformly as a technological fact, not as an arrangement seats negotiated or coordinated. The sibling readings (mutual_shaping, strategic_deployment) would compute different per-seat types because those readings assign agency and strategic choice to the other parties; this reading drains agency from the structure entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   In the technological determinism frame, directionality is determined by exogenous technological flow, not by strategic positioning. The Church is the target (d near 1.0: loses its monopoly, cannot avoid the constraint's operation, has no arbitrage exit). Reformation advocates and vernacular populations are beneficiaries (d near 0.0: gain access without investment or strategy). Printers are secondary beneficiaries (d near 0.0: profit from the constraint's operation without having created it). The printing press technology itself is an observer/analytical seat (agent=false) — it does not collect or pay, it operates as a non-agentive force. No directionality override is necessary; the derivation from beneficiary/victim declarations produces the correct d values under the technological determinism reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The technological determinism reading avoids mandatrophy by treating the constraint as a natural law (mountain), not as an arrangement with a founding problem that could be solved. The Reformation did not 'solve' anything — it was a consequence of technological displacement. Under the sibling reading strategic_deployment, the constraint would have a founding problem (reformers wanted to spread their ideas; the press solved that problem). Under mutual_shaping, the founding problem would be mutual: technology and agency co-evolved to solve coordination challenges neither could have solved alone. The technological determinism reading escapes mandatrophy entirely by denying that the constraint has a problem it was built to solve — it is simply what happened when printing technology met medieval institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_agency,
    'Is the printing press''s role in the Reformation causally sufficient (deterministic — the press made reform inevitable) or merely enabling (the press created capability that agents could exploit or ignore)?',
    'Counterfactual analysis: if printing had not arrived until 1600, would reform movements have succeeded through other mechanisms? Historical comparison to movements in non-printing societies (Ottoman, Safavid Islamic theology under manuscript constraint). Textual evidence of reformer intentions: did they plan the press revolution or respond to it?',
    'If deterministic, the constraint is a mountain: technological capacity whose operation cannot be suspended. If enabling only, the constraint becomes tangled_rope or strategic_deployment: technology + agency in mutual feedback. The two readings produce different classifications and different structural relationships to reformer agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(determinism_vs_agency, conceptual, 'Whether the printing press effect is deterministic or merely enabling for Reformation outcomes.').

omega_variable(
    economic_necessity_vs_intentional_production,
    'Did printers publish reformation texts because reformers recruited them strategically, or because market demand for controversial theology was profitable and printers were indifferent to theological outcomes?',
    'Archival evidence from printer contracts, correspondence, and inventories. Analysis of which texts were reprinted most frequently and whether this matches reformer priorities or market-profitability signals. Comparison of printer behavior in reformation-sympathetic vs. reformed vs. Catholic-loyal regions.',
    'If strategic recruitment dominated, the constraint moves toward strategic_deployment (agents controlled technology). If profit-driven indifference dominated, technological determinism holds (the press''s inherent economics drove outcomes). The reading''s strength depends on establishing that printers'' behavior was shaped by technology and profit rather than theological allegiance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_necessity_vs_intentional_production, empirical, 'Whether the press amplified reformation through strategic alliance or market dynamics independent of reformer intent.').

omega_variable(
    natural_law_vs_constructed_benefit_claim,
    'Is the printing press''s mass-reproduction capacity a natural physical law (unmodifiable, inevitable in its operation) or a constructed technological system whose deployment patterns beneficiaries influenced?',
    'Historical analysis of censorship and suppression: could unified institutional resistance have stopped printing''s spread (as the Inquisition and state censors partially did)? If suppression was possible, the constraint was not inevitable. Examination of whether reformers influenced printer decisions, paper supply, typography choices, or distribution networks.',
    'If truly natural-law inevitable, the constraint is a mountain and reformers are passive beneficiaries. If constructed and suppressible, the constraint is tangled_rope or snare: the Church''s failure was strategic/institutional, not technological. This omega addresses false-summit candidate status: declaring beneficiaries (reformation advocates, vernacular populations) on a claimed mountain triggers FSM evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_benefit_claim, empirical, 'Whether the printing press constraint is a natural technological inevitability or a constructed system benefiting particular parties.').

omega_variable(
    causation_direction_ambiguity,
    'Did the printing press cause the Reformation, or did the existence of reformation-motivated audiences cause the printing press to be deployed for theological texts rather than other purposes?',
    'Temporal analysis: did theological printing come before reformation success, or after? Comparison to other early-printing applications (legal, commercial, classical texts): was theological content the natural first market or a redirected technology? Analysis of printer location (did they cluster in reformation strongholds or scatter independently)?',
    'If reformation causation is temporal precedent (press first, reformation follows), technological determinism holds. If reformation demand preceded large-scale theological printing, causation flows the other way: agency and context shaped technology deployment. The sibling reading mutual_shaping depends on rejecting a simple temporal ordering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_direction_ambiguity, empirical, 'The direction of causal influence between printing technology deployment and reformation success.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causation__technological_determinism, theater_ratio, 1440, 0.0).
narrative_ontology:measurement_basis(pres_tr_t1440, projected).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__technological_determinism, theater_ratio, 1480, 0.01).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causation__technological_determinism, theater_ratio, 1520, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1520, observed).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__technological_determinism, theater_ratio, 1560, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1560, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__technological_determinism, theater_ratio, 1600, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causation__technological_determinism, base_extractiveness, 1440, 0.0).
narrative_ontology:measurement_basis(pres_be_t1440, projected).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__technological_determinism, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causation__technological_determinism, base_extractiveness, 1520, 0.18).
narrative_ontology:measurement_basis(pres_be_t1520, observed).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__technological_determinism, base_extractiveness, 1560, 0.15).
narrative_ontology:measurement_basis(pres_be_t1560, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__technological_determinism, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causation__technological_determinism, suppression_requirement, 1440, 0.0).
narrative_ontology:measurement_basis(pres_su_t1440, projected).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__technological_determinism, suppression_requirement, 1480, 0.03).
narrative_ontology:measurement_basis(pres_su_t1480, observed).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causation__technological_determinism, suppression_requirement, 1520, 0.06).
narrative_ontology:measurement_basis(pres_su_t1520, observed).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causation__technological_determinism, suppression_requirement, 1560, 0.08).
narrative_ontology:measurement_basis(pres_su_t1560, observed).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__technological_determinism, suppression_requirement, 1600, 0.08).
narrative_ontology:measurement_basis(pres_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The kernel 'press_reformation_causation' decomposes into three constraint stories instantiating three distinct readings: (1) technological_determinism (this constraint) — exogenous technology as causally sufficient; (2) strategic_deployment — reformers and printers as active deployers; (3) mutual_shaping — technology and agency in co-evolutionary feedback. Each reading has different ε (technological_determinism treats the constraint as natural-law-like with minimal extraction; strategic_deployment and mutual_shaping treat it as arranged capacity with higher extraction if suppression is active). The three constraints are linked via network.affects_constraints to enable comparative analysis of how reading choice structures classification divergence. This is not a single constraint viewed from multiple angles — the readings generate structurally distinct constraints with different beneficiary/victim sets and different types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
