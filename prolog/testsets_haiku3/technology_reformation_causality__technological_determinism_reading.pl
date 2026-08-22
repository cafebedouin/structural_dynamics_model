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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press Technological Determinism: Mass Vernacular Scripture Distribution Inevitability
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   The technological determinism reading frames the printing press as a
 *   physical and economic constraint that made the Protestant Reformation
 *   inevitable. The argument is that movable type reduced the per-unit cost
 *   of book reproduction from person-years of labor to person-hours, and that
 *   this cost reduction necessarily enabled the mass distribution of
 *   vernacular scripture. That enabled distribution, in turn, necessarily
 *   undermined the Church's institutional control over textual authority and
 *   scriptural interpretation — a control that had depended on scarcity and
 *   clerical gatekeeping. The reading treats the printing press as a
 *   mountain: a structural fact of physics and economics whose consequences
 *   unfold regardless of human choice. Reformers are downstream adapters of
 *   technological capability, not strategic agents who chose to deploy
 *   printing to challenge ecclesiastical authority. The constraint is this:
 *   once printing press technology exists, mass vernacular scripture
 *   distribution becomes inevitable, and the institutional preconditions for
 *   a centralized Church monopoly on doctrine become structurally impossible
 *   to maintain. This is ONE reading of a contested kernel; other readings
 *   (beneficiary_agency and co_constitution) instantiate different
 *   constraints from the same historical event.
 *
 * KEY AGENTS:
 *   - printing_press_economic_logic (analytical, the cost-reduction mechanism itself)
 *   - protestant_reformers (organized, excluded as agents in the deterministic frame)
 *   - catholic_church_institutional_authority (institutional, trapped as monasticism/clerical control becomes economically unviable)
 *   - manuscript_copyists (moderate power, displaced by technology)
 *   - literate_vernacular_speakers (moderate power, enabled beneficiaries)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.12).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.05).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press Technological Determinism: Mass Vernacular Scripture Distribution Inevitability").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history/technology/religious").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, 'd0e16ea4-7e35-4820-a107-5705e265e5a1').
narrative_ontology:cs_kernel_codification('d0e16ea4-7e35-4820-a107-5705e265e5a1', distributed).
narrative_ontology:cs_authority_grounding('d0e16ea4-7e35-4820-a107-5705e265e5a1', expertise).
narrative_ontology:cs_reading_relation('d0e16ea4-7e35-4820-a107-5705e265e5a1', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('d0e16ea4-7e35-4820-a107-5705e265e5a1', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('d0e16ea4-7e35-4820-a107-5705e265e5a1', foundational, technology_determines_outcomes).
narrative_ontology:cs_axiom_status(technology_determines_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('d0e16ea4-7e35-4820-a107-5705e265e5a1', technology_determines_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('d0e16ea4-7e35-4820-a107-5705e265e5a1', foundational, human_agency_epiphenomenal_to_cost_structure).
narrative_ontology:cs_axiom_status(human_agency_epiphenomenal_to_cost_structure, holdable).
narrative_ontology:cs_axiom_grounding('d0e16ea4-7e35-4820-a107-5705e265e5a1', human_agency_epiphenomenal_to_cost_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('d0e16ea4-7e35-4820-a107-5705e265e5a1', printing_press_cost_reduction_imperative).
narrative_ontology:cs_drift_state('d0e16ea4-7e35-4820-a107-5705e265e5a1', contemporary_agency_theory_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0e16ea4-7e35-4820-a107-5705e265e5a1', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, printing_press_economic_logic).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, literate_vernacular_speakers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_institutional_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, manuscript_copyists_scribal_profession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical and economic properties of movable type printing reduce per-unit reproduction cost below hand-copying by orders of magnitude. This cost structure is not contingent on human preference or institutional choice — it is a material fact of the technology. The constraint 'vernacular scripture becomes mass-distributable' follows from this cost reduction as water follows downhill.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printing_press_economic_logic, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__technological_determinism_reading, printing_press_economic_logic).

% In the technological determinism reading, reformers are downstream adapters of the printing press's capabilities, not strategic deployers of it. The constraint frames their doctrinal and organizational innovations as responses enabled (not chosen) by the mass production of vernacular texts. Their excluded position is that they are historical agents with strategic intent — a position this reading structurally denies by placing technology as the primary causal force.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, protestant_reformers, excluded,
    organized, biographical, constrained, continental).

% The Church's authority over scriptural interpretation depended on controlling the reproduction and distribution of texts. The printing press made that control structurally impossible — not because anyone chose to defy the Church (that was attempted), but because the economics of mass production could not be bottlenecked by any medieval institutional structure. The Church bears the cost of its monopoly's collapse as an inevitable consequence of technology, not as a result of strategic action against it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_institutional_authority, payer,
    institutional, civilizational, trapped, continental).

% The economic basis of manuscript copying is erased by printing press cost reduction. Scribes paid the cost of technological displacement — their labor became economically obsolete not through institutional choice but through the inevitability of cost-driven market competition. In the deterministic reading, this displacement was not avoidable; the technology determined the outcome.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, manuscript_copyists_scribal_profession, payer,
    moderate, biographical, trapped, regional).

% Mass-produced vernacular scripture becomes physically available — not because anyone advocated for their access (though many did), but because the printing press made reproduction cheap enough that profit-seeking publishers distributed it. They gain access to texts previously locked behind clerical gatekeeping and Latin literacy, an outcome framed as technologically inevitable rather than politically won.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, literate_vernacular_speakers, beneficiary,
    moderate, biographical, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination problem solved by the printing press as a constraint — the technology is a unidirectional cost-reduction mechanism, not a solution to a collective-action problem. The deterministic reading brackets coordination entirely and frames the printing press as a material fact whose consequences unfold regardless of human coordination.
% TRANSFER_FUNCTION: The constraint transfers the economic basis of textual authority from the Church (monopoly on scribal production) to anyone with access to printing capital. It also transfers the economic viability of the scribal profession to zero. These transfers are not negotiated or chosen — they are the inevitable consequences of cost structures shifting.
% ABSENT_VOICES: Everyone who would argue that the Reformation was a product of human agency, strategic choice, and deliberate religious innovation — the reformers themselves, their patrons, the theological disputants, the bishops and cardinals who made institutional decisions in response. The technological determinism reading structurally excludes human agency as a primary causal force; those voices would object that technology was a tool they deployed, not a cause that determined outcomes.
% DISAPPEARANCE_RATIONALE: The deterministic reading claims the Reformation became inevitable once printing press technology existed — without it, mass scripture distribution was impossible and the religious institutional monopoly would have held. But this verdict is contested by the other readings: the beneficiary_agency_reading argues reformers would have found alternative means of distribution (portable manuscript copies, sermons, oral transmission) and the co_constitution_reading argues the press and the Reformation movement shaped each other such that neither 'caused' the other.
% FOUNDING_PROBLEM: The founding problem in the technological determinism frame is not a problem at all — it is a physical/economic fact: the cost of reproducing a book falls from person-years of labor per copy to person-hours via printing press technology.
% FOUNDING_PROBLEM_CORROBORATION: The cost structure of printing relative to hand-copying is established by historical technology studies (Eisenstein, Febvre & Martin) and physical analysis of reproduction technology — independent sources outside the Reformation's own theological narrative confirm the cost reduction fact. However, the CLAIM that this cost reduction made Reformation 'inevitable' is contested by historians of technology and religious studies who emphasize agency, choice, and contingency; no corroborating source outside the deterministic reading itself endorses the inevitability thesis.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The claimed type is mountain because the deterministic reading asserts that printing press technology is a structural fact whose consequences follow necessarily from cost economics. The extractiveness score is very low (0.12) because a genuine mountain constraint extracts nothing — it simply IS. The low score reflects the minimal extraction structure (the only 'beneficiary' is the abstract logic of cost reduction, not an organized human agent who collects rents). Suppression is negligible (0.05) because natural laws do not require active enforcement — they enforce themselves through the logic of cost competition and institutional economics. Accessibility of alternatives is nearly complete collapse (0.92) because once printing exists, the old scribal monopoly on text reproduction becomes economically impossible; no institutional effort can suppress printing back into non-existence. Resistance is very low (0.08) because the constraint is treated as inevitable, not as something human agents actively resist — they may resist its consequences (Counter-Reformation, Index of Prohibited Books) but they cannot resist the technology itself. Theater ratio is zero (0.0) because there is no performative activity — the constraint operates through pure economic logic, not through maintained theatrical compliance. The measurement series shows extractiveness rising slightly from invention (1440) to adoption (1480-1520) as the technology's cost advantage becomes undeniable, then plateauing as the new equilibrium (1520-1600) stabilizes. Theater and suppression stay flat at zero because a mountain does not require defense or performance — it simply operates.
 *
 * PERSPECTIVAL GAP:
 *   The technological determinism reading should produce radically different classifications from its sibling readings when the engine computes per-seat types. From the printing press's economic-logic seat, the constraint appears as a pure mountain — inevitable consequence of physics and cost. From the reformers' excluded seat (if they were given standing), the constraint would appear as a rope or tangled_rope — they made strategic choices, deployed technology deliberately, and organized a movement that technology enabled but did not determine. The beneficiary_agency_reading would seat reformers as the actual beneficiaries (they orchestrated the Reformation) and would shift the constraint's type away from mountain. The co_constitution_reading would seat both reformers and technology as co-evolving forces and would classify as rope (mutual enablement without determinism). The engine's per-seat computation will reveal these gaps.
 *
 * DIRECTIONALITY LOGIC:
 *   The technological determinism reading has no traditional beneficiary/payer directionality in the sense of organized human agents. The 'beneficiary' listed (printing_press_economic_logic) is not an agent — it is a non-agent entity (a doctrine, an economic law) included for analytical completeness. This placement triggers FSM (False Summit Mountain) evaluation: the constraint names a beneficiary, so the schema requires at least one omega documenting the natural-law vs. constructed ambiguity. The Catholic Church is listed as a payer because institutional ecclesiastical authority bears the cost of technological displacement (loss of textual monopoly). But the directionality is atypical: the payer is not being extracted from by an organized beneficiary; rather, the payer's monopoly is undermined by an impersonal economic force. Reformers are excluded rather than seated as beneficiaries because the deterministic reading denies them agency — they do not benefit as orchestrators of a coordination function; they benefit as passive recipients of what technology made possible. This exclusion is the reading's structural claim and the site of contention with the other sibling readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy analysis centers on the omega variable 'inevitability_vs_enablement': the founding problem (cost reduction fact) is live as a physical/economic fact, but the MANDATE (inevitability of Reformation) is dead or contested — historical scholarship increasingly emphasizes contingency, agency, and the role of political conditions (the Peace of Augsburg, the Peasants' War, patronage networks) in shaping Reformation outcomes. The constraint survives as a theoretical claim (determinism) even though the founding reason (that printing made Reformation inevitable) is now widely disputed. This is the classic mandatrophy pattern: the justification persists theatrically long after its truth has become contentious.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_enablement,
    'Does the printing press constrain outcomes (making Reformation inevitable once the technology exists) or merely enable outcomes (making Reformation possible but not necessary)?',
    'Counterfactual historical analysis: would a Reformation with functionally similar institutional and doctrinal characteristics have occurred without printing press technology via alternative distribution means (manuscript networks, portable codices, oral transmission, sermon publication)? Evidence from non-European reformatory movements lacking printing press access.',
    'If Reformation outcomes were achievable without printing (alternative mechanisms were viable), the constraint shifts from mountain to rope — technology enables but does not determine. If no viable alternative distribution existed and Reformation would have been impossible, the mountain claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inevitability_vs_enablement, empirical, 'Whether the printing press determines religious outcomes or merely enables them.').

omega_variable(
    technological_determinism_vs_co_constitution,
    'Is this constraint a genuine natural law of technology (cost reduction follows from physics and economics) or a reading constructed by attributing agency to technology rather than to the human actors who deployed and shaped printing technology?',
    'Genealogical analysis of deterministic framing itself: when did historians begin attributing Reformation causation to printing technology, and what theoretical commitments (economic materialism, technology studies positivism) justified that attribution? Does the deterministic reading reflect the technology''s actual causal properties or an interpretive choice to center technology over agency?',
    'If determinism is a theoretical attribution rather than a structural fact, the constraint is a false summit — beneficiaries (technological determinism as a doctrine, institutions that cite it to deny human responsibility) exist, and the mountain claim should trigger FSM evaluation and reclassification as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_co_constitution, conceptual, 'Whether technological determinism is a natural law or a constructed reading that serves institutional interests.').

omega_variable(
    agency_exclusion_mechanism,
    'Does the technological determinism reading structurally deny human agency (treating it as epiphenomenal to technology) or does it merely de-emphasize agency in favor of technological factors?',
    'Close reading of deterministic historical accounts: do they model reformers as making choices within technologically enabled possibilities, or do they model reformers as automatically executing outcomes that technology determines? If the former, agency is preserved and co-constitution applies; if the latter, agency is excluded and the reading becomes empirically falsifiable by showing reformers made choices that technology did not determine.',
    'If agency is excluded as epiphenomenal, the reading is a falsifiable claim: evidence of reformer choice-points where different outcomes were possible (e.g., Luther could have been suppressed if political conditions were different; printing could have been restricted if Church had acted earlier) falsifies determinism. If agency is preserved as constrained-but-real, the reading is compatible with co_constitution and the kernel contest becomes terminological rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_exclusion_mechanism, empirical, 'Whether the reading treats human agency as real-but-constrained or as illusory-epiphenomenal.').

omega_variable(
    natural_law_candidate_falsifiability,
    'What would constitute falsification of the mountain claim that printing press technology made Reformation inevitable?',
    'Specification of prediction: if printing press technology makes Reformation inevitable, then in any society with printing press access and Christian population, reformation would occur with similar institutional and doctrinal characteristics. Evidence of print-equipped Christian societies that did not experience reformation, or reformation with radically different trajectories, would falsify the mountain claim.',
    'The printing press''s inevitability is testable against non-European Christian contexts and non-Reformation periods. If the claim is falsifiable, it is not a genuine natural law (mountains are not falsifiable by evidence of specific cases — 2+2=4 does not become false if one mathematician rejects it). The mountain framing should shift to a contested empirical claim (rope or tangled_rope) dependent on context (geographic, institutional, political) that the deterministic reading brackets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_candidate_falsifiability, empirical, 'Whether the inevitability claim is a falsifiable empirical hypothesis or a genuine natural law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1440, 0.0).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1480, 0.0).
narrative_ontology:measurement(tech_tr_t1520, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1520, 0.0).
narrative_ontology:measurement(tech_tr_t1560, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1560, 0.0).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1600, 0.0).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1440, 0.0).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1480, 0.08).
narrative_ontology:measurement(tech_be_t1520, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1520, 0.12).
narrative_ontology:measurement(tech_be_t1560, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1560, 0.12).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1600, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__technological_determinism_reading, 0.05).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel technology_reformation_causality. The three readings decompose the Reformation-printing nexus into distinct structural claims: technological_determinism_reading frames printing as a mountain (inevitable cause); beneficiary_agency_reading frames reformers as strategic agents deploying a tool; co_constitution_reading frames technology and agency as co-evolving. Each reading instantiates a different constraint with different ε, beneficiary structures, and types. The three stories form a constraint family linked via network.affects_constraints. The genealogical uncertainty (whether Reformation was determined by technology, chosen by agents, or co-constituted) is routed through omega variables rather than folded into one story's claim/metric gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
