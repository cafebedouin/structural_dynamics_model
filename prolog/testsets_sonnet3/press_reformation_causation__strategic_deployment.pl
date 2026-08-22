% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: The Press as Strategically Deployed Coordination Tool (Reformation Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the 'strategic_deployment' reading of the
 *   press_reformation_causation kernel: the printing press is treated as
 *   neutral coordination capacity, and the Reformation's information dynamics
 *   are explained by the deliberate, purposive choices of reformers,
 *   printers, and allied princes to exploit that capacity for doctrinal and
 *   political advantage. Under this reading the press is structurally a rope
 *   — a coordination tool with low inherent extraction — while the extraction
 *   that does appear in the record comes from the deployment decisions
 *   layered onto it: reformers and printers captured audience, revenue, and
 *   political leverage that the old scribal-ecclesiastical system had held.
 *   This is one of three sibling readings of the same kernel
 *   (technological_determinism, which holds the press itself as the causal
 *   engine; mutual_shaping, which holds press and agency as co-evolving). Per
 *   the ε-invariance principle, each reading is authored as its own
 *   constraint with its own ε — this file does not average across readings or
 *   hedge between them.
 *
 * KEY AGENTS:
 *   - reformist_theologians: primary strategic agent — chooses print as a tool
 *   - commercial_printers: primary strategic agent — supplies and profits from the tool
 *   - territorial_princes_backing_reform: secondary beneficiary — converts coordination into political capital
 *   - catholic_ecclesiastical_hierarchy: primary target — loses transmission monopoly to a rival's deliberate strategy
 *   - displaced_scribal_copyists: incidental victim — absorbs the cost of the substitution with no voice in it
 *   - historians_of_technology: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.42).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.28).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "The Press as Strategically Deployed Coordination Tool (Reformation Reading)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '32c8781e-6bb4-42b1-bd42-551f3304dfee').
narrative_ontology:cs_kernel_codification('32c8781e-6bb4-42b1-bd42-551f3304dfee', distributed).
narrative_ontology:cs_authority_grounding('32c8781e-6bb4-42b1-bd42-551f3304dfee', distributed).
narrative_ontology:cs_reading_relation('32c8781e-6bb4-42b1-bd42-551f3304dfee', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('32c8781e-6bb4-42b1-bd42-551f3304dfee', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('32c8781e-6bb4-42b1-bd42-551f3304dfee', foundational, technology_is_normatively_neutral_capacity).
narrative_ontology:cs_axiom_status(technology_is_normatively_neutral_capacity, holdable).
narrative_ontology:cs_axiom_grounding('32c8781e-6bb4-42b1-bd42-551f3304dfee', technology_is_normatively_neutral_capacity, conventional).
narrative_ontology:cs_axiom('32c8781e-6bb4-42b1-bd42-551f3304dfee', foundational, causal_priority_belongs_to_deliberate_human_choice).
narrative_ontology:cs_axiom_status(causal_priority_belongs_to_deliberate_human_choice, holdable).
narrative_ontology:cs_axiom_grounding('32c8781e-6bb4-42b1-bd42-551f3304dfee', causal_priority_belongs_to_deliberate_human_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('32c8781e-6bb4-42b1-bd42-551f3304dfee', artisanal_print_capacity_as_available_tool).
narrative_ontology:cs_drift_state('32c8781e-6bb4-42b1-bd42-551f3304dfee', post_media_theory_turn, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('32c8781e-6bb4-42b1-bd42-551f3304dfee', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformist_theologians).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, territorial_princes_backing_reform).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, displaced_scribal_copyists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, lay_readers_and_congregants).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, lay_readers_and_congregants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Figures like Luther and his allies deliberately commissioned pamphlets, translated scripture into vernacular, and worked with sympathetic printers to distribute doctrine at unprecedented speed and volume. They chose print over manuscript or oral preaching alone because it multiplied reach per unit of effort; the technology was available to anyone with capital, and reformers actively sought it out rather than being swept along by it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformist_theologians, beneficiary,
    moderate, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, reformist_theologians, agenda_setter).

% Print shop owners in Wittenberg, Basel, Strasbourg and elsewhere recognized that reform pamphlets sold briskly and cheaply produced, and they competed for the business, sometimes pirating popular tracts. Their choice to print reform material over other content was a commercial calculation; they could and did print Catholic responses too when the market favored it. Their exit option was simply printing something else.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, commercial_printers, beneficiary,
    moderate, biographical, mobile, regional).

% Secular rulers who adopted reform doctrine used printed propaganda and legal pronouncements to consolidate authority against papal and imperial claims, extracting political leverage (control of church lands, reduced Rome remittances) from the coordination the press enabled. They funded print runs deliberately as an instrument of statecraft.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, territorial_princes_backing_reform, beneficiary,
    powerful, generational, arbitrage, regional).

% The papacy and allied bishops found their traditional control of doctrinal transmission through manuscript, sermon, and controlled Latin literacy undercut by an opposing strategic use of the same available technology. They attempted counter-printing and index censorship, but reformers had a first-mover organizational advantage in exploiting the tool. They bear the cost of lost doctrinal monopoly and lost revenue streams tied to it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_hierarchy, payer,
    institutional, generational, constrained, continental).

% Professional manuscript copyists, largely attached to monasteries and university towns, lost livelihood as reformers and printers redirected demand toward mechanically reproduced vernacular texts. They had no organized voice in the strategic decisions being made around them and few alternative trades to move into quickly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, displaced_scribal_copyists, payer,
    powerless, biographical, trapped, local).

% Ordinary literate and semi-literate laypeople gained direct access to vernacular scripture and pamphlets, a benefit reformers deliberately targeted them with. They also became targets of competing propaganda campaigns and, in some territories, coercive religious conformity imposed by whichever faction's strategic printing campaign prevailed locally.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, lay_readers_and_congregants, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, lay_readers_and_congregants, payer).

% Scholars evaluating primary print-run records, correspondence between reformers and printers, and patronage patterns to assess whether agency or technological affordance was the upstream driver of the Reformation's information dynamics.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press, in this reading, coordinates dispersed reformist actors — theologians, printers, and sympathetic princes — around a shared distribution infrastructure that lets doctrinal and political messaging scale far beyond what individual preaching or hand-copying could achieve, at low marginal cost.
% TRANSFER_FUNCTION: Moves doctrinal authority, tithe revenue, and political legitimacy away from the papal-scribal apparatus toward reformist theological networks and allied territorial rulers, via the deliberate choice to route persuasion through a purchasable, replicable print technology rather than through channels the old hierarchy controlled.
% ABSENT_VOICES: Displaced scribal copyists and rank-and-file clergy loyal to Rome had no seat in the strategic calculus of either reformers or printers; their economic and institutional stake in the older transmission system was not weighed by either side's deployment decisions.
% DISAPPEARANCE_RATIONALE: If the strategic-deployment relationship disappeared — i.e., if reformers and printers had NOT actively sought each other out and organized deliberate distribution campaigns — this reading holds that the Reformation's speed and geographic reach would have been substantially reduced even with the press physically existing; the press alone, unexploited by organized purposive actors, would not have produced the same outcome. Removing the strategic agency removes the causal engine this reading identifies.
% FOUNDING_PROBLEM: Reformers needed to bypass an ecclesiastical hierarchy that controlled manuscript production, university licensing, and pulpit access, in order to get vernacular scripture and dissenting doctrine to a lay audience faster than the hierarchy could suppress it.
% FOUNDING_PROBLEM_CORROBORATION: Book-trade historians examining printer account books and reformer correspondence (outside both the reformist and Catholic institutional traditions) attest that the original problem of hierarchy-controlled information transmission was resolved by the 17th century as printing infrastructure became routine and confessional lines stabilized; the strategic-deployment arrangement itself is now a historical event, not a persisting institution, so no party currently benefits from maintaining a 'founding problem' framing — the dead status is descriptive of the past event's resolution, not a live cover story.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (peaking ~0.42 around 1540) because this reading locates the extraction in human strategic choices — reformers and printers capturing revenue, audience, and legitimacy — not in the press itself, which functions here as low-cost, low-suppression coordination infrastructure. Suppression is authored comparatively low (0.28) relative to a determinism-flavored reading because under strategic_deployment the press does not compel any outcome; agents chose to use it, and the ecclesiastical hierarchy could and did counter-print. Theater ratio stays low throughout (peaking 0.18) because the deployment was substantially functional — real audiences were reached, real doctrine transmitted — not performative. accessibility_collapse (0.35) and resistance (0.4) are both moderate: alternatives (oral preaching, manuscript circulation, Catholic counter-printing) persisted throughout the interval, and active resistance (papal indices, licensing controls, competing print campaigns) was real and sustained, which is inconsistent with a mountain profile and consistent with a rope whose beneficiaries deliberately built and defended their advantage.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist/printer agenda-setting seat, the press reads as pure coordination success — a tool correctly identified and skillfully used to solve a real problem (bypassing hierarchy-controlled transmission). From the ecclesiastical hierarchy's seat, the same deployment reads as an asymmetric extraction of authority and revenue enabled by a rival's superior strategic use of a shared, neutral resource. The engine computing these as different seat-level classifications from the same structural data is exactly the point of the strategic_deployment reading: it locates all directionality in agency, none in the artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist theologians, printers, and backing princes are declared beneficiaries because, under this reading, they are the upstream deciding agents who chose to deploy the tool and captured the resulting doctrinal, commercial, and political gains — their directionality sits near the full-beneficiary end. The Catholic hierarchy is declared a victim/payer because it structurally lost the transmission monopoly and associated revenue to a rival's deliberate strategy, and its exit options were constrained (it could counter-print but could not un-exploit the technology). Displaced scribal copyists sit furthest toward the target end: powerless, trapped, with no seat in either side's strategic calculus — their cost was a pure externality of the deployment decision, not a negotiated trade-off.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (bypassing hierarchy-controlled information transmission) is authored as dead by 1600 — confessional lines had stabilized and print infrastructure had become routine commercial activity rather than an active reform weapon. This prevents mislabeling the now-ordinary business of printing as an ongoing extraction mechanism: the strategic advantage reformers captured was real and time-bound, not a persisting mandate that outlived its function. Declaring founding_problem_status dead here documents that the reading does not claim print-as-reform-weapon persists into the modern era.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_affordance_priority,
    'Is the correct causal ordering agency-first (reformers/printers chose to exploit a neutral tool, as this reading holds) or affordance-first (the press''s cost/speed/replication properties made certain reformist strategies newly possible or even inevitable, as the technological_determinism reading holds)?',
    'Comparative case analysis: regions/periods where equivalent printing capacity existed without comparable reform outcomes (e.g., early print centers with limited reform uptake) versus regions where reform succeeded with minimal print infrastructure, would help isolate whether the technology or the strategic choice was doing the causal work.',
    'If affordance-first is correct, this constraint''s claimed_type and beneficiary structure should shift toward the technological_determinism reading''s framing (press as mountain-like enabling condition rather than rope deployed by agents); the extraction would then be relocated from deployment choices to the technology''s own structural bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_affordance_priority, conceptual, 'Whether strategic agency or technological affordance is the upstream causal driver of Reformation information dynamics — the central kernel contest.').

omega_variable(
    printer_neutrality_assumption,
    'Were printers genuinely neutral commercial actors serving whichever market paid (as this reading assumes), or did printing-guild structures, capital requirements, and urban geography systematically favor reformist content regardless of individual printer intent?',
    'Archival analysis of print-run ratios (reformist vs. Catholic content) by city and decade, cross-referenced with capital investment and guild regulation records.',
    'If printer behavior was structurally channeled rather than freely chosen, the ''strategic_deployment'' framing overstates agency and some of the declared beneficiary status of commercial_printers should migrate toward a structural/affordance account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(printer_neutrality_assumption, empirical, 'Whether printer commercial choice was genuinely unconstrained or structurally channeled toward reform content.').

omega_variable(
    counterfactual_reform_without_print,
    'Would a reform movement with equivalent doctrinal content but no printing-press access have achieved comparable reach through manuscript and preaching networks alone, given enough time?',
    'Comparison with pre-print heretical/reform movements (Hussites, Waldensians) that achieved regional persistence without print, assessing whether the difference is one of degree (speed/scale) or kind (impossible without print).',
    'A finding of ''difference of degree'' would support this reading''s claim that the press was a tool that accelerated a process agents could have pursued by other means; a finding of ''difference of kind'' would support the determinism reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_reform_without_print, empirical, 'Whether print was necessary-in-kind or merely accelerating for Reformation-scale outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement_basis(pres_tr_t1450, observed).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__strategic_deployment, theater_ratio, 1480, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1480, observed).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causation__strategic_deployment, theater_ratio, 1570, 0.16).
narrative_ontology:measurement_basis(pres_tr_t1570, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__strategic_deployment, theater_ratio, 1600, 0.15).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__strategic_deployment, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement_basis(pres_be_t1450, observed).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__strategic_deployment, base_extractiveness, 1480, 0.15).
narrative_ontology:measurement_basis(pres_be_t1480, observed).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.3).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.42).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causation__strategic_deployment, base_extractiveness, 1570, 0.4).
narrative_ontology:measurement_basis(pres_be_t1570, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__strategic_deployment, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement_basis(pres_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__strategic_deployment, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__strategic_deployment, 0.03).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language claim 'the printing press caused the Reformation' per the ε-invariance principle. 'technological_determinism' authors the press's affordances as the upstream causal driver (higher structural inevitability, lower authored agency); 'mutual_shaping' authors a co-evolutionary account with distributed causal weight; this story ('strategic_deployment') authors agency as fully upstream and the press as neutral rope-like infrastructure. Each carries its own ε, beneficiary/victim structure, and claimed_type; none averages or hedges across the others. All three should be read together only at the network level, never merged into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
