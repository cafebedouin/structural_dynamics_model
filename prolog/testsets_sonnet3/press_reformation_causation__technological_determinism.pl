% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinism reading of the
 *   printing-press/Reformation kernel: the claim that movable-type printing
 *   made ecclesiastical censorship structurally impossible and vernacular
 *   scripture historically inevitable, with the Reformation as a downstream
 *   consequence of an exogenous technological shift rather than a contingent
 *   outcome of human strategy. The reading is presented as it is held by its
 *   proponents (the Eisenstein historiographical tradition and its heirs),
 *   evaluated by its own lights — not averaged against the sibling
 *   strategic-deployment or mutual-shaping readings, which are separate
 *   constraints in this kernel family. Theater ratio rises over the interval
 *   because as the reading calcified into pedagogical orthodoxy, an
 *   increasing share of its citation and restatement became performative
 *   reinforcement of a settled narrative rather than fresh archival
 *   engagement with contested censorship-effectiveness evidence.
 *
 * KEY AGENTS:
 *   - protestant_reformers: downstream beneficiary of exogenous technological capacity in this reading's account
 *   - vernacular_printers: mechanism-bearers whose commercial dispersal is treated as itself causal
 *   - catholic_church_authorities: institutional payer whose resistance is framed as structurally futile
 *   - print_technology_historians: analytical seat that constructs and maintains the reading professionally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.42).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.35).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '88653bf0-1681-442a-9772-3dd4e1518b3e').
narrative_ontology:cs_kernel_codification('88653bf0-1681-442a-9772-3dd4e1518b3e', distributed).
narrative_ontology:cs_authority_grounding('88653bf0-1681-442a-9772-3dd4e1518b3e', expertise).
narrative_ontology:cs_interpretation_layer_present('88653bf0-1681-442a-9772-3dd4e1518b3e').
narrative_ontology:cs_reading_relation('88653bf0-1681-442a-9772-3dd4e1518b3e', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('88653bf0-1681-442a-9772-3dd4e1518b3e', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('88653bf0-1681-442a-9772-3dd4e1518b3e', foundational, technology_possesses_autonomous_causal_force).
narrative_ontology:cs_axiom_status(technology_possesses_autonomous_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('88653bf0-1681-442a-9772-3dd4e1518b3e', technology_possesses_autonomous_causal_force, empirically_contingent).
narrative_ontology:cs_axiom('88653bf0-1681-442a-9772-3dd4e1518b3e', secondary, institutional_resistance_to_diffusion_is_structurally_futile).
narrative_ontology:cs_axiom_status(institutional_resistance_to_diffusion_is_structurally_futile, holdable).
narrative_ontology:cs_axiom_grounding('88653bf0-1681-442a-9772-3dd4e1518b3e', institutional_resistance_to_diffusion_is_structurally_futile, empirically_contingent).
narrative_ontology:cs_reference_frame('88653bf0-1681-442a-9772-3dd4e1518b3e', eisenstein_print_revolution_thesis).
narrative_ontology:cs_drift_state('88653bf0-1681-442a-9772-3dd4e1518b3e', post_book_history_archival_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('88653bf0-1681-442a-9772-3dd4e1518b3e', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, print_technology_historians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_autonomy_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_ecology_determinism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned by this reading as downstream recipients of an exogenous technological capacity. Luther's theses and vernacular Bible translations spread because the press already existed as an unstoppable diffusion mechanism; the reading credits the technology's inherent properties (rapid, cheap, decentralized copying) rather than reformers' rhetorical or organizational strategy for the outcome. They benefit narratively from being cast as riders of an inevitable wave rather than as calculating strategic actors.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    moderate, generational, mobile, continental).

% Print shops proliferated across German and Swiss cities faster than any licensing authority could track. This reading treats their commercial dispersal as itself the causal mechanism — the technology's physical replicability made control structurally impossible, independent of any printer's individual choice to publish heterodox material.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_printers, beneficiary,
    moderate, biographical, mobile, continental).

% Scholars in the Eisenstein tradition who advance the technological-determinism reading professionally; the reading's persuasiveness and citation currency accrue to them as a body of interpretation, distinct from any material extraction.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, print_technology_historians, beneficiary,
    analytical, civilizational, analytical, global).

% Attempted licensing regimes, the Index of Forbidden Books, and local suppression campaigns against heterodox print runs. Within this reading, their resistance is cast as structurally futile from the outset — the printing press's exponential replication capacity is treated as having already exceeded any feasible enforcement apparatus, rendering the Church's institutional response a doomed rearguard action rather than a contest with uncertain outcome.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_authorities, payer,
    institutional, generational, trapped, continental).

% A sibling reading of the same kernel (not an actor within this constraint) holding that reformers and printers strategically exploited a neutral technology through deliberate distribution networks, coded pamphlets, and market targeting. This reading treats that agency as background noise against exogenous technological capacity, foreclosing strategic explanation as the primary causal account within this framework.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, strategic_deployment_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, strategic_deployment_reading).

% A sibling reading (not an actor within this constraint) holding that technology and reformist agency co-evolved iteratively — press capabilities shaped tactics, tactics shaped print innovation in turn. This reading's clean upstream-mountain/downstream-beneficiary structure has no room for this feedback loop without collapsing into the mutual-shaping account.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, mutual_shaping_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, mutual_shaping_reading).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this reading is a causal-historical claim, not a coordination mechanism. Its narrative function is to fix responsibility and inevitability: it coordinates historical explanation around a single dominant variable (technological capacity) rather than distributed human choice.
% TRANSFER_FUNCTION: The reading transfers explanatory credit from human strategic agency (reformers, printers, patrons) to an impersonal technological cause. This moves narrative agency and historiographical authority away from actors who made deliberate choices and toward the artifact itself.
% ABSENT_VOICES: Catholic institutional actors who mounted genuinely contested, sometimes locally effective censorship campaigns (Spanish and Roman Inquisitions had real, if partial, success suppressing print in some territories) are structurally muted by the 'futility' framing — their partial successes complicate a clean determinism story and are largely absent from the reading's own account. Print-shop laborers and financiers whose commercial calculations shaped which texts got printed are also absent, subsumed into the aggregate 'inevitable diffusion' narrative.
% DISAPPEARANCE_RATIONALE: If the deterministic reading disappeared as a historiographical framework, professional debate would rearrange around the surviving strategic-deployment and mutual-shaping accounts, and popular media-studies pedagogy (which frequently cites this reading as settled) would need retooling. Whether the underlying historical events (the Reformation's actual spread) would be understood differently is itself contested among historians — hence 'contested' rather than a clean verdict.
% FOUNDING_PROBLEM: The reading was built to explain why an ecclesiastical institution with centuries of accumulated coercive and doctrinal authority failed to suppress a movement it identified early and moved forcefully against — the explanatory gap between the Church's real coercive capacity and its actual containment failure.
% FOUNDING_PROBLEM_CORROBORATION: Elizabeth Eisenstein's own work and its subsequent citation network attest the problem as she framed it. Outside that tradition, historians of the book (Adrian Johns among them) have argued from print-shop records and regional case studies that censorship was frequently effective for extended periods in specific territories, corroborating that the 'inevitability' premise is not attested by archival evidence independent of the determinist tradition's own framing.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) — this is a historiographical/explanatory claim, not a material extraction mechanism, so ε here measures the degree to which the reading extracts explanatory legitimacy and professional authority from a contested causal question by treating it as settled. Suppression (0.35) reflects the reading's tendency to structurally exclude the strategic-agency and mutual-shaping accounts from serious consideration within its own framework, though it does not coercively prevent their scholarly existence elsewhere. Accessibility collapse is fairly high (0.68) because once the determinist framing is adopted, the alternative explanations become difficult to reintroduce without reframing the whole causal architecture. Resistance (0.4) reflects ongoing historiographical pushback from book historians using archival censorship-effectiveness data.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printers, and the historians who advance this reading are coded as beneficiaries: the reading assigns them favorable narrative positions (inevitable historical actors, or professionally validated interpreters) without requiring them to bear responsibility for strategic choices that might otherwise be scrutinized. The Catholic Church authorities are coded as the payer/target: their historical agency and partially successful local suppression efforts are discounted by the reading's foundational premise of futility, which is itself the extractive move — it takes explanatory credit away from a contested historical contest and assigns it to technological inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (explaining an explanatory gap between Church coercive capacity and containment failure) is contested rather than resolved: archival work on regional censorship effectiveness suggests the 'inevitability' framing overstates its case in specific well-documented instances (Spanish territories, some Italian city-states). Treating this as a live historiographical dispute rather than declaring the determinist account either fully vindicated or fully debunked prevents the corpus from mislabeling a genuinely contested empirical-historical question as either settled coordination-of-knowledge (rope) or a completed extraction of false authority (snare) — it remains a mountain claim under authentic contest, which is exactly why FSM evaluation and the omega variables below matter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_constructed_narrative,
    'Is the printing press''s causal role in the Reformation a genuine structural/technological mountain (an artifact whose replication properties made censorship physically and economically infeasible regardless of any actor''s choices), or is ''technological inevitability'' itself a constructed narrative that benefits reformist historiography and post-Enlightenment media theory by naturalizing what was actually a contingent, strategically contested outcome?',
    'Comparative archival analysis of regions/periods where censorship WAS locally effective for extended durations (e.g., Spain, parts of Italy under Inquisition control) versus regions where it collapsed rapidly (German territories). If suppression effectiveness correlates strongly with institutional capacity and political will rather than print-technology penetration alone, the determinism claim weakens in favor of the mutual-shaping or strategic-deployment readings.',
    'If genuinely deterministic, this reading is a defensible mountain (natural feature of the technology''s diffusion properties). If beneficiaries'' historiographical dominance is doing the naturalizing work, FSM logic applies: a claimed mountain with declared beneficiaries and a metric profile showing rising theater_ratio is exactly the signature the corpus is built to catch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_constructed_narrative, empirical, 'Whether press-driven censorship-impossibility is a genuine structural mountain or a constructed inevitability narrative benefiting the determinist historiographical tradition.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings of the press_reformation_causation kernel disagree — is it about the DIRECTION of causal primacy (technology-first vs. agency-first), the DEGREE of technological autonomy (full inevitability vs. contingent facilitation), or the TEMPORAL structure (one-shot cause vs. iterative feedback)?',
    'Structural comparison of the three constraint stories'' beneficiary/victim declarations and ε values: technological_determinism assigns causal primacy to the artifact and treats Church resistance as futile from the outset; strategic_deployment assigns primacy to reformer/printer agency and treats the press as neutral capacity; mutual_shaping denies that either pole is primary and models iterative co-evolution. The disagreement is located specifically in how much independent causal weight is assigned to the technology''s physical/economic replication properties versus human strategic choice.',
    'If the field''s evidence base ultimately favors mutual_shaping, this reading''s foreclosure of Church-agency and printer-strategy as decisive factors misrepresents a genuinely interactive historical process. If future scholarship instead recovers strong technology-autonomy evidence (e.g., quantitative studies showing diffusion rates outpacing any feasible enforcement scaling), this reading''s core claim is vindicated and the siblings become the minority positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement among the three kernel readings: causal direction, degree of autonomy, or temporal structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.3).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__technological_determinism, theater_ratio, 1517, 0.42).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__technological_determinism, theater_ratio, 1560, 0.52).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__technological_determinism, theater_ratio, 1600, 0.55).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__technological_determinism, theater_ratio, 1650, 0.58).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.2).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.25).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__technological_determinism, base_extractiveness, 1517, 0.32).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__technological_determinism, base_extractiveness, 1560, 0.38).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__technological_determinism, base_extractiveness, 1600, 0.4).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__technological_determinism, base_extractiveness, 1650, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the press_reformation_causation kernel, decomposed per the ε-invariance principle because the natural-language label 'the printing press caused the Reformation' conflates structurally distinct causal claims with different beneficiary structures and different implied agency distributions. technological_determinism (this story) treats the press as an upstream mountain and reformers as downstream beneficiaries of exogenous capacity, with Church resistance framed as futile. strategic_deployment treats the press as neutral capacity strategically exploited by purposeful agents. mutual_shaping treats technology and agency as co-evolving with neither pole primary. Each carries its own ε and its own claimed_type; they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
