% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Printing Press as Deterministic Cause of the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism reading of the
 *   press-Reformation kernel: the printing press is treated as an exogenous,
 *   self-actualizing capacity whose mechanical replication and distribution
 *   logic made censorship structurally impossible and vernacular scripture
 *   structurally inevitable, independent of any reformer's strategy or the
 *   Church's countermeasures. As a Mountain reading, ε is authored low (this
 *   reading regards the outcome as a natural consequence of
 *   physical/informational scaling, not as an extraction relationship), but
 *   the reading is authored as an FSM candidate: it names concrete
 *   beneficiaries (reformers, printers, literate laity), which is exactly the
 *   tension the false-summit signature exists to surface — a 'natural law'
 *   framing that happens to line up neatly with who profits from it. The
 *   claimed type (mountain) and the metrics (moderate accessibility_collapse,
 *   non-trivial theater_ratio rising over the interval) are authored
 *   independently; where they diverge is the datum.
 *
 * KEY AGENTS:
 *   - protestant_reformers: beneficiary of exogenous technological capacity, moderate power, constrained exit
 *   - print_shop_proprietors: beneficiary/commercial operator, moderate power, mobile exit
 *   - vernacular_literate_laity: powerless beneficiary of downstream access
 *   - catholic_ecclesiastical_authority: institutional payer, trapped exit, resistance framed as futile
 *   - manuscript_scriptoria_and_scribal_guilds: organized payer displaced by the technology
 *   - media_historians: analytical observer assessing the determinism claim's evidentiary weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.28).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.15).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'fedd372f-6f59-4153-ac8f-b59b5c744935').
narrative_ontology:cs_kernel_codification('fedd372f-6f59-4153-ac8f-b59b5c744935', distributed).
narrative_ontology:cs_authority_grounding('fedd372f-6f59-4153-ac8f-b59b5c744935', distributed).
narrative_ontology:cs_reading_relation('fedd372f-6f59-4153-ac8f-b59b5c744935', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('fedd372f-6f59-4153-ac8f-b59b5c744935', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('fedd372f-6f59-4153-ac8f-b59b5c744935', foundational, technology_as_exogenous_unstoppable_force).
narrative_ontology:cs_axiom_status(technology_as_exogenous_unstoppable_force, holdable).
narrative_ontology:cs_axiom_grounding('fedd372f-6f59-4153-ac8f-b59b5c744935', technology_as_exogenous_unstoppable_force, empirically_contingent).
narrative_ontology:cs_axiom('fedd372f-6f59-4153-ac8f-b59b5c744935', secondary, church_resistance_structurally_futile_not_merely_outmatched).
narrative_ontology:cs_axiom_status(church_resistance_structurally_futile_not_merely_outmatched, holdable).
narrative_ontology:cs_axiom_grounding('fedd372f-6f59-4153-ac8f-b59b5c744935', church_resistance_structurally_futile_not_merely_outmatched, empirically_contingent).
narrative_ontology:cs_reference_frame('fedd372f-6f59-4153-ac8f-b59b5c744935', print_capacity_as_exogenous_mountain).
narrative_ontology:cs_drift_state('fedd372f-6f59-4153-ac8f-b59b5c744935', post_annales_school_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fedd372f-6f59-4153-ac8f-b59b5c744935', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, print_shop_proprietors).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literate_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_ecclesiastical_authority).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, manuscript_scriptoria_and_scribal_guilds).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_determinism_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, inevitability_of_vernacular_scripture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers such as Luther are cast, in this reading, as passive downstream recipients of an exogenous technological capacity: the press existed and its diffusion logic made vernacular dissemination of doctrine unstoppable regardless of any individual reformer's strategy or the Church's countermeasures. Their pamphlets ride a wave whose direction and velocity they did not set.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    moderate, generational, constrained, continental).

% Owners of movable-type workshops profit from the mechanical multiplication of texts. In this reading their commercial choices are epiphenomenal to the technology's own diffusion curve — the press would have flooded Europe with vernacular texts whoever operated the presses.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, print_shop_proprietors, beneficiary,
    moderate, biographical, mobile, regional).

% Ordinary readers gain access to scripture and polemic in their own tongues. This reading treats their new access as the automatic output of a technological threshold being crossed, not as the product of anyone's deliberate campaign to reach them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_literate_laity, beneficiary,
    powerless, generational, trapped, continental).

% The Church attempts indices of forbidden books, licensing regimes, and local suppression campaigns. In this reading, every countermeasure is structurally futile: the technology's replication rate and geographic dispersal outrun any centralized censorship apparatus by mechanical necessity, not by any deficiency of will or resources on the Church's part. Its resistance is real but pre-defeated.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_ecclesiastical_authority, payer,
    institutional, civilizational, trapped, continental).

% Institutions whose economic and cultural function (hand-copied manuscript production, controlled textual transmission) is rendered obsolete by mechanical printing. Their displacement is framed as a side effect of the technology's inherent superiority, not a contested outcome.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, manuscript_scriptoria_and_scribal_guilds, payer,
    organized, biographical, trapped, regional).

% Scholars evaluate whether the causal weight assigned to the press as an autonomous, self-actualizing force is empirically supportable or whether it retrojects inevitability onto a contingent, agent-driven historical process.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this reading describes a one-way causal mechanism (a technology diffusing according to its own logic) rather than a negotiated arrangement solving a collective-action problem among parties.
% TRANSFER_FUNCTION: The reading claims the press transfers informational capacity from centralized ecclesiastical control to distributed lay and reformist hands as a mechanical inevitability — not as a resource anyone bargains over, but as a byproduct of replication physics and distribution economics.
% ABSENT_VOICES: Individual reformers' strategic choices, printers' commercial calculations, and local political patrons who selectively protected or suppressed presses are structurally absent from this reading's account — they are treated as noise around a deterministic signal, not as causally load-bearing.
% DISAPPEARANCE_RATIONALE: If 'the press as determinative cause' were removed as an explanatory frame, historians who hold this reading would say the Reformation's vernacular scale and speed cannot be reconstructed at all without positing the technology as the binding constraint; rival readings (mutual_shaping, strategic_deployment) would say the historical record rearranges very little, since the same outcomes are attributed instead to agency and negotiated deployment.
% FOUNDING_PROBLEM: Explaining why the Reformation achieved a scale, speed, and geographic simultaneity that prior heterodox movements (Hussites, Lollards) never achieved despite comparable theological content and comparable ecclesiastical opposition.
% FOUNDING_PROBLEM_CORROBORATION: Media-history scholars outside the confessional traditions that benefit from either a triumphalist Protestant narrative or a defensive Catholic one have examined print-run and literacy data and are divided: some (e.g., quantitative book-history scholars) find diffusion-curve evidence consistent with a strong technological threshold effect; others find the correlation better explained by pre-existing demand and elite patronage networks that would have used any available medium. No consensus corroboration exists outside the two confessional beneficiary traditions.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because a deterministic reading assigns the outcome to physical/technological necessity rather than to anyone extracting a rent from anyone else — but it rises over the century as the doctrine hardens into a naturalized historiographical common sense that happens to flatter the winning confessional side. Theater ratio rises sharply (0.1 to 0.4) as post-hoc historiographical narrative-building (the 'press caused the Reformation' story used in Protestant self-legitimation) increasingly substitutes for close causal analysis. Accessibility collapse is authored high (0.72) because once the determinist frame takes hold, alternative explanations (agency, patronage, strategic deployment) become difficult to articulate within confessional historiography — but not as high as a genuine mountain, because professional historians visibly contest it (resistance 0.35, non-trivial).
 *
 * PERSPECTIVAL GAP:
 *   From the reformers' and printers' seats, the technology's inevitability is a comforting frame — it removes agency and therefore removes responsibility or credit-claiming disputes, presenting the outcome as history's mechanism rather than their choice. From the Church's seat, the same frame is exculpatory in a different direction: it explains failure without conceding strategic or moral defeat. From the analytical historian's seat, the frame is a testable (and contested) causal claim, not a settled mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformers, printers, literate laity) are placed near the low-d end because the determinist reading, if true, means they did not have to expend costly effort or bear risk to win — the technology did the work for them, which subsidizes their position. The Church and scribal guilds sit at high-d because the reading assigns them a fixed, futile-resistance role: their institutional investment in censorship is structurally wasted by assumption, which is itself an extraction of legitimacy and resources with no compensating benefit in this reading's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining the Reformation's unusual scale/speed/simultaneity) is contested as live vs. dead: it remains a genuine explanatory puzzle, but the technological-determinism ANSWER to it has arguably outlived its evidentiary support and now persists partly as historiographical convention (theater_ratio rising) rather than as an actively defended empirical thesis. This is not full mandatrophy resolution — the puzzle itself is still live — but the specific deterministic answer shows drift consistent with an answer that has calcified past its evidentiary warrant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_beneficiary_structure,
    'Is the press-caused-Reformation claim a genuine natural-law-like technological determinism, or is it a constructed historiographical convenience that happens to benefit identifiable confessional and commercial actors (reformers, printers, literate laity) by naturalizing an outcome that was actually contingent on strategic choices?',
    'Comparative case analysis: examine heterodox movements with press access that did NOT achieve Reformation-scale diffusion (e.g., certain print-adjacent reform attempts suppressed successfully), and press-poor regions where vernacular reform nonetheless spread through non-print channels. Convergent findings across these controls would support determinism; divergent findings would support strategic or co-produced accounts.',
    'If the determinist claim is a naturalized cover story, this Mountain reading reclassifies toward tangled_rope (the false_summit_mountain signature) — coordination function (mass vernacular access) coexisting with asymmetric extraction (confessional legitimacy accruing disproportionately to reformers while the Church''s resistance is retroactively rendered illegitimate by narrative fiat rather than by demonstrated impossibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_vs_beneficiary_structure, conceptual, 'Natural-law determinism vs. constructed/naturalized historiographical convenience benefiting reformers and printers.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (technological_determinism, strategic_deployment, mutual_shaping) locate their disagreement — is it about the DIRECTION of causal arrows (tech-to-agency vs agency-to-tech), the DEGREE of counterfactual dependence (could the Reformation have happened without print at all), or the UNIT OF ANALYSIS (aggregate diffusion statistics vs. individual reformer decisions)?',
    'Formal counterfactual history exercise: model Reformation outcomes under press-absent and reformer-absent conditions separately using available quantitative print-run and vernacular-literacy data, and compare which counterfactual removal produces the larger predicted divergence from the observed historical outcome.',
    'Resolving this would determine whether the three readings are genuinely incompatible framings (forecloses) or compatible partial accounts operating at different levels of description (coexists_with/influences) — which affects how much weight the technological_determinism reading''s ε and beneficiary claims should carry against its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Committer-structure ambiguity: what specific structural element the sibling kernel readings actually disagree about.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.15).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.2).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__technological_determinism, theater_ratio, 1517, 0.28).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__technological_determinism, theater_ratio, 1540, 0.35).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__technological_determinism, theater_ratio, 1560, 0.4).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.14).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.18).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__technological_determinism, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__technological_determinism, base_extractiveness, 1540, 0.26).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__technological_determinism, base_extractiveness, 1560, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causation kernel. technological_determinism (this story) authors the press as an exogenous Mountain-like force with reformers/printers as downstream beneficiaries and Church resistance as structurally futile — ε is authored low-moderate and rising, reflecting naturalized inevitability rather than active extraction. strategic_deployment authors the press as neutral capacity strategically captured by purposeful actors — a different beneficiary/agency structure with likely higher ε (deliberate capture rather than natural diffusion). mutual_shaping authors a co-evolutionary account where technology and agency mutually constitute each other — the hardest case to locate ε in, since neither pole is purely upstream. All three share the same underlying historical episode but are structurally distinct constraints per the ε-invariance principle; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
