% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Printing Press as Exogenous Causal Driver of the Reformation
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   This reading instantiates the technological determinist claim: the
 *   printing press (c. 1440) operated as an exogenous mountain-like
 *   constraint on the European religious order. By mechanically reproducing
 *   text at scale, it made systematic censorship impossible (the cost of
 *   suppressing printed vernacular materials exceeded any institutional
 *   capacity) and made vernacular scripture inevitable (the economics of
 *   printing favored mass-market vernacular editions over Latin manuscripts).
 *   The Reformation is not caused by the press in a simple monocausal sense,
 *   but the press constitutes a structural boundary condition — a mountain —
 *   that the Catholic Church could not enforce its way across, and that
 *   reformers did not build but inherited. The constraint is the press's
 *   material capacity as a fixed feature of the historical situation, not a
 *   human arrangement.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiaries (powerless/moderate -> organized) — inherited exogenous capacity to bypass censorship and reach vernacular publics
 *   - catholic_magisterium: Primary victims (institutional) — lost monopoly on scriptural interpretation and censorship enforcement; resistance structurally futile against press economics
 *   - vernacular_printers: Secondary beneficiaries (organized) — commercial actors who gained from the new production economics the press created
 *   - latin_literate_clerisy: Secondary victims (organized) — saw their gatekeeping role and professional basis eroded by vernacular diffusion
 *   - literate_lay_public: Tertiary beneficiaries (powerless -> moderate) — gained direct access to scripture and religious argument in vernacular
 *   - analytical_observer: Observer (analytical) — sees the press as a mountain that restructured the possibility space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.12).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.05).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.12).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Exogenous Causal Driver of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "historical/technological/religious").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'd839c786-9825-45ed-9915-b0b05dff1959').
narrative_ontology:cs_kernel_codification('d839c786-9825-45ed-9915-b0b05dff1959', distributed).
narrative_ontology:cs_authority_grounding('d839c786-9825-45ed-9915-b0b05dff1959', distributed).
narrative_ontology:cs_reading_relation('d839c786-9825-45ed-9915-b0b05dff1959', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('d839c786-9825-45ed-9915-b0b05dff1959', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('d839c786-9825-45ed-9915-b0b05dff1959', foundational, technology_as_exogenous_mountain).
narrative_ontology:cs_axiom_status(technology_as_exogenous_mountain, holdable).
narrative_ontology:cs_axiom_grounding('d839c786-9825-45ed-9915-b0b05dff1959', technology_as_exogenous_mountain, empirically_contingent).
narrative_ontology:cs_axiom('d839c786-9825-45ed-9915-b0b05dff1959', foundational, censorship_impossibility_under_print).
narrative_ontology:cs_axiom_status(censorship_impossibility_under_print, holdable).
narrative_ontology:cs_axiom_grounding('d839c786-9825-45ed-9915-b0b05dff1959', censorship_impossibility_under_print, empirically_contingent).
narrative_ontology:cs_axiom('d839c786-9825-45ed-9915-b0b05dff1959', secondary, vernacular_inevitability_from_print_economics).
narrative_ontology:cs_axiom_status(vernacular_inevitability_from_print_economics, holdable).
narrative_ontology:cs_axiom_grounding('d839c786-9825-45ed-9915-b0b05dff1959', vernacular_inevitability_from_print_economics, empirically_contingent).
narrative_ontology:cs_reference_frame('d839c786-9825-45ed-9915-b0b05dff1959', pre_print_manuscript_order).
narrative_ontology:cs_drift_state('d839c786-9825-45ed-9915-b0b05dff1959', post_reformation_print_culture, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d839c786-9825-45ed-9915-b0b05dff1959', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, literate_lay_public).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_magisterium).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, latin_literate_clerisy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, vernacular_printers).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_as_independent_causal_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherited the press as an exogenous capability. Could not have built it; could not have achieved vernacular reach without it. Their movement's scale and speed structurally depend on print economics. Exit from the press's logic would mean returning to manuscript circulation — structurally foreclosed for mass communication.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    moderate, biographical, constrained, continental).

% Lost the monopoly on scriptural interpretation and censorship enforcement that defined its authority. The press's economics (low marginal cost per copy, wide geographic reach) made systematic suppression structurally infeasible. Could not adopt the press without legitimizing vernacular scripture; could not suppress it without prohibitive cost. Trapped by the mountain's logic.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_magisterium, payer,
    institutional, generational, trapped, continental).

% Commercial actors who invested in press technology and profited from the new production economics. They bore costs (capital, risk, censorship persecution) but gained a structurally new market. Their exit options were mobile — they could relocate to tolerant cities (Basel, Strasbourg, Geneva, Antwerp) where press freedom existed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_printers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, vernacular_printers, payer).

% Professional gatekeepers of Latin textual culture. The press flooded the market with vernacular alternatives, eroding the scarcity that grounded their professional authority. Could not prevent vernacular diffusion; could only adapt (some became reformers, some doubled down on Latin humanism). Exit was constrained by career investment in Latin literacy.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, latin_literate_clerisy, payer,
    organized, biographical, constrained, continental).

% Gained direct access to scripture and religious argument in vernacular for the first time at scale. This access was not a choice they made but a structural consequence of press economics. Their exit from this access would require returning to clerical mediation — structurally difficult once direct access existed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, literate_lay_public, beneficiary,
    powerless, biographical, constrained, continental).

% Observes the press as a mountain: a fixed alteration of the communication possibility space that restructured religious, political, and intellectual life across centuries. Does not experience the constraint as extraction or coordination but as a boundary condition.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solves the coordination problem of mass communication: how to reproduce identical text at scale across geography without scribal error or gatekeeper control. It coordinates printers, distributors, and readers around a shared material standard (the printed edition).
% TRANSFER_FUNCTION: Moves control of textual reproduction from scribal guilds and ecclesiastical censors to commercial printers and, ultimately, to the reading public. Moves interpretive authority from Latin clerisy to vernacular readers. Moves economic value from manuscript production to print commerce.
% ABSENT_VOICES: Illiterate peasantry (majority of population) — would experience the press's effects only indirectly through preaching and oral transmission of printed materials. Their voice is absent from the textual record. Women — largely excluded from Latin literacy and early print authorship; their experience of vernacular access is mediated through male translators/printers. Jewish and Muslim communities in Europe — subject to separate censorship regimes (Hebrew/Arabic printing licenses); their experience of the press differs from the Christian mainstream.
% DISAPPEARANCE_RATIONALE: If the printing press vanished in 1480 (after establishment but before Reformation), the Reformation as a mass movement would not occur. Vernacular scripture would remain manuscript-limited. Catholic censorship would regain effectiveness. The entire trajectory of European religious, political, and intellectual history reorganizes around manuscript constraints. The press is a genuine mountain: its removal rearranges the world.
% FOUNDING_PROBLEM: The press was not 'built to solve' a problem — it emerged from a convergence of technological innovations (movable type, oil-based ink, paper, screw press) and commercial incentives. The founding problem framing is inapplicable to a mountain. The *institutional responses* to the press (censorship machinery, vernacular translation projects) were built to solve problems the press created.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) treats the press as an exogenous agent; Febvre & Martin (1958) emphasize social preconditions; Pettegree (2010) shows the press as a commercial venture shaped by market forces. No consensus exists on whether the press is a mountain or a socially embedded technology. The corroboration is that historians *disagree* — the kernel is genuinely contested.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The press's material properties (movable type, oil-based ink, screw press mechanics, paper supply chains) constitute a genuine mountain: they persist regardless of who defends them, no party collects rents from their operation, and they impose hard limits on what censorship can achieve. The low extractiveness (0.12) reflects that the press itself extracts nothing — it is a capability. The near-zero suppression (0.05) reflects that the press does not coerce; it enables. The high accessibility_collapse (0.92) reflects that once the press existed, the alternative (manuscript-only communication) was structurally foreclosed for mass communication. The near-zero resistance (0.03) reflects that the press as technology met no resistance — only its *consequences* were contested. The modest theater_ratio (0.08) captures that later historiographical debates about 'the press caused the Reformation' perform a causal argument but the press itself is not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat, the press is a gift — an exogenous capability that makes their project possible (rope-like coordination function). From the magisterium seat, the press is a catastrophe — an exogenous force that destroys their enforcement capacity (snare-like extraction of control). The analytical seat sees the mountain: the press is neither gift nor catastrophe but a fixed alteration of the possibility space. The engine computes this divergence from the structural data; the claim (mountain) reflects the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the press's capabilities. Reformers and vernacular publics are beneficiaries (d near 0.0) — the press subsidizes their reach, bypassing gatekeepers. The Catholic magisterium and Latin clerisy are targets (d near 1.0) — the press extracts their monopoly control. Printers sit near symmetric (d ~ 0.5) — they invest in the technology and profit from it, but do not control its structural effects. The analytical observer sits at d = 0.5 by definition. No overrides needed; the beneficiary/victim declarations and exit structures (reformers: constrained -> mobile via print networks; magisterium: institutional -> trapped by press economics) derive the correct directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has no mandate to atrophy — it is a technological capability, not an institutional arrangement. The mandatrophy question applies to the *institutional responses* to the press (Index of Prohibited Books, Tridentine censorship machinery), which are separate constraints (scaffold/snare) that did suffer mandatrophy. The press itself remains a mountain; its effects persist regardless of institutional adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_as_mountain_vs_constructed_capability,
    'Is the printing press''s causal role a genuine natural-law-like mountain (fixed material capacities), or a constructed historical outcome contingent on prior social choices (paper mills, trade routes, urban literacy, capital formation)?',
    'Counterfactual historical analysis: if European paper supply, trade networks, or urban literacy had developed differently, would the press still have emerged with the same causal force? Comparative analysis with Chinese/Korean movable type (earlier, different social uptake).',
    'If the press''s emergence and causal force are contingent on prior social choices, the mountain claim is a false summit — the constraint is a tangled_rope (coordination of trade/literacy/capital + extraction by early printers/publishers). FSM would trigger reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_mountain_vs_constructed_capability, conceptual, 'Natural-law status of the press as exogenous mountain vs. socially constructed capability').

omega_variable(
    censorship_impossibility_degree,
    'Did the press make censorship *impossible* (absolute mountain) or merely *radically more expensive* (high-cost rope/snare)?',
    'Quantitative analysis of censorship effectiveness pre/post press: survival rates of prohibited texts, cost per suppressed edition, geographic reach of enforcement. Compare Catholic Index effectiveness in print vs. manuscript eras.',
    'If censorship remained partially effective (e.g., in Spain/Italy via Inquisition + Index), the ''impossible'' claim overstates the mountain''s absoluteness — the constraint is a high-suppression tangled_rope, not a zero-suppression mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_impossibility_degree, empirical, 'Absolute vs. marginal impossibility of censorship under print').

omega_variable(
    kernel_reading_framing,
    'Does the technological_determinism reading foreclose, coexist with, or influence the strategic_deployment and mutual_shaping readings?',
    'Historiographical analysis: do scholars who hold the determinist reading *logically reject* the others (forecloses), or do they hold determinism as a macro-frame while granting agency at micro-level (coexists_with)? Does the determinist frame shape funding/institutional legitimacy for the other readings (influences)?',
    'Determines reading_relations in cs_structure. If forecloses, the kernel has genuine logical exclusion; if coexists_with, the kernel hosts plural live positions; if influences, determinism structures the field''s resource allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Structural relationship of this reading to sibling readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1580).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1440, press_reformation_causation__technological_determinism, theater_ratio, 1440, 0.02).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1460, press_reformation_causation__technological_determinism, theater_ratio, 1460, 0.04).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1480, press_reformation_causation__technological_determinism, theater_ratio, 1480, 0.06).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.07).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1520, press_reformation_causation__technological_determinism, theater_ratio, 1520, 0.08).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1540, press_reformation_causation__technological_determinism, theater_ratio, 1540, 0.08).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1560, press_reformation_causation__technological_determinism, theater_ratio, 1560, 0.08).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_tr_t1580, press_reformation_causation__technological_determinism, theater_ratio, 1580, 0.08).

% Extraction over time
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1440, press_reformation_causation__technological_determinism, base_extractiveness, 1440, 0.08).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1460, press_reformation_causation__technological_determinism, base_extractiveness, 1460, 0.1).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1480, press_reformation_causation__technological_determinism, base_extractiveness, 1480, 0.11).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1520, press_reformation_causation__technological_determinism, base_extractiveness, 1520, 0.12).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1540, press_reformation_causation__technological_determinism, base_extractiveness, 1540, 0.12).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1560, press_reformation_causation__technological_determinism, base_extractiveness, 1560, 0.12).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_be_t1580, press_reformation_causation__technological_determinism, base_extractiveness, 1580, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1440, press_reformation_causation__technological_determinism, suppression_requirement, 1440, 0.02).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1460, press_reformation_causation__technological_determinism, suppression_requirement, 1460, 0.03).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1480, press_reformation_causation__technological_determinism, suppression_requirement, 1480, 0.04).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1520, press_reformation_causation__technological_determinism, suppression_requirement, 1520, 0.05).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1540, press_reformation_causation__technological_determinism, suppression_requirement, 1540, 0.05).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1560, press_reformation_causation__technological_determinism, suppression_requirement, 1560, 0.05).
narrative_ontology:measurement(press_reformation_causation__technological_determinism_su_t1580, press_reformation_causation__technological_determinism, suppression_requirement, 1580, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.02).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, catholic_index_censorship_machinery).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, vernacular_bible_translation_network).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, protestant_pamphlet_circulation).

% DUAL FORMULATION NOTE:
% This constraint (technological_determinism) is one of three readings of the press_reformation_causation kernel. The other readings (strategic_deployment, mutual_shaping) instantiate different constraints with different ε, beneficiaries, and types. This reading claims mountain; the others will likely claim tangled_rope (strategic_deployment: coordination of reformers/printers + extraction from Catholic control) and rope (mutual_shaping: genuine co-evolutionary coordination). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
