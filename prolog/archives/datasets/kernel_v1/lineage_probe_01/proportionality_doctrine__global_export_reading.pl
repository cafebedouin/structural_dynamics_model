% ============================================================================
% CONSTRAINT STORY: proportionality_doctrine__global_export_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proportionality_doctrine__global_export_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proportionality_doctrine__global_export_reading
 *   human_readable: Proportionality Doctrine as Global Constitutional Export (Global Grammar Reading)
 *   domain: constitutional_law/doctrinal_diffusion
 *
 * SUMMARY:
 *   Proportionality doctrine—the four-step test requiring (1) legitimate aim,
 *   (2) suitability of means, (3) necessity relative to alternatives, (4)
 *   proportionality in the narrow sense—originated in German constitutional
 *   law and has become the dominant framework for rights-protective
 *   constitutional review globally. From Canada's Charter to South Africa's
 *   Constitutional Court to the European Court of Human Rights,
 *   proportionality now structures how courts balance state power against
 *   individual liberty. This constraint story instantiates the GLOBAL EXPORT
 *   READING: proportionality represents a successful transnational doctrinal
 *   diffusion that creates a common grammar of rights across constitutional
 *   jurisdictions. However, this reading coexists with rival interpretations:
 *   the BALANCING CRITIQUE READING (which argues that proportionality's final
 *   step disguises judicial preference as measurement) and the STRUCTURED
 *   REASON READING (which defends proportionality as genuine public reasoning
 *   discipline). The global export reading emphasizes coordination benefits
 *   (courts can now argue across borders, cite each other, converge on shared
 *   methodology) while acknowledging that this diffusion suppresses
 *   alternative review methods, particularly the American categorical rule
 *   tradition (strict scrutiny tiers, per se rules), which is now globally
 *   framed as provincial or backward.
 *
 * KEY AGENTS:
 *   - Transnational Constitutional Community: Primary beneficiary (institutional/arbitrage) — courts, law faculties, constitutional commissions adopting proportionality gain prestige, citation networks, and constitutional dialogue capacity
 *   - German Constitutional Doctrine Apparatus: Primary beneficiary (institutional/arbitrage) — German constitutional theory achieves global influence; Bundesverfassungsgericht reasoning becomes reference point for constitutional courts worldwide
 *   - American Categorical Rule Tradition: Primary victim (powerless/trapped) — categorical reasoning displaced globally; jurists trained in categorical logic face suppression of their methodological tools; career legitimacy now requires proportionality fluency
 *   - Balancing Critique Coalition: Secondary victim (organized/constrained) — jurists recognizing proportionality's opacity face suppression of critical perspectives; adopting proportionality undermines their own critical position
 *   - Reform Constituency: Moderate agent (moderate/mobile) — sees proportionality as temporary institutional scaffold enabling rights review in post-colonial democracies before refined doctrinal traditions mature
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent doctrinal choice as inevitable constitutional evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proportionality_doctrine__global_export_reading, 0.38).
domain_priors:suppression_score(proportionality_doctrine__global_export_reading, 0.48).
domain_priors:theater_ratio(proportionality_doctrine__global_export_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proportionality_doctrine__global_export_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(proportionality_doctrine__global_export_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(proportionality_doctrine__global_export_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proportionality_doctrine__global_export_reading, tangled_rope).
narrative_ontology:human_readable(proportionality_doctrine__global_export_reading, "Proportionality Doctrine as Global Constitutional Export (Global Grammar Reading)").
narrative_ontology:topic_domain(proportionality_doctrine__global_export_reading, "constitutional_law/doctrinal_diffusion").

domain_priors:requires_active_enforcement(proportionality_doctrine__global_export_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proportionality_doctrine__global_export_reading, '2cec97dc-16eb-49d8-9067-50eea945eef8').
narrative_ontology:cs_kernel_codification('2cec97dc-16eb-49d8-9067-50eea945eef8', formalized).
narrative_ontology:cs_authority_grounding('2cec97dc-16eb-49d8-9067-50eea945eef8', lineage).
narrative_ontology:cs_interpretation_layer_present('2cec97dc-16eb-49d8-9067-50eea945eef8').
narrative_ontology:cs_reading_relation('2cec97dc-16eb-49d8-9067-50eea945eef8', proportionality_doctrine__balancing_critique_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cec97dc-16eb-49d8-9067-50eea945eef8', proportionality_doctrine__structured_reason_reading, coexists_with).
narrative_ontology:cs_axiom('2cec97dc-16eb-49d8-9067-50eea945eef8', foundational, proportionality_represents_doctrinal_progress).
narrative_ontology:cs_axiom_status(proportionality_represents_doctrinal_progress, holdable).
narrative_ontology:cs_axiom_grounding('2cec97dc-16eb-49d8-9067-50eea945eef8', proportionality_represents_doctrinal_progress, instrumental).
narrative_ontology:cs_axiom('2cec97dc-16eb-49d8-9067-50eea945eef8', foundational, common_transnational_grammar_enables_rights_protection).
narrative_ontology:cs_axiom_status(common_transnational_grammar_enables_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('2cec97dc-16eb-49d8-9067-50eea945eef8', common_transnational_grammar_enables_rights_protection, instrumental).
narrative_ontology:cs_reference_frame('2cec97dc-16eb-49d8-9067-50eea945eef8', categorical_rule_fragmentation).
narrative_ontology:cs_drift_state('2cec97dc-16eb-49d8-9067-50eea945eef8', contemporary_global_proportionality_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2cec97dc-16eb-49d8-9067-50eea945eef8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(proportionality_doctrine__global_export_reading, proportionality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proportionality_doctrine__global_export_reading, transnational_constitutional_community).
narrative_ontology:constraint_beneficiary(proportionality_doctrine__global_export_reading, german_constitutional_doctrine).
narrative_ontology:constraint_victim(proportionality_doctrine__global_export_reading, categorical_rule_traditions).
narrative_ontology:constraint_victim(proportionality_doctrine__global_export_reading, american_exceptionalist_review).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMERICAN CATEGORICAL RULE TRADITION (SNARE) — Categorical rules (strict scrutiny tiers, per se rules, formalist tests) are globally displaced by the proportionality four-step. Domestic jurists who trained in categorical logic face suppression of their methodological tools; migration to proportionality is framed as inevitable doctrinal progress, not choice. Career paths and judicial legitimacy now require fluency in proportionality framing. Maximum extraction: the tradition is neither killed outright nor reformed, but rendered backward and provincial.
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BALANCING CRITIQUE COALITION (TANGLED ROPE) — Jurists who recognize proportionality's 'proportionate in the narrow sense' step as a disguised value judgment face both coordination benefit and extraction. They benefit from the common transnational vocabulary (can argue across borders, engage with constitutional courts globally). But they bear extraction costs: their critique of proportionality's opacity is suppressed in doctrinal discourse (framed as unsophisticated or obstructionist), and adopting the proportionality frame itself undermines their critical position.
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: TRANSNATIONAL CONSTITUTIONAL COMMUNITY (ROPE) — Courts, law faculties, and constitutional commissions from Canada to South Africa to the European Court of Human Rights benefit from adopting proportionality as a shared grammar. The framework enables cross-border constitutional dialogue, citation networks, and convergence. Extraction toward this agent is negligible or negative (they gain institutional prestige and coordination capacity). The beneficiary experiences the constraint as pure coordination.
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GERMAN CONSTITUTIONAL DOCTRINE APPARATUS (PITON) — German constitutional theory (Bundesverfassungsgericht precedent, academic infrastructure) exports proportionality but does not enforce adoption; the export succeeds through voluntary institutional convergence, not coercion. From this perspective, the constraint is partially performative — it celebrates German intellectual success while remaining analytically agnostic about whether proportionality actually constrains judicial discretion (the balancing critique suggests it does not). Theater ratio reflects that the export is both real (genuine doctrinal adoption) and partly mythologized (proportionality as inevitable progress in constitutional thought).
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM CONSTITUENCY (SCAFFOLD) — Jurists, legal scholars, and reform movements seeking to modernize constitutional review in post-colonial and transitional democracies see proportionality as a temporary bridge: a common framework that enables rights-protective review in jurisdictions where categorical rule traditions never took root. The scaffold logic: proportionality provides institutional capacity for a generation, then yields to refined frameworks (rights-as-capabilities approaches, democratic experimentalism) as these develop. Sunset rationale: as regional constitutional courts mature and develop their own doctrinal traditions, reliance on German-exportable proportionality decreases.
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational timescale, proportionality appears as a natural evolution of rights-based constitutionalism: whenever a legal order must balance state power against individual liberty, some form of proportionality analysis becomes inevitable. The four-step structure emerges not as German intellectual export but as a structural requirement of any constitutional system protecting fundamental rights. However, the beneficiary/victim structure reveals this as a false summit: the naturalized 'inevitable evolution' narrative masks the doctrinal power asymmetry (displacement of categorical rules, suppression of balancing critique) that serves specific institutional interests.
constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proportionality_doctrine__global_export_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proportionality_doctrine__global_export_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(proportionality_doctrine__global_export_reading, TR),
    TR >= 0.70.

:- end_tests(proportionality_doctrine__global_export_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The global export of proportionality doctrine does extract doctrinal resources (suppresses categorical rules, privileges German constitutional reasoning) but also delivers genuine coordination benefits (common vocabulary enabling cross-border constitutional dialogue). The extraction is asymmetric—concentrated on American categorical traditions and critical voices—but not total. Extraction rises over time (from 0.12 to 0.38) as proportionality becomes institutionalized globally and alternatives become harder to defend. Suppression (0.48): Moderate-high. Suppression mechanisms include: (1) framing categorical reasoning as backward or unsophisticated, (2) requiring doctrinal fluency in proportionality for career advancement in constitutional law, (3) citation networks that privilege proportionality-based reasoning and marginalize critiques, (4) institutional pressure toward convergence with global constitutional courts. Suppression is not absolute—some jurisdictions maintain categorical elements, critique persists in academic discourse—but barriers to exit from proportionality adoption are substantial. Theater ratio (0.52): Moderate. The global export narrative is partly functional (proportionality does enable transnational constitutional dialogue) and partly performative (celebrating German intellectual success, naturalizing doctrinal choice as inevitable progress). Theater rises over time (from 0.35 to 0.52) as the mythology of inevitable proportionality consolidates while the lived experience of judicial discretion beneath the final step becomes more apparent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates perspectival gap through institutional position. The transnational constitutional community (institutional/arbitrage) experiences the constraint as pure coordination—proportionality provides common language and prestige. The American categorical tradition (powerless/trapped) experiences it as suppression—their methodological tools are globally discredited. The balancing critics (organized/constrained) experience mixed signals: they benefit from proportionality's common grammar but are suppressed when they critique its opacity. The reform constituency (moderate/mobile) sees it as temporary (scaffold)—useful for a generation, then to be refined or replaced. The German doctrine apparatus (institutional/arbitrage) experiences it as net benefit—intellectual influence without enforcement cost. The analytical observer risks naturalizing this distribution as inevitable doctrinal evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from structural position: who benefits and who bears extraction costs. The transnational constitutional community and German doctrine apparatus are beneficiaries (d ≈ 0.15, low extraction experienced). The American categorical tradition and balancing critics are victims (d ≈ 0.80, high extraction experienced). The reform constituency is mixed (d ≈ 0.55, moderate extraction with exit pathway). For institutional actors, the derivation chain prioritizes: (1) beneficiary status + arbitrage exit → low d → negative f(d); (2) victim status + trapped or constrained exit → high d → high f(d). The piton perspective (German doctrine apparatus) derives d from the institutional power position, which permits arbitrage but experiences the constraint as increasingly performative rather than functionally constraining.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy resolution through reading differentiation. The global-export reading (this one) emphasizes coordination benefits and doctrinal diffusion. The balancing-critique reading emphasizes suppression of the final step's opacity. The structured-reason reading emphasizes public reasoning discipline. No single type is correct—rather, each reading instantiates a different structural aspect of the same kernel. The mandatrophy resolves when the analytical apparatus recognizes that proportionality doctrine is simultaneously: (a) a successful coordination mechanism enabling transnational constitutional dialogue (Rope from beneficiary view), (b) a suppression mechanism displacing categorical reasoning (Snare from victim view), (c) a temporary institutional scaffold (Scaffold from reform view), (d) a performative ritual (Piton from German apparatus view), and (e) a naturalized doctrinal imperative (false Mountain from analytical view).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_opacity_degree,
    'Does proportionality''s final step (''proportionate in the narrow sense'') function as a genuine constraint on judicial discretion, or is it a post hoc rationalization device that permits unstated judicial preferences?',
    'Comparative doctrinal analysis: tracking how courts apply the final step across jurisdictions; correlation between stated proportionality reasoning and precedent-based outcomes; meta-analysis of whether the final step ever reverses a judicial predisposition established in earlier steps.',
    'If genuine constraint: proportionality is closer to structured-reason reading (Rope). If post hoc rationalization: proportionality is closer to balancing-critique reading (Snare/Tangled Rope from critic''s position). Affects whether export suppresses rival methods or merely extends the same opacity globally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_opacity_degree, empirical, 'Whether proportionality''s final balancing step constrains discretion or rationalizes preference').

omega_variable(
    doctrinal_imperialism_vs_convergence,
    'Is global adoption of proportionality driven by genuine doctrinal merit and problem-solving capacity, or by institutional power asymmetries and deference to German constitutional prestige?',
    'Institutional analysis: tracking adoption pathways (was proportionality adopted through colonial inheritance, treaty obligation, mimicry of peer courts, or deliberate doctrinal choice?); studying resistance cases and explaining which jurisdictions rejected proportionality and why; measuring doctrinal citation patterns to distinguish voluntary convergence from citation coercion.',
    'If merit-driven convergence: this reading (global export via coordination) is correct; extraction is secondary. If power-driven: the constraint is better understood as doctrinal imperialism (suppression mechanism); beneficiary is the transnational constitutional elite, not ''the transnational community.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_imperialism_vs_convergence, empirical, 'Whether proportionality adoption is merit-driven or power-driven').

omega_variable(
    categorical_rule_extinction_timeline,
    'Is the categorical rule tradition permanently suppressed, or will it undergo regional revival as post-proportionality jurisdictions develop sufficient constitutional maturity to defend local doctrinal traditions?',
    'Historical monitoring: tracking whether any constitutional court returns to categorical reasoning after proportionality adoption; studying emerging doctrinal movements in the Global South that might reconstruct categorical logics adapted to local contexts; analyzing generational shifts in legal education.',
    'If extinction permanent: suppression value stable at current levels. If regional revival possible: suppression is temporary (constraint reclassifies toward scaffold); the global export reading describes a generational phenomenon, not a permanent doctrinal regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_rule_extinction_timeline, empirical, 'Whether categorical rule tradition extinction is permanent or cyclical').

omega_variable(
    reading_contest_foreclosure,
    'Does adoption of the global-export reading logically foreclose the balancing-critique reading, or can both coexist within the same institutional framework?',
    'Logical analysis: testing whether accepting proportionality-as-successful-export requires rejecting the critique that proportionality masks judicial preference. If a court can simultaneously endorse the global adoption narrative AND acknowledge opacity in the final step, readings coexist. If courts must choose between celebrating proportionality and critiquing it, they foreclose one reading.',
    'If foreclose: global-export and balancing-critique readings are genuinely incompatible; adoption of this reading suppresses the critical reading. If coexist: both readings remain live and rival positions held by different constituencies within the constitutional community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether this reading forecloses or coexists with the balancing-critique reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proportionality_doctrine__global_export_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(propex_tr_t0, proportionality_doctrine__global_export_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(propex_tr_t10, proportionality_doctrine__global_export_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(propex_tr_t20, proportionality_doctrine__global_export_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(propex_be_t0, proportionality_doctrine__global_export_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(propex_be_t10, proportionality_doctrine__global_export_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(propex_be_t20, proportionality_doctrine__global_export_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(propex_su_t0, proportionality_doctrine__global_export_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(propex_su_t10, proportionality_doctrine__global_export_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(propex_su_t20, proportionality_doctrine__global_export_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proportionality_doctrine__global_export_reading, information_standard).
narrative_ontology:affects_constraint(proportionality_doctrine__global_export_reading, proportionality_doctrine__balancing_critique_reading).
narrative_ontology:affects_constraint(proportionality_doctrine__global_export_reading, proportionality_doctrine__structured_reason_reading).
narrative_ontology:affects_constraint(proportionality_doctrine__global_export_reading, american_strict_scrutiny_decline).
narrative_ontology:affects_constraint(proportionality_doctrine__global_export_reading, categorical_rule_suppression_mechanism).

% DUAL FORMULATION NOTE:
% The proportionality doctrine kernel decomposed into three constraint stories, each instantiating a different reading: (1) global_export_reading emphasizes coordination and diffusion (this story); (2) balancing_critique_reading emphasizes opacity and judicial discretion; (3) structured_reason_reading emphasizes public reasoning discipline. Each has its own extractiveness value reflecting the reading's structural emphasis. The global-export reading (ε=0.38) represents the reading that emphasizes coordination benefits; the balancing-critique reading (ε≈0.65 estimated) represents the reading that emphasizes suppression mechanisms; the structured-reason reading (ε≈0.15 estimated) represents the reading emphasizing genuine constraint on discretion. The three stories are linked via network.affects_constraints to show that they are readings of a single kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proportionality_doctrine__global_export_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
