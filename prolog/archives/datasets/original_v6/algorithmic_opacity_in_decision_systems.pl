% ============================================================================
% CONSTRAINT STORY: algorithmic_opacity_in_decision_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_opacity_in_decision_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_opacity_in_decision_systems
 *   human_readable: Algorithmic Opacity in Decision Systems
 *   domain: technology/governance/political_economy
 *
 * SUMMARY:
 *   Algorithmic opacity in decision systems creates a structural conflict
 *   between legitimate coordination needs (technology firms protecting
 *   intellectual property, institutions managing liability and consistency)
 *   and asymmetric extraction (systematic information asymmetries enabling
 *   discrimination, suppression of contestation, and institutional avoidance
 *   of accountability). The constraint exhibits all three middle-tier types
 *   depending on perspective: genuine rope from the developer's view (opacity
 *   enables IP protection coordination), tangled rope from the deploying
 *   institution's view (opacity both coordinates decisions and shields from
 *   accountability), and snare from the algorithmic subject's view (trapped
 *   with no recourse and no transparency). The analytical observer sees a
 *   mixed constraint that satisfies tangled rope criteria: beneficiaries
 *   exist (developers and deployers who extract value from opacity), victims
 *   exist (affected populations and system auditability), suppression is high
 *   (technical complexity, legal barriers, information asymmetry), and a
 *   genuine coordination function is embedded (algorithms do coordinate
 *   decisions at scale). The theater ratio (0.64) reflects that regulatory
 *   apparatus and transparency frameworks maintain substantial performative
 *   content: algorithmic impact assessments check boxes, audits are
 *   resource-constrained and lag system sophistication, and many transparency
 *   requirements are honored in form but not substance. However, unlike the
 *   verification bottleneck where theater is increasing, here theater is
 *   stabilizing as interpretability research advances and regulatory pressure
 *   increases — the scaffold perspective identifies a genuine sunset
 *   mechanism where opacity becomes less defensible.
 *
 * KEY AGENTS:
 *   - Algorithmic Subjects: Primary victims (powerless/trapped) — individuals subject to algorithmic decisions with no transparency, no contestation mechanism, and no exit option from systems that are now infrastructure
 *   - Algorithm Developers: Primary beneficiaries (institutional/arbitrage) — technology firms and research institutions who benefit from opacity as IP protection and competitive advantage
 *   - Deploying Institutions: Secondary beneficiary (institutional/constrained) — banks, agencies, platforms that use algorithms and benefit from opacity as liability shield while also bearing regulatory risk
 *   - Advocacy Groups: Secondary victim (moderate/constrained) — civil rights, consumer protection, and transparency advocates fighting for disclosure requirements while themselves bearing extraction through policy capture and legal barriers
 *   - Regulatory Apparatus: Institutional actor (powerful/mobile) — regulators maintaining performance of auditing and oversight while their technical capacity lags system sophistication (piton perspective)
 *   - Transparency Coalition: Organized actor (organized/constrained) — researchers, standards bodies, regulatory reformers building interpretability methods and transparency requirements as alternative pathway (scaffold perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as genuinely mixed coordination-extraction hybrid with structural instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_opacity_in_decision_systems, 0.58).
domain_priors:suppression_score(algorithmic_opacity_in_decision_systems, 0.68).
domain_priors:theater_ratio(algorithmic_opacity_in_decision_systems, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_opacity_in_decision_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_opacity_in_decision_systems, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_opacity_in_decision_systems, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_opacity_in_decision_systems, tangled_rope).
narrative_ontology:human_readable(algorithmic_opacity_in_decision_systems, "Algorithmic Opacity in Decision Systems").
narrative_ontology:topic_domain(algorithmic_opacity_in_decision_systems, "technology/governance/political_economy").

domain_priors:requires_active_enforcement(algorithmic_opacity_in_decision_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_opacity_in_decision_systems, algorithm_developers).
narrative_ontology:constraint_beneficiary(algorithmic_opacity_in_decision_systems, deploying_institutions).
narrative_ontology:constraint_victim(algorithmic_opacity_in_decision_systems, affected_populations).
narrative_ontology:constraint_victim(algorithmic_opacity_in_decision_systems, system_auditability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC SUBJECT (SNARE) — Individuals subject to algorithmic decisions (credit scoring, hiring, parole, content moderation, bail recommendation) face high extraction with minimal escape routes. Cannot understand why decisions are made about them, cannot meaningfully contest outcomes, and cannot exit systems (digital platforms, financial services, criminal justice) that are now essential infrastructure. Suppression is severe: technical complexity, legal barriers to transparency (trade secret claims, proprietary code), and information asymmetry prevent meaningful resistance or alternative coordination.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADVOCACY GROUP (TANGLED ROPE) — Civil rights organizations, transparency advocates, and consumer protection groups coordinate on legitimate transparency goals (understanding how decisions affecting people work) but are suppressed by legal barriers (trade secret doctrine, proprietary data exclusions, litigation costs). Faces extraction through policy capture: their transparency wins are diluted by weak implementation, narrow scope, and sunset clauses. But they also benefit from the constraint — advocacy funding and organizational legitimacy flow from fighting opacity. Moderate power with meaningful constrained exit (can exit the advocacy relationship, shift focus) produces tangled rope classification.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM DEVELOPER (ROPE) — Technology firms and research institutions experience algorithmic opacity as coordination: transparent algorithms enable competitors to reverse-engineer intellectual property, replicate proprietary methods, and commoditize innovations. Opacity enables coordination on safety (hiding implementation details prevents malicious exploit), IP protection (trade secret claims are legitimate for genuinely proprietary methods), and risk management (limiting visibility of model biases to authorized auditors). This is genuine rope — the coordination function is real, suppression is not primarily coercive but contractual (IP law), and the developer benefits through arbitrage (exclusive access to methods). No victim class exists from the developer's perspective; the constraint solves a coordination problem.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEPLOYING INSTITUTION (TANGLED ROPE) — Banks, government agencies, and platforms deploy algorithms because opacity reduces liability exposure: if the algorithm's logic is opaque, the institution can claim the decision was 'data-driven' (more legitimate-sounding than discretionary) while limiting transparency to regulators. Genuine coordination function exists: algorithms coordinate hiring, credit, and criminal justice decisions across thousands of cases consistently. But extraction is embedded: the institution captures opacity as a liability shield, hides from accountability for disparate outcomes, and benefits from regulatory ambiguity. Constrained exit (regulated industries cannot simply stop using algorithms, and switching to transparent alternatives incurs costs) produces tangled rope.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Regulators (SEC, FTC, CFPB, EU data protection authorities) maintain extensive algorithmic auditing and transparency frameworks that are substantially performative. Audits check boxes on algorithmic impact assessments; regulators maintain technical expertise at institutional levels far below the sophistication of deployed systems; enforcement is slow and poorly resourced. The regulatory machinery persists through institutional inertia and bureaucratic legitimacy despite widespread recognition that it cannot effectively verify algorithmic claims. Theater ratio is high because the regulatory ritual (audits, impact assessments, disclosure requirements) maintains appearance of control while actual verification is limited. This is piton — a former rope (genuine coordination mechanism) that has atrophied into theatrical compliance.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPARENCY COALITION (SCAFFOLD) — Organized actors (DARPA's AI Explainability program, EU AI Act, algorithmic accountability standards groups, open-source interpretability tools) see opacity as a temporary coordination failure with a clear sunset. Explainable AI research, interpretability frameworks, and regulatory mandates for transparency are building alternative pathways that make opacity less defensible. Suppression is high today but declining (interpretability tools are improving, regulatory pressure is increasing). The sunset is structural: as interpretability methods mature and regulatory requirements strengthen, claiming opacity becomes unsustainable. Organized agent status + constrained exit + clear sunset logic produce scaffold classification.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, algorithmic systems do coordinate genuine collective decision-making across populations too large for individual negotiation. Opacity is partly an inherent feature: complex machine learning systems are genuinely difficult to interpret even to their creators (the alignment and interpretability problem). But opacity is also partly a choice: many algorithms could be more transparent than they are, and institutions actively resist transparency to reduce liability and maintain control. The constraint is tangled rope because it mixes a real coordination function (distributed decision-making) with real asymmetric extraction (institutional liability shields, information asymmetry enabling discrimination). Suppression is high but not total — some transparency is happening, and the constraint is unstable.
constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_opacity_in_decision_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_opacity_in_decision_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_opacity_in_decision_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_opacity_in_decision_systems, TR),
    TR >= 0.70.

:- end_tests(algorithmic_opacity_in_decision_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from algorithmic subjects (denied transparent decision-making, denied contestation, denied exit) and from the public interest in system auditability. But extraction is not maximal (some transparency exists, regulatory requirements are being implemented, interpretability research is advancing) and some genuine coordination value is present (algorithms do enable consistent large-scale decision-making). Suppression (0.68): High. Multiple mechanisms: technical complexity creates genuine barriers to understanding; trade secret doctrine and IP law create legal barriers; information asymmetry (deployers know more than subjects) creates structural barriers; career and funding incentives (maintaining contract with deploying institutions) create professional barriers. Theater ratio (0.64): Moderate-high and increasing. Regulatory audits maintain appearance of oversight while actual verification capability lags system sophistication; algorithmic impact assessments exist but are heterogeneous in rigor; transparency requirements are honored inconsistently. Theater is increasing because regulatory burden is growing but regulatory capacity is not keeping pace — more boxes are being checked without proportional increase in actual verification.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap is between the developer (rope) and the algorithmic subject (snare). Both perceive the same constraint structure: algorithms are deployed, opacity is present, outcomes are distributed. But the developer experiences this as a coordination solution (IP protection enables R&D) while the subject experiences it as pure extraction (opacity enables discrimination without recourse). This gap reveals that directionality is not symmetrical — the same constraint structure produces opposite classifications for different agents. The developer's rope is not a contradiction of the subject's snare; both are valid perspectival readings. The tangled rope classification from the analytical and deploying-institution perspectives integrates these views: opacity does coordinate decisions AND does extract from subjects simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Developers (beneficiary/arbitrage) → d ≈ 0.15 → f(d) ≈ -0.01 → effective χ negative (benefit from opacity). Subjects (victim/trapped) → d ≈ 0.90 → f(d) ≈ 1.32 → effective χ amplified (extract maximum). Advocacy groups (victim/constrained) → d ≈ 0.65 → f(d) ≈ 1.00 → effective χ moderate. Deploying institutions (mixed beneficiary-victim/constrained) → d ≈ 0.50 → f(d) ≈ 0.65 → effective χ moderate. The network effect: as institutions deploy more algorithms, the beneficiary pool widens (more deploying institutions benefit), which should lower average d and reduce experienced extraction. But in practice, coordination fails (developers don't internalize subject costs, deployers don't coordinate on transparency standards), and the constraint remains extractive. This is a network failure — the potential for coordination exists but is not realized.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that algorithmic opacity genuinely satisfies BOTH coordination AND extraction criteria. Coordination function is real: algorithms coordinate decisions at scale, developers coordinate on IP protection, institutions coordinate on liability management. Extraction is real: subjects bear opacity costs with no recourse, disparate impact is systematic, accountability is suppressed. The constraint is NOT a false positive for tangled rope (it genuinely has both functions) and NOT mislabeled as snare (snare would require pure extraction with no coordination, which is false — the coordination is present). The mandatrophy is resolved by accepting that constraints can be hybrid and that perspectives reveal which function dominates from each agent's position. The subject's snare perspective is valid (from their view, extraction dominates). The developer's rope perspective is valid (from their view, coordination dominates). The analytical observer's tangled rope perspective is the structural fact: both functions are present. The scaffold perspective identifies that this constraint is unstable — as interpretability advances and regulatory requirements strengthen, the coordination rationale for opacity weakens and the extraction becomes less defensible. This is the sunset mechanism: the constraint's tangled_rope classification is sustainable only while interpretability remains technically hard; as it becomes easier, staying opaque requires explicit choice to extract, shifting the classification toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretability_inherent_limits,
    'Is algorithmic opacity inherent to complex machine learning (a natural limit on explainability) or primarily a choice by institutions to avoid transparency?',
    'Comparison of interpretability across systems: similar-complexity models with different transparency designs, longitudinal tracking of explainability improvements, technical analysis of trade-offs between accuracy and interpretability',
    'If inherent: opacity is partly a mountain (unavoidable technical limit), scaffold sunset is unrealistic, and suppression reflects genuine constraint on knowledge. If primarily institutional choice: opacity is extractive design, scaffold sunset is realistic, and suppression is coercive. Classification shifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_inherent_limits, empirical, 'Whether algorithmic opacity is inherent technical limit or institutional choice').

omega_variable(
    transparency_enable_gaming,
    'Does transparency about algorithmic decision criteria enable affected populations to game the system, or does it primarily enable accountability and contestation?',
    'Empirical tracking post-transparency: do disclosure requirements lead to criterion gaming or to reduced disparate impact? Comparative analysis of gaming rates pre/post transparency mandates; countermeasures analysis (how easily institutions can change criteria to remain opaque after transparency is mandated)',
    'If transparency enables significant gaming: institutions'' opacity claims are partly legitimate (transparency creates new coordination problems). If gaming is minimal: transparency is primarily accountability mechanism, and continued opacity is pure extraction. Classification implications: if gaming is real, institutions'' constrained exit status is justified; if gaming is minimal, exit status should downgrade to arbitrage (meaning they stay opaque for extractive rather than coordination reasons).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_enable_gaming, empirical, 'Whether algorithmic transparency enables gaming or primarily enables accountability').

omega_variable(
    regulatory_capacity_asymmetry,
    'Can regulatory institutions ever achieve technical capacity equivalent to deployed systems, or is the asymmetry inherent to the speed of private R&D versus regulatory budgeting?',
    'Comparative technical capability analysis: years of training required for regulator expertise, bug-discovery timelines for private versus public audits, resource scaling analysis for regulatory expansion versus system complexity growth',
    'If inherent asymmetry: the piton perspective is realistic (regulatory apparatus cannot catch up), and the scaffold perspective requires institutional bypass (open-source tools, third-party audits) rather than regulatory strengthening. If capacity asymmetry is surmountable with resources: regulatory apparatus is degraded rather than piton, and regulatory investment could restore it to rope status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capacity_asymmetry, empirical, 'Whether regulatory-private technical capacity asymmetry is surmountable or inherent').

omega_variable(
    liability_shield_necessity,
    'Is the deploying institution''s need for opacity-as-liability-shield a genuine coordination problem (opacity enables consistent decision-making without institutional liability) or primarily an extraction mechanism (opacity enables discrimination without accountability)?',
    'Litigation analysis: do transparent algorithms face more frivolous or valid liability claims than opaque ones? Comparative study of institutions with high transparency requirements (EU GDPR-compliant institutions) versus low transparency: do transparent institutions face higher costs or better risk profiles? Disparate impact analysis: does transparency reduce discrimination or merely expose it?',
    'If opacity genuinely reduces frivolous liability: the deploying institution''s constrained exit and tangled rope classification are realistic. If opacity primarily enables discrimination without accountability: the extraction is malicious, and institutions choose opacity for extractive rather than coordination reasons. This would shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_shield_necessity, empirical, 'Whether deploying institution''s opacity need is genuine coordination or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_opacity_in_decision_systems, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algopacity_tr_t0, algorithmic_opacity_in_decision_systems, theater_ratio, 0, 0.52).
narrative_ontology:measurement(algopacity_tr_t5, algorithmic_opacity_in_decision_systems, theater_ratio, 5, 0.58).
narrative_ontology:measurement(algopacity_tr_t10, algorithmic_opacity_in_decision_systems, theater_ratio, 10, 0.64).
narrative_ontology:measurement(algopacity_tr_t15, algorithmic_opacity_in_decision_systems, theater_ratio, 15, 0.66).

% Extraction over time
narrative_ontology:measurement(algopacity_be_t0, algorithmic_opacity_in_decision_systems, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(algopacity_be_t5, algorithmic_opacity_in_decision_systems, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(algopacity_be_t10, algorithmic_opacity_in_decision_systems, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(algopacity_be_t15, algorithmic_opacity_in_decision_systems, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_opacity_in_decision_systems, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_opacity_in_decision_systems, algorithmic_bias_in_hiring).
narrative_ontology:affects_constraint(algorithmic_opacity_in_decision_systems, predictive_policing_systems).
narrative_ontology:affects_constraint(algorithmic_opacity_in_decision_systems, credit_scoring_opacity).
narrative_ontology:affects_constraint(algorithmic_opacity_in_decision_systems, content_moderation_appeals).

% DUAL FORMULATION NOTE:
% Algorithmic opacity in decision systems is upstream of several domain-specific constraints: hiring discrimination relies on opacity, predictive policing depends on opacity, credit disparities are enabled by opacity, and content moderation lack of appeals is sustained by opacity. This story focuses on the general structural pattern; domain-specific stories inherit the base opacity constraint and add domain-specific extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_opacity_in_decision_systems, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
