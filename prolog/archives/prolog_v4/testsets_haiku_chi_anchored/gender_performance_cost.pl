% ============================================================================
% CONSTRAINT STORY: gender_performance_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gender_performance_cost, []).

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
 *   constraint_id: gender_performance_cost
 *   human_readable: The Labor of the Borrowed Mask: Gender Performance Cost
 *   domain: gender_theory/ontological_ethics
 *
 * SUMMARY:
 *   The gender performance cost constraint represents the extraction of
 *   labor, cognitive resources, and ontological recognition from individuals
 *   whose embodied presentation is subject to institutional monitoring and
 *   conformity demands. This is not merely the biological work of maintaining
 *   a body—all humans perform such work—but the surplus extraction required
 *   by systems that demand bodies conform to gendered aesthetic, behavioral,
 *   and interactive standards. The constraint operates through a hybrid
 *   mechanism: (1) coordination function for patriarchal/divine orders
 *   (organizing reproduction, resource distribution, social hierarchy); (2)
 *   suppression of alternatives (surveillance, economic precarity tied to
 *   non-conformity, social erasure, physical danger); (3) theater of
 *   necessity (beauty industry, fashion, medical aestheticization claim
 *   inevitability and naturalness). The performer is trapped between the cost
 *   of performance (psychological vigilance, body modification, constant
 *   monitoring, cognitive load) and the cost of non-performance (economic
 *   vulnerability, social erasure, ontological delegitimization). The
 *   constraint exemplifies how structures can simultaneously function as
 *   coordination (solving real problems of social organization) and
 *   extraction (distributing costs asymmetrically to those performing
 *   conformity). The divergence between perspectives reveals that what
 *   appears natural law to the distant observer is a snare to the powerless
 *   performer, a rope to institutional beneficiaries, and a scaffold with a
 *   real sunset to organized resistance movements.
 *
 * KEY AGENTS:
 *   - Gender non-conforming performers: Primary victims (powerless/trapped) — bear full cognitive and bodily cost of monitoring and conformity demands; have minimal exit options without economic precarity or social erasure
 *   - Patriarchal/religious institutional order: Primary beneficiary (institutional/arbitrage) — captures coordination benefits (social predictability, reproductive organization, resource distribution control) while externalizing costs to performers
 *   - Male gaze beneficiaries: Secondary beneficiary (powerful/arbitrage) — gain status, labor, and recognition value from gendered performance standards; often unconscious of extraction mechanism
 *   - Conditionally mobile gender performers: Mixed victim-beneficiary (moderate/constrained) — gain some access to resources through conformity (professional advancement, relationship recognition) while also bearing extraction costs; have constrained but real options for exit or renegotiation
 *   - Feminist/queer resistance coalition: Organized agents (organized/mobile) — see constraint as temporary and resolvable; building alternative aesthetics, community norms, legal protections that create sunset for enforcement
 *   - Beauty/fashion/medical/diet industry: Institutional degradation (institutional/arbitrage) — maintains extraction apparatus through performative necessity and marketing rather than genuine coordination function; internally aware of artificiality (piton characteristic)
 *   - Analytical observer: Civilizational distance (analytical/analytical) — risks naturalizing the constraint as inherent to embodiment rather than recognizing it as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gender_performance_cost, 0.58).
domain_priors:suppression_score(gender_performance_cost, 0.68).
domain_priors:theater_ratio(gender_performance_cost, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gender_performance_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(gender_performance_cost, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gender_performance_cost, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gender_performance_cost, tangled_rope).
narrative_ontology:human_readable(gender_performance_cost, "The Labor of the Borrowed Mask: Gender Performance Cost").
narrative_ontology:topic_domain(gender_performance_cost, "gender_theory/ontological_ethics").

domain_priors:requires_active_enforcement(gender_performance_cost).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gender_performance_cost, divine_order).
narrative_ontology:constraint_beneficiary(gender_performance_cost, patriarchal_institutional_structure).
narrative_ontology:constraint_beneficiary(gender_performance_cost, male_gaze_beneficiaries).
narrative_ontology:constraint_victim(gender_performance_cost, gender_nonconforming_performers).
narrative_ontology:constraint_victim(gender_performance_cost, body_cognitive_resource_reserve).
narrative_ontology:constraint_victim(gender_performance_cost, authentic_self_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENDER PERFORMER (SNARE) — Those performing gender (especially across non-normative axes) experience this as a pure extraction mechanism. Exit options are severely constrained: the cost of non-performance includes social erasure, economic precarity, physical danger, and ontological delegitimization. The performer is trapped in the performance by survival necessity. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(gender_performance_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: DIVINE/PATRIARCHAL ORDER (ROPE) — From the standpoint of institutional power structures (religious, social, state), the constraint functions as pure coordination: it organizes social hierarchy, regulates reproduction, controls resource distribution, and maintains predictability. The order experiences the constraint as solving a collective action problem (who performs beauty/gender maintenance, under what terms). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary through coordination efficiency.
constraint_indexing:constraint_classification(gender_performance_cost, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: CONDITIONAL BENEFICIARY (TANGLED ROPE) — Some agents gain conditional access to resources, safety, or status through successful gender performance (upward social mobility via beauty/femininity standards, professional advancement through gender conformity). They experience both extraction (constant monitoring, body/cognitive demands) and coordination (access to networks, recognition, resource flows). Exit is constrained but possible through collective organizing or geographic mobility. d≈0.58, f(d)≈0.68, σ=1.0 → χ≈0.39. Mixed extraction with genuine coordination benefit.
constraint_indexing:constraint_classification(gender_performance_cost, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEMINIST/QUEER RESISTANCE (SCAFFOLD) — Organized agents (feminist theory, queer communities, anti-gender-normative movements) see the constraint as temporary and resolvable through consciousness-raising, alternative aesthetics, institutional reform, and cultural shift. The coalition has agency and exit pathways (new norms, alternative community spaces, legal protections). Theater is declining as norms shift. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.36. Sunset clause: as gender non-conformity becomes institutionally recognized, enforcement costs rise and extraction mechanism weakens.
constraint_indexing:constraint_classification(gender_performance_cost, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: BEAUTY INDUSTRY/GENDER APPARATUS (PITON) — The institutional machinery (beauty standards, fashion, cosmetics, plastic surgery, diet culture, aesthetic medicine) that extracts value from the constraint is substantially degraded/inertial. It maintains itself through performative necessity rather than coordination function. Theater ratio is high (0.65) because the apparatus performs efficiency and necessity that it no longer provides — it persists through marketing and institutional lock-in rather than actual indispensability. The apparatus knows it is theatrical (industry-internal discussions of artificial scarcity) but continues because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(gender_performance_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational distance, there is a risk that gender performance costs appear as immutable natural law: 'Bodies always require maintenance,' 'Beauty is innate to human thriving,' 'Gender differentiation is biologically necessary.' This perspective risks naturalizing what is structurally a contingent institutional arrangement. However, the base properties (ε=0.58, suppression=0.68, theater=0.65) contradict the mountain classification. The false summit detector will flag this as naturalization: the constraint is not inherent to embodiment, but to specific social systems that extract value through gendered embodiment demands.
constraint_indexing:constraint_classification(gender_performance_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gender_performance_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gender_performance_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gender_performance_cost, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gender_performance_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gender_performance_cost, TR),
    TR >= 0.70.

:- end_tests(gender_performance_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts cognitive resources (constant self-monitoring), bodily labor (maintenance of appearance), economic resources (spending on conformity), and ontological recognition (self-definition constrained by external standards). However, it is not maximal (0.46-0.70 range) because: (a) some agents gain genuine resource access through conformity, not pure extraction; (b) the coordination function is real (the order does solve social problems), even though costs are asymmetrically distributed; (c) exit options exist for some agents at high resource cost, not total trapping. The value reflects that this is hybrid extraction-coordination, not pure predation. Suppression (0.68): High. Multiple mechanisms prevent exit: economic precarity (beauty standards enforcement tied to employment); surveillance (constant social monitoring of presentation); physical danger (violence against non-conforming embodiment); ontological erasure (gender non-conformity delegitimized as inauthenticity or delusion); psychological internalization (self-monitoring as habitus). However, suppression is not maximal (0.80+) because alternative communities exist, legal protections are expanding in some jurisdictions, and consciousness-raising is creating exit pathways. Theater ratio (0.65): Moderate-high. The constraint operates substantially through theatrical enforcement—the beauty industry's claim of necessity, the aesthetic medicine promise of 'restoration,' the fashion system's performance of eternal recurrence. However, theater is not dominant (0.70+) because some enforcement is material (employment discrimination, violence) and some coordination is functional (the system does organize social relationships and reproductive outcomes). The rising trajectory (0.52→0.65) reflects that as material enforcement becomes less normalized, theater compensates—marketing increasingly claims that conformity is authenticity, biology, or empowerment rather than necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across the observation site. The powerless performer (snare) sees pure trapping: exit is materially and psychologically impossible. The institutional order (rope) sees coordination: the constraint solves the problem of organizing social hierarchy and reproduction. The conditionally mobile agent (tangled_rope) sees mixed constraint: they are extracted from but also gain access. The organized resistance (scaffold) sees a temporary problem with a real sunset: alternative norms are building. The beauty industry (piton) sees degraded necessity: it maintains the constraint through marketing rather than genuine function. The analytical observer risks seeing immutable nature: it naturalizes institutional extraction as biological fact. The perspectival gaps arise from (a) differential exit options (powerless has none; institutional has full arbitrage), (b) differential benefit distribution (institutional gains, performer loses; conditional agent gets mixed return), (c) differential time horizons (performer sees immediate biographical cost; organized agent sees generational sunset; institutional sees civilizational permanence), and (d) differential agency (organized agent has capacity to change norms; powerless has only compliance or suicidal resistance).
 *
 * DIRECTIONALITY LOGIC:
 *   Gender non-conforming performer: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. Exit is genuinely unavailable at sustainable cost. Patriarchal order: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through coordination. Has full flexibility in enforcement. Male gaze beneficiary: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Secondary beneficiary; often passive rather than active enforcer. Conditional mobile agent: Victim + constrained → d≈0.58, f(d)≈0.68. Significant extraction but not maximal; constrained exit is possible through resources or collective action. Feminist/queer coalition: Organized + mobile → d≈0.42, f(d)≈0.43. Low effective extraction relative to base; mobility and organization provide agency. Beauty industry: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.65≥0.70 fails for piton, but piton also works at lower theater if degradation is clear). Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk: naturalizing contingent constraint as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the coordination function (real: the system does organize social hierarchy, reproduction, resource distribution) from the extraction mechanism (real: asymmetrically distributed costs). Both are genuine. The constraint is not 'actually a rope being mislabeled as snare' (false positive coordination) nor 'actually a snare with no coordination function' (mislabeled pure extraction). It is genuinely tangled: it solves real problems AND extracts asymmetrically. The resolution lies in: (1) acknowledging that coordination and extraction can coexist in the same structure; (2) recognizing that the distribution of costs determines classification from each perspective (the performer sees snare; the order sees rope; a balanced agent sees tangled_rope); (3) accepting that the analytical observer cannot dissolve the perspectival divergence by appeal to natural law—the constraint is socially contingent, not naturally necessary. The beauty industry's piton status reveals that the constraint increasingly relies on theater because material enforcement (legal, economic) is weakening—the apparatus must convince rather than compel, indicating the sunset is real and classification as scaffold (with organized resistance) is structurally sound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_maintenance_baseline,
    'What constitutes the irreducible biological maintenance cost of human bodies versus culturally-imposed gender performance costs?',
    'Cross-cultural ethnography comparing baseline body maintenance costs (hygiene, rest, nutrition) across gender-normative and gender-nonconforming societies; neuroscientific study of cognitive load differences; time-use data on non-negotiable vs performative body work',
    'If baseline is <20% of current cost: most extraction is institutional (snare classification holds). If baseline is >50%: some cost is inherent (rope/tangled_rope classification stronger). If >70%: mountain classification gains credence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_maintenance_baseline, empirical, 'Proportion of gender performance cost attributable to biology vs institutional demands').

omega_variable(
    exit_option_availability_threshold,
    'Under what material conditions do gender non-conforming agents actually have exit options (arbitrage, mobile) rather than being trapped?',
    'Economic analysis of income/wealth thresholds required to exit performance (renounce beauty standards, adopt gender non-conforming presentation); legal protection analysis (jurisdictions with gender expression rights); social network analysis of communities with low conformity pressure; historical data on exit success rates by resource level',
    'If exit is available only to high-wealth agents: snare classification for 95%+ of population. If exit is genuinely available to moderate-wealth agents: tangled_rope classification legitimate. If exit is cost-prohibitive universally: pure snare (χ>0.80).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability_threshold, empirical, 'Material conditions enabling or blocking exit from gender performance').

omega_variable(
    coordination_function_genuineness,
    'Does gender performance actually solve real coordination problems (sexual/reproductive selection, parental role differentiation, resource allocation, social predictability) or is the coordination rationale post-hoc justification for extraction?',
    'Historical analysis of societies with different gender performance systems; comparison of coordination outcomes (social stability, resource efficiency, reproductive success) across cultures; examination of whether coordination benefits are symmetrically distributed or concentrated',
    'If genuinely coordinating: tangled_rope and scaffold perspectives hold; beneficiaries can justify extraction as payment for coordination. If coordination is post-hoc: snare classification dominates; extraction is pure, not mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether gender performance solves coordination problems or merely rationalizes extraction').

omega_variable(
    cognitive_load_measurability,
    'Can the cognitive and emotional load of constant gender performance monitoring be empirically quantified and compared to other institutional extraction mechanisms?',
    'Neuroscientific study of attention/executive function costs during gender-conforming vs non-conforming presentation; psychological measurement of vigilance/monitoring burden; comparison to other high-suppression constraints (surveillance, precarious labor); fMRI studies of self-monitoring in gendered contexts',
    'If cognitive load is measurable and high (equivalent to 20-30% of daily executive function): supports high suppression score (0.68). If lower than expected: may adjust suppression downward, changing extraction classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_load_measurability, empirical, 'Empirical measurability of cognitive load from gender performance monitoring').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gender_performance_cost, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpc_theater_t0, gender_performance_cost, theater_ratio, 0, 0.52).
narrative_ontology:measurement(gpc_theater_t50, gender_performance_cost, theater_ratio, 50, 0.6).
narrative_ontology:measurement(gpc_theater_t100, gender_performance_cost, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(gpc_extractiveness_t0, gender_performance_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpc_extractiveness_t50, gender_performance_cost, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(gpc_extractiveness_t100, gender_performance_cost, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gender_performance_cost, resource_allocation).
narrative_ontology:affects_constraint(gender_performance_cost, male_gaze_ontological_primacy).
narrative_ontology:affects_constraint(gender_performance_cost, reproductive_labor_asymmetry).
narrative_ontology:affects_constraint(gender_performance_cost, beauty_standard_capital_extraction).

% DUAL FORMULATION NOTE:
% Gender performance cost decomposes into three structurally distinct claims: (1) the immediate cognitive/bodily labor cost (this constraint, ε=0.58); (2) the ontological erasure of non-conforming embodiment (downstream, higher ε); (3) the reproductive/sexual labor asymmetry (downstream, higher ε). This story focuses on the labor cost. The network links indicate how the performance cost constraint enables and depends on the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gender_performance_cost, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
