% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause Substantial Effects Test (Limited Reading)
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   The substantial effects test represents the Supreme Court's settled
 *   reading of the Commerce Clause since the 1940s-1990s: Congress may
 *   regulate intrastate activity if it substantially affects interstate
 *   commerce, provided the activity is genuinely economic in nature and the
 *   regulation does not use the commerce power as a pretext for police power
 *   concerns. This reading emerged from the New Deal crisis (when the Court
 *   abandoned Lochner formalism) and crystallized in cases like Wickard v.
 *   Filburn (wheat production for home consumption) and Gonzales v. Raich
 *   (homegrown marijuana). The reading benefits Congress and federal agencies
 *   by providing a stable jurisdictional framing that avoids the categorical
 *   explosion of pre-New Deal jurisprudence while preserving some state
 *   autonomy for genuinely local matters. It harms states attempting to
 *   regulate noneconomic intrastate activity (family law, criminal procedure,
 *   education) — if courts find even attenuated commerce connections, federal
 *   preemption follows. The constraint exhibits the structure of a tangled
 *   rope: genuine coordination function (preventing regulatory
 *   races-to-the-bottom in genuinely economic domains, enabling Congress to
 *   address multi-state problems) combined with asymmetric extraction (states
 *   lose authority in borderline cases, federal power systematically expands
 *   when economic effects are plausibly claimed). The rising theater ratio
 *   (0.48 → 0.65) reflects increasing performativity in boundary policing —
 *   courts struggle to distinguish genuine economic effects from pretextual
 *   invocation, especially as the domain of 'economic' regulation has
 *   expanded to cover environmental, health, and labor domains with complex
 *   causal chains to interstate commerce. The suppressiveness trajectory
 *   (0.38 → 0.48) reflects increasing barriers to state regulatory autonomy
 *   as federal agencies build interpretive infrastructure around the commerce
 *   power.
 *
 * KEY AGENTS:
 *   - Congress: Primary beneficiary (institutional/arbitrage) — gains stable framing for enumerating commerce power without pretext; coordinates multi-state problems
 *   - Federal Regulatory Agencies (EPA, OSHA, FCC, etc.): Organized beneficiaries (organized/constrained) — benefit from substantial effects legitimacy but must maintain appearance of economic nexus
 *   - States Exercising Police Power: Primary victims (powerless/trapped) — lose authority over noneconomic intrastate matters if federal commerce nexus is found; no exit from Supremacy Clause
 *   - State Economic Regulators: Secondary agents (powerful/constrained) — retain substantial authority for genuinely economic regulation while sharing regulatory space with federal agencies
 *   - Federal Courts (Doctrine Enforcers): Organized actors (organized/constrained) — perform boundary policing function; see themselves as temporary umpires but face mounting complexity at margins
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the category boundary as a necessary feature of federalism rather than a constructed distinction requiring active maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.48).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial Effects Test (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'f7a888fd-4509-4b61-9e9c-005d0afed6ab').
narrative_ontology:cs_kernel_codification('f7a888fd-4509-4b61-9e9c-005d0afed6ab', fixed_text).
narrative_ontology:cs_authority_grounding('f7a888fd-4509-4b61-9e9c-005d0afed6ab', lineage).
narrative_ontology:cs_interpretation_layer_present('f7a888fd-4509-4b61-9e9c-005d0afed6ab').
narrative_ontology:cs_reading_relation('f7a888fd-4509-4b61-9e9c-005d0afed6ab', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7a888fd-4509-4b61-9e9c-005d0afed6ab', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('f7a888fd-4509-4b61-9e9c-005d0afed6ab', foundational, economic_noneconomic_distinction_coherent).
narrative_ontology:cs_axiom_status(economic_noneconomic_distinction_coherent, holdable).
narrative_ontology:cs_axiom_grounding('f7a888fd-4509-4b61-9e9c-005d0afed6ab', economic_noneconomic_distinction_coherent, empirically_contingent).
narrative_ontology:cs_axiom('f7a888fd-4509-4b61-9e9c-005d0afed6ab', secondary, congress_factfinding_deference_appropriate).
narrative_ontology:cs_axiom_status(congress_factfinding_deference_appropriate, holdable).
narrative_ontology:cs_axiom_grounding('f7a888fd-4509-4b61-9e9c-005d0afed6ab', congress_factfinding_deference_appropriate, deontological).
narrative_ontology:cs_reference_frame('f7a888fd-4509-4b61-9e9c-005d0afed6ab', enumerated_commerce_power_dual_federalism).
narrative_ontology:cs_drift_state('f7a888fd-4509-4b61-9e9c-005d0afed6ab', contemporary_economic_integration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f7a888fd-4509-4b61-9e9c-005d0afed6ab', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, congress_enumerated_commerce_power).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_regulatory_apparatus).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_police_power_domain).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_regulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% States attempting to regulate noneconomic intrastate activity (family law, criminal procedure, education) face federal preemption if courts find an attenuated link to interstate commerce. The state has no exit — the Supremacy Clause forecloses alternative regulatory frameworks. Effective suppression: 0.60. States cannot operate outside this constraint structure without constitutional amendment. Maximum experienced extraction.
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% States that regulate genuinely economic intrastate activity (labor standards, environmental protection, professional licensing affecting commerce) retain real authority under the substantial effects test while also benefiting from federal coordination of multi-state problems. Mixed experience: genuine state autonomy for economic regulation within the commerce domain, but subject to federal override if Congress acts. Suppression: 0.35.
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Congress experiences this constraint as pure coordination: the substantial effects test provides a stable framing for enumerating its commerce power without requiring explicit invocation of police power pretext. Congress can coordinate multi-state problems without preempting all state autonomy. Low effective extraction — coordination function is genuine.
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Agencies benefit from the substantial effects framing as a legitimacy story for their authority over intrastate economic activity (EPA, OSHA, FCC). They also bear some constraint: they must demonstrate economic nexus and avoid obvious pretext (noneconomic objectives). Organized power with constrained exit — can challenge the doctrine through litigation strategy but cannot escape jurisdictional requirement.
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Courts enforcing the substantial effects test see themselves as temporary umpires maintaining federalism balance. The doctrine has a structural sunset: as the gap between local and national economic integration widens, the boundary between 'substantial' economic effects and trivial ones becomes arbitrary, pressuring courts toward either pure expansion (coexistence with expansive reading) or abandonment. Current status: performative boundary policing (theater_ratio: 0.70).
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical perspective risks naturalizing the substantial effects test as a necessary feature of any federal system. However, structural data reveals this as a constructed category boundary (economic vs noneconomic) maintained through active enforcement by courts. Not a natural law — a doctrine with beneficiaries.
constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_text__substantial_effects_limited_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate. The substantial effects test carries genuine coordination benefits for Congress (multi-state externalities, regulatory spillovers) but also enables federal expansion into domains with attenuated or constructed commerce connections. The value reflects a hybrid: real coordination function combined with real asymmetric extraction of state authority. Not pure extraction (snare: 0.66+) because the doctrine preserves genuine state autonomy in legitimately local domains. Not pure coordination (rope: 0.35 or less) because states systematically lose authority at boundaries and federal agencies have extractive incentive to expand the definition of 'economic.' Suppression (0.48): Moderate-high. Significant barriers to state regulatory autonomy in contested domains: states cannot effectively challenge federal commerce power through the courts (rational basis review is highly deferential), cannot opt out through intergovernmental compacts without federal permission, and face preemption risk if they attempt regulation of activities with plausible commerce connections. But suppression is not total (snare: 0.60+) because states retain genuine authority in many domains and can influence the boundary through political channels. Theater ratio (0.65): High. Boundary policing between economic and noneconomic activity requires increasingly performative justification as economic integration deepens. Courts claim to measure actual economic effects but apply the doctrine flexibly to reach pre-determined conclusions. Federal agencies engage in post-hoc rationalization of economic effects to justify regulations motivated by other concerns (public health, environmental protection). The trajectory reflects increasing theater as the boundary becomes more strained.
 *
 * PERSPECTIVAL GAP:
 *   Congress sees coordination (Rope from immediate perspective): the substantial effects framing enables efficient multi-state regulation without the transaction costs of negotiated compacts or state-by-state patchworks. Federal agencies see beneficiary status (Rope): they gain jurisdictional legitimacy. States attempting noneconomic regulation see extraction (Snare): their police power domain is progressively compressed. States with economic regulatory capacity see mixed experience (Tangled Rope at generational scale): genuine autonomy for labor, environmental, and commercial regulation, but subject to federal override and category boundary pressure. Courts see themselves as performing necessary boundary maintenance (Scaffold with sunset logic): the doctrine is temporary, sustainable only if the economic/noneconomic distinction remains coherent, but the distinction is becoming unstable as integration deepens. The analytical observer risks seeing natural federalism (Mountain): federalism structures require some allocation of regulatory authority, and the substantial effects test is a reasonable way to allocate it. But structural data reveals this as false summit: the allocation mechanism benefits identifiable actors (Congress, federal agencies) and harms others (states exercising police power), and the category boundary is actively maintained through court enforcement rather than emerging naturally.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress (institutional beneficiary, arbitrage exit): derives d ≈ 0.10 from beneficiary status + ability to arbitrage policy venues. Federal agencies (organized beneficiary, constrained exit): derive d ≈ 0.25 — they benefit substantially but face constraints from needing to maintain the facade of economic nexus. States with police power (powerful/moderate power, trapped exit in preemption disputes): derive d ≈ 0.90 — they are structural targets, face federal supremacy, have no exit from the constitutional hierarchy. States with economic regulatory capacity (powerful power, constrained exit): derive d ≈ 0.55 — they are partially targeted (boundary pressure) but retain genuine authority and can negotiate federal arrangements. Courts (organized enforcers, constrained exit): derive d ≈ 0.40 — they are neither pure beneficiaries nor targets, but maintain the constraint mechanism. The perspectival variation in d explains why the same base extractiveness produces different classifications: beneficiaries perceive rope (low f(d) → low chi), targets perceive snare (high f(d) → high chi), mixed agents perceive tangled rope (moderate f(d) → moderate chi).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends on which structural position one occupies within the constraint. Congress genuinely benefits from having a stable jurisdictional framing (rope): the coordination function is real — multi-state externalities are costly to address through compacts, and a clear federal authority reduces transaction costs. States genuinely lose authority over noneconomic regulation (snare for police power domain): the extraction is real — once federal courts find a commerce nexus, Supremacy Clause forecloses state alternatives. States with economic regulation (tangled rope): the mixed experience is real — they benefit from federal coordination of multi-state externalities while bearing costs of preemption risk and regulatory overlap. The constraint is a genuinely hybrid coordination-extraction mechanism, not a misclassification. The tension arises from the fact that 'coordination' (multi-state externality management) and 'extraction' (state authority loss) flow through the same institutional mechanism (federal commerce jurisdiction). The doctrine distributes benefits and harms asymmetrically across agents. This is precisely what tangled rope describes: both functions are structural, both are real, and their coexistence creates the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_boundary_coherence,
    'Is the economic/noneconomic distinction coherent and stable, or is it a constructed category boundary maintained through judicial line-drawing?',
    'Historical analysis of decisions at the boundary: cases where activities were reclassified (e.g., family law as economic, criminal procedure as implicating commerce); doctrine trajectory showing increasing incoherence at margins; comparison to alternative coherence boundaries (e.g., political economy framework, structural dependence rather than economic nature).',
    'If boundary is coherent and stable: substantial effects test is a sustainable doctrine. If boundary is constructed: the constraint is a tangled rope with built-in instability; boundary policing becomes increasingly expensive (theater rises); doctrine drifts toward either pure expansion or abandonment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_coherence, conceptual, 'Coherence and stability of economic/noneconomic boundary').

omega_variable(
    substantial_effects_measurement_circularity,
    'Can ''substantial effects on interstate commerce'' be measured without circularity? Do courts measure actual economic effects or apply the doctrine to pre-justify conclusions reached through other reasoning?',
    'Systematic analysis of Commerce Clause cases: correlation between stated quantification of economic effects and decision outcome; whether courts grant deference to congressional factfinding about economic effects or substitute their own judgment; meta-analysis of effect sizes claimed across doctrinal areas (labor regulation, environmental regulation, noneconomic activity regulation).',
    'If measurement is circular: suppression value (0.48) understates the true constraint mechanism. The real constraint is category pretext, not genuine economic effect analysis. Theater ratio should exceed 0.70. If courts genuinely measure effects: doctrine is less extractive; suppression may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effects_measurement_circularity, empirical, 'Whether substantial effects measurement avoids circularity and pretext').

omega_variable(
    sibling_doctrine_incompatibility,
    'Is the substantial effects test logically compatible with both the originalist narrow reading and the expansive federal reading? Or does the boundary policing mechanism inherently pressure toward one sibling or away from the other?',
    'Formal analysis of axiom structures: which axioms are shared across readings, which are contradictory, which create path dependency. Historical case law showing doctrine drift toward or away from each sibling. Evolution of lower-court interpretation of boundary in response to Supreme Court signals.',
    'If test is genuinely coexistent with both siblings: three readings remain structurally viable. If test inherently pressures toward expansion: coexistence with expansive reading is strained; foreclosure of originalist reading may be latent. If test is unstable: all three readings face delegitimacy pressures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_doctrine_incompatibility, conceptual, 'Logical compatibility of substantial effects test with sibling readings').

omega_variable(
    categorical_vs_functional_federalism,
    'Does the category boundary (economic/noneconomic) track real federalism structural distinctions, or would a functional federalism framework (assessing multi-state spillovers, coordination costs, regulatory arbitrage incentives) better capture the normative concern?',
    'Comparison of outcomes: cases where categorical and functional analyses reach different conclusions; historical periods where the boundary was drawn differently; analysis of whether the doctrine''s instability correlates with boundary misalignment with functional spillover structure.',
    'If functional framework better tracks federalism structure: the substantial effects test is a second-best doctrine with built-in drift. The constraint would be more accurately modeled as a piton (degraded from a better functional framing). If categorical boundary is appropriate: doctrine is more stable than omega suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_functional_federalism, conceptual, 'Whether categorical boundary tracks functional federalism structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccse_tr_t0, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ccse_tr_t15, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement(ccse_tr_t30, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(ccse_be_t0, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ccse_be_t15, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(ccse_be_t30, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ccse_su_t0, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ccse_su_t15, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(ccse_su_t30, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, resource_allocation).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, state_police_power_boundary).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, regulatory_preemption_doctrine).

% DUAL FORMULATION NOTE:
% The substantial effects test is one component of a broader Commerce Clause constraint family. Upstream: the constitutional text (interpretation problem). Downstream: dormant commerce clause (state burden on interstate commerce even absent federal regulation) and preemption doctrine (federal vs state regulatory authority). The three sibling readings (substantial_effects_limited, expansive_federal, originalist_narrow) form a contest over the same legal text. Each reading would decompose differently into network-affected constraints — the expansive reading would upstream affect dormant commerce doctrine differently than the limited reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__substantial_effects_limited_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
