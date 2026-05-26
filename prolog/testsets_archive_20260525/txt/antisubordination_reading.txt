% ============================================================================
% CONSTRAINT STORY: antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antisubordination_reading, []).

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
 *   constraint_id: antisubordination_reading
 *   human_readable: Equal Protection as Antisubordination: Constitutional Prohibition on Caste-like Subordination
 *   domain: constitutional_law/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   The antisubordination reading of equal protection identifies the
 *   constitutional commitment to preventing policies that perpetuate
 *   caste-like subordination of historically disadvantaged groups as the core
 *   function of the Fourteenth Amendment. This reading positions subordinated
 *   groups as primary rights-holders (not merely individuals entitled to
 *   colorblind treatment) and makes facially neutral policies that perpetuate
 *   subordination subject to strict constitutional scrutiny. The constraint
 *   exhibits high extractiveness (0.58) because enforcement requires proving
 *   systemic causation across multiple policy domains, creates litigation
 *   burdens on resource-constrained groups, and faces institutional
 *   resistance from colorblind doctrine. The theater ratio (0.65) reflects
 *   that antisubordination principles are formally robust in constitutional
 *   doctrine but functionally degraded by narrow evidentiary standards and
 *   judicial skepticism of systemic claims. The constraint operates as
 *   Tangled Rope: it coordinates a genuine constitutional principle
 *   (structural equality) while simultaneously extracting costs through high
 *   proof burdens and limited remedies. From the perspective of historically
 *   subordinated groups, it appears as Snare — the constitutional protection
 *   exists but enforcement mechanisms are so constrained that subordination
 *   persists.
 *
 * KEY AGENTS:
 *   - Historically Subordinated Groups: Primary rights-holders (powerless/trapped) — identified as targets of caste-like subordination systems; constitute the beneficiary class in principle but experience Snare classification due to enforcement barriers
 *   - Civil Rights Enforcement Institutions: Institutional beneficiaries (institutional/constrained) — DOJ, civil rights agencies, courts gain jurisdictional authority but face doctrinal constraints on enforcement
 *   - Civil Rights Plaintiffs and Communities: Secondary victims (moderate/constrained) — bear litigation burdens and high evidentiary requirements to prove systemic subordination
 *   - Courts and Constitutional Interpreters: Doctrinal authorities (institutional/arbitrage) — benefit from interpretive power granted by antisubordination doctrine; experience minimal extraction
 *   - Policymakers and Institutional Architects: Secondary targets (powerful/mobile) — constrained by strict scrutiny for facially neutral subordinating policies but retain ability to evade through policy redesign
 *   - Colorblind Doctrine Institutional Inertia: Countervailing force (institutional/constrained) — maintains competing doctrine that narrows antisubordination enforcement; creates performative rather than substantive protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antisubordination_reading, 0.58).
domain_priors:suppression_score(antisubordination_reading, 0.72).
domain_priors:theater_ratio(antisubordination_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antisubordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(antisubordination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(antisubordination_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(antisubordination_reading, "Equal Protection as Antisubordination: Constitutional Prohibition on Caste-like Subordination").
narrative_ontology:topic_domain(antisubordination_reading, "constitutional_law/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(antisubordination_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antisubordination_reading, structural_equality_principle).
narrative_ontology:constraint_beneficiary(antisubordination_reading, civil_rights_enforcement_institutions).
narrative_ontology:constraint_victim(antisubordination_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(antisubordination_reading, facially_neutral_policy_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED GROUPS (SNARE) — Trapped in systems designed to perpetuate subordination. The antisubordination constraint theoretically protects them but enforcement requires proving systemic intent and causation across multiple policy domains (housing, education, employment, criminal justice). Groups bear costs of subordination while the constraint's enforcement mechanisms remain weak. High suppression through institutional barriers to remedy.
constraint_indexing:constraint_classification(antisubordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS PLAINTIFFS (SNARE) — Constrained by litigation costs, evidentiary burden (disparate impact doctrine requires statistical proof), and judicial skepticism of systemic discrimination claims. Must prove not merely disparate impact but subordination effect. Career and social costs for challenging embedded institutional practices. Significant extraction through litigation burden.
constraint_indexing:constraint_classification(antisubordination_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CIVIL RIGHTS ENFORCEMENT INSTITUTIONS (TANGLED ROPE) — Benefit from antisubordination mandate (expanded jurisdictional authority, enforcement power), but constrained by narrowing judicial interpretations (strict scrutiny for facially neutral policies; intent doctrine narrowing disparate impact). Active enforcement required but repeatedly blocked by doctrinal shifts. Mixed: genuine coordination function (adjudicating subordination claims) with asymmetric extraction (limited resources for expanding enforcement against facially neutral policies).
constraint_indexing:constraint_classification(antisubordination_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COURTS (ROPE) — See antisubordination as pure coordination: establishing clear constitutional standards for equal protection enables predictable governance and rule of law. Courts experience the constraint as a coordination mechanism (clarifying what 'equal protection' means in practice). They benefit from the interpretive authority it grants. No meaningful exit costs.
constraint_indexing:constraint_classification(antisubordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICYMAKERS (TANGLED ROPE) — Constrained by antisubordination doctrine (certain facially neutral policies now subject to strict scrutiny), but benefit from the coordination it provides (clear rules for policy design). Mobile: can restructure policies to appear facially neutral while maintaining subordinating effects through indirect mechanisms. Experiences both extraction (judicial review) and coordination (legal clarity).
constraint_indexing:constraint_classification(antisubordination_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLORBLIND DOCTRINE RESISTANCE (PITON) — Antisubordination enforcement persists in parallel with colorblind doctrine, creating performative motion without structural change. The constraint's enforcement mechanism (disparate impact doctrine, intent scrutiny) remains formally robust but functionally degraded by countervailing doctrines that narrow its application. Theater ratio high: litigation continues, constitutional language asserts protection, but remedies remain limited. Piton: institutional inertia of colorblind framing prevents antisubordination principle from achieving its stated function.
constraint_indexing:constraint_classification(antisubordination_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational perspective, caste-like subordination appears as an immutable structural fact: hierarchies are inherent to human societies, and subordination reflects natural differences or inevitable power distributions. This perspective risks naturalizing what the antisubordination reading identifies as contingent institutional design. Engine will flag as false summit candidate.
constraint_indexing:constraint_classification(antisubordination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antisubordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antisubordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antisubordination_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antisubordination_reading, TR),
    TR >= 0.70.

:- end_tests(antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint requires subordinated groups to bear litigation costs and meet demanding evidentiary burdens (proving systemic causation across institutional domains, establishing intent or severe disparate impact) to access remedies. The extraction is not as severe as pure Snare (0.66+) because antisubordination doctrine theoretically recognizes group-level harm and systemic effects, unlike colorblind doctrine that limits claims to individualized discrimination. But enforcement barriers are substantial: courts demand statistical proof, causal inference across domains, and increasingly scrutinize race-conscious remedies under strict scrutiny. Suppression (0.72): High. Multiple institutional barriers suppress antisubordination claims: (1) evidentiary burden (systemic proof across housing, education, employment, criminal justice is resource-intensive), (2) jurisdictional fragmentation (multiple agencies with overlapping authority), (3) doctrine narrowing (intent doctrine, strict scrutiny for facially neutral policies), (4) social suppression (challenging embedded institutional practices carries career and community costs for plaintiffs). Theater ratio (0.65): Moderate-high. Antisubordination language is prominent in constitutional and statutory doctrine (Civil Rights Act, Fair Housing Act, Title VI), and litigation continues actively. But measured remedies remain limited: institutional practices that perpetuate subordination are often left untouched, and race-conscious affirmative remedies face heightened constitutional scrutiny. The constraint performs protection (constitutional doctrine asserts it; courts adjudicate claims) while delivering limited structural change. The gap between doctrinal robustness and remedial weakness accounts for the theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence between beneficiaries and victims. Subordinated groups see Snare (trapped by enforcement barriers). Civil rights plaintiffs see Snare (constrained by litigation burden). Courts see Rope (pure coordination on constitutional standards). Policymakers see Tangled Rope (coordinate on legal standards; constrained by review). Colorblind doctrine inertia sees Piton (performative protection). The analytical observer risks seeing Mountain (caste-like subordination as inherent social fact). The gap is diagnostic: subordinated groups are nominally empowered by the constraint (named as protected) but structurally disadvantaged by its implementation (high proof burdens, limited remedies). The constraint redistributes authority toward courts and enforcement institutions while leaving substantive remedial power with those institutions that benefit from maintaining subordination patterns.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural position relative to antisubordination enforcement. Subordinated groups are theoretically beneficiaries (the constraint targets their protection) but structurally trapped, yielding high d (0.90+) — they are nominal beneficiaries experiencing victimization through enforcement barriers. Civil rights institutions are beneficiaries with constrained exit: they gain authority but face doctrinal limits, yielding moderate d (0.35-0.45). Courts are pure beneficiaries with arbitrage exit (they can interpret doctrine flexibly), yielding low d (0.15-0.25). Policymakers are caught between benefit (coordination on legal standards) and constraint (strict scrutiny for subordinating policies), yielding moderate-high d (0.55-0.65). Colorblind doctrine operates as a countervailing force preventing full enforcement, maintaining high institutional inertia — it is neither beneficiary nor victim but institutional resistance that reduces the constraint's effectiveness. The perspectival gap reveals the antisubordination reading's core tension: the groups it aims to protect face the highest enforcement burden; the institutional actors it empowers face the fewest constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVABLE VIA KERNEL READING: The constraint's classification shifts dramatically across different equal protection readings. This antisubordination reading produces Tangled Rope at institutional level (genuine coordination on anti-subordination principle; asymmetric extraction through proof burdens and remedy narrowing). The sibling colorblind reading produces Rope at institutional level (pure coordination without recognition of systemic subordination; perceived equality through formal rules). The sibling remedial reading produces Snare at institutional level (remedies themselves become extractive; affirmative action seen as reverse discrimination). The mandatrophy is not resolved by asking 'which type is correct?' but by recognizing that each reading instantiates a different constraint with different ε values. The antisubordination reading requires high evidentiary proof (ε=0.58); the colorblind reading avoids systemic proof entirely (would be ε≈0.30, Rope); the remedial reading accepts past discrimination but narrowly tailors remedies (would be ε≈0.45, Tangled Rope). The choice of reading is not empirical but constitutional — it depends on which equal protection principle courts adopt as authoritative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of equal protection is constitutionally correct: antisubordination (this constraint), colorblind doctrine, or remedial targeting?',
    'Constitutional interpretation via text analysis, original meaning, living constitutionalism, or precedent weight; jurisprudential commitment to one reading over others; implementation track record of each reading in reducing subordination',
    'If antisubordination prevails in jurisprudence: constraint classification remains Tangled Rope at institutional level. If colorblind doctrine prevails: antisubordination becomes Piton (theatrical, inertial). If remedial reading dominates: antisubordination becomes subordinated to narrower compensatory paradigm (shifts from systemic to individualized proof).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested constitutional reading: which equal protection doctrine is authoritative').

omega_variable(
    systemic_subordination_proof,
    'What evidentiary standard sufficiently demonstrates caste-like subordination across multiple institutional domains (housing, education, employment, criminal justice)?',
    'Development of cross-domain subordination metrics; longitudinal data on intergenerational effects of facially neutral policies; causal inference linking specific policy structures to subordination outcomes; judicial doctrine clarifying burden of proof',
    'If standard is achievable: antisubordination enforcement becomes viable (χ decreases, subordinated groups experience less snare). If standard remains prohibitively high: constraint remains Snare from subordinated group perspective; enforcement becomes performative (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_subordination_proof, empirical, 'Evidentiary standard for systemic subordination across domains').

omega_variable(
    intent_vs_effect_doctrine,
    'Does the constraint target subordination through intent (discriminatory motive) or effect (disparate impact), and which test is legally enforceable?',
    'Jurisprudential doctrine clarification; statutory revision of intent doctrine; precedent establishing disparate impact as sufficient for equal protection violation',
    'If intent required: extractiveness remains high (proving motivation across institutional actors is burdensome). If effect suffices: extractiveness decreases (systemic subordination becomes actionable without motive proof). Current doctrine ambiguity creates theater: both standards are invoked; neither operates consistently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_effect_doctrine, conceptual, 'Intent vs. effect standard in subordination doctrine').

omega_variable(
    remedial_scope_constraint,
    'What remedies are permissible under antisubordination? Are affirmative race-conscious policies (affirmative action, disparate impact correction) constitutionally justified as subordination remedies, or do they violate colorblind principles?',
    'Supreme Court doctrine on remedial classifications; sociological evidence on remedial policy effectiveness in reducing subordination; constitutional weight given to antisubordination vs. colorblind equal protection',
    'If remedies are broad: subordinated groups experience lower extraction (remedies reduce subordination). If remedies are narrow: constraint becomes aspirational but non-remedial (Piton: performs protection without delivering it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_scope_constraint, preference, 'Scope of permissible remedies for subordination').

omega_variable(
    institutional_capacity_for_systemic_judgment,
    'Do courts have institutional capacity and legitimacy to assess systemic subordination across multiple policy domains, or does adjudication of caste-like subordination exceed judicial competence?',
    'Track record of institutional competence (courts'' ability to design and oversee systemic remedies); comparison with legislative or administrative approaches to subordination; doctrinal debate over judicial role in structural reform',
    'If courts are competent: antisubordination enforcement is viable (Tangled Rope). If courts lack capacity: enforcement becomes symbolic (Piton); subordination remedies devolve to political process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_for_systemic_judgment, conceptual, 'Institutional capacity for adjudicating systemic subordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antisubordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antisub_theater_t0, antisubordination_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(antisub_theater_t25, antisubordination_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(antisub_theater_t50, antisubordination_reading, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(antisub_extract_t0, antisubordination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(antisub_extract_t25, antisubordination_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(antisub_extract_t50, antisubordination_reading, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(antisubordination_reading, colorblind_reading).
narrative_ontology:affects_constraint(antisubordination_reading, remedial_reading).
narrative_ontology:affects_constraint(antisubordination_reading, disparate_impact_doctrine).
narrative_ontology:affects_constraint(antisubordination_reading, strict_scrutiny_standard).

% DUAL FORMULATION NOTE:
% The antisubordination reading is one of three structurally distinct interpretations of the equal protection commitment. Each reading produces a different constraint with different ε values, beneficiary/victim structures, and classification types. The antisubordination reading is upstream of specific doctrinal constraints (disparate impact, strict scrutiny) that operationalize its principle; doctrinal constraints share the kernel but manifest at different scales (constitutional principle vs. evidentiary standard vs. remedy scope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(antisubordination_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
