% ============================================================================
% CONSTRAINT STORY: altruistic_misery_paradox_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_altruistic_misery_paradox_2026, []).

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
 *   constraint_id: altruistic_misery_paradox_2026
 *   human_readable: The Paradox of Altruistic Misery
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The paradox of altruistic misery captures a structural constraint
 *   operating across social, familial, and institutional contexts: social
 *   pressure to prioritize others' wellbeing through self-sacrifice creates a
 *   system where those who give most suffer most. This constraint exhibits
 *   the defining features of a Tangled Rope: it coordinates care provision
 *   (genuine coordination function) while simultaneously extracting emotional
 *   labor, financial resources, and health from the sacrificing agents
 *   (asymmetric extraction). The constraint is enforced through guilt,
 *   religious/cultural narrative, social status signals, and institutional
 *   design (understaffed care systems, inadequate public support). The
 *   theater ratio (0.64) reflects that much of the observable 'altruism' is
 *   performative virtue signaling rather than effective care provision.
 *   Mental health advocacy and public support initiatives represent a
 *   structural sunset pathway, though cultural change is slow. The analytical
 *   observer faces a false summit: the temptation to naturalize altruistic
 *   misery as inherent to human bonding masks the contingent institutional
 *   arrangements (guilt-based obligation, inadequate systemic support) that
 *   could be redesigned.
 *
 * KEY AGENTS:
 *   - Self-Sacrificing Agents: Primary victim (powerless/trapped) — caregivers, family members, care workers bearing emotional and material extraction with no viable exit
 *   - Dependent Beneficiaries: Secondary victim/beneficiary (moderate/constrained) — children, parents, vulnerable persons who receive care but also bear the psychological weight of 'being the reason' for another's sacrifice
 *   - Social Norm Enforcement System: Primary beneficiary (institutional/arbitrage) — religious institutions, family structures, cultural narratives that legitimize obligation and reduce accountability for systemic care failures
 *   - Institutional Power Brokers: Secondary beneficiary (powerful/mobile) — corporations, hospitals, states benefiting from unpaid care work and volunteer labor; can exit exploitation through alternative labor models but dependent on continued altruistic obligation
 *   - Mental Health and Wellness Coalition: Organized agents (organized/constrained) — therapists, boundary-setting movements building alternative narratives; represent structural sunset pathway through psychological literacy
 *   - Collective Wellbeing: Structural victim (powerless/trapped) — abstract collective good degraded by caregiver burnout, mental health deterioration, reduced care quality under extraction conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(altruistic_misery_paradox_2026, 0.52).
domain_priors:suppression_score(altruistic_misery_paradox_2026, 0.68).
domain_priors:theater_ratio(altruistic_misery_paradox_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(altruistic_misery_paradox_2026, tangled_rope).
narrative_ontology:human_readable(altruistic_misery_paradox_2026, "The Paradox of Altruistic Misery").
narrative_ontology:topic_domain(altruistic_misery_paradox_2026, "social/psychological").

domain_priors:requires_active_enforcement(altruistic_misery_paradox_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, social_norm_enforcers).
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, extraction_beneficiaries).
narrative_ontology:constraint_victim(altruistic_misery_paradox_2026, self_sacrificing_agents).
narrative_ontology:constraint_victim(altruistic_misery_paradox_2026, collective_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SELF-SACRIFICING CAREGIVER (SNARE) — Trapped within family, community, or institutional roles where self-denial is framed as moral duty. Exit is existentially costly (loss of identity, community expulsion, guilt). Bears full extraction: emotional labor, financial depletion, health deterioration. No viable exit option. Maximum experienced extraction due to trapped status and powerlessness.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEPENDENT BENEFICIARY (TANGLED_ROPE) — Child, parent, or dependent family member who receives care and material benefit from the sacrificer. Benefits from the extraction (care, resources) but also constrained by the psychological weight of being 'the reason' for another's misery. Experiences mixed coordination (legitimate caregiving) and extraction (guilt, obligation imposed on them). Constrained exit — can separate but at significant social/emotional cost.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOCIAL NORM ENFORCEMENT SYSTEM (ROPE) — Religious institutions, family structures, cultural narratives, and social media amplify altruistic obligation as coordination mechanism. Experiences the constraint as solving a collective action problem: ensuring care provision for vulnerable members. Beneficiary through institutional legitimacy, social control mechanisms, and reduced accountability for systemic failures. Arbitrage exit — can reframe norms without substantial cost.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MENTAL HEALTH ADVOCACY COALITION (SCAFFOLD) — Organized agents (therapists, wellness advocates, boundary-setting movements) are constructing alternative narratives emphasizing self-care and psychological autonomy. See the altruistic misery trap as a temporary institutional failure with sunset: as mental health literacy spreads and therapeutic practices normalize self-compassion, the extractive power of guilt-based obligation declines. Constrained exit because cultural change is slow, but trajectory is toward structural decline of the constraint.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS/VIRTUE-SIGNALING SYSTEMS (PITON) — Traditional religious frameworks and contemporary social-media-driven virtue displays maintain performative altruism long after their functional coordination purpose has atrophied. Rituals of self-sacrifice (tithing, emotional labor performances, conspicuous suffering) persist through institutional inertia rather than genuine coordination function. Theater ratio (0.64) reflects that much altruistic display is performative signaling rather than effective care. Degraded institutions maintained through habit and social pressure rather than demonstrated benefit.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL POWER BROKERS (TANGLED_ROPE) — Large organizations (corporations, hospitals, religious institutions, states) benefit from voluntary self-sacrifice norms reducing their obligation to provide adequate compensation, support systems, or safety infrastructure. They coordinate resource allocation while extracting uncompensated labor (care workers, volunteers, clergy). Mobile exit (can shift to exploitative contracting models) but also genuinely dependent on volunteer labor — mixed coordination and extraction.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN - FALSE SUMMIT) — The universal/civilizational view risks naturalizing altruistic misery as an inherent feature of human bonding: 'Love requires sacrifice, sacrifice requires suffering, therefore those who love must suffer.' This perspective treats the paradox as an immutable law of social existence. However, the structural data reveals this as contingent institutional arrangements (guilt-based obligation, inadequate public support, concentrated caregiving responsibility) rather than natural laws. Engine detects false summit.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(altruistic_misery_paradox_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(altruistic_misery_paradox_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(altruistic_misery_paradox_2026, TR),
    TR >= 0.70.

:- end_tests(altruistic_misery_paradox_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts substantial costs from sacrificing agents (emotional labor, financial depletion, health deterioration, lost opportunities) while concentrating benefits among institutional norm enforcers and dependent beneficiaries. The value reflects that extraction is real and structural but not total — some genuine care coordination occurs alongside the extraction, and some agents experience genuine fulfillment. Suppression (0.68): High. Multiple barriers prevent exit: psychological (internalized guilt, identity dissolution), social (community expulsion, status loss), economic (inadequate alternative care systems), and institutional (concentrated caregiving responsibility). The suppression value reflects that alternatives exist but carry substantial costs. Theater ratio (0.64): Moderate-high. Much observable 'altruism' is performative: social media virtue signaling, religious displays of suffering, conspicuous self-sacrifice for status. But the ratio is not maximal because some genuine care coordination persists. The theater has increased as social media enables performance-intensive virtue display.
 *
 * PERSPECTIVAL GAP:
 *   The constraint reveals a profound perspectival gap between the sacrificer and beneficiary. The self-sacrificing agent sees a Snare: they are trapped, their needs are systematically deprioritized, they bear extraction costs with no viable exit. The dependent beneficiary sees Tangled Rope: they receive genuine care (coordination benefit) but also bear the psychological weight of obligation and guilt — mixed experience. The institutional norm enforcer sees Rope: they experience the constraint as solving a legitimate coordination problem (ensuring care provision) while extracting uncompensated labor (arbitrage benefit). The analytical observer risks seeing a Mountain: naturalizing the paradox as inherent to human bonding rather than recognizing the contingent institutional arrangements. The mental health coalition sees a Scaffold with sunset: they recognize structural pathways (public support systems, therapeutic literacy, boundary-setting norms) that will degrade the constraint's extractive power. The religious/virtue-signaling system sees a Piton: the performative displays of altruism persist through inertia even as their functional coordination purpose has atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the constraint. Sacrificing agents are trapped victims with no exit (d ~0.92, f(d) ~1.40): maximum experienced extraction. Dependent beneficiaries are constrained victims-beneficiaries (d ~0.55, f(d) ~0.75): mixed experience reflecting genuine care coordination alongside obligation. Institutional norm enforcers are beneficiaries with arbitrage exit (d ~0.08, f(d) ~-0.16): negative effective extraction (they benefit from the constraint). Power brokers are beneficiaries with mobile exit (d ~0.45, f(d) ~0.55): moderate extraction benefit. Organized coalitions are constrained with agency (d ~0.42, f(d) ~0.43): moderate experienced extraction because they perceive sunset pathways. The analytical observer has analytical exit (d ~0.72, f(d) ~1.15) and risks naturalizing contingency, producing false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The paradox of altruistic misery resolves mandatrophy by demonstrating how the same structural arrangement can coordinate necessary care while extracting from caregivers. The Tangled Rope classification prevents two mislabelings: (1) treating it as pure coordination (Rope) would ignore the genuine harm to sacrificing agents and the institutional benefits from their subordination; (2) treating it as pure extraction (Snare) would ignore that some care coordination is genuinely needed and that some beneficiaries experience authentic care rather than pure predation. The Tangled Rope accurately captures the hybrid: there is a real coordination function (care provision for vulnerable persons) that requires genuine cooperation, AND there is asymmetric extraction (guilt-based obligation, uncompensated labor, health deterioration of sacrificers). The extractive component is not accidental to the coordination — it is the mechanism that makes the coordination 'free' for institutional beneficiaries. The psychological component (guilt, identity fusion with caregiver role) amplifies the extraction by making exit appear impossible even where material alternatives exist. The mental health coalition's Scaffold perspective confirms that institutional redesign (public support systems, therapeutic literacy, boundary-setting norms) can reduce the extractive power while preserving legitimate care coordination. The analytical observer's false summit reveals how naturalization language ('love requires sacrifice', 'that's just how families work') disguises contingent institutional arrangements as natural laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_boundary,
    'What distinguishes genuine voluntary altruism from coerced self-sacrifice masked as choice?',
    'Empirical study of exit costs and psychological freedom; comparison of self-sacrifice rates in high-support vs unsupported contexts; longitudinal tracking of burnout and guilt trajectories',
    'If boundary is permeable: most ''altruistic'' sacrifice is actually coerced, raising extractiveness to 0.65+. If boundary is clear: some genuine altruism exists, extractiveness remains ~0.52.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_boundary, empirical, 'Boundary between voluntary altruism and coerced self-sacrifice').

omega_variable(
    collective_wellbeing_extraction,
    'Does normalized altruistic misery reduce collective wellbeing by degrading the mental health of the sacrificers, or does it maintain societal cohesion through distributed care?',
    'Population-level mental health metrics in high-altruism vs high-self-care cultures; correlation between altruistic obligation norms and suicide rates, depression incidence, and caregiver burnout; measurement of care quality under coerced vs supported conditions',
    'If collective wellbeing degrades: the constraint is pure extraction (Snare from field perspective). If it maintains minimal cohesion: the constraint is hybrid coordination-extraction (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_wellbeing_extraction, empirical, 'Whether altruistic misery reduces or maintains collective wellbeing').

omega_variable(
    guilt_mechanism_origin,
    'Is guilt-based obligation a coordinating mechanism selected for its effectiveness, or a residual control tool from patriarchal/authoritarian structures?',
    'Historical analysis of guilt-obligation norms; comparison with alternative coordination mechanisms (reciprocity, mutual aid, institutional support); ethnographic study of cultures with high altruism but low guilt-based obligation',
    'If selected for effectiveness: the constraint is coordination-heavy (Rope/Tangled Rope). If residual control tool: the constraint is extraction-heavy (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guilt_mechanism_origin, conceptual, 'Origin and function of guilt-based obligation mechanisms').

omega_variable(
    public_support_sufficiency,
    'Would universal, well-funded public support systems (healthcare, elder care, childcare, mental health services) eliminate the extractive power of altruistic obligation, or does the paradox persist at deeper psychological levels?',
    'Policy intervention studies comparing jurisdictions with high vs low public care infrastructure; measurement of altruistic misery in wealthy vs resource-poor contexts; psychological analysis of guilt persistence despite material support availability',
    'If public support eliminates extraction: the constraint degrades to Scaffold/Piton. If paradox persists: the constraint''s extractive mechanism is psychological rather than structural (impacts classification rationale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_support_sufficiency, empirical, 'Whether public support systems can eliminate altruistic misery constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(altruistic_misery_paradox_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altru_tr_t0, altruistic_misery_paradox_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(altru_tr_t5, altruistic_misery_paradox_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(altru_tr_t10, altruistic_misery_paradox_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(altru_be_t0, altruistic_misery_paradox_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(altru_be_t5, altruistic_misery_paradox_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(altru_be_t10, altruistic_misery_paradox_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(altruistic_misery_paradox_2026, resource_allocation).
narrative_ontology:affects_constraint(altruistic_misery_paradox_2026, caregiver_labor_undervaluation).
narrative_ontology:affects_constraint(altruistic_misery_paradox_2026, guilt_based_compliance).
narrative_ontology:affects_constraint(altruistic_misery_paradox_2026, family_enmeshment_dynamics).

% DUAL FORMULATION NOTE:
% The altruistic misery paradox represents a hybrid coordination-extraction constraint. Upstream constraints (family enmeshment dynamics, guilt-based compliance mechanisms) create the psychological conditions enabling extraction. Downstream constraints (caregiver labor undervaluation, institutional inadequacy of public care) concentrate benefits among norm enforcers. The three are linked through the guilt mechanism: guilt functions simultaneously as a coordination signal (care is needed) and an extraction tool (obligation is internalized). Each related constraint has its own extractiveness value reflecting its specific structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(altruistic_misery_paradox_2026, moderate, 0.55).
constraint_indexing:directionality_override(altruistic_misery_paradox_2026, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
