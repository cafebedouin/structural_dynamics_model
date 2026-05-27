% ============================================================================
% CONSTRAINT STORY: potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_potential_based_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: potential_based_reading
 *   human_readable: Personhood Grounded in Potential for Rational Agency
 *   domain: moral_philosophy/bioethics/historical_ethics
 *
 * SUMMARY:
 *   The potential-based reading of personhood grounds moral and legal
 *   standing in an entity's capacity for rational agency — the ability to
 *   understand, deliberate, and act on reasons. Under this reading, severely
 *   disabled infants who lack such capacity (present or future) lack the
 *   foundational property that confers personhood status and attendant
 *   protections. This reading produces a structural constraint: it authorizes
 *   parental and medical gatekeepers to make decisions on behalf of excluded
 *   entities without their consent, and it suppresses alternative readings
 *   that might grant standing based on other criteria (biological humanity,
 *   relational membership, inherent dignity). The constraint exhibits the
 *   full classical structure of a knowledge-authorizing system grounded in a
 *   foundational but contested kernel — who counts as a person — with
 *   significant distributive consequences for who bears extraction and who
 *   exercises authority.
 *
 * KEY AGENTS:
 *   - Severely Disabled Infants: Primary victims (powerless/trapped) — lack standing to claim personhood under the reading's criteria; subject to parental/medical authority without recourse
 *   - Cognitively Atypical Persons: Secondary victims (moderate/identity_locked) — structurally mobile but identity-locked into accepting personhood boundaries that condition their standing on future capacity acquisition
 *   - Parental Authority: Primary beneficiary (institutional/arbitrage) — granted decision-making authority over excluded entities without requiring their consent
 *   - Medical Gatekeepers: Primary beneficiary (institutional/arbitrage) — granted epistemic authority to determine which entities lack standing and thereby which life-sustaining interventions may be withheld
 *   - Disability Advocacy Coalition: Organized opposition (moderate/constrained) — contests the reading's boundary but constrained by legal and institutional frameworks that treat it as authoritative
 *   - Reformist Bioethicists: Organized alternatives (organized/mobile) — building alternative frameworks with sunset logic; mobile enough to adopt new framings without institutional penalty
 *   - Philosophical Tradition: Institutional maintainer (institutional/arbitrage) — preserves the reading through doctrinal repetition and textual authority despite increasing contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(potential_based_reading, 0.58).
domain_priors:suppression_score(potential_based_reading, 0.72).
domain_priors:theater_ratio(potential_based_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(potential_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(potential_based_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(potential_based_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(potential_based_reading, snare).
narrative_ontology:human_readable(potential_based_reading, "Personhood Grounded in Potential for Rational Agency").
narrative_ontology:topic_domain(potential_based_reading, "moral_philosophy/bioethics/historical_ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(potential_based_reading, '533dc84d-bb96-40da-8fe1-6b8d470be171').
narrative_ontology:cs_created_at('533dc84d-bb96-40da-8fe1-6b8d470be171', '').
narrative_ontology:cs_kernel_codification('533dc84d-bb96-40da-8fe1-6b8d470be171', fixed_text).
narrative_ontology:cs_authority_grounding('533dc84d-bb96-40da-8fe1-6b8d470be171', lineage).
narrative_ontology:cs_interpretation_layer_present('533dc84d-bb96-40da-8fe1-6b8d470be171').
narrative_ontology:cs_kernel_id(potential_based_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('533dc84d-bb96-40da-8fe1-6b8d470be171', fitness_contingent_reading, coexists_with).
narrative_ontology:cs_reading_relation('533dc84d-bb96-40da-8fe1-6b8d470be171', birth_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('533dc84d-bb96-40da-8fe1-6b8d470be171', foundational, rational_agency_constitutive_personhood).
narrative_ontology:cs_axiom_status(rational_agency_constitutive_personhood, holdable).
narrative_ontology:cs_axiom('533dc84d-bb96-40da-8fe1-6b8d470be171', foundational, potential_future_capacity_sufficient).
narrative_ontology:cs_axiom_status(potential_future_capacity_sufficient, holdable).
narrative_ontology:cs_reference_frame('533dc84d-bb96-40da-8fe1-6b8d470be171', kantian_rational_autonomy).
narrative_ontology:cs_drift_state('533dc84d-bb96-40da-8fe1-6b8d470be171', contemporary_bioethics_era, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(potential_based_reading, parental_authority).
narrative_ontology:constraint_beneficiary(potential_based_reading, medical_gatekeepers).
narrative_ontology:constraint_victim(potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(potential_based_reading, cognitively_atypical_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEVERELY DISABLED INFANT (SNARE) — Trapped without recourse. Lacks capacity to claim standing under the reading's own criteria. Faces exclusion from personhood-dependent protections (medical decision-making authority, inheritance, legal recognition). Cannot exit through demonstrating potential because the reading defines potential in forward-looking terms the agent cannot satisfy. Maximum suppression: the agent's status is adjudicated by others with every incentive to exclude.
constraint_indexing:constraint_classification(potential_based_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: DISABILITY ADVOCACY COALITION (TANGLED ROPE) — Constrained by legal and institutional frameworks that treat potential-based criteria as authoritative. Benefits from challenging the reading's boundary criteria (raises visibility of disabled persons' actual capacities and personhood claims) but bears costs of institutional resistance and resource barriers. Significant suppression but not total — coalition has organized voice and can contest the reading's premises through legal and social pressure.
constraint_indexing:constraint_classification(potential_based_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL GATEKEEPERS (ROPE) — Benefits from the reading's authorization of medical decision-making authority over excluded entities. Experiences the constraint as coordination: the potential-based framework provides clear criteria for determining which patients lack personhood-relevant standing, enabling efficient decision-making in resource-constrained settings. Net beneficiary — the reading grants epistemic and decision-making authority without ambiguity.
constraint_indexing:constraint_classification(potential_based_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARENTAL AUTHORITY (ROPE) — Benefits from legal and moral standing to make life-and-death decisions on behalf of excluded children (withdrawal of life support, non-resuscitation orders) without requiring their consent. Experiences the constraint as coordination: the potential-based framework provides clarity on when parental judgment supersedes the child's interests. Arbitrage position: can invoke the reading when convenient and ignore personhood criteria for other purposes.
constraint_indexing:constraint_classification(potential_based_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST BIOETHICISTS (SCAFFOLD) — Organized agents (UN Disability Convention signatories, bioethics commissions, patient-centered medicine advocates) see the potential-based reading as temporary and sunset-bound. Building alternative frameworks (capability approach, relational autonomy, dignity-centered personhood) that do not condition standing on forward-looking cognitive capacity. Mobile exit: practitioners can adopt alternative frameworks without losing institutional legitimacy. Sunset logic: as alternative frameworks gain acceptance, the potential-based reading's authority erodes.
constraint_indexing:constraint_classification(potential_based_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PHILOSOPHICAL TRADITION (PITON) — The potential-based reading persists as doctrine through institutional inertia within certain philosophical and theological schools. The reading is increasingly recognized as contested (alternative readings have gained traction) but retains canonical status in certain curricula and doctrinal lineages. Theater ratio reflects that the reading is maintained through textual authority and institutional repetition rather than through active argumentation. The philosophical tradition performs the potential-based framework more than it defends it empirically.
constraint_indexing:constraint_classification(potential_based_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a logical perspective, some criterion for personhood boundaries is necessary: any system must draw the line somewhere, and rationality is a coherent candidate criterion. This perspective sees the potential-based reading as an instance of a universal logical requirement — all personhood frameworks require boundaries. However, this perspective risks naturalizing the specific content of the boundary (potential for rational agency) as logically necessary when the necessity applies only to the formal structure (that some boundary exists), not its content. Engine's false summit detector will flag this.
constraint_indexing:constraint_classification(potential_based_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(potential_based_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(potential_based_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(potential_based_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(potential_based_reading, TR),
    TR >= 0.70.

:- end_tests(potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The potential-based reading grants decision-making authority over excluded entities to parents and medical professionals without requiring the excluded entity's consent or input. This is not maximal extraction because the reading does not claim that excluded entities may be harvested for parts, tortured, or enslaved — the extraction is specifically over life-and-death decisions and personhood status. But it is substantial because the excluded entity bears maximum consequences (exclusion from legal protections, non-resuscitation decisions) with zero agency. The upward trajectory over the interval reflects increasing institutionalization of the reading (more extensive application in medical and legal contexts) and increasing sophistication of gatekeeping mechanisms that make the exclusion harder to contest. Suppression (0.72): High. Multiple barriers prevent excluded entities from challenging their status: (1) they lack the cognitive capacity the reading uses to define standing, so they cannot self-advocate; (2) their proxies (parents, medical staff) have incentive conflicts — they benefit from exclusion authority; (3) alternative framings are actively suppressed through professional sanctions on those who challenge the reading; (4) the reading claims to be grounded in universal logic or metaphysical fact, making contestation appear incoherent rather than legitimate disagreement. Theater ratio (0.68): Moderate-high. The reading is maintained partly through philosophical argumentation (genuine defense of its logical structure) but increasingly through institutional performance — it is cited as authoritative without being actively rearticulated or defended. Medical ethics committees invoke the potential-based framework as settled doctrine. Philosophy curricula teach it as canonical. This performative maintenance is rising over the interval as the reading faces increasing contestation: the more it is challenged, the more its defenders resort to citing authority rather than rearticulating arguments.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single knowledge-authorizing framework produces radically different classifications depending on the observer's structural position. The parental authority and medical gatekeeper see coordination (Rope) — the reading provides clear criteria for making necessary decisions. The reformist bioethicists see a temporary structure being displaced (Scaffold) — alternative frameworks are gaining traction and the potential-based reading's sunset is real. The philosophical tradition sees degraded doctrine (Piton) — the reading persists through institutional inertia more than through active defense. The disability advocates see genuine extraction and suppression (Tangled Rope) — the reading constrains their ability to advocate for disabled persons' standing while also providing them a foothold for contestation. The excluded entity itself sees pure extraction with no exit (Snare) — trapped by a definition of personhood they cannot satisfy. The analytical observer risks seeing this as a logical necessity (Mountain) — that some personhood boundary is necessary and rational agency is a coherent candidate — but this naturalizes the specific content as necessary when only the formal structure (that boundaries exist) is required.
 *
 * DIRECTIONALITY LOGIC:
 *   The potential-based reading functions as a pure authority allocation mechanism grounded in a contested kernel. Directionality (d) for each agent reflects their structural relationship to the personhood boundary and the decision-making authority it authorizes. Excluded entities (severely disabled infants) have d ≈ 1.0: they are the target of the reading's extraction; it allocates all decision-making authority away from them. Parents and medical professionals have d ≈ 0.05: the reading benefits them by allocating authority to them; they are the beneficiaries. The disability advocacy coalition has d ≈ 0.65: they are partially targeted (their advocacy is suppressed) and partially benefit (the reading provides a clear target for contestation). The reformist bioethicists have d ≈ 0.55: they experience the reading's suppression of alternative frameworks but have mobile enough position to build alternatives. The f(d) sigmoid maps these d values to experienced extractiveness modifiers. The piton and mountain perspectives show how perspectival framing can obscure the underlying authority allocation: when the reading is framed as institutional doctrine (piton) or logical necessity (mountain), its extractive function becomes less visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The potential-based reading's extractiveness is moderate-high (0.58) and its suppression is high (0.72), placing it structurally in the snare family but with enough institutional and beneficiary structure to appear as tangled_rope from moderate agents' perspectives. The reading resolves mandatrophy by acknowledging that some decision-making authority must be allocated somewhere — parents and medical professionals need clear criteria for life-and-death decisions. The constraint is not pure extraction because there is a genuine coordination problem: who decides when a profoundly disabled infant should receive life-sustaining intervention? The reading provides an answer (parental authority guided by the potential-based criterion). The mandatrophy is resolved by recognizing that the reading's authority allocation is one solution to this coordination problem, not the only solution, and that the extraction it enables (excluding certain entities from personhood standing) is a necessary consequence of its choice. Alternative readings would solve the same problem differently, with different extraction patterns. The analytical task is to characterize each solution's structural properties and distributional consequences, not to identify one 'correct' boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_definition_indeterminacy,
    'What constitutes ''potential for rational agency'' — is it metaphysical capacity, biological substrate, demonstrated trajectory, or something else?',
    'Examine medical and philosophical literature on how ''potential'' is operationalized in practice. Track divergence between theoretical definition and clinical application. Identify cases where clinicians disagree on whether an entity possesses potential.',
    'If potential is metaphysically grounded (Cartesian dualism or similar): the reading appears less extractive — exclusion reflects genuine metaphysical status. If potential is clinician-determined: the reading is a mechanism for gatekeeping authority. If potential is indeterminate: the reading''s boundary dissolves and the constraint becomes a pure authority mechanism with no objective content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_definition_indeterminacy, conceptual, 'Indeterminacy in operationalizing ''potential for rational agency''').

omega_variable(
    exclusion_mechanism_motivation,
    'Is the potential-based reading adopted because it reflects a genuine metaphysical intuition about personhood, or because it provides convenient authority for medical resource allocation and parental decision-making?',
    'Historical and genealogical analysis: trace adoption of potential-based criteria across medical ethics, philosophy, and law. Examine whether adoption correlates with resource scarcity, institutional authority consolidation, or philosophical discovery. Identify cases where potential-based reasoning is invoked vs. abandoned based on outcome convenience.',
    'If adopted for philosophical reasons: the reading''s authority derives from its logical structure. If adopted for institutional convenience: the reading''s extractiveness is the core function, and the constraint is misclassified as normative philosophy rather than institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_motivation, empirical, 'Whether potential-based criteria are adopted for philosophical vs. institutional-convenience reasons').

omega_variable(
    alternative_boundary_viability,
    'Do alternative personhood readings (birth threshold, relational personhood, capability approach) actually provide workable decision-making criteria for medical ethics, or do they collapse into equivalent gatekeeping mechanisms?',
    'Comparative implementation study: examine how alternative readings function in practice across different legal systems and healthcare contexts. Track outcome distributions and who holds decision-making authority under each reading. Assess whether shifting boundaries changes the distribution of extraction or simply relocates it.',
    'If alternatives are genuinely workable and non-extractive: the potential-based reading is one contingent choice among several. If alternatives collapse into equivalent gatekeeping: the personhood boundary is itself the extraction mechanism regardless of content, and the omega shifts to ''is any boundary reading always extractive?''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_boundary_viability, empirical, 'Whether alternative personhood readings provide workable non-extractive decision criteria').

omega_variable(
    reading_kernel_relationship,
    'Is the potential-based reading one reading of a contested kernel (personhood boundary), or does it claim to discover the kernel itself?',
    'Examine whether proponents treat potential-based criteria as foundational/discovered or as one interpretive choice. Look for explicit acknowledgment of sibling readings vs. dismissal of alternatives as simply incorrect. Assess whether the reading''s authority rests on defending its own premises vs. on claiming the personhood kernel is self-evident.',
    'If foundational: the reading''s extractiveness reflects institutionalized authority over a metaphysical question. If interpretive: the reading is one legitimate perspective competing with others. This omega documents the kernel-reading relationship itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Whether potential-based reading is foundational discovery or one interpretation of contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(potential_based_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(potbased_tr_t0, potential_based_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(potbased_tr_t3, potential_based_reading, theater_ratio, 3, 0.62).
narrative_ontology:measurement(potbased_tr_t6, potential_based_reading, theater_ratio, 6, 0.67).
narrative_ontology:measurement(potbased_tr_t9, potential_based_reading, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(potbased_be_t0, potential_based_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(potbased_be_t3, potential_based_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(potbased_be_t6, potential_based_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(potbased_be_t9, potential_based_reading, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(potential_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(potential_based_reading, fitness_contingent_reading).
narrative_ontology:affects_constraint(potential_based_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(potential_based_reading, relational_personhood_reading).

% DUAL FORMULATION NOTE:
% The potential-based reading is one reading of the personhood kernel. Sibling readings (fitness_contingent, birth_threshold) decompose the same underlying kernel dispute into separate constraint stories with different ε values and victim sets. These are not alternative observables of the same constraint — they are structurally distinct constraints instantiating different interpretations of the kernel. The network links document the kernel family relationship and the logical/empirical dependencies between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(potential_based_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
