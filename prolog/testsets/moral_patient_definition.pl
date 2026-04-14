% ============================================================================
% CONSTRAINT STORY: moral_patient_definition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_patient_definition, []).

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
 *   constraint_id: moral_patient_definition
 *   human_readable: Moral Patient Definition and Recognition
 *   domain: moral_philosophy/applied_ethics
 *
 * SUMMARY:
 *   The definition of moral patient status — who counts as deserving of moral
 *   consideration — is a foundational structural constraint that operates
 *   across philosophy, law, medicine, and policy. It determines which beings
 *   can have rights, which harms matter morally, and who has standing to
 *   advocate for protection. The constraint exhibits a fundamental asymmetry:
 *   those with institutional power to define moral patients are typically not
 *   at risk of being excluded from consideration, while those who stand to be
 *   excluded have minimal power to contest their exclusion. The definitional
 *   frameworks (based on criteria like rationality, sentience, autonomy,
 *   personhood) function simultaneously as coordination mechanisms enabling
 *   moral argument and as extraction tools that protect the moral status of
 *   existing moral agents against challenge. The constraint has historically
 *   demonstrated periodic expansion — formerly excluded groups like non-human
 *   animals, women, disabled humans, and indigenous peoples have gradually
 *   gained moral patient recognition — yet the mechanism remains extractive
 *   during exclusion periods. The high theater ratio reflects that
 *   contemporary definitional debates often employ sophisticated
 *   philosophical language and apparent neutrality while maintaining stable
 *   exclusions that serve the interests of those with definitional power.
 *
 * KEY AGENTS:
 *   - Excluded Entities: Primary victims (powerless/trapped) — denied moral patient status with no mechanism for appeal or voice in definitional process; includes sentient non-humans, conscious AI candidates, marginalized human groups
 *   - Rights Advocates: Secondary agents (moderate/constrained) — push for definitional expansion; benefit from moral frameworks that enable argument but face institutional suppression of challenges to established boundaries
 *   - Moral Framework Authorities: Primary beneficiaries (institutional/arbitrage) — philosophers, legal systems, policy bodies that hold definitional power; perceive constraint as coordination mechanism enabling moral argument
 *   - Organized Excluded Coalitions: Organized agents (organized/constrained) — disability rights, animal welfare, indigenous movements; challenge definitional authority from collective position; both benefit from moral arguments and suffer extraction through exclusion
 *   - Traditional Definitional Institutions: Institutional actors maintaining inertial definitions (institutional/arbitrage) — perform sophistication while resisting change; benefit from definitional stability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing definitional boundaries as necessary features of any moral system rather than recognizing power asymmetries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_patient_definition, 0.58).
domain_priors:suppression_score(moral_patient_definition, 0.65).
domain_priors:theater_ratio(moral_patient_definition, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_patient_definition, extractiveness, 0.58).
narrative_ontology:constraint_metric(moral_patient_definition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(moral_patient_definition, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_patient_definition, tangled_rope).
narrative_ontology:human_readable(moral_patient_definition, "Moral Patient Definition and Recognition").
narrative_ontology:topic_domain(moral_patient_definition, "moral_philosophy/applied_ethics").

domain_priors:requires_active_enforcement(moral_patient_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_patient_definition, moral_agents_with_definitional_power).
narrative_ontology:constraint_beneficiary(moral_patient_definition, established_rights_frameworks).
narrative_ontology:constraint_victim(moral_patient_definition, potential_moral_patients_excluded).
narrative_ontology:constraint_victim(moral_patient_definition, moral_status_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ENTITY (SNARE) — An agent denied moral patient status (sentient non-human animal, conscious AI, fetus, severely disabled human, non-Western person in colonial context) bears all costs of exclusion with no mechanism for appeal or reclassification. The denial is enforced through institutional definitions and philosophical consensus that the excluded party cannot contest. Maximum suppression: the excluded entity has no voice in the definitional process and cannot exit the category.
constraint_indexing:constraint_classification(moral_patient_definition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RIGHTS ADVOCATE (TANGLED ROPE) — Advocates for expanding moral patient status (animal rights movements, disability rights, AI safety researchers) benefit from the definitional framework that enables moral argument, but are constrained by the high burden of proof required to challenge established definitions. They coordinate with allies and contribute to expanding the moral circle, but face epistemic and political suppression. Moderate extraction: they have agency and some success but face institutional resistance.
constraint_indexing:constraint_classification(moral_patient_definition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MORAL FRAMEWORK AUTHORITY (ROPE) — Institutional actors (philosophy departments, legal systems, international bodies) who define moral patient status benefit from the definitional power and coordinate the distribution of moral status through established criteria. They perceive the constraint as coordination: defining who counts as a moral patient is essential to making moral argument possible. Low experienced extraction because they have institutional power and high exit optionality.
constraint_indexing:constraint_classification(moral_patient_definition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED EXCLUDED COALITION (TANGLED ROPE) — When formerly excluded groups organize (disability rights coalitions, indigenous peoples' movements, animal welfare organizations), they challenge definitional authority from a position of collective power. They both benefit from moral status arguments and suffer extraction through definitional exclusion. Higher agency than individual excluded agents but still constrained by need to persuade rather than control the definitional process.
constraint_indexing:constraint_classification(moral_patient_definition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRADITIONAL DEFINITIONAL RITUAL (PITON) — Philosophical debates about moral patient criteria (rationality, sentience, personhood) persist largely as theater: the criteria have become increasingly difficult to defend philosophically, yet institutional inertia maintains the established definitions. New criteria (ecological participation, relational value) emerge regularly but fail to dislodge the classical frameworks. Theater ratio is high because the definitional process performs legitimacy while concrete moral status assignments remain highly resistant to change.
constraint_indexing:constraint_classification(moral_patient_definition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN-RISK) — From a civilizational view, some definitional gating is perhaps inevitable to any moral system: you must draw a line somewhere between those who count morally and those who do not. This perspective risks naturalizing a contingent institutional arrangement as a logical necessity. However, the structural data (high extraction, suppression, asymmetric definitional power) contradicts the mountain classification — the engine will flag this as a false summit, revealing that the 'logical necessity' framing obscures the power asymmetries embedded in definition-making.
constraint_indexing:constraint_classification(moral_patient_definition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_patient_definition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_patient_definition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_patient_definition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_patient_definition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(moral_patient_definition, TR),
    TR >= 0.70.

:- end_tests(moral_patient_definition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts by controlling who receives moral consideration. The measurement trajectory shows increasing extractiveness over the 100-unit interval, reflecting that as moral discourse becomes more sophisticated and philosophical criteria more elaborate, the definitional barriers to new moral patients grow more difficult to overcome. The base value (0.35 at start) reflects periods of relative definitional openness; the current value (0.58) reflects entrenchment of classical criteria and sophisticated philosophical arguments defending exclusion. The extractiveness is not at snare levels because organized coalitions have achieved partial success in expanding definitions (animals, disability, indigenous populations), indicating that the exclusion mechanism is not absolute. Suppression (0.65): Moderate-high and stable. Significant suppression mechanisms include: (1) epistemic suppression — excluded entities cannot articulate their own moral status claims from within excluded position; (2) institutional suppression — definitional authority is concentrated in philosophy departments, legal systems, and policy bodies that are not representative of potentially excluded groups; (3) discourse suppression — challenging the established criteria is often treated as philosophically naive rather than as a legitimate challenge to power; (4) practical suppression — even groups that achieve nominal moral patient status face implementation barriers in practice. Theater ratio (0.68): High and increasing. Philosophical debates about moral patient criteria have become increasingly sophisticated in recent decades, with elaborate arguments about consciousness, capacity for suffering, autonomy, relational value, and other criteria. Yet the concrete assignments of moral status to new groups remain highly resistant to change, suggesting that the philosophical sophistication functions largely as legitimating theater rather than as genuine decision procedure. The measurement trajectory shows theater increasing faster than extractiveness, indicating that the gap between philosophical debate and practical status assignment is widening.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the full span of classification from a single structural data set. The excluded entity sees snare (impossible to exit, no voice in definition, maximum extraction). The rights advocate sees tangled rope (can argue and occasionally succeed, but constrained and suppressed). The moral authority sees rope (coordination mechanism that enables their own moral framework). The organized coalition sees tangled rope with scaffold aspects (some definitional victories achieved, some hope for further expansion). The institution sees piton (performs sophisticated debate while maintaining stable exclusions). The analytical observer risks mountain (logic requires some boundary) but the data reveals false summit. All six readings are legitimate from their positions. The mandatrophy is resolved not by asking 'which is correct?' but by recognizing that the presheaf over the observation site — all six perspectives simultaneously — is the actual constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the definitional constraint. Excluded entities have d ≈ 1.0 (trapped + victim status → maximum extraction experienced). Rights advocates have d ≈ 0.65 (moderate power + constrained exit + victim/beneficiary mix → moderate-high extraction). Moral framework authorities have d ≈ 0.10 (institutional power + arbitrage exit + beneficiary status → minimal extraction, possibly negative). Organized coalitions have d ≈ 0.45 (organized power + constrained exit + mixed victim/beneficiary → moderate extraction). Traditional institutions have d ≈ 0.08 (institutional power + arbitrage + beneficiary status → minimal extraction). The sigmoid f(d) maps these into the effective extractiveness chi experienced by each perspective, producing the classification divergence. Beneficiaries are those who retain secure moral patient status (existing moral agents, those with institutional power to define). Victims are those at risk of exclusion or currently excluded (candidates for expanded moral consideration, vulnerable populations, new forms of potentially sentient entities).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (impossibility of single correct classification) is resolved by recognizing that moral patient definition is genuinely a mixed-function constraint. It IS a coordination mechanism (you must be able to define moral patient status to do moral philosophy at all). It IS extractive (definitional power is distributed asymmetrically and protects existing moral agents against challenge). It IS partially permeable (historical data shows definitional expansion is possible, suggesting scaffold characteristics). It IS theatrical (philosophical debate often exceeds actual definitional change). And it IS at risk of naturalization as mountain (there exists a genuine logical need for some boundary, making it easy to naturalize the current boundary as natural). All six types are legitimate readings. The constraint cannot be classified to a single type because the structural function is genuinely hybrid. The analytics resolve this by making perspectival position explicit: FROM POSITION X, THE CONSTRAINT IS TYPE Y. The presheaf of all six perspectives is the complete description. False natural law detection flags the mountain perspective as a risk — the naturalization of a contingent institutional arrangement as necessity — which is exactly the diagnostic value of applying the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criterion_objectivity_ambiguity,
    'Are the criteria for moral patient status (sentience, rationality, autonomy) objective features of the candidate entity, or constructed definitions that reflect the moral agent''s own cognitive architecture and interests?',
    'Philosophical analysis of criterion independence; empirical investigation of whether excluded entities meet criteria by alternative measurement; cross-cultural comparison of definitional criteria across societies',
    'If objective: the extraction is legitimate gatekeeping of moral status based on real properties. If constructed: the criteria are tools for maintaining the power to define, and the extraction is deliberate. If mixed: some criteria objective, others constructed — must identify which are which.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_objectivity_ambiguity, conceptual, 'Whether moral patient criteria are objective or constructed').

omega_variable(
    moral_patient_boundary_stability,
    'Is the boundary between moral patients and non-patients intrinsically stable, or does it reflect temporary definitional consensus that will expand as moral knowledge improves?',
    'Historical analysis of past definitional boundaries (non-human animals, women, enslaved people, disabled humans); tracking of empirical discoveries that challenged exclusion (animal cognition research, neuroscience of consciousness); identification of patterns in successful definitional expansion',
    'If stable: the classification is mountain (natural boundary). If unstable: the classification is snare (extraction mechanism depends on artificial boundary). If pattern-driven: the classification is tangled rope with a foreseeable sunset (scaffold aspect).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_patient_boundary_stability, empirical, 'Whether moral patient boundaries are stable or expanding').

omega_variable(
    excluded_agent_self_advocacy_paradox,
    'Can an entity prove it deserves moral patient status without already possessing enough moral status to have its claims taken seriously?',
    'Case analysis of successful status expansions; identification of which entities won status through their own advocacy vs external advocacy; examination of evidentiary standards applied to candidates vs beneficiaries',
    'If paradox is real and unresolvable: the constraint is snare (extraction mechanism is the impossible proof requirement). If resolution exists: identification of the mechanism enables optimization of definitional inclusion procedures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_agent_self_advocacy_paradox, conceptual, 'Paradox of proving moral patient status from an excluded position').

omega_variable(
    enforcement_mechanism_opacity,
    'Who actually enforces the definitions of moral patient status, and through what mechanisms? Is enforcement explicit (legal systems, explicit hierarchies) or implicit (discourse norms, epistemic authority)?',
    'Institutional analysis of who has definitional authority in different domains (philosophy, law, medicine, corporate ethics); tracking of enforcement mechanisms (professional gatekeeping, publication bias, funding control); comparison of explicit vs implicit enforcement costs',
    'If enforcement is explicit and transparent: agents can contest it directly, reducing suppression and snare characteristics. If enforcement is implicit: suppression is higher and the constraint more closely resembles snare than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_opacity, empirical, 'Transparency and mechanisms of moral patient definitional enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_patient_definition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mora_tr_t0, moral_patient_definition, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mora_tr_t50, moral_patient_definition, theater_ratio, 50, 0.62).
narrative_ontology:measurement(mora_tr_t100, moral_patient_definition, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(mora_be_t0, moral_patient_definition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mora_be_t50, moral_patient_definition, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(mora_be_t100, moral_patient_definition, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_patient_definition, identity_coordination).
narrative_ontology:affects_constraint(moral_patient_definition, rights_expansion_mechanism).
narrative_ontology:affects_constraint(moral_patient_definition, moral_circle_boundaries).

% DUAL FORMULATION NOTE:
% Moral patient definition is the upstream constraint that gates access to moral status and moral rights frameworks. Downstream constraints like specific rights expansions (animal welfare protection, disability rights) and moral circle boundary disputes inherit their structure from this foundational definition mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(moral_patient_definition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
