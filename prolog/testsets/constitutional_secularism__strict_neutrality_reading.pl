% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/religious_governance/political_theory
 *
 * SUMMARY:
 *   Constitutional secularism — the principle that the state maintains equal
 *   distance from all religions, providing no preferential treatment or
 *   interference — is one of the foundational commitments of modern liberal
 *   democracies. This constraint story examines the STRICT NEUTRALITY READING
 *   of that kernel: the interpretation that state neutrality toward religion
 *   means the state must treat all religions identically, enforce uniform
 *   rules regardless of religious impact, and abstain from measures designed
 *   to protect, accommodate, or remedy religious discrimination. This reading
 *   contrasts with sibling readings: the PRINCIPLED INTERVENTION reading
 *   (which holds that true neutrality requires state action to protect
 *   minorities from majority domination and counteract structural religious
 *   discrimination) and the REFORMIST reading (which interprets neutrality as
 *   permitting state guidance toward modernization of religious practice).
 *   The strict neutrality reading presents itself as the most neutral of all
 *   — purely formal, rule-based, non-preferential. Yet its implementation
 *   reveals asymmetries: uniform rules protect minorities in some contexts
 *   and enable majority dominance in others. The constraint's extractiveness
 *   has risen over time (0.22 → 0.38) as the gap between formal neutrality
 *   and substantive outcomes has become more visible. Theater ratio rising
 *   (0.42 → 0.55) reflects that formal neutrality increasingly masks
 *   pervasive informal majority cultural advantage. The strict neutrality
 *   reading exemplifies how a principle presented as natural law (immutable,
 *   neutral, discoverable) actually instantiates a contingent institutional
 *   choice that benefits some agents while constraining others.
 *
 * KEY AGENTS:
 *   - Religious minorities: Beneficiary (moderate/constrained) — the strict neutrality constraint protects from majority domination and state-sponsored discrimination; experiences as protective coordination
 *   - Historically marginalized denominations: Victim (powerless/trapped) — small dispersed communities with no voice in constitutional interpretation; trapped in formal equality that masks substantive marginalization
 *   - Majority religious community: Mixed beneficiary/victim (powerful/constrained) — retains cultural influence but constrained by neutrality rule in embedding religious values in institutions; experiences as extraction
 *   - Civil rights enforcement institutions: Organized observer (organized/mobile) — courts and civil rights agencies building affirmative protections beyond neutrality; sees scaffold with sunset potential
 *   - Constitutional state apparatus: Institutional beneficiary (institutional/arbitrage) — maintains formal neutrality as legitimacy theater while perpetuating majority cultural dominance through norms and selective enforcement
 *   - Analytical observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent institutional choice as immutable law of legitimate governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.45).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/religious_governance/political_theory").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '308131fa-3661-4141-9ae7-8a92b8fea9d2').
narrative_ontology:cs_kernel_codification('308131fa-3661-4141-9ae7-8a92b8fea9d2', formalized).
narrative_ontology:cs_authority_grounding('308131fa-3661-4141-9ae7-8a92b8fea9d2', lineage).
narrative_ontology:cs_interpretation_layer_present('308131fa-3661-4141-9ae7-8a92b8fea9d2').
narrative_ontology:cs_reading_relation('308131fa-3661-4141-9ae7-8a92b8fea9d2', principled_intervention_reading, influences).
narrative_ontology:cs_reading_relation('308131fa-3661-4141-9ae7-8a92b8fea9d2', reformist_reading, coexists_with).
narrative_ontology:cs_axiom('308131fa-3661-4141-9ae7-8a92b8fea9d2', foundational, state_non_preference_procedural).
narrative_ontology:cs_axiom_status(state_non_preference_procedural, holdable).
narrative_ontology:cs_axiom_grounding('308131fa-3661-4141-9ae7-8a92b8fea9d2', state_non_preference_procedural, deontological).
narrative_ontology:cs_axiom('308131fa-3661-4141-9ae7-8a92b8fea9d2', foundational, formal_equality_sufficient_protection).
narrative_ontology:cs_axiom_status(formal_equality_sufficient_protection, overridden).
narrative_ontology:cs_axiom_grounding('308131fa-3661-4141-9ae7-8a92b8fea9d2', formal_equality_sufficient_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('308131fa-3661-4141-9ae7-8a92b8fea9d2', liberal_constitutional_equal_treatment).
narrative_ontology:cs_drift_state('308131fa-3661-4141-9ae7-8a92b8fea9d2', contemporary_multireligious_pluralism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('308131fa-3661-4141-9ae7-8a92b8fea9d2', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_state_apparatus).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, historically_marginalized_denominations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY (ROPE) — The strict neutrality constraint protects this agent from majority domination and state-sponsored discrimination. The constraint functions genuinely as coordination — it establishes predictable rules that enable minority religious practice. Exit options are constrained by the legal/institutional environment, but the constraint itself enables rather than restricts meaningful autonomy. Experiences the constraint as protective coordination with low extraction overhead.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: HISTORICALLY MARGINALIZED DENOMINATIONS (SNARE) — Small, dispersed communities with no organized voice in constitutional interpretation. Strict neutrality appears neutral but provides no affirmative protection against social discrimination or majority cultural dominance. The constraint traps these communities in a formal equality that masks substantive inequality. They cannot exit the jurisdiction, cannot organize effective political counter-pressure, and lack resources to participate in constitutional litigation. The neutrality principle suppresses their ability to claim remediation for historical harms.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJORITY RELIGIOUS COMMUNITY (TANGLED ROPE) — The strict neutrality constraint both coordinates and extracts. It coordinates shared civic space and prevents sectarian conflict. But it simultaneously constrains the majority's capacity to embed its own religious values in public institutions, education, law, and culture — a genuine loss of power. The majority is not powerless (retains significant cultural influence), but is constrained by the neutrality rule. This generates a perspectival gap: the majority experiences the constraint as extraction disguised as neutrality, while minorities experience it as protection. Both perceptions are structurally valid.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS ENFORCEMENT INSTITUTIONS (SCAFFOLD) — Courts, civil rights agencies, and watchdog organizations see strict neutrality as a temporary coordination mechanism that has become inadequate. These organized actors are building more granular protections (hate crime statutes, hostile environment doctrine, religious accommodation law) that go beyond neutrality to affirmative inclusion. The scaffold perspective sees strict neutrality as a starting point now being superseded by more sophisticated frameworks. Has sunset clause potential as legal doctrine evolves.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL STATE APPARATUS (PITON) — The state maintains formal neutrality as institutional doctrine, but the actual enforcement and application are heavily performative. State institutions (legislatures, bureaucracies, law enforcement) maintain cultural dominance of majority religious values through embedded norms, statutory language, calendar regulation, and selective enforcement. The neutrality principle persists through institutional inertia and legitimacy theater — it is the stated rule that no longer governs actual practice. Theater ratio high because formal neutrality masks pervasive informal preference.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, state neutrality toward religion appears as an irreducible structural feature of legitimate governance: any state that privileges one religion is per se illegitimate. This perspective treats neutrality as a discovery about legitimate authority, not a contingent choice. However, the presence of identifiable beneficiaries (religious minorities) and victims (majority communities and historically marginalized groups) triggers false summit detection — the constraint naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_secularism__strict_neutrality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from majority communities (constrains embedding of majority values in institutions) and from historically marginalized minorities (formal equality masks substantive disadvantage). But extraction is not severe because neutrality genuinely protects religious minorities from discrimination and provides stable predictability. The value reflects that the constraint performs real coordination function (prevents sectarian conflict, establishes predictable rules) alongside extractive asymmetries. Suppression (0.45): Moderate. Formal neutrality suppresses majority capacity to embed preferred values institutionally, and suppresses marginalized communities' capacity to claim remediation for historical harms. But suppression is not total — majority retains significant cultural influence, and minorities can organize legal challenges. Theater ratio (0.55): Moderate-high and rising. The state presents formal neutrality as purely mechanical rule application, but actual practice heavily embeds majority religious values through embedded norms, calendar regulation, statutory language patterns, and selective enforcement. The gap between formal principle and substantive practice has widened as awareness of structural disadvantage has increased.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement about whether formal neutrality is protective (minorities' view), constraining (majority's view), or masking inequality (marginalized communities' view). Religious minorities see the constraint as genuine coordination protecting their autonomy. The majority sees it as extraction of their capacity to transmit values through institutions. Historically marginalized communities see it as suppression of their claims for remediation. Civil rights institutions see it as a scaffold being superseded by more granular protections. The state apparatus sees it as a legitimacy doctrine that masks continued majority dominance. The analytical observer risks naturalizing this entire institutional choice as a law of legitimate governance. The perspectival gaps reveal that 'neutrality' is not a transparent, value-neutral principle but an institutional choice embedded in a particular liberal tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by whether the agent benefits from or bears costs from uniform application of formal neutrality rules. Religious minorities experience low directionality (d ≈ 0.25) because the constraint protects them from majority predation — they are net beneficiaries despite constrained exit options. Historically marginalized denominations experience high directionality (d ≈ 0.85) because formal neutrality suppresses their claims for special consideration or remediation — they bear extraction costs. The majority community experiences mid-range directionality (d ≈ 0.55) because they are constrained from embedding preferred values but retain cultural influence — they are neither pure beneficiaries nor pure targets. The civil rights institutions experience low-to-moderate directionality (d ≈ 0.40) because they have some agency and exit options (litigation, legislative advocacy) and see the constraint as temporary. The state apparatus experiences very low directionality (d ≈ 0.10) because it benefits from maintaining formal neutrality as legitimacy cover for substantive majority preference. The analytical observer experiences moderate directionality (d ≈ 0.72) as a structurally external position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in strict neutrality is whether formal equal treatment can ever be truly neutral when applied to structurally unequal communities. The constraint resolves by showing that 'neutrality' is a perspectival choice: from the minority protection perspective, it is protective coordination (rope). From the majority constraint perspective, it is mixed coordination and extraction (tangled_rope). From the marginalized community perspective, it is pure extraction (snare). From the institutional apparatus perspective, it is degraded theater (piton). From the analytical observer perspective, it risks naturalizing contingent choice as law (false summit mountain). No single type is 'correct' — the constraint's actual structure is the presheaf of these competing perspectives. The mandatrophy resolves not by finding the true type but by recognizing that 'neutrality' is a legitimacy claim that different parties read differently based on their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_vs_affirmative_protection,
    'Is strict formal neutrality sufficient to protect religious minorities, or does it require affirmative state intervention to counteract social discrimination and structural inequality?',
    'Comparative analysis of minority outcomes under strict neutrality regimes vs. regimes with affirmative accommodation and hate crime protection; longitudinal data on minority political representation and resource access',
    'If neutrality sufficient: rope classification holds across more perspectives. If affirmative protection necessary: constraint reclassifies to snare/tangled_rope for marginalized communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neutrality_vs_affirmative_protection, empirical, 'Whether formal neutrality alone protects minority interests or requires affirmative intervention').

omega_variable(
    majority_accommodation_as_extraction,
    'When the state accommodates majority religious practices (Sunday closures, religious holidays as public holidays, chaplaincies, tax exemptions for majority churches), is this a violation of neutrality or a legitimate instance of the majority''s cultural expression through democratic process?',
    'Doctrinal analysis of court holdings on majority accommodation; inventory of state practices that embed majority religious values; comparison with treatment of minority accommodation requests',
    'If interpreted as violation: state apparatus piton reclassifies toward snare/tangled_rope. If interpreted as legitimate expression: majority community''s tangled_rope reclassifies toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_accommodation_as_extraction, conceptual, 'Whether majority religious accommodation constitutes neutrality violation or legitimate democratic expression').

omega_variable(
    liberal_theory_natural_vs_constructed,
    'Is the strict neutrality principle a natural law of legitimate governance (discovered), or is it a contingent institutional choice grounded in a particular liberal tradition (constructed)?',
    'Historical analysis of secular governance regimes; comparative study of post-colonial and non-Western constitutional traditions; philosophical interrogation of whether neutrality is a necessary condition or a culturally specific preference',
    'If natural law: mountain classification sustained for analytical perspective. If constructed: false summit triggered, constraint reclassified as tangled_rope or piton across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_theory_natural_vs_constructed, conceptual, 'Whether neutrality is discovered natural law or contingent institutional choice').

omega_variable(
    committer_reading_ambiguity,
    'What is the foundational distinction between the strict neutrality reading and the principled intervention reading of the constitutional secularism kernel?',
    'Axiom examination: strict neutrality axiom is ''state must not prefer, support, or disadvantage any religion''; principled intervention axiom is ''state must actively counteract religious discrimination and protect minority practice even when this requires preferential measures''. These axioms do not logically foreclose each other — a single framework could hold both if the second is understood as applying only to protective intervention, not preferential advantage. But they pull in opposed institutional directions.',
    'If axioms coexist: multiple readings are holdable within competing frameworks. If axioms foreclose: only one reading can be institutionalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_ambiguity, conceptual, 'Relationship between strict neutrality and principled intervention axioms').

omega_variable(
    identity_locked_majority_culture,
    'Is the majority religious community''s experience of strict neutrality as extraction driven by structural loss of institutional power, or by identity fusion with institutions that formerly embedded majority values?',
    'Longitudinal analysis of majority political mobilization: do majorities mobilize most strongly in jurisdictions where they have lost institutional power (structural explanation) or where their religious identity has been publicly diminished (identity explanation)? Comparative data on majority politics in secularizing vs. already-secular societies.',
    'If structural: majority''s tangled_rope classification is accurate and reflects real power asymmetry. If identity: majority''s classification may misidentify identity_locked constraint for a different underlying constraint (religious identity in public institutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_majority_culture, empirical, 'Whether majority reaction to neutrality is driven by structural power loss or identity fusion with institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitsec_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(constitsec_tr_t3, constitutional_secularism__strict_neutrality_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(constitsec_tr_t6, constitutional_secularism__strict_neutrality_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(constitsec_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(constitsec_be_t3, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(constitsec_be_t6, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(constitsec_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(constitsec_su_t3, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(constitsec_su_t6, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 6, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, religious_establishment_doctrine).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, minority_religious_accommodation).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, majority_cultural_transmission).

% DUAL FORMULATION NOTE:
% Constitutional secularism is a kernel with multiple readings. This story captures the strict neutrality reading. The principled intervention reading would have different ε (likely higher extractiveness when applied to marginalized communities, lower when applied to minorities), different beneficiary/victim structure, and different perspectives. The reformist reading would have even higher extractiveness and different institutional pathology. All three are linked constraints in the constitutional_secularism family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__strict_neutrality_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
