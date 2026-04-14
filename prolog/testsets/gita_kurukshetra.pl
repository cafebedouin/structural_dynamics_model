% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra, []).

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
 *   constraint_id: gita_kurukshetra
 *   human_readable: The Duty of the Kshatriya (Warrior Caste)
 *   domain: religious/philosophical/social
 *
 * SUMMARY:
 *   The Bhagavad Gita's central ethical argument — that Arjuna must fight in
 *   the Kurukshetra war despite killing kin because it is his svadharma
 *   (personal duty derived from caste birth) — instantiates a constraint
 *   structure that operates simultaneously as religious cosmology,
 *   institutional extraction mechanism, and genuine coordination framework
 *   for managing violence in hierarchical society. The constraint claims
 *   naturalization ('this is your dharma, inherent to your birth and cosmic
 *   function') but exhibits all structural signatures of engineered
 *   enforcement: suppression of alternative moral frames (Arjuna's kinship
 *   obligation is delegitimized), institutional authority (brahmins define
 *   what dharma is), and asymmetric benefit (brahminical class maintains
 *   interpretive authority while warriors bear violence cost). The Gita's
 *   philosophical genius — Krishna's reframing of duty as detached action —
 *   can be read as either a genuine coordination solution (warriors need a
 *   frame that lets them execute duty without psychological breakdown) or as
 *   a sophisticated legitimation technology that makes the extraction
 *   mechanism invisible by relocating moral agency into metaphysics. The
 *   extractiveness value (0.58) reflects this ambiguity: it is lower than
 *   pure snare (0.70+) because the constraint does provide genuine
 *   coordination function for ordering political succession and violence
 *   management; it is higher than pure rope (0.35) because that coordination
 *   function is asymmetrically distributed (brahmins define, warriors
 *   execute) and cannot be renegotiated by warriors themselves. The theater
 *   ratio (0.68) captures the increasing reliance on ritual performance over
 *   time — as the constraint ages, more enforcement comes through repeated
 *   invocation ('this is dharma') rather than through genuine re-coordination
 *   of conflict.
 *
 * KEY AGENTS:
 *   - Arjuna: Primary target (powerless/trapped) — individual warrior facing maximal suppression; personal moral objection overridden by duty framework
 *   - Brahminical Authority: Primary beneficiary (institutional/arbitrage) — defines dharma, maintains interpretive monopoly, can reframe or reinterpret constraint; retains exit option to modify teachings
 *   - Kshatriya Caste Collectively: Secondary beneficiary and victim (organized/constrained) — benefits from martial status and political participation but cannot collectively exit the constraint; constrained by internal hierarchy and brahminical oversight
 *   - Shudra and Untouchable Castes: Tertiary victims (powerless/trapped) — excluded from warrior duty but bear material costs of warfare; have no voice in duty definition; trapped by varna system itself
 *   - Established Kingdom: Institutional beneficiary (institutional/arbitrage) — receives orderly succession and predictable warrior compliance; can appeal to dharma to legitimize political choices
 *   - Brahmin Priesthood: Institutional beneficiary with enforcement role (institutional/constrained) — benefits from hierarchical stability and interpretive authority; constrained by need to maintain credibility of dharma teachings; faces criticism if constraint fails
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent caste hierarchy as cosmic necessity; can see either natural law or engineered extraction depending on interpretive frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra, 0.58).
domain_priors:suppression_score(gita_kurukshetra, 0.72).
domain_priors:theater_ratio(gita_kurukshetra, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra, extractiveness, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra, "The Duty of the Kshatriya (Warrior Caste)").
narrative_ontology:topic_domain(gita_kurukshetra, "religious/philosophical/social").

domain_priors:requires_active_enforcement(gita_kurukshetra).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra, brahminical_authority).
narrative_ontology:constraint_beneficiary(gita_kurukshetra, kingdom_stability).
narrative_ontology:constraint_beneficiary(gita_kurukshetra, warrior_caste_privilege).
narrative_ontology:constraint_victim(gita_kurukshetra, individual_moral_agency).
narrative_ontology:constraint_victim(gita_kurukshetra, kinship_bonds).
narrative_ontology:constraint_victim(gita_kurukshetra, non_warrior_castes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RELUCTANT WARRIOR (SNARE) — Arjuna's position before Krishna's teaching. The warrior sees absolute extraction: duty demands killing kin without personal moral choice. No exit option exists — caste birth determines obligation. Dharma becomes coercive force. The constraint presents as natural law ('this is your svadharma') but operates as pure extraction: suppress moral doubt, suppress kinship obligation, suppress the question of consent. Maximum suppression (0.72) and high extraction experienced by the agent trapped in this role.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BRAHMIN PRIEST CLASS (TANGLED ROPE) — Brahminical authority benefits from hierarchical stability where warriors execute duty without questioning. But brahmins also bear costs: they are responsible for articulating dharma, managing moral legitimacy, and defending against the charge of using caste hierarchy for extraction. The constraint has genuine coordination function (orderly succession, predictable martial hierarchy) but asymmetric extraction (brahmins define duty, warriors execute it). Enforcement required: brahmins actively teach, interpret, and validate the constraint. Constrained exit because brahmins depend on warrior caste cooperation but can theoretically renounce or reinterpret dharma (though career/authority loss follows).
constraint_indexing:constraint_classification(gita_kurukshetra, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ESTABLISHED KINGDOM (ROPE) — The state benefits from predictable warrior compliance and orderly succession of power. The constraint solves a genuine coordination problem: how to manage political violence and dynastic conflict through binding rules. The kingdom experiences the constraint as low extraction (or even negative — stability benefits outweigh costs). Can exit by declaring competing dharma (arbitrage: they can reinterpret duty or substitute alternative legitimacy, though with costs). This institutional perspective sees coordination function primarily.
constraint_indexing:constraint_classification(gita_kurukshetra, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE REIGNING MONARCH (PITON) — The king benefits materially from warrior obedience but also depends on its credibility. Over time, the performative dimension grows: kings stage battles, invoke dharma selectively, and use the constraint theater to legitimize otherwise self-interested decisions. Theater ratio (0.68) captures this degradation — the constraint is partially maintained through ritual invocation ('this is dharma') rather than through genuine coordination function. The monarch has mobile exit options (can appeal to other authorities, reinterpret duty, shift alliances) but theater masks this mobility from the warrior caste. Piton classification reflects inertial maintenance: the constraint persists because the legitimacy performance is valuable, not because its primary function (orderly succession) couldn't be achieved other ways.
constraint_indexing:constraint_classification(gita_kurukshetra, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE NON-WARRIOR CASTES (SNARE) — Shudras and untouchables are excluded from warrior duty but subject to its consequences: they bear the material costs of warfare (taxes, conscription into non-warrior roles, vulnerability) without any voice in how duty is defined. They are trapped: cannot exit the caste system, cannot question the warrior privilege, cannot rise through martial valor (reserved to kshatriyas). The constraint extracts from them doubly: it legitimizes their exclusion from power and concentrates violence-authority in a caste that can use it against them. Suppression is maximal — the entire varna system enforces this positioning without alternative.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the constraint might appear as natural law: birth-based role assignment reflecting immutable cosmic order (rta). Dharma is presented as inherent to the structure of existence itself, not as a contingent institutional arrangement. However, the structural data reveals this as a false summit. The constraint requires active enforcement (brahmins teach, kingdoms enforce, ritual legitimizes). The extraction values (ε=0.58, suppression=0.72) show contingent institutional design, not natural law. The mountain classification at this perspective indicates naturalization of the caste system — treating contingent social hierarchy as cosmic necessity.
constraint_indexing:constraint_classification(gita_kurukshetra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE WARRIOR CASTE COLLECTIVELY (TANGLED ROPE) — As organized agents rather than individuals, warriors have coordination benefits: the dharma framework provides clear status, training subsidies, political participation, and martial identity. But the constraint also extracts: warriors cannot refuse duty, cannot exit the caste role, and bear all direct violence cost. Suppression operates through socialization and honor codes ('shame to refuse'). Enforcement is required: the brahminical priesthood actively maintains the authority structure. The collective perspective shows both genuine coordination (how do warriors govern their own status?) and asymmetric extraction (brahminical control of dharma definition). Constrained exit: the caste could theoretically renounce the system, but individual warriors cannot unilaterally opt out without caste ostracism.
constraint_indexing:constraint_classification(gita_kurukshetra, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gita_kurukshetra, TR),
    TR >= 0.70.

:- end_tests(gita_kurukshetra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits moderate-to-high extraction reflecting the asymmetric burden structure. Warriors bear direct violence cost (death, injury, psychological trauma, kinship violation) while brahminical class bears only intellectual/authority cost (maintaining interpretive coherence). The value is not maximal (0.70+) because the constraint does solve a genuine coordination problem — ordering political succession through predictable duty rules is functionally valuable, not pure predation. The rise over time (0.42→0.68) indicates degradation: earlier enforcement relied more on genuine institutional coherence, later enforcement increasingly relies on ritual repetition. Suppression (0.72): High suppression reflects multiple enforcement mechanisms operating simultaneously. Caste birth creates inescapable role. Socialization (training from childhood) internalizes duty. Honor codes ('shame to refuse') create internal enforcement. Brahminical authority delegitimizes alternative moral frames (kinship obligation, individual conscience). Ritual repetition ('this is dharma') naturalizes the constraint. Warriors have no legitimate exit option — renunciation is only path, and it carries severe social cost (caste loss, spiritual ambiguity). Theater ratio (0.68): Rising theater reflects increasing reliance on performative legitimation. Early in the constraint's operation, enforcement could rely on genuine belief in cosmic order. Over time, as worldly challenges to the system emerged (warrior rebellions, heterodox philosophies questioning varna, historical defeats), the constraint increasingly operated through ritual invocation and performative repetition rather than genuine re-coordination of conflict. The theater rise (0.45→0.68) indicates the constraint becoming degraded — it persists through inertial performance rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Arjuna the individual warrior sees pure extraction (Snare) — duty as absolute coercion overriding kinship and personal moral agency. The warrior caste collectively sees mixed coordination and extraction (Tangled Rope) — the system provides status and political power but cannot be renegotiated from within. Brahminical authority sees coordination (Rope, or even beneficial structure) — the dharma framework enables orderly succession and stable hierarchy. The established kingdom sees pure benefit (Rope) — warriors fight predictably, authority is legitimate, succession is orderly. The reigning monarch sees inertial theater (Piton) — the dharma constraint is performatively useful for legitimizing decisions but could be replaced with other legitimacy claims. Non-warrior castes see pure extraction with no coordination benefit (Snare) — they bear violence cost while excluded from martial roles. The analytical observer risks seeing natural law (Mountain, false summit) — treating the contingent caste hierarchy as cosmic necessity. The perspectival gap reflects true structural asymmetry: the constraint genuinely serves different functions for different agents, and these functions are not reconcilable into a single type without flattening the power differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: beneficiaries with exit options have low d (experience negative or low extraction); victims with no exit have high d (experience maximum extraction). Arjuna's d ≈ 0.95 (victim + trapped): he experiences maximum effective extraction despite the rhetoric of dharma. Brahminical authority's d ≈ 0.05 (beneficiary + arbitrage): they can reinterpret dharma, exit the system through philosophical innovation, maintain interpretive control. The warrior caste collectively has d ≈ 0.60 (mixed beneficiary/victim + constrained): they have some agency through martial skill and collective organization but cannot unilaterally change the constraint. Non-warriors have d ≈ 0.90 (victim + trapped): they bear costs without being incorporated into the system. The kingdom's d ≈ 0.10 (institutional beneficiary + arbitrage): it can reframe or substitute alternative legitimacy claims. The derivation chain here shows how the same constraint produces radically different experienced extraction (χ) across agents despite the same base extractiveness value (ε). Arjuna's f(d) ≈ 1.42 produces high χ; brahmin's f(d) ≈ -0.12 produces negative χ. The engine's directionality mathematics are critical here — they are what reveal that 'dharma' is NOT a neutral cosmic law but a technique that extracts from powerless warriors while benefiting brahminical authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how linguistic naturalization ('dharma is cosmic law') masks institutional extraction. The philosophically sophisticated Gita argument — that action without attachment (nishkama karma) reconciles duty with morality — is itself part of the extraction mechanism. It allows warriors to perform duty without psychological rebellion by relocating moral agency to metaphysical framework. This is not a defect in the Gita's argument; rather, it shows how the most sophisticated legitimation technologies make extraction invisible by providing an interpretive frame where the victim consents to their own victimization. The mandatrophy is resolved by understanding that the constraint operates through both genuine coordination function (ordering succession, managing violence) AND asymmetric extraction (brahminical authority, warrior burden), and these cannot be separated. The Gita's brilliance is precisely that it articulates a frame where the warrior can internalize the constraint and experience it as meaningful duty rather than coercive extraction. But internalization does not erase extraction — it makes it structural. The engine's perspectival gap (Snare for Arjuna, Rope for brahmins, Mountain for the naturalized view) is itself the mandatrophy resolution: the constraint is NOT a single natural law, but a presheaf of perspectival readings where the topology is asymmetric and the benefits flow toward brahminical authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharma_as_natural_vs_constructed,
    'Is dharma (the duty framework) a description of natural cosmic order or a contingent institutional arrangement masquerading as natural law?',
    'Historical analysis of how dharma teachings change across texts, regions, and time periods; identification of adaptations to political circumstances; comparison with non-dharmic duty frameworks in contemporaneous societies',
    'If natural: Mountain classification is correct, constraint is immutable, reform is futile. If constructed: Tangled Rope/Snare classifications are correct, the constraint is a social technology enforcing extraction, reform is possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dharma_as_natural_vs_constructed, conceptual, 'Whether dharma is cosmic law or institutional construction').

omega_variable(
    brahminical_intent_vs_function,
    'Do brahmin interpreters of dharma deliberately engineer warrior subordination, or do they genuinely believe in a cosmic duty hierarchy?',
    'Analysis of brahminical texts for internal contradiction and strategic framing; comparison of public teachings vs private correspondence; longitudinal study of dharma interpretation changes in response to warrior rebellion or refusal',
    'If deliberate: extraction is conscious, institutional conspiracy. If genuine belief: extraction is emergent effect of sincere cosmology. Both result in extraction, but resolution clarifies agency and intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_intent_vs_function, empirical, 'Brahminical intent in dharma instruction').

omega_variable(
    warrior_consent_and_internalization,
    'Do warriors genuinely accept the dharma constraint or merely perform acceptance while suppressing doubt?',
    'Analysis of warrior behavior when brahminical authority is absent or weakened; study of secret correspondence and private teachings; examination of warriors who defect or refuse duty and their justifications',
    'If genuine acceptance: suppression value should be lower (0.50-0.60), warriors actively maintain constraint. If performance: suppression is high (0.70+), external coercion required, constraint is brittle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrior_consent_and_internalization, empirical, 'Degree of warrior internalization of dharma constraint').

omega_variable(
    caste_boundary_permeability,
    'Can individual warriors exit the caste role through renunciation, skilled labor change, or gender/age transitions? How permeable are caste boundaries in practice?',
    'Historical data on renunciate movements (sannyasi); occupational transition records; legal and social penalties for caste-boundary crossing; comparison across regions and time periods',
    'If exit is possible: exit_options should be ''mobile'' not ''trapped'', reducing experienced extraction. If boundary is rigid: ''trapped'' is correct, experienced extraction is maximal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_boundary_permeability, empirical, 'Permeability of caste boundaries and exit mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra, theater_ratio, 50, 0.62).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra, enforcement_mechanism).
narrative_ontology:affects_constraint(gita_kurukshetra, varna_system_structural_hierarchy).
narrative_ontology:affects_constraint(gita_kurukshetra, brahminical_interpretive_monopoly).

% DUAL FORMULATION NOTE:
% The Kshatriya duty constraint is downstream of the varna system itself (which fixes caste-based role assignment) and of brahminical monopoly on dharma interpretation. The duty constraint is a specific instantiation of how the broader caste hierarchy maintains itself through assigned roles and interpretive control. The separation into three constraint stories allows differential analysis: varna system as foundational hierarchy, brahminical monopoly as authority structure, kshatriya duty as extraction mechanism using both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
