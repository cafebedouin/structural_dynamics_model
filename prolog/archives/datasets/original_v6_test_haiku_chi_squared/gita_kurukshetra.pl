% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The Kshatriya dharma (sacred duty to fight in lawful warfare) as codified
 *   in the Bhagavad Gita and Dharmaśāstra texts represents a classic case of
 *   constraint hybridization. On its surface, it is a coordination mechanism:
 *   establishing rules for rightful warfare, legitimizing sovereign
 *   authority, and creating shared codes of honor. The Gita frames Arjuna's
 *   duty to fight as a transcendent spiritual obligation that supersedes
 *   familial love and individual moral doubt. Yet structurally, it functions
 *   as a Tangled Rope extracting military service from the warrior caste
 *   while benefiting the Brahmin priesthood (who are exempted from fighting)
 *   and ruling dynasties (who gain legitimacy for their wars). The constraint
 *   is maintained through religious authority (textual cosmology), social
 *   hierarchy (caste immobility), and suppression of alternative
 *   interpretations of what constitutes dharma. The theater ratio has
 *   increased over the classical period as philosophical elaboration around
 *   duty (the Gita itself represents mounting performative justification) has
 *   grown, while the direct enforcement mechanism has partially weakened. The
 *   constraint is neither a pure natural law (mountain) nor pure coordination
 *   (rope) — it is a hybrid that uses sacred duty language to mobilize
 *   military resources and consolidates priestly and ruling-class authority.
 *
 * KEY AGENTS:
 *   - Kshatriya Soldier: Primary victim (powerless/trapped) — conscripted by caste birth; forbidden to refuse warfare without spiritual death
 *   - Lower Caste War Casualty: Secondary victim (powerless/trapped) — subject to Kshatriya military duty; bears casualties without agency
 *   - Brahmin Priesthood: Primary beneficiary (institutional/arbitrage) — exempted from fighting; gains authority through textual interpretation and religious legitimacy of the entire system
 *   - Dynasty Ruler: Dual role (powerful/constrained) — benefits from legitimacy of lawful war but is also constrained by duty-based obligation to fight; more powerful but less mobile than beneficiaries
 *   - Rival Dynasty: Inter-institutional actor (organized/mobile) — can refuse or reinterpret duty; operates at same institutional level as primary dynasty but with differentiated exit options
 *   - Vedic Textual Tradition: Institutional actor maintaining theater (institutional/arbitrage) — performs abstract philosophical justification; theater increases as direct coercion weakens
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra, 0.38).
domain_priors:suppression_score(gita_kurukshetra, 0.68).
domain_priors:theater_ratio(gita_kurukshetra, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra, extractiveness, 0.38).
narrative_ontology:constraint_metric(gita_kurukshetra, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gita_kurukshetra, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra, "The Duty of the Kshatriya (Warrior Caste)").
narrative_ontology:topic_domain(gita_kurukshetra, "religious/philosophical/social").

domain_priors:requires_active_enforcement(gita_kurukshetra).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(gita_kurukshetra, ruling_dynasties).
narrative_ontology:constraint_beneficiary(gita_kurukshetra, patriarchal_kinship_order).
narrative_ontology:constraint_victim(gita_kurukshetra, kshatriya_soldiers).
narrative_ontology:constraint_victim(gita_kurukshetra, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra, war_casualties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KSHATRIYA SOLDIER (SNARE) — Bound by caste dharma to fight; refusal means ritual pollution, social death, and exclusion from afterlife. No genuine exit option; trapped by religious cosmology and kinship obligation. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.56.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOWER CASTE CASUALTY (SNARE) — Subject to Kshatriya duty to fight, serving as foot soldiers and support. Bears cost of warfare without agency in its declaration. Trapped by both caste hierarchy and military necessity. d≈0.98, f(d)≈1.46, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(gita_kurukshetra, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DYNASTY RULER (TANGLED ROPE) — Powerful agent whose authority is legitimized by military duty (dharma) but also constrained by it. Must fight lawful wars to maintain rightful sovereignty; warfare is both privilege and obligation. Experiences mixed extraction and coordination. d≈0.48, f(d)≈0.62, σ=0.9 → χ≈0.21.
constraint_indexing:constraint_classification(gita_kurukshetra, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BRAHMIN PRIESTHOOD (ROPE) — Coordinates the entire caste-duty system; exempted from fighting themselves via textual interpretation. Benefits from institutional legitimacy and social hierarchy maintained by Kshatriya military function. Sees the constraint as pure coordination: stabilizing social order. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(gita_kurukshetra, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIVAL DYNASTY (TANGLED ROPE / INTER-INSTITUTIONAL) — Organized institutional actor with mobile exit options relative to THIS constraint. Can refuse battle (violating dharma codes but facing military/political consequences rather than caste death). The constraint extracts loyalty and obedience while offering legitimacy to lawful rule. Sees both enforcement (active suppression of non-martial outcomes) and coordination (shared rules of lawful warfare). d≈0.52, f(d)≈0.69, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(gita_kurukshetra, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: VEDIC TEXTUAL TRADITION (PITON) — The Gita encodes dharma rules with high theater: elaborate philosophical justifications for duty (Arjuna's hesitation, Krishna's counters) mask what is fundamentally a social enforcer mechanism. The performative aspect (moral philosophy around duty) has become primary; the actual enforcement (caste-based conscription) persists through textual authority even as direct coercion weakens. theater_ratio=0.61 reflects the substantial philosophical apparatus built around enforcement. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.02.
constraint_indexing:constraint_classification(gita_kurukshetra, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN RISK) — From a civilizational view, some theorists argue caste-duty is immutable: role differentiation by heredity reflects natural hierarchy (brahmin reasoning). However, empirical data (ε=0.38, suppression=0.68, theater=0.61) contradicts this naturalization. The constraint is socially constructed, enforced through religious authority, and maintained by suppression of alternative interpretations. This perspective risks false summitry — treating contingent social arrangements as laws of nature.
constraint_indexing:constraint_classification(gita_kurukshetra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gita_kurukshetra, TR),
    TR >= 0.70.

:- end_tests(gita_kurukshetra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint extracts military service and life risk from the warrior caste while concentrating authority and exemption in the priesthood. However, this is not maximum extraction (ε≤0.46 for Snare threshold) because the system does provide benefits to Kshatriyas: social status, wealth from military service and rule, and sacred legitimacy that makes them society's honored class. The extraction is asymmetric (not equal return) but not totalizing. Suppression (0.68): High. The warrior caste cannot exit without ritual pollution, loss of caste status, exclusion from favorable rebirth, and social ostracism. Kinship obligation and caste immobility (hereditary membership) make suppression severe. Yet some theoretical flexibility exists in interpreting what 'fighting' means — hence 0.68 rather than 0.85. Theater ratio (0.61): Moderate-high. The Gita represents substantial performative elaboration: philosophical arguments for duty (Arjuna's dilemma, Krishna's theodicy) mask what is functionally a coercive enforcement mechanism. Over the classical period, as direct coercion of lower castes weakened, philosophical justification intensified — theater ratio increased from 0.40 to 0.61. Claimed type: Tangled Rope. The constraint exhibits both genuine coordination (shared rules for lawful warfare, honor codes) and asymmetric extraction (warrior caste mobilized for beneficiary dynasties). Active enforcement is required (priestly interpretation + caste sanctions). Beneficiaries are clear (priesthood, dynasties); victims are clear (soldiers, lower castes).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across perspectives. The Kshatriya soldier sees a Snare: trapped by caste birth, unable to refuse without spiritual death, conscripted into warfare with no genuine exit. The lower caste casualty sees a deeper Snare: trapped both by military conscription and by caste hierarchy. The dynasty ruler sees a Tangled Rope: constrained by duty but also privileged by legitimacy and power — they are both beneficiary and victim of the obligation they enforce. The Brahmin priesthood sees a Rope: pure coordination and spiritual order — they are structurally insulated from the duty's costs. The rival dynasty (inter-institutional perspective) sees a more mobile Tangled Rope: they can refuse or reinterpret dharma; their exit options are genuinely constrained but not trapped. The Vedic textual tradition sees its own role as degraded (Piton): the philosophical apparatus around duty has become increasingly performative as the social reality it justified has weakened. The analytical observer risks a false summit (Mountain): treating caste-duty as an immutable law of social order — but the data show it is a constructed constraint, weakening over time, maintained increasingly by theater rather than force.
 *
 * DIRECTIONALITY LOGIC:
 *   Kshatriya soldier: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Bound by caste birth, religious cosmology, kinship obligation. No exit. Lower caste casualty: Victim + trapped → d≈0.98, f(d)≈1.46. Slightly higher d than Kshatriya due to double subordination (caste + military). Dynasty ruler: Beneficiary and victim both; powerful but constrained → d≈0.48, f(d)≈0.62. Mixed position: benefits from legitimacy but cannot escape duty without losing authority. Brahmin priesthood: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Exempted from fighting, gains authority through interpretation. Rival dynasty (inter-institutional): Victim of OTHER's duty but beneficiary of own legitimacy; organized, mobile → d≈0.52, f(d)≈0.69. Can negotiate or refuse reinterpretation; exit is costly but possible.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that Tangled Rope classification is correct DESPITE the strong philosophical (rope-like) justification that surrounds it. The Gita's elaborate moral philosophy (Krishna's arguments) is NOT evidence for Rope classification — it is evidence for the presence of theater (0.61 ratio). The underlying structure shows: (1) beneficiaries clearly concentrated (priesthood, dynasties), (2) victims clearly identified (soldiers, lower castes), (3) active enforcement required (caste sanctions, afterlife threat, kinship obligation), (4) asymmetric extraction (warriors receive status but not equivalent return for life risk). These satisfy the Tangled Rope gates despite the coordination language. The mandatrophy is resolved by distinguishing the RHETORIC of duty (coordination, sacred order) from the STRUCTURE of benefit distribution (extraction). The philosophical apparatus is part of the enforcement mechanism, not evidence against extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharma_versus_coercion,
    'Is the Kshatriya duty experienced primarily as sacred obligation (internalized dharma) or as coercive enforcement by priestly authority?',
    'Historical analysis of Vedic texts; ethnographic study of actual warrior caste narratives across centuries; comparison of caste-duty with voluntary martial traditions; linguistic analysis of metaphors for duty (internalization vs external force)',
    'If internalized (dharma): constraint appears as Rope from warrior perspective (shared coordination). If coercive: constraint is Snare (pure extraction masked by religious language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharma_versus_coercion, conceptual, 'Whether duty is internalized dharma or enforced coercion').

omega_variable(
    brahmin_agency_in_enforcement,
    'Do Brahmin priests actively enforce the caste-duty system through religious sanction, or do they merely articulate an emergent social order?',
    'Historical reconstruction of priest-king dynamics; textual analysis of priestly claims to authority over caste duties; examination of cases where warrior caste rejected priestly interpretation',
    'If active enforcement: Brahmin priesthood is primary beneficiary (high extraction). If articulative: priesthood is secondary coordinator (lower extraction, higher rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmin_agency_in_enforcement, empirical, 'Whether Brahmin priesthood actively enforces caste-duty system').

omega_variable(
    alternative_duty_interpretations,
    'Could a Kshatriya satisfy dharma through non-martial roles (administration, commerce, protection) rather than warfare?',
    'Textual hermeneutics: careful reading of dharma definitions in Dharmaśāstra; identification of counter-examples or minority interpretations; analysis of how alternative roles were actually treated in historical kingdoms',
    'If yes: exit options upgrade from trapped to constrained (d decreases, χ decreases). Constraint shifts from Snare toward Tangled Rope. If no: trap is absolute, Mountain classification risk increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_duty_interpretations, conceptual, 'Whether non-martial roles satisfy Kshatriya dharma').

omega_variable(
    afterlife_punishment_mechanism,
    'How directly does religious doctrine enforce caste duty through promised afterlife punishment (rebirth in lower caste)?',
    'Textual analysis of rebirth consequences in Vedic/Classical Hindu texts; comparison with other religions'' enforcement mechanisms (heavenly reward, eternal punishment); ethnographic evidence of caste-duty acceptance correlated with afterlife belief strength',
    'If strong causal link: suppression increases (cosmological trap); warrior appears more powerless. If weak or culture-dependent: suppression decreases; exit options improve to constrained (from trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(afterlife_punishment_mechanism, empirical, 'Strength of afterlife punishment mechanism for duty violation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra, theater_ratio, 0, 0.4).
narrative_ontology:measurement(gita_tr_t250, gita_kurukshetra, theater_ratio, 250, 0.55).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra, theater_ratio, 500, 0.61).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gita_be_t250, gita_kurukshetra, base_extractiveness, 250, 0.35).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra, base_extractiveness, 500, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra, enforcement_mechanism).
narrative_ontology:affects_constraint(gita_kurukshetra, varna_social_hierarchy).
narrative_ontology:affects_constraint(gita_kurukshetra, brahminical_textual_authority).
narrative_ontology:affects_constraint(gita_kurukshetra, patriarchal_kinship_obligation).

% DUAL FORMULATION NOTE:
% The Kshatriya dharma constraint decomposes into three related claims: (1) varna_social_hierarchy (ε≈0.15, Mountain-like natural law claims), (2) brahminical_textual_authority (ε≈0.35, Tangled Rope coordination + priestly extraction), (3) patriarchal_kinship_obligation (ε≈0.42, Snare for junior kinship positions). The present story (gita_kurukshetra, ε≈0.38) represents the battlefield-specific instantiation of these family members. Each has different empirical status and different failure modes. The network represents causal dependency: kinship obligation and textual authority FEED the battlefield duty constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
