% ============================================================================
% CONSTRAINT STORY: personhood_boundary__spartan_eugenic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__spartan_eugenic_reading, []).

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
 *   constraint_id: personhood_boundary__spartan_eugenic_reading
 *   human_readable: Personhood Boundary: Spartan Eugenic Reading (Gerousia Confirmation)
 *   domain: moral_philosophy/historical_ethics/developmental_biology
 *
 * SUMMARY:
 *   The Spartan eugenic reading of the personhood boundary kernel positions
 *   legal and moral personhood as dependent on state confirmation of fitness
 *   through the gerousia (council of elders). Under this reading, an
 *   unconfirmed neonatal organism lacks personhood status and is therefore
 *   'legitimately excludable' — exposure is not homicide but a mechanism of
 *   population regulation. This is ONE reading of the contested personhood
 *   kernel; sibling readings ground personhood at birth (birth-threshold) or
 *   in the emergence of individual developmental capacities
 *   (developmental-capacity). The Spartan eugenic reading is logically
 *   distinct because it places the constitutive moment of personhood not in
 *   biological fact (conception, birth) or in the child's own capacities
 *   (cognitive, volitional development) but in state adjudication of fitness.
 *   This creates a structural constraint with identifiable beneficiaries (the
 *   state apparatus, confirmed citizens) and victims (unconfirmed infants,
 *   disabled neonates). The extractiveness (0.78) reflects that the state
 *   gains substantial reproductive control authority and resource allocation
 *   power through this personhood boundary, while the powerless neonates and
 *   their parents bear the suppression (0.82) of legal exclusion and exposure
 *   risk. The theater ratio (0.55) indicates that the gerousia confirmation
 *   process performs ceremonial legitimation of state authority while the
 *   underlying mechanism is straightforward state extraction of reproductive
 *   regulation.
 *
 * KEY AGENTS:
 *   - Spartan State Apparatus (Gerousia): Institutional beneficiary (institutional/arbitrage) — gains complete authority over population composition, reproductive norms, and personhood status
 *   - Unconfirmed Infants: Primary victim (powerless/trapped) — exist in legal limbo; subject to exposure if deemed unfit; have no agency, voice, or personhood protection
 *   - Parents of Unconfirmed Offspring: Secondary victim (powerless/trapped) — cannot prevent state's personhood determination; legally prohibited from resisting exposure; trapped by biological capacity and state authority
 *   - Confirmed Citizen Body: Secondary beneficiary (powerful/constrained) — benefit from population-quality maintenance and military cohesion; also subject to same fitness criteria and reproductive control; constrained by citizen norms
 *   - Contemporary Analytical Observer: Neutral observer (analytical/analytical) — sees the gerousia confirmation process as performative ritual maintaining degraded institutional authority; recognizes the structure as extraction rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__spartan_eugenic_reading, 0.78).
domain_priors:suppression_score(personhood_boundary__spartan_eugenic_reading, 0.82).
domain_priors:theater_ratio(personhood_boundary__spartan_eugenic_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__spartan_eugenic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__spartan_eugenic_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__spartan_eugenic_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__spartan_eugenic_reading, snare).
narrative_ontology:human_readable(personhood_boundary__spartan_eugenic_reading, "Personhood Boundary: Spartan Eugenic Reading (Gerousia Confirmation)").
narrative_ontology:topic_domain(personhood_boundary__spartan_eugenic_reading, "moral_philosophy/historical_ethics/developmental_biology").

domain_priors:requires_active_enforcement(personhood_boundary__spartan_eugenic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__spartan_eugenic_reading, '12fd3bba-4ee7-4cd3-aa08-b33a0b076942').
narrative_ontology:cs_kernel_codification('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', fixed_text).
narrative_ontology:cs_authority_grounding('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', lineage).
narrative_ontology:cs_interpretation_layer_present('12fd3bba-4ee7-4cd3-aa08-b33a0b076942').
narrative_ontology:cs_reading_relation('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', personhood_boundary__developmental_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', foundational, state_confirmed_fitness_constitutes_personhood).
narrative_ontology:cs_axiom_status(state_confirmed_fitness_constitutes_personhood, overridden).
narrative_ontology:cs_axiom_grounding('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', state_confirmed_fitness_constitutes_personhood, deontological).
narrative_ontology:cs_axiom('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', foundational, unconfirmed_organism_is_excludable).
narrative_ontology:cs_axiom_status(unconfirmed_organism_is_excludable, overridden).
narrative_ontology:cs_axiom_grounding('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', unconfirmed_organism_is_excludable, deontological).
narrative_ontology:cs_reference_frame('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', gerousia_fitness_adjudication).
narrative_ontology:cs_drift_state('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', contemporary_human_rights_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('12fd3bba-4ee7-4cd3-aa08-b33a0b076942', '').
narrative_ontology:cs_kernel_id(personhood_boundary__spartan_eugenic_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__spartan_eugenic_reading, spartan_state_apparatus).
narrative_ontology:constraint_beneficiary(personhood_boundary__spartan_eugenic_reading, confirmed_citizen_body).
narrative_ontology:constraint_victim(personhood_boundary__spartan_eugenic_reading, unconfirmed_infants).
narrative_ontology:constraint_victim(personhood_boundary__spartan_eugenic_reading, disabled_neonates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCONFIRMED INFANT (SNARE) — No agency, no voice, no legal personhood status. Trapped in the dependency condition pending gerousia assessment. High suppression (physical dependency, legal exclusion, no voice in confirmation process). Maximum experienced extraction — existence itself contingent on fitness evaluation controlled entirely by the state apparatus.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PARENT OF UNCONFIRMED INFANT (SNARE) — Trapped by state authority over their child's personhood status. Cannot exit the confirmation process. Cannot prevent exposure if assessment deems the child unfit. Suppression operates through legal prohibition on infanticide resistance and complete state authority over reproduction norms. Extraction runs as reproductive constraint — the state extracts population-control authority from parental attachment and biological capacity.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SPARTAN STATE / GEROUSIA (ROPE) — State sees this constraint as pure coordination: maintaining military strength and population quality requires collective vetting of offspring. The gerousia interprets this as solving a collective action problem (ensuring only healthy, capable citizens are born). Beneficiary experiencing the constraint as coordination mechanism. Has complete exit capacity (can choose fitness criteria, arbitrage boundaries of personhood, adjust confirmation threshold). Net beneficiary — extraction runs toward the institutional apparatus, not from it.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: CONFIRMED CITIZEN BODY (TANGLED ROPE) — Benefits from the constraint (population quality maintenance, military cohesion, resource concentration). But also subject to the same fitness criteria and state population control. Constrained by inability to reject the framework without losing citizen status. Experiences both coordination (collective security through vetting) and extraction (reproductive control). Exit options limited — conforming to citizen norms is the only path; deviation risks descending to unconfirmed or outcast status.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONTEMPORARY FRAME (PITON) — Historical institutions grounding personhood in state-confirmed fitness appear to the modern observer as vestigial, maintained through inertia and historical mythology rather than functional necessity. The gerousia assessment process is performative (ceremonial confirmation masking state extraction authority). Theater ratio (0.55) reflects that much of the confirmation ritual serves ceremonial legitimation rather than actual fitness evaluation. The modern view sees this as a degraded institutional form — logically vulnerable to challenge but historically persistent.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: NATURAL LAW / UNIVERSALIZED VIEW (MOUNTAIN) — At universal scope and civilizational horizon, there exists a reading that positions state authority over neonatal personhood as grounded in natural law: populations naturally select for fitness, reproduction is inherently regulated by material scarcity and genetic capacity, and the state merely formalizes what nature already does. This perspective risks naturalizing the contingent institutional arrangement (gerousia confirmation) as an expression of immutable biological necessity. The engine's false summit detector will identify this as problematic — the structural data reveals beneficiaries and victims, indicating extraction rather than natural law.
constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__spartan_eugenic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__spartan_eugenic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__spartan_eugenic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personhood_boundary__spartan_eugenic_reading, TR),
    TR >= 0.70.

:- end_tests(personhood_boundary__spartan_eugenic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The state captures substantial authority over who is granted personhood and who is exposed. This is not voluntary coordination — the neonates cannot consent, and parents have no legal recourse. The trajectory from 0.62 to 0.78 reflects that as the gerousia system matures and becomes institutionalized, it increasingly formalizes what began as ad-hoc population control into systematic state power. Suppression (0.82): Very high. Multiple reinforcing barriers prevent resistance: legal exclusion of unconfirmed organisms from personhood status (removing moral standing to resist exposure), parental dependency on state approval (economic and legal), cultural normalization of fitness-based selection, and the biological vulnerability of neonates (cannot flee, communicate, or organize). Claimed type (Snare): The constraint is pure extraction with minimal coordination function from the victim perspective. The state benefits from reproductive control; victims bear costs of exclusion and death risk. Theater ratio (0.55): Moderate. The gerousia confirmation process includes ceremonial elements (formal inspection, ritual declaration) but is not entirely theatrical — the functional outcome (personhood determination, exposure authorization) is real and consequential. The theater serves to legitimize what is fundamentally a state extraction of reproductive authority.
 *
 * PERSPECTIVAL GAP:
 *   The unconfirmed infant and trapped parent classify the constraint as Snare (pure extraction with no coordination benefit). The state apparatus classifies it as Rope (coordination mechanism solving the collective problem of population quality). The confirmed citizen body sees Tangled Rope (both benefits and constraints). The contemporary observer sees Piton (degraded, performative ritual maintained through inertia). The naturalized universal view risks classifying it as Mountain (fitness regulation as natural law). These perspectival gaps reveal the constraint's central function: it legitimizes what the powerless experience as pure extraction by framing it, from the beneficiary perspective, as coordination for the common good. The Spartan reading's power derives from successfully naturalizing this reframing.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) for each perspective is determined by their structural position relative to the personhood-boundary extraction. Unconfirmed infants have d ≈ 0.98 (full target of extraction — their personhood is contingent on state approval). Parents of unconfirmed infants have d ≈ 0.92 (nearly full targets — their reproductive autonomy is constrained; their attachment to offspring is leveraged as suppression mechanism). The state apparatus has d ≈ 0.05 (full beneficiary — gains extraction authority; can arbitrage personhood boundaries). Confirmed citizens have d ≈ 0.55 (symmetric — both benefit and constrained by the same fitness regime). The analytical observer, lacking structural position as beneficiary or victim, takes canonical analytical d ≈ 0.73. The engine computes f(d) from these values; the sigmoid amplifies the chi (effective extraction) for trapped powerless agents and dampens it for arbitrary-exit beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint resolves the mandatrophy by revealing its source. The Spartan eugenic reading appears to be two different constraints depending on perspective: Rope (state coordination) and Snare (victim extraction). Mandatrophy resolves by recognizing that both are simultaneously true — they describe the same institutional structure from different structural positions. The resolution is not 'which type is correct?' but 'how does the state maintain authority over these two incompatible perspectives?' Answer: through suppression of the victim's voice and control of the official framing as coordination. The constraint exhibits mandatrophy, and its resolution is the engine of its own persistence — the suppression that maintains the Rope perspective against the Snare reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_determinacy,
    'Who determines what counts as ''fitness'' in gerousia assessment, and by what epistemic standard?',
    'Historical analysis of actual gerousia criteria across time and cases; comparison of stated fitness standards (health, intelligence, capacity) versus revealed criteria (family status, economic resources, political allegiance)',
    'If criteria are genuinely objective and transparent: constraint is coordination mechanism (Rope). If criteria are opaque or serve state preference: constraint is extraction mechanism (Snare). If criteria have shifted over time: constraint exhibits drift toward extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criterion_determinacy, empirical, 'Whether fitness criteria are objective, transparent, and stable or serve state extraction').

omega_variable(
    foreclose_birth_threshold_reading,
    'Does the Spartan eugenic reading logically foreclose the birth-threshold reading (personhood begins at parturition, regardless of state confirmation)?',
    'Logical analysis: if personhood is defined as requiring gerousia confirmation, then a reading that grants personhood at birth without state confirmation directly contradicts this axiom. The readings cannot coexist in a single unified framework — one must deny the other''s core premise.',
    'If forecloses: the readings are in genuine logical conflict; a single framework cannot hold both. If coexists: the readings represent different parties'' commitments and can persist simultaneously across different institutional contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclose_birth_threshold_reading, conceptual, 'Logical relationship between gerousia-confirmation and birth-threshold readings').

omega_variable(
    developmental_capacity_reading_influence,
    'How does the Spartan eugenic reading influence the developmental-capacity reading (personhood emerges gradually with cognitive and volitional development)?',
    'Structural analysis: the gerousia reading grounds personhood in state-adjudicated fitness at a fixed moment (confirmation). The developmental reading grounds it in the child''s own emerging capacities. Does one create pressure on the other? Does acceptance of one reading affect the viability of the other?',
    'If influences: the readings have asymmetric structural relationship (one upstream, one downstream). If coexists: they represent genuinely orthogonal axes that different parties can hold simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_capacity_reading_influence, conceptual, 'Whether gerousia reading influences developmental-capacity reading').

omega_variable(
    exposure_moral_status_ambiguity,
    'In this reading, is exposure of an unconfirmed infant homicide, infanticide, or neither? Is it morally equivalent to refusing to provide life support to a non-person?',
    'Textual and historical analysis: what did Spartan law and moral philosophy classify exposure as? What did contemporary critics and philosophers (Plutarch, Aristotle, later commentators) classify it as? Do modern reconstructions of Spartan ethics treat it as distinct from homicide?',
    'If exposure is homicide: the reading''s claim that unconfirmed organisms are ''legitimately excludable'' contradicts established moral categories and the reading''s axiom is unstable. If exposure is not homicide in the reading''s framework: the reading''s axioms cohere but rest on a non-universalizable moral distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exposure_moral_status_ambiguity, conceptual, 'Moral classification of neonatal exposure in Spartan eugenic framework').

omega_variable(
    state_authority_grounding_basis,
    'What grounds the Spartan state''s authority to confirm or deny personhood? Natural law, divine will, collective utility, genealogical right, or prior conquest?',
    'Textual analysis of Spartan political philosophy and constitutional claims (Plutarch, Xenophon, Aristotle''s descriptions of Lycurgan law); examination of whether the gerousia''s authority derives from expertise, lineage, practice, or extractive institutional interest',
    'If grounded in expertise or lineage: the reading''s authority structure is coherent within its own tradition. If grounded only in institutional extraction: the reading''s legitimacy claim is weaker — it is masked extraction rather than genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_grounding_basis, conceptual, 'Source of gerousia''s authority to determine personhood status').

omega_variable(
    contemporary_sustainability_of_axioms,
    'Are the foundational axioms of this reading (state authority over population quality, fitness-based personhood boundaries) still holdable in contemporary discourse, or have they been overridden by intervening moral and legal developments?',
    'Survey of contemporary bioethics, human rights law, disability rights frameworks, and philosophical consensus. Do any credible contemporary actors defend these axioms, or have they been universally rejected?',
    'If holdable: the reading remains a live position (even if minoritarian). If overridden: the reading is a historical artifact with minimal contemporary force; it is maintained only as a reference point for understanding past institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_sustainability_of_axioms, preference, 'Whether axioms of state population authority and fitness-based personhood remain defensible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__spartan_eugenic_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__spartan_eugenic_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pers_tr_t2, personhood_boundary__spartan_eugenic_reading, theater_ratio, 2, 0.52).
narrative_ontology:measurement(pers_tr_t4, personhood_boundary__spartan_eugenic_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__spartan_eugenic_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(pers_be_t2, personhood_boundary__spartan_eugenic_reading, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(pers_be_t4, personhood_boundary__spartan_eugenic_reading, base_extractiveness, 4, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__spartan_eugenic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(personhood_boundary__spartan_eugenic_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__spartan_eugenic_reading, personhood_boundary__developmental_capacity_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three constraint stories, one per reading: spartan_eugenic_reading (state confirmation), birth_threshold_reading (birth as constitutive moment), developmental_capacity_reading (gradual emergence). All three stories share the same kernel but instantiate different structural relationships between personhood status, victim sets, and authority structures. Each story has its own extractiveness value reflecting the different empirical status and structural force of each reading. The stories are linked via network.affects_constraints to show their mutual influence — accepting one reading affects the viability and scope of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
