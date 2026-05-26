% ============================================================================
% CONSTRAINT STORY: welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_welfare_reading, []).

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
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: welfare_reading
 *   human_readable: Animal Welfare Reading: Suffering Minimization Within Regulated Use
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The welfare reading of animal moral status asserts that sentient beings
 *   are capable of suffering, that suffering should be minimized, and that
 *   use of animals for human benefit is permissible when conducted under
 *   welfare standards that limit cruelty. This reading has become
 *   institutionalized across most Western jurisdictions as the dominant
 *   framework for animal ethics in law and policy. However, the welfare
 *   reading is one reading of a contested kernel—the underlying question of
 *   what moral status animals possess and what obligations humans have toward
 *   them. The property reading holds that animals are property with no
 *   independent moral status; the abolitionist reading holds that use is
 *   inherently incompatible with moral status and should be prohibited. The
 *   welfare reading positions itself as a middle path: acknowledging
 *   sentience and the wrongness of cruelty while preserving use systems. This
 *   structural compromise creates a Tangled Rope constraint: the reading
 *   genuinely coordinates between animal protection advocates, regulated
 *   industries, and consumers by establishing a shared language of 'humane'
 *   treatment. Simultaneously, it extracts by legitimating continued use
 *   under a welfare frame that may not deliver meaningful suffering
 *   reduction. The extractiveness measure (0.52) reflects that the reading
 *   benefits regulated industries and consumer comfort at the expense of
 *   animals in use systems and of ethical consistency. The theater ratio
 *   (0.64) reflects that welfare certification and labeling are substantially
 *   performative—measuring suffering reduction within use rather than
 *   questioning use itself.
 *
 * KEY AGENTS:
 *   - Farmed Animals: Primary victim (powerless/trapped) — unable to exit use systems; experience minimal coercion reduction despite welfare certification
 *   - Animal Advocacy Organizations: Secondary beneficiary/victim (moderate/constrained) — benefit from welfare frameworks as institutional entry point but constrained by reform incrementalism; enforce the cruelty-use boundary that legitimates the system
 *   - Regulated Industries: Primary beneficiary (institutional/arbitrage) — capture market legitimacy, consumer confidence, and immunity from cruelty prosecution through welfare compliance
 *   - Consumers: Beneficiary/victim dual role (powerful/mobile) — benefit from welfare labeling (reduced cognitive dissonance, information) but experience extraction through continued participation in use systems
 *   - Welfare Certification Systems: Institutional actor (institutional/arbitrage) — maintain legitimacy capture through performative audits; theater persists because system addresses appearance of ethical concern without disrupting economics
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the cruelty-use distinction as moral law rather than contingent institutional boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(welfare_reading, 0.52).
domain_priors:suppression_score(welfare_reading, 0.58).
domain_priors:theater_ratio(welfare_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(welfare_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(welfare_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(welfare_reading, tangled_rope).
narrative_ontology:human_readable(welfare_reading, "Animal Welfare Reading: Suffering Minimization Within Regulated Use").
narrative_ontology:topic_domain(welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(welfare_reading, fixed_text).
narrative_ontology:cs_authority_grounding(welfare_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(welfare_reading).
narrative_ontology:cs_kernel_id(welfare_reading, animal_moral_status).
narrative_ontology:cs_reading_relation(welfare_reading, property_reading, forecloses).
narrative_ontology:cs_reading_relation(welfare_reading, abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom(welfare_reading, foundational, sentience_generates_moral_status).
narrative_ontology:cs_axiom_status(sentience_generates_moral_status, holdable).
narrative_ontology:cs_axiom_grounding(welfare_reading, sentience_generates_moral_status, deontological).
narrative_ontology:cs_axiom(welfare_reading, foundational, use_permissible_under_welfare_constraints).
narrative_ontology:cs_axiom_status(use_permissible_under_welfare_constraints, holdable).
narrative_ontology:cs_axiom_grounding(welfare_reading, use_permissible_under_welfare_constraints, conventional).
narrative_ontology:cs_reference_frame(welfare_reading, sentient_moral_patients_with_permissible_use).
narrative_ontology:cs_drift_state(welfare_reading, contemporary_escalating_abolition_pressure, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(welfare_reading, welfare_organizations).
narrative_ontology:constraint_beneficiary(welfare_reading, regulated_industries).
narrative_ontology:constraint_beneficiary(welfare_reading, consumer_comfort).
narrative_ontology:constraint_victim(welfare_reading, sentient_animals_in_use).
narrative_ontology:constraint_victim(welfare_reading, ethical_consistency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FARMED ANIMAL (SNARE) — Trapped in systems justified by welfare frameworks; experiences minimal coercion reduction despite welfare certification. Suppression is total: animal cannot exit, cannot negotiate, cannot refuse the use system. Perceives constraint as extraction: permitted suffering framed as 'humane.' High extraction chi from this perspective due to trapped status and maximal directionality.
constraint_indexing:constraint_classification(welfare_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ANIMAL ADVOCACY ORGANIZATION (TANGLED ROPE) — Constrained by need for institutional legitimacy and funding from welfare-sympathetic donors; benefits from welfare frameworks as entry point to discourse but also trapped in incremental reform logic. The organization both coordinates welfare standards (genuine function) and enforces the boundary between 'acceptable use' and 'cruelty,' legitimating the use system. Significant extraction embedded in coordination role: the organization's existence depends on the constraint persisting. Moderate exit costs — abandoning welfare frameworks risks funding and platform.
constraint_indexing:constraint_classification(welfare_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as coordination: welfare standards provide market legitimacy, consumer comfort, and a clear boundary between legal operations and liability. The industry can exit welfare compliance (arbitrage) by relocating to non-regulated jurisdictions or accepting market penalties. Net beneficiary: extraction flows toward this actor through preserved market access, consumer confidence, and immunity from 'cruelty' prosecution. Experiences the constraint as beneficial coordination rather than extraction.
constraint_indexing:constraint_classification(welfare_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CONSUMER (TANGLED ROPE) — Mobile with respect to the constraint; can exit by adopting vegetarianism or selecting welfare-certified products. Experiences coordination: welfare labeling solves the information problem and moral discomfort. Also experiences extraction: welfare framing permits continued consumption that abolitionists would classify as inherently extractive. Powerful enough to influence markets through purchasing but distributed across populations with heterogeneous preferences. Moderate extraction: the constraint enables participation in use systems while reducing cognitive dissonance.
constraint_indexing:constraint_classification(welfare_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WELFARE CERTIFICATION SYSTEM (PITON) — Institutional theater. Welfare audits, third-party certification, and labeling protocols are substantially performative: they measure suffering reduction within use contexts but do not measure whether use itself is justified. The system persists through institutional inertia and legitimacy capture—maintained because it appears to address ethical concerns without disrupting economic systems. Theater ratio high because the primary function (appearing to minimize suffering) is decoupled from the structural question (whether use is permissible at all). Piton classification derives from theater gate, not from low extraction: the certification system's existence depends on the constraint persisting.
constraint_indexing:constraint_classification(welfare_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a universalized perspective, this reading risks naturalizing the cruelty/use distinction as an immutable moral boundary. The perspective treats 'minimizing suffering is the only obligation we have toward sentient beings' as a law of ethics rather than as a contingent institutional arrangement that benefits certain parties. The engine's false summit detector will identify this as naturalization: the beneficiary structure (welfare organizations, regulated industries, consumer comfort) reveals that the 'natural' boundary between permissible and impermissible treatment is actually contingent on who benefits from the distinction.
constraint_indexing:constraint_classification(welfare_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(welfare_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(welfare_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(welfare_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(welfare_reading, TR),
    TR >= 0.70.

:- end_tests(welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The welfare reading benefits regulated industries by providing market legitimacy and consumer comfort while constraining use methods rather than use itself. Animals bear the structural cost: suffering is limited but not eliminated, and the welfare frame legitimates continued use that abolitionists would classify as inherently extractive. The reading also benefits advocacy organizations by providing an institutional entry point and funding, but constrains them within incremental reform logic. The extractiveness value reflects that the reading's primary function is not reducing animal suffering but distributing moral legitimacy between use advocates and protection advocates. Suppression (0.58): Moderate-high. Animals have no exit option and cannot negotiate welfare standards. The consumer and advocacy organization can exit (through vegetarianism or abolitionist framing) but face significant costs. Suppression is embedded in the kernel itself: the reading presumes that use is permissible, constraining moral discourse to methods rather than justification. Theater ratio (0.64): Moderate-high. Welfare certification systems measure suffering reduction within use contexts but do not address whether use is justified. Third-party audits, labeling protocols, and welfare standards are substantially performative—they appear to address ethical concerns while leaving use systems intact. Theater has increased over time as certification systems have proliferated without corresponding use reduction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon—the welfare reading of animal moral status—appears as coordination to beneficiaries (Rope for regulated industry) and extraction to victims (Snare for trapped animals). The advocacy organization experiences a Tangled Rope: it genuinely coordinates between welfare advocates and use advocates (coordination function) while being constrained by the very boundary it helps enforce (extraction mechanism). The consumer experiences Tangled Rope: the welfare label provides information and moral comfort (coordination) while enabling continued participation in use systems that abolitionists would classify as unjustifiable (extraction). The certification system appears as a Piton: the performative protocols persist through institutional inertia even as their functional capacity to reduce suffering plateaus. The civilizational analytical observer risks seeing the cruelty-use distinction as a natural moral boundary (Mountain) rather than a contingent institutional arrangement that benefits specific parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The welfare_reading's directionality derives from its function as a middle path between property rights (which exclude animals from moral status) and abolition (which excludes use entirely). The reading's beneficiary structure—regulated industries, welfare organizations, consumers—creates a specific flow of extraction. Regulated industries experience low directionality (d ≈ 0.2) because they benefit from welfare as coordination mechanism. Advocacy organizations experience moderate directionality (d ≈ 0.5) because they both benefit from institutional legitimacy and are constrained by incremental logic. Animals experience maximum directionality (d ≈ 0.95) because they are trapped in use systems and cannot negotiate welfare standards. The reading's institutional position depends on coexistence with the property_reading (which it influences by appearing more protective) and the abolitionist_reading (which it forecloses within mainstream policy discourse by capturing the term 'animal welfare' for a use-permissive framework). The chi formula yields moderate extraction at the institutional level but severe extraction from the animal perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cruelty_use_distinction_stability,
    'Is the distinction between ''cruelty'' (impermissible suffering) and ''use'' (permissible suffering) a stable moral boundary or a rhetorical construction that shifts with institutional power?',
    'Historical analysis of welfare standards across jurisdictions and time periods; identification of whether the boundary tracks genuine changes in animal suffering or changes in industry capacity to absorb costs; comparison with property_reading and abolitionist_reading frameworks to test whether the distinction survives cross-framework analysis.',
    'If stable: welfare_reading is a legitimate ethical framework. If constructed: the reading is a Snare for animals, masked as coordination. If unstable: the reading is a Piton sustained by institutional inertia rather than coherent principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cruelty_use_distinction_stability, conceptual, 'Whether cruelty-use distinction is stable moral boundary or institutional construct').

omega_variable(
    welfare_asymmetry_mechanism,
    'Does the welfare framework reduce animal suffering measurably, or does it primarily reduce human discomfort with the use system?',
    'Neuroscientific and ethological evidence on suffering reduction under welfare vs. conventional systems; comparative stress biomarkers; measurement of actual behavioral freedom vs. labeling claims; analysis of whether welfare improvements correlate with use reduction or only with use methods.',
    'If welfare reduces suffering: tangled_rope classification confirmed with genuine coordination function. If welfare primarily reduces human discomfort: the constraint is extraction mechanism disguised as coordination—reclassify toward snare across more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_asymmetry_mechanism, empirical, 'Whether welfare framework reduces animal suffering or primarily human discomfort').

omega_variable(
    institutional_lock_in_reading_dependency,
    'Would the welfare_reading be institutionally sustainable if the property_reading and abolitionist_reading were not available as alternatives?',
    'Institutional analysis: removal of alternatives reveals whether the welfare_reading''s legitimacy depends on appearing moderate relative to abolition while remaining permissive relative to property rights. Test: does welfare advocacy collapse or transform its framing if abolition becomes dominant? Does industry support for welfare disappear if property rights fail?',
    'If lock-in confirmed: the welfare_reading''s beneficiary structure is partly conditional on the availability of ''more extreme'' alternatives. The reading itself becomes an institutional equilibrium mechanism rather than a self-standing ethical framework. This would be omega resolution into the Tangled Rope territory—the reading coordinates multiple parties by positioning itself between extremes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_reading_dependency, conceptual, 'Whether welfare_reading''s legitimacy depends on coexistence with alternative readings').

omega_variable(
    kernel_reading_identity,
    'This story instantiates the welfare_reading of the animal_moral_status kernel. Are the sibling readings (property_reading, abolitionist_reading) structurally distinct constraints or alternative perspectives on a single constraint?',
    'Compare epsilon values: if property_reading and abolitionist_reading have ε values that differ from welfare_reading by > 0.25, they are distinct constraints (different structural mechanisms). If ε values differ by < 0.1, they may be perspectival variants of the same constraint (different observables, same structure). The ε-invariance principle applies: if changing the observable changes ε substantially, you have multiple constraints.',
    'If distinct constraints: welfare_reading, property_reading, and abolitionist_reading are three separate stories linked via network.affects_constraints and cs_structure.reading_relations. If perspectival variants: they are single-constraint perspectives. This determines whether the committer frame (kernel reading system) or the ordinary perspectival frame applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether sibling readings are distinct constraints or perspectives on one constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(welfare_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welfare_tr_t0, welfare_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(welfare_tr_t15, welfare_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(welfare_tr_t30, welfare_reading, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(welfare_be_t0, welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(welfare_be_t15, welfare_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(welfare_be_t30, welfare_reading, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(welfare_reading, identity_coordination).
narrative_ontology:affects_constraint(welfare_reading, property_reading).
narrative_ontology:affects_constraint(welfare_reading, abolitionist_reading).

% DUAL FORMULATION NOTE:
% The welfare_reading, property_reading, and abolitionist_reading form a constraint family around the animal_moral_status kernel. Each reading has its own ε value reflecting different structural mechanisms. The welfare_reading's moderate ε (0.52) reflects its position as a Tangled Rope that genuinely coordinates between welfare advocates and use advocates while simultaneously legitimating use systems. Sibling readings have different epsilon values: the property_reading treats animals as having no moral status (lower ε if seen as pure coordination of market logic, higher ε if seen as extraction mechanism for animal use). The abolitionist_reading treats use as inherently unjustifiable (lower ε as pure coordination among abolitionists, higher ε as constraint on existing use systems). Each reading operates from a different reference frame on the kernel, producing different classifications and beneficiary-victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(welfare_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
