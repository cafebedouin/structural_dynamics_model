% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Property: Unrestricted Ownership Reading
 *   domain: legal_philosophy/applied_ethics/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel 'animal
 *   status' — specifically, the property reading under which animals are
 *   legal objects (things) without independent moral standing, and human
 *   ownership is unrestricted except by welfare statutes treated as external
 *   constraints rather than fundamental limits on property rights. This is a
 *   kernel reading because the same underlying commitment (what is the moral
 *   and legal status of animals?) admits multiple structurally distinct
 *   resolutions: the property reading (animals = things, ownership
 *   unrestricted), the welfare reading (animals = beings with interests,
 *   ownership restricted by welfare standards), and the abolitionist reading
 *   (animals = moral patients, property status illegitimate). The property
 *   reading has been the dominant legal framework in Western property law for
 *   centuries, but it is not logically inevitable — it is one position in an
 *   ongoing dispute. Within this reading, extractiveness is minimal (ε ≈
 *   0.05) because there is no victim set: animals are not recognized as
 *   bearers of interests, so no extraction is admitted to be occurring. The
 *   constraint classifies as rope (pure coordination) from the perspectives
 *   of owners and commercial actors — the legal framework solves the problem
 *   of allocating use rights and preventing resource conflict among humans.
 *   The constraint also registers as rope from the doctrinal analyst's
 *   perspective (property law coherence) and as a false mountain from the
 *   natural law analyst (risking naturalization of a contingent choice). The
 *   welfare movement, operating within this reading, experiences tangled
 *   rope: they must advocate within the property frame while constrained by
 *   the frame's definition of animals as non-moral-patients. The omega
 *   variables document the irreducible uncertainties: whether animals are
 *   moral patients, whether welfare statutes redefine ownership or merely
 *   constrain it, whether property is the natural default or one constructed
 *   reading, and whether the reading's stability depends on institutional
 *   continuity or philosophical argument.
 *
 * KEY AGENTS:
 *   - Property Owners: Primary beneficiary (powerful/arbitrage) — capture ownership benefits, control use, extract economic value from animal bodies and labor without obligation to the animals themselves
 *   - Commercial Animal Users (agriculture, research, entertainment): Primary beneficiary (institutional/arbitrage) — benefit from property framing that permits unrestricted use and exits by relocating to lighter-regulation jurisdictions
 *   - Animals: Explicitly not victims in this reading — defined as non-moral-patients without interests, so extraction is not admitted. This is the reading's defining move.
 *   - Jurisprudential Tradition (Property Doctrine): Institutional beneficiary — the coherence and continuity of property law depend on this reading; alternative readings threaten doctrinal stability
 *   - Animal Welfare Advocates: Secondary participant (organized/constrained) — accept the property frame but work to expand welfare constraints; experience the frame as limiting because it denies animals intrinsic standing
 *   - Analytical Observer: Civilizational perspective — risks naturalizing the property reading as an inevitable logical conclusion from human rationality, missing the reading's contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.08).
domain_priors:theater_ratio(animal_status__property_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property: Unrestricted Ownership Reading").
narrative_ontology:topic_domain(animal_status__property_reading, "legal_philosophy/applied_ethics/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '6373d43a-16e7-4270-9bff-9e8eb5c55b2e').
narrative_ontology:cs_kernel_codification('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', formalized).
narrative_ontology:cs_authority_grounding('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', lineage).
narrative_ontology:cs_interpretation_layer_present('6373d43a-16e7-4270-9bff-9e8eb5c55b2e').
narrative_ontology:cs_reading_relation('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', foundational, animals_lack_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', animals_lack_moral_standing, conventional).
narrative_ontology:cs_axiom('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', foundational, human_rationality_grounds_property_right).
narrative_ontology:cs_axiom_status(human_rationality_grounds_property_right, holdable).
narrative_ontology:cs_axiom_grounding('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', human_rationality_grounds_property_right, deontological).
narrative_ontology:cs_reference_frame('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', western_property_law_tradition).
narrative_ontology:cs_drift_state('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', contemporary_welfare_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6373d43a-16e7-4270-9bff-9e8eb5c55b2e', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, commercial_animal_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTY OWNER (ROPE) — Within this reading, animal ownership is pure coordination: the legal framework establishes clear property rights, enabling market transactions, investment decisions, and resource planning. Low extraction experienced because the framework supports the owner's interests. Welfare statutes appear as minimal external constraints, not asymmetric extraction.
constraint_indexing:constraint_classification(animal_status__property_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMERCIAL SECTOR (ROPE) — Experiences the property reading as enabling coordination: clear ownership rules facilitate contracts, financing, and economies of scale. Welfare statutes are treated as minor operational costs rather than fundamental constraints on property rights. The sector has arbitrage options (exit to other jurisdictions with lighter welfare regulation).
constraint_indexing:constraint_classification(animal_status__property_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: JURISPRUDENTIAL ANALYST / PROPERTY DOCTRINE (ROPE) — From a doctrinal standpoint within property law tradition, animal ownership is coherent and low-extraction coordination: it allocates use rights, prevents tragedy of the commons, and enables productive labor. Within this reference frame, animals are not moral patients — the framework treats them as things, not persons. The analyst sees welfare statutes as add-on regulations consistent with property ownership, not as fundamental constraints on the reading itself.
constraint_indexing:constraint_classification(animal_status__property_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ANIMAL WELFARE MOVEMENT (TANGLED ROPE) — Organized advocates for animal welfare within THIS reading see a mixed constraint: the property framework enables welfare protections (owners can be held legally accountable for cruelty) but also naturalizes animals as mere things, making welfare claims seem like secondary restrictions rather than fundamental rights. The movement must work within the property frame to advocate for stronger standards, creating asymmetric effort. However, this perspective sees potential in expanding welfare statute scope, not in dismantling property status.
constraint_indexing:constraint_classification(animal_status__property_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURAL LAW ANALYST (MOUNTAIN) — From a foundational philosophical perspective that treats property as a natural or inevitable human institution, animal property status appears as a logical conclusion from human cognitive superiority and the absence of enforceable duties between humans and non-persons. The analyst sees this reading as stable across all contexts — animals cannot claim rights because they lack the rational agency that grounds moral standing. However, this risks a false summit: what appears natural may be a contingent historical and institutional choice.
constraint_indexing:constraint_classification(animal_status__property_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. Within the property reading, extractiveness is near-zero because the framework does not admit a victim set — animals are ontologically excluded from moral standing. No extraction is recognized because no moral patient is present to be extracted from. The small non-zero value (not exactly 0.0) reflects that welfare statutes create minimal overhead: owners must meet basic welfare requirements, creating small friction costs. However, these are not asymmetric extraction because they are framed as duties to manage property, not duties to the property itself. Suppression (0.08): Very low. Owners have full legal discretion to use animals subject only to welfare statutes; suppression consists only of these external constraints, which are relatively light. Animals have no legal standing to resist, so no active suppression mechanism is needed — the ontological exclusion is sufficient. Theater ratio (0.35): Low-to-moderate. The property doctrine is substantive rather than performative: it establishes clear rules and enforcement mechanisms. However, some theater is present in welfare statutes, which sometimes perform concern for animals while maintaining the fundamental property status (performance of care within an extraction framework). The theater has increased slightly over the interval as welfare language has expanded while property status persists.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the property reading's internal classification (rope: pure coordination among humans) and the alternative readings' classification of the same structural facts (welfare reading: tangled_rope; abolitionist reading: snare or mountain extracted harm). Within the property reading, the gap appears between the owner's experience (rope: stable allocation of use rights) and the welfare advocate's experience (tangled_rope: constrained advocacy within a limiting frame). The natural law perspective risks a false summit, claiming that property status is an immutable conclusion from human rationality rather than one reading of a contested kernel. The jurisprudential analyst's rope classification is coherent within property doctrine but depends entirely on accepting the reading's foundational claim that animals lack moral standing.
 *
 * DIRECTIONALITY LOGIC:
 *   The property reading produces minimal directionality asymmetry because it contains no victim set. Owners and commercial actors experience low extraction (d ≈ 0.15) because they benefit from the framework and face minimal welfare constraints. Animals are not present in the directionality calculation because they are not recognized as agents with interests. From the standpoint of alternative readings (welfare, abolitionist), the property reading would appear to have high directionality (owners benefit, animals are harmed) — but within the property reading itself, animals are not counted as participants in the constraint. This is the reading's structural feature: it excludes animals from moral standing, so no extraction is admitted. The welfare movement experiences moderate directionality (d ≈ 0.45) because they are constrained by the property frame while working to expand welfare norms.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the property reading is ONE reading of a contested kernel, not THE correct classification of animal status. The property reading produces rope classification (low extraction) because it excludes animals from the victim set by defining them as non-moral-patients. The welfare reading produces tangled_rope (mixed coordination and extraction) because it admits animals as beings with interests but maintains property ownership with welfare constraints. The abolitionist reading produces snare or mountain (extraction or immutable harm) because it treats property ownership itself as the violation. No single type is 'correct' — the classification depends on which reading is adopted. The property reading is internally consistent and produces low extractiveness only if one accepts its foundational axioms (animals lack moral standing). Challenge those axioms (the moral_patients omega) and the constraint reclassifies. The mandatrophy is resolved by making the reading choice explicit and documenting the axioms that drive the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animals_as_moral_patients,
    'Are animals moral patients (beings whose interests matter morally) within the property reading framework?',
    'Logical analysis: if animals are moral patients, property status is inconsistent with moral duty. If they are not, the reading is internally coherent but rests on an empirical and ethical claim about sentience and moral considerability.',
    'If animals ARE moral patients: the property reading is internally contradictory and the constraint reclassifies toward snare (extractive relation masked as ownership). If animals are NOT moral patients: the reading is coherent but its foundation depends on a contestable empirical claim (absence of sentience/interests). This is the core omega structuring the kernel dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animals_as_moral_patients, conceptual, 'Whether animals are moral patients given the property reading''s definitional frame').

omega_variable(
    welfare_statute_integration,
    'Do welfare statutes operate as external constraints on property rights, or as constitutive definitions of legitimate property ownership?',
    'Doctrinal analysis: if welfare statutes are external restraints, property status and welfare can coexist with the statutes as add-ons. If they are constitutive, expanded welfare standards would eventually redefine ownership away from the property reading entirely. Track jurisdictional variation: do welfare expansions correlate with ownership restrictions or remain compatible with property framing?',
    'If external: the property reading remains structurally stable and welfare advocates work within the frame. If constitutive: welfare expansion is a creeping reclassification toward the abolitionist reading, and the property reading is unstable under pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_integration, conceptual, 'Whether welfare statutes constrain or constitutively redefine property ownership').

omega_variable(
    owner_interests_vs_animal_interests,
    'When owner interests directly conflict with animal welfare (e.g., confinement vs movement), does the property reading permit the owner to prevail, and if so, does this constitute extractive harm?',
    'Case law analysis and philosophical investigation: does property law permit practices that impose severe suffering (confinement, pain) on sentient beings? If yes, and if animals have interests, this may reveal extractive structure masked by property framing. If the property reading must be qualified by welfare constraints to avoid admitting this extraction, the reading''s coherence is compromised.',
    'If extraction is admitted: the reading moves toward snare or at minimum tangled_rope. If extraction is denied (animals have no interests to violate): the reading depends on the contestable moral_patients omega above. If conflicts are resolved by welfare constraints that limit property, the property reading''s autonomy is compromised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(owner_interests_vs_animal_interests, empirical, 'Structural conflict between owner property rights and animal welfare in practice').

omega_variable(
    sentience_threshold_for_moral_status,
    'What threshold of sentience or cognitive capacity is required for moral standing, and does the property reading''s implicit threshold (none) reflect a defensible position?',
    'Empirical investigation of animal cognition and neurobiological markers of sentience. Philosophical analysis of whether the absence of human-type rationality genuinely removes moral standing or merely reduces the complexity of moral duties.',
    'If many animals cross the sentience threshold: the property reading''s foundation is empirically false and the constraint reclassifies. If animals do not have sentience: the property reading is coherent but depends on an empirical discovery. If sentience exists on a spectrum: the threshold becomes a policy choice, not a natural fact, and the constraint moves toward tangled_rope or snare (asymmetric choice benefiting owners).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_for_moral_status, empirical, 'Empirical status of animal sentience and its moral significance').

omega_variable(
    reading_kernel_relationship,
    'Is this property reading one of several coherent readings of a contested kernel (animal status), or is it the default natural interpretation that alternative readings must justify as exceptions?',
    'Historical and comparative analysis: did the property reading emerge naturally or was it constructed? In legal systems that recognize animal rights or grant standing to animals, does the property reading persist as one live option or collapse entirely? Does the reading''s persistence depend on institutional inertia or philosophical argument?',
    'If natural default: the reading is more stable and resistant to reclassification. If constructed: it is more dependent on active enforcement and more vulnerable to institutional drift. This omega determines whether the constraint is inherently stable (rope/mountain) or contingent on continuous reinforcement (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Kernel status of animal property reading: natural default vs. constructed reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anprop_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anprop_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(anprop_tr_t100, animal_status__property_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(anprop_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(anprop_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.04).
narrative_ontology:measurement(anprop_be_t100, animal_status__property_reading, base_extractiveness, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the animal_status kernel family consisting of three structurally distinct readings with different extractiveness values and victim sets. The property reading (this story) has ε ≈ 0.05 and no victims (animals excluded from moral standing). The welfare reading has ε ≈ 0.40 and admits animals as moral patients with constrained standing. The abolitionist reading has ε ≈ 0.70+ and treats property ownership itself as extractive harm. Each reading is internally coherent but produces different classifications and victim sets. They are linked not as sequential historical states but as simultaneous live positions in legal and ethical discourse. The property reading does not logically lead to the welfare reading — they represent different answers to the same kernel question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
