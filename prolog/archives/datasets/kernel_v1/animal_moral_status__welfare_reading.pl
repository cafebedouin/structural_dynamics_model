% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Moral Status: Welfare Reading (Sentience + Regulated Use)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The animal moral status constraint exhibits a fundamental tension between
 *   acknowledging animal sentience and preserving human use of animals. The
 *   welfare reading resolves this tension by accepting sentience as a moral
 *   fact (animals can suffer and deserve consideration) while declaring use
 *   itself permissible if conducted without cruelty. This creates a hybrid
 *   constraint: genuine coordination function (reducing animal suffering
 *   through welfare standards) coexists with extraction function (the use
 *   system remains protected by the welfare framework, which becomes the
 *   ceiling for moral concern). The constraint operates across multiple
 *   institutional domains (agriculture, research, entertainment, companions)
 *   and generates perspectival conflict: animals experience it as a trap
 *   (welfare regulations but no exit from use); welfare organizations
 *   experience it as legitimacy and career pathway; regulated industries
 *   experience it as manageable compliance; rights-aware consumers experience
 *   it as identity lock that forecloses deeper moral commitment. The theater
 *   ratio (0.62) reflects the performative layer of welfare certification —
 *   formal standards, audits, certifications, and labeling that provide
 *   consumer reassurance while the fundamental use premise remains unchanged.
 *   The extractiveness trajectory (0.22 → 0.38 over 30 years) shows rising
 *   extraction as welfare frameworks become institutionalized: the welfare
 *   reading absorbs moral concern and reduces pressure on the use system
 *   itself, allowing the use system to expand under the legitimacy provided
 *   by welfare compliance.
 *
 * KEY AGENTS:
 *   - Sentient Animals in Use Systems: Primary victim (powerless/trapped) — welfare constraints bind their experience but cannot exit use; bear full cost of the 'permissible use' premise
 *   - Animal Welfare Organizations: Institutional beneficiary (institutional/arbitrage) — provide legitimacy for the welfare framework and derive funding, career advancement, and influence through welfare advocacy
 *   - Regulated Industries (Agriculture, Research, Entertainment): Implicit institutional beneficiary (institutional/arbitrage) — use system continues under welfare compliance; moral pressure is absorbed into manageable regulation
 *   - Individual Welfare Advocates: Secondary victim (moderate/constrained) — capacity to improve welfare but identity-locked within the use system; cannot challenge use itself without institutional marginalization
 *   - Rights-Aware Consumers: Identity-locked victim (moderate/identity_locked) — structurally mobile but committed to the premise that 'humane use is acceptable'; exit would require identity-level shift
 *   - Future Generations and Genuine Animal Interests: Abstract victim (powerless/trapped) — institutionalization of welfare framework prevents deeper moral recognition
 *   - Regulatory Bodies: Institutional co-beneficiary (institutional/arbitrage) — exercise authority over welfare standards; maintain legitimacy through enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as genuinely hybrid: real coordination (welfare improvements) and real extraction (use system protection)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.38).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.48).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Moral Status: Welfare Reading (Sentience + Regulated Use)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '95dd2402-4333-4d25-8e96-b7f50ad8b519').
narrative_ontology:cs_kernel_codification('95dd2402-4333-4d25-8e96-b7f50ad8b519', fixed_text).
narrative_ontology:cs_authority_grounding('95dd2402-4333-4d25-8e96-b7f50ad8b519', lineage).
narrative_ontology:cs_interpretation_layer_present('95dd2402-4333-4d25-8e96-b7f50ad8b519').
narrative_ontology:cs_reading_relation('95dd2402-4333-4d25-8e96-b7f50ad8b519', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('95dd2402-4333-4d25-8e96-b7f50ad8b519', animal_moral_status__property_reading, influences).
narrative_ontology:cs_axiom('95dd2402-4333-4d25-8e96-b7f50ad8b519', foundational, animal_sentience_moral_relevance).
narrative_ontology:cs_axiom_status(animal_sentience_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('95dd2402-4333-4d25-8e96-b7f50ad8b519', animal_sentience_moral_relevance, empirically_contingent).
narrative_ontology:cs_axiom('95dd2402-4333-4d25-8e96-b7f50ad8b519', foundational, use_permissibility_with_welfare_constraint).
narrative_ontology:cs_axiom_status(use_permissibility_with_welfare_constraint, holdable).
narrative_ontology:cs_axiom_grounding('95dd2402-4333-4d25-8e96-b7f50ad8b519', use_permissibility_with_welfare_constraint, deontological).
narrative_ontology:cs_reference_frame('95dd2402-4333-4d25-8e96-b7f50ad8b519', sentience_recognized_use_preserved).
narrative_ontology:cs_drift_state('95dd2402-4333-4d25-8e96-b7f50ad8b519', contemporary_animal_advocacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('95dd2402-4333-4d25-8e96-b7f50ad8b519', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulatory_bodies).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, sentient_animals_in_use_systems).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, genuine_animal_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENTIENT ANIMAL (SNARE) — Trapped within systems of regulated use with no exit option. Bears full extraction: welfare constraints are binding (cannot escape use) while the use itself is declared permissible. The 'cruelty-prevention' mechanism is the only floor; the use continues regardless. Maximum experienced extraction from the powerless position.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL WELFARE ADVOCATE (TANGLED ROPE) — Constrained by institutional pressure and resource limits, but also beneficiary of the legitimacy the welfare framework provides. Can organize around specific welfare improvements and has agency within the use system, but cannot challenge use itself without institutional marginalization. Mixed coordination (improving welfare methods) and extraction (being coopted by the use system itself).
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE ORGANIZATION (ROPE) — Institutional beneficiary with arbitrage options. Experiences the constraint as pure coordination: translating concern for animal suffering into manageable policy solutions that preserve the use system while reducing cruelty. Funding, legitimacy, and career paths flow to organizations that work within the welfare paradigm. Net beneficiary — the constraint enables their institutional existence.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATED INDUSTRY (ROPE) — Implicit institutional beneficiary with arbitrage options. Experiences the constraint as manageable coordination: welfare regulations add compliance costs but preserve the use system. The welfare paradigm permits operation without requiring fundamental change to business models. Net beneficiary — extraction flows toward them through continued market legitimacy.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHTS-AWARE CONSUMER (SNARE) — Structurally mobile (can exit consumption patterns) but identity-locked within the welfare framework. Has internalized the premise that 'humane use is acceptable' despite cognitive awareness that this premise forecloses deeper moral concern. Exit would require abandoning the identity-level commitment to the welfare compromise. Experiences extraction through the identity frame that makes exit unthinkable from within.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: FUTURE GENERATIONS / GENUINE ANIMAL INTERESTS (SNARE) — Abstract collective with no voice in the present constraint. Trapped by institutionalization of the welfare framework, which prevents recognition of deeper moral status. The welfare reading becomes the ceiling for moral concern; future generations inherit a system that has absorbed and neutralized the more demanding moral claim.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the welfare reading is a hybrid constraint: it genuinely coordinates animal welfare improvements (coordination function is real — suffering is reduced) while simultaneously extracting from deeper moral concern for animal autonomy and status (extraction function is real — the use premise is protected by the welfare framework). The constraint has both functions, making it structurally tangled.
constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(animal_moral_status__welfare_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, moderate): The welfare reading coordinates genuine animal welfare improvements (reduced suffering is real coordination) but simultaneously extracts by protecting the use system itself. The reading forecloses the deeper moral claim that animals have rights independent of human use. Extractiveness is not high (like a snare) because welfare improvements are genuine; not low (like rope) because the use premise is protected by the welfare framework. The trajectory from 0.22 to 0.38 reflects institutional capture: as welfare regulations become standard, they absorb moral concern and reduce pressure on the use system, allowing expansion under legitimacy cover. Suppression (0.48, moderate-high): Animals are trapped in use systems with no legal exit, and alternative moral framings (rights-based) are suppressed through institutional marginalization of abolitionist voices. However, suppression is not total because welfare discourse acknowledges sentience and creates spaces for advocacy within the use system. Theater ratio (0.62, moderate-high): Welfare standards, certifications, audits, and labeling provide consumer reassurance and institutional legitimacy but are substantially performative. The standards often do not prevent fundamental welfare degradation; they provide assurance that the use system is 'humane' without addressing use itself. The theater has risen as welfare frameworks became institutionalized and professionalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival divergence. The sentient animal (powerless/trapped) classifies it as Snare — welfare regulations are the only floor, but use continues without possibility of exit. The welfare organization (institutional/arbitrage) classifies it as Rope — they experience it as pure coordination, solving the legitimate problem of reducing animal suffering. The analytical observer (analytical/analytical) classifies it as Tangled Rope — the constraint has both coordination function (real welfare improvements) and extraction function (use system protection). The rights-aware consumer (moderate/identity_locked) classifies it as Snare not because of structural barriers but because their identity is fused with the welfare premise; exit would require abandoning a core self-definition. The gap reveals the constraint's hybrid nature: for those who benefit from use system legitimacy, it is coordination; for those trapped in use systems, it is extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. Animals (powerless/trapped) have d ≈ 0.95 (full targets of extraction) because they bear costs without exit. Welfare organizations (institutional/arbitrage) have d ≈ 0.10 (beneficiaries with low extraction) because they gain legitimacy and funding while use continues. Regulated industries (institutional/arbitrage) have d ≈ 0.12 (implicit beneficiaries) because compliance costs are manageable and use system is protected. Individual welfare advocates (moderate/constrained) have d ≈ 0.62 (mixed victims) because they can improve welfare but are constrained from challenging use itself. Rights-aware consumers (moderate/identity_locked) have d ≈ 0.70 (victims with identity lock rather than external barriers) because their exit capacity is cognitive rather than structural. The directionality values feed into the sigmoid f(d) to compute experienced extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the welfare reading produces a genuinely tangled structure: the constraint coordinates welfare improvements (beneficiary organizations, reduced suffering) while extracting from deeper moral concern (the use premise is protected). The tension is not resolvable by choosing 'is it coordination or extraction?' — it is both, structurally. The mandatrophy resolves through recognizing that the welfare reading is one reading among contested alternatives. From within the welfare framework, the constraint is legitimate coordination: suffering matters morally, and welfare regulations are the appropriate response. From the abolitionist framework, the same structural arrangement is extraction: protecting use while neutralizing moral concern. From the property framework, both welfare and abolitionist readings are false: animals have no independent moral status. The constraint does not become false by being contested; it becomes precise. The classification (Tangled Rope) is correct for the welfare reading itself — the extraction is real (the use premise is protected) and the coordination is real (welfare improves).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_sufficiency_ambiguity,
    'Is ''minimizing suffering within use systems'' a genuine moral achievement or a rhetorical ceiling that prevents recognition of animal moral status as rights-bearers?',
    'Comparison of animal outcome trajectories under welfare-only frameworks vs. rights-based frameworks; analysis of whether welfare improvements asymptotically approach abolitionist standards or plateau at distinctly different thresholds',
    'If welfare creates asymptotic improvement toward rights: reading is legitimate intermediate framework. If welfare plateau is structurally distinct from abolitionist baseline: reading forecloses deeper moral claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_sufficiency_ambiguity, empirical, 'Whether welfare improvements approach or foreclose rights-based baselines').

omega_variable(
    institutional_capture_mechanism,
    'Does the welfare framework systematically absorb moral concern and neutralize it through regulation, thereby protecting the use system more effectively than crude denial of animal sentience would?',
    'Institutional analysis: examine whether welfare organizations that achieve regulatory victories subsequently reduce pressure on the use system itself; track correlation between welfare legitimacy gains and industry expansion or regulatory capture',
    'If capture confirmed: welfare reading is extraction mechanism disguised as coordination. If capture absent: reading is genuine coordination with extraction as unintended side effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_mechanism, empirical, 'Institutional capture: welfare legitimacy enabling industry expansion').

omega_variable(
    identity_lock_cognitive_cost,
    'What proportion of people adopting the welfare reading do so through genuine moral reasoning vs. identity-level commitment to the use system that forecloses deeper concern?',
    'Cognitive and identity research: test whether welfare framework adoptees show identity-fusion markers (defensive response to abolitionist arguments, difficulty articulating why use is permissible beyond ''welfare is sufficient''); measure whether explicit rights arguments produce genuine reconsideration or identity threat',
    'If high identity fusion: welfare reading is entry point to identity lock. If low: reading is transparent moral position subject to revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cognitive_cost, empirical, 'Proportion of welfare-reading adoption driven by identity lock vs. moral reasoning').

omega_variable(
    kernel_reading_contest_structure,
    'Which reading of the animal_moral_status kernel is this constraint instantiating, and what are the structural relationships to sibling readings?',
    'Committer-axis analysis: this constraint instantiates the welfare_reading, which coexists_with the abolitionist_reading (both live in public discourse) and influences the property_reading (by establishing that animal sentience matters, even if use is permissible — property_reading must defend against sentience acknowledgment). See cs_structure.reading_relations.',
    'Clarifies that this is ONE reading among contested alternatives, not a natural law or settled fact. The constraint''s classification depends on which reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'This constraint is the welfare_reading of the animal_moral_status kernel').

omega_variable(
    regulatory_implementation_gap,
    'What is the gap between welfare standards as written and welfare standards as enforced? Does enforcement asymmetry differ by industry and jurisdiction?',
    'Audit and compliance analysis: compare written welfare standards to actual animal conditions in regulated facilities; measure enforcement intensity, penalty severity, and regulatory capture by industry',
    'If gap is large and variable: suppression metric may underestimate actual coercion (on-the-ground suppression exceeds nominal standards). If gap is small: suppression metric accurately reflects stated protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_implementation_gap, empirical, 'Gap between nominal welfare standards and actual enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_theater_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(animal_welfare_theater_t15, animal_moral_status__welfare_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(animal_welfare_theater_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(animal_welfare_extract_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(animal_welfare_extract_t15, animal_moral_status__welfare_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(animal_welfare_extract_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_suppress_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(animal_welfare_suppress_t15, animal_moral_status__welfare_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(animal_welfare_suppress_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, factory_farming_intensification).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_research_institutional_review).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three constraint stories: property_reading (animals are property), welfare_reading (THIS story — animals are sentient but use is permissible), and abolitionist_reading (animals are rights-bearers; use is violation). Each story instantiates one reading with its own ε, own perspectives, and own beneficiary/victim structure. The readings are linked via reading_relations in cs_structure. All three stories should include network.affects_constraints entries pointing to instantiations of the kernel (the other readings) to enable contamination analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
