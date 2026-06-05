% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Legitimacy of Imposed Practice via Hybrid Scaffolding (State-Ideological Mandate with Elite Modeling)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_scaffolding_reading of the
 *   legitimacy_of_imposed_practice kernel. The kernel asks: by what mechanism
 *   does a top-down mandate for cultural practice displacement succeed, fail,
 *   or achieve partial success? The hybrid_scaffolding_reading claims that
 *   pure decree fails (no internalization), pure endogenous climb is slow (no
 *   state reinforcement), and scaffolded imposition — combining state mandate
 *   with ideological messaging and elite modeling — achieves partial
 *   displacement (quasi-endogenous pull). The empirical delta across cases:
 *   Calendar reform (decree without scaffolding: failed). Dress codes
 *   (scaffolded via elite modeling and ideological prestige messaging:
 *   partial success with hybrid adoption). The constraint operates as a
 *   tangled_rope: it coordinates new social identity (genuine coordination
 *   function) while extracting compliance and cultural displacement from
 *   those excluded from scaffolding infrastructure (asymmetric extraction).
 *   Beneficiaries are urban elites who gain status and network access through
 *   the new practice; victims are rural populations facing pure decree
 *   without ideological pathway, and cultural practice bearers whose prior
 *   traditions are displaced without legitimacy-building alternatives.
 *
 * KEY AGENTS:
 *   - Urban Elites: Primary beneficiaries (institutional/arbitrage) — gain cultural prestige, state-network access, and status consolidation through adoption of imposed practice within scaffolding framework
 *   - State Apparatus: Secondary beneficiary and mandate authority (organized/constrained) — achieves cultural consolidation and unified national identity through scaffolded mandate; experiences both coordination (social unification) and extraction (political control)
 *   - Urban Middle Populations: Primary victims of tangled_rope extraction (moderate/constrained) — experience both genuine access to scaffolding infrastructure and extraction pressure (compliance, identity ambiguity, partial adoption costs)
 *   - Rural Populations: Secondary victims of pure snare (powerless/trapped) — face decree without scaffolding pathways; maximum suppression and coercive enforcement; no ideological messaging or elite modeling available; pure extraction without coordination benefit
 *   - Prior Practice Bearers: Tertiary victims (power varies/identity_locked) — cultural traditions displaced; identity fused with prior practice; scaffolding pathway creates identity lock preventing exit even where material exit might be possible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.52).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Legitimacy of Imposed Practice via Hybrid Scaffolding (State-Ideological Mandate with Elite Modeling)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e4d4fdae-787c-4347-aeec-8713b88730d0').
narrative_ontology:cs_kernel_codification('e4d4fdae-787c-4347-aeec-8713b88730d0', distributed).
narrative_ontology:cs_authority_grounding('e4d4fdae-787c-4347-aeec-8713b88730d0', extraction).
narrative_ontology:cs_reading_relation('e4d4fdae-787c-4347-aeec-8713b88730d0', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4d4fdae-787c-4347-aeec-8713b88730d0', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('e4d4fdae-787c-4347-aeec-8713b88730d0', foundational, scaffolding_success_mechanism).
narrative_ontology:cs_axiom_status(scaffolding_success_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e4d4fdae-787c-4347-aeec-8713b88730d0', scaffolding_success_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('e4d4fdae-787c-4347-aeec-8713b88730d0', foundational, pure_decree_insufficient).
narrative_ontology:cs_axiom_status(pure_decree_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('e4d4fdae-787c-4347-aeec-8713b88730d0', pure_decree_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('e4d4fdae-787c-4347-aeec-8713b88730d0', scaffolded_mandate_with_stratified_outcomes).
narrative_ontology:cs_drift_state('e4d4fdae-787c-4347-aeec-8713b88730d0', contemporary_cultural_imposition_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4d4fdae-787c-4347-aeec-8713b88730d0', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prior_cultural_practice_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POPULATION (SNARE) — Faces pure decree without ideological scaffolding infrastructure. Suppression is maximal: no elite modeling pathway available, no ideological messaging reaching them, no partial-adoption hybrid opportunity. Enforcement is coercive (legal mandate). No coordination benefit; pure extraction of compliance through legal/administrative force.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN MIDDLE POPULATION (TANGLED ROPE) — Experiences hybrid scaffolding: some ideological messaging reaches them, some elite modeling is visible, but access to full legitimacy infrastructure is limited by economic position. Extraction occurs (compliance with partial adoption, identity ambiguity costs), but genuine coordination benefit exists (access to state resources, recognition through partial adoption, generation pathways for children). Suppression is moderate — alternatives exist but carry cost.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN ELITES (ROPE) — Experiences the constraint as coordination: ideological scaffolding + elite modeling creates identification opportunity and social legitimacy. Net benefits from the new practice (status, access to state-centered networks, cultural prestige). The constraint is experienced as enabling their group's cultural position, not extracting from them. Arbitrage exit means they could adopt alternative practices but choose not to — the scaffolded practice aligns with their interests.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE APPARATUS (TANGLED ROPE) — Experiences dual function: coordinating social identity (creating unified national practice through scaffolding) AND extracting compliance (enforcement through legal mandate). The state has genuine coordination interest (unified national culture simplifies governance), but also extractive interest (cultural displacement serves political consolidation). Enforcement machinery required; ideological messaging is both a coordination tool and an extraction mechanism.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks framing the constraint as an immutable law: 'practice displacement requires both top-down mandate AND ideological scaffolding.' This naturalization obscures that the scaffolding infrastructure is itself a contingent political choice, not an inevitable feature of how cultural change works. The engine's false summit detector will identify this as a false natural law — beneficiaries exist (urban elites, state apparatus), and the scaffolding mechanism is a constructed institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hybrid scaffolding mechanism achieves significant displacement through ideological pull and elite modeling, but achieves only partial success — not total practice replacement. The initial extractiveness (0.38) reflects the mechanism's dependency on voluntary ideological adoption at the elite level; as scaffolding infrastructure matures and succeeds in creating internalized commitment (t=20, ε=0.52), extractiveness rises. However, it does not reach snare threshold (0.66+) because the constraint retains genuine coordination function: it does unify social practice and create shared identity, not merely coerce compliance. Theater ratio (0.65): Moderate-high and rising. Elite modeling and ideological messaging contain performative elements (public displays of adoption, identity signaling), but are functionally different from pure theater — the elite adoption is genuine (not performed for show, but drives real status). The theater ratio rises over time as the scaffolding infrastructure becomes more established and more purely performative (younger generations adopt through socialization rather than active ideological persuasion). Suppression (0.58): Moderate-high but declining. Initial suppression is high because pure decree dominates; as scaffolding succeeds, explicit suppression can decrease (alternatives are suppressed through lack of ideological attractiveness rather than legal coercion). The decline from 0.70 to 0.58 reflects the hybrid mechanism's core claim: quasi-endogenous pull reduces need for active suppression.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across power positions. Rural populations (trapped/powerless) experience pure snare: decree without scaffolding, maximum suppression, zero coordination benefit. Urban middle (constrained/moderate) experience genuine tangled_rope: some scaffolding access, some ideological messaging, mixed extraction and coordination benefit. Urban elites (arbitrage/institutional) experience rope: scaffolding amplifies their status, genuine coordination of unified practice, net benefit. The state apparatus (constrained/organized) experiences tangled_rope: genuine coordination function (social unification) alongside extractive function (political control and cultural consolidation). The analytical observer risks a false-summit mountain classification — framing scaffolded cultural displacement as an inherent natural law rather than a contingent institutional arrangement. This perspectival gap reveals the reading's core claim: that hybrid scaffolding's success depends entirely on the availability of ideological pathways and elite modeling infrastructure, not on inevitable properties of human psychology or social change.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) determines experienced extractiveness (χ) via the sigmoid f(d). Rural populations are pure victims (high d ~ 0.95) with trapped exit → f(d) ~ 1.42 → high χ. Urban middle have mixed status (moderate d ~ 0.55) with constrained exit → f(d) ~ 0.75 → moderate χ. Urban elites are beneficiaries (low d ~ 0.15) with arbitrage exit → f(d) ~ -0.01 → low/negative χ. State apparatus is secondary beneficiary (d ~ 0.35) with organized power → moderate χ but tempered by organizational internal contradictions (coordination function vs. extraction function). No directionality overrides are required — the structural derivation from beneficiary/victim status + exit options produces coherent d values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_mechanism_genuine_coordination,
    'Does ideological scaffolding (elite modeling + messaging) constitute genuine coordination benefit for adopters, or is it internalized coercion that the constraint exploits by framing as voluntary identification?',
    'Historical ethnography: post-displacement survey asking adopters whether they experienced scaffolding as enabling opportunity or as internalized pressure. Comparison with parallel cases where identical ideological messaging was offered without state mandate — did adopters embrace it? Comparison with cases where mandate was imposed without scaffolding — what was the compliance/internalization ratio?',
    'If genuine coordination: the tangled_rope classification is correct — hybrid scaffolding produces real mixed benefits. If internalized coercion: the constraint is functionally a Snare for most agents below elite level, merely disguised as rope through ideological framing (false summit risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_mechanism_genuine_coordination, empirical, 'Whether ideological scaffolding is genuine coordination or internalized coercion').

omega_variable(
    elite_modeling_sufficiency_threshold,
    'What proportion of the elite population must visibly adopt the imposed practice for scaffolding to succeed in generating quasi-endogenous pull at the middle/lower levels?',
    'Cross-historical comparison: cases where elite adoption was high (>70%) vs. low (<30%); correlation with adoption rates at lower strata. Identify whether a phase transition exists (e.g., >50% elite adoption produces rapid mass adoption, <30% elite adoption stalls at plateau).',
    'If high threshold (>70% required): scaffolding is brittle — depends on near-universal elite compliance. If low threshold (<30% sufficient): scaffolding is robust — minority elite modeling generates sufficient legitimacy. Affects whether this reading forecasts success or coexists with endogenous_climb_reading (slow uptake without elite saturation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_modeling_sufficiency_threshold, empirical, 'Elite adoption threshold for quasi-endogenous pull generation').

omega_variable(
    rural_exclusion_structural_or_contingent,
    'Is the rural population''s exclusion from scaffolding infrastructure a necessary feature of this imposed-practice mechanism, or a contingent outcome of the specific historical cases where it succeeded?',
    'Counterfactual analysis: were there technical barriers (geography, communication infrastructure) that made rural scaffolding impossible, or was rural exclusion a deliberate political choice (resources concentrated on urban centers to consolidate core elite support)? Cases with attempted rural scaffolding — did they succeed or fail? What resources were required?',
    'If necessary: rural populations MUST experience pure snare (decree without scaffolding). If contingent: the constraint could be reformulated with expanded scaffolding (scaffold type replacing tangled_rope for more agents). Affects the structural stability of the reading — whether hybrid scaffolding is necessarily an urban/rural dual constraint, or whether it could achieve full population coverage with different resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_exclusion_structural_or_contingent, empirical, 'Whether rural exclusion is structural or contingent to hybrid scaffolding').

omega_variable(
    reading_competition_kernel_settlement,
    'Do the three readings of the legitimacy_of_imposed_practice kernel coexist as live positions in actual historical/political dispute, or does historical evidence forecast that one reading will eventually foreclose the others?',
    'Examine contemporary policy debates about cultural imposition (e.g., language policy, education standards, religious practice restrictions): Are all three readings articulated as live positions by real political actors? Or does evidence (success/failure rates of different mandates) begin to narrow the live set?',
    'If coexist: all three readings remain holdable axioms; the kernel is genuinely contested and unresolved. If one reading is foreclosing others: the axiom status changes from ''holdable'' to ''overridden'' or ''foreclosed'' in coming generations. Affects whether cs_structure.reading_relations correctly maps as coexists_with or should be updated to influences/forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_competition_kernel_settlement, conceptual, 'Long-term settlement trajectory of contested readings about imposed practice legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(limp_hybrid_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(limp_hybrid_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(limp_hybrid_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(limp_hybrid_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(limp_hybrid_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(limp_hybrid_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(limp_hybrid_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(limp_hybrid_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(limp_hybrid_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family decomposing the contested claim 'legitimacy of imposed practice.' All three readings share the same empirical domain (state mandates for cultural displacement) but partition it into distinct structural constraints based on which mechanism each reading claims is operative. The sibling readings are NOT observable-dependent variants of one constraint — they are three genuinely competing theoretical claims about how cultural imposition works. Each reading gets its own constraint story with its own epsilon, beneficiary/victim structure, and perspectives. The network edges establish which readings coexist, influence, or foreclose each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
