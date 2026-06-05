% ============================================================================
% CONSTRAINT STORY: exclusionary_base__citizen_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusionary_base__citizen_privilege_reading, []).

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
 *   constraint_id: exclusionary_base__citizen_privilege_reading
 *   human_readable: Athenian Citizenship as Guarded Estate (Pericles' Double Descent Law)
 *   domain: political/historical/citizenship
 *
 * SUMMARY:
 *   In 451/0 BCE, Pericles enacted a law limiting Athenian citizenship to
 *   those with both parents of citizen status — double descent, or purity of
 *   blood. This constraint marks a critical moment in democratic history: as
 *   the franchise and its material benefits grew (jury pay, assembly
 *   stipends, military advancement), the demos tightened the border. The law
 *   excluded thousands of residents with one foreign parent, stripping them
 *   of legal recourse and civic participation despite residential history and
 *   kinship ties to citizens. This story instantiates ONE READING of the
 *   contested kernel called 'exclusionary_base' — specifically, the
 *   citizen_privilege reading. This reading asks: what structural function
 *   does the citizenship restriction serve for those it benefits? The answer
 *   is extractive and coordinative in equal measure: the law coordinates the
 *   citizen body while extracting from those excluded, amplifying the value
 *   of membership by restricting supply. The law is not primarily about
 *   xenophobia (though that is the cover story) but about rent-seeking by an
 *   expanding demos that sought to concentrate the growing material returns
 *   of empire and slavery within a shrinking denominator. The constraint's
 *   extractiveness (0.52) reflects that the extraction is substantial but
 *   masked as law and tradition rather than overt coercion. The suppression
 *   (0.68) reflects that alternatives (residence-based membership,
 *   property-based membership) were structurally available but actively
 *   foreclosed. The theater ratio (0.35) reflects that the law is justified
 *   through formal narratives of purity and order, but the primary mechanism
 *   is benefit concentration — relatively low theater because the motivation
 *   is material, not performative.
 *
 * KEY AGENTS:
 *   - Existing Citizen Body (institutional/arbitrage) — primary beneficiary; captures concentrated civic power and material benefits
 *   - Propertied Families with Dual-Lineage Heritage (institutional/arbitrage) — secondary beneficiary; consolidates heritable citizenship capital
 *   - Mixed-Descent Residents (powerless/trapped) — primary victim; excluded despite residential presence and kinship ties
 *   - Metic/Foreign Resident Community (moderate/constrained) — secondary victim; constrained legal status with some trade and property rights
 *   - Democratic Assembly Reformers (organized/constrained) — agent perceiving sunset; trajectories toward inclusion contest the restriction
 *   - Legitimation Apparatus (institutional/arbitrage) — maintains narrative that citizenship reflects merit (Pericles' Funeral Oration) despite blood-based reality
 *   - Analytical Observer (analytical/analytical) — detects naturalization of contingent choice as immutable boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusionary_base__citizen_privilege_reading, 0.52).
domain_priors:suppression_score(exclusionary_base__citizen_privilege_reading, 0.68).
domain_priors:theater_ratio(exclusionary_base__citizen_privilege_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusionary_base__citizen_privilege_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(exclusionary_base__citizen_privilege_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(exclusionary_base__citizen_privilege_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusionary_base__citizen_privilege_reading, tangled_rope).
narrative_ontology:human_readable(exclusionary_base__citizen_privilege_reading, "Athenian Citizenship as Guarded Estate (Pericles' Double Descent Law)").
narrative_ontology:topic_domain(exclusionary_base__citizen_privilege_reading, "political/historical/citizenship").

domain_priors:requires_active_enforcement(exclusionary_base__citizen_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exclusionary_base__citizen_privilege_reading, '9ecc243b-75a4-4a38-a378-42059d2d958b').
narrative_ontology:cs_kernel_codification('9ecc243b-75a4-4a38-a378-42059d2d958b', formalized).
narrative_ontology:cs_authority_grounding('9ecc243b-75a4-4a38-a378-42059d2d958b', lineage).
narrative_ontology:cs_interpretation_layer_present('9ecc243b-75a4-4a38-a378-42059d2d958b').
narrative_ontology:cs_reading_relation('9ecc243b-75a4-4a38-a378-42059d2d958b', exclusionary_base__imperial_tribute_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ecc243b-75a4-4a38-a378-42059d2d958b', exclusionary_base__slave_economy_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('9ecc243b-75a4-4a38-a378-42059d2d958b', foundational, citizenship_defined_by_dual_descent).
narrative_ontology:cs_axiom_status(citizenship_defined_by_dual_descent, holdable).
narrative_ontology:cs_axiom_grounding('9ecc243b-75a4-4a38-a378-42059d2d958b', citizenship_defined_by_dual_descent, conventional).
narrative_ontology:cs_axiom('9ecc243b-75a4-4a38-a378-42059d2d958b', foundational, membership_scarcity_preserves_share_value).
narrative_ontology:cs_axiom_status(membership_scarcity_preserves_share_value, holdable).
narrative_ontology:cs_axiom_grounding('9ecc243b-75a4-4a38-a378-42059d2d958b', membership_scarcity_preserves_share_value, instrumental).
narrative_ontology:cs_reference_frame('9ecc243b-75a4-4a38-a378-42059d2d958b', autochthonous_bloodline_legitimacy).
narrative_ontology:cs_drift_state('9ecc243b-75a4-4a38-a378-42059d2d958b', post_democratic_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ecc243b-75a4-4a38-a378-42059d2d958b', '').
narrative_ontology:cs_kernel_id(exclusionary_base__citizen_privilege_reading, exclusionary_base).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusionary_base__citizen_privilege_reading, existing_citizen_body).
narrative_ontology:constraint_beneficiary(exclusionary_base__citizen_privilege_reading, propertied_families_with_dual_lineage).
narrative_ontology:constraint_victim(exclusionary_base__citizen_privilege_reading, mixed_descent_residents).
narrative_ontology:constraint_victim(exclusionary_base__citizen_privilege_reading, female_non_citizen_relatives).
narrative_ontology:constraint_victim(exclusionary_base__citizen_privilege_reading, children_of_citizen_fathers_and_foreign_mothers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIXED-DESCENT RESIDENT (SNARE) — Born in Athens to a citizen father and foreign mother, legally excluded from citizenship by Pericles' law. No path to belonging, no legal recourse, no participation in the demos. Trapped in permanent civic inferiority despite residential history and kinship to citizens. The exclusion has zero coordination benefit for this agent and total extraction of civic dignity.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: METIC/FOREIGN RESIDENT COMMUNITY (TANGLED ROPE) — Constrained by lack of civic participation but able to conduct trade, maintain households, and accumulate property. Some benefits from the legal order and market coordination that citizenship-based institutions provide; also excluded from juries, assembly, and military command. The constraint coordinates the boundary of the demos while extracting from those outside it.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: EXISTING CITIZEN BODY (ROPE) — Pericles' law protects and amplifies the value of citizenship shares by restricting the denominator. Citizens benefit from coordination of the demos, maintenance of civic institutions, and the concentrated power that scarcity of membership provides. For the citizen body, the law is experienced as stabilization and protection of a coordination mechanism, not as coercive extraction.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROPERTIED FAMILIES WITH DUAL-LINEAGE HERITAGE (ROPE) — The law especially benefits families of sufficient wealth to maintain endogamous marriage patterns and dual-descent purity. For these actors, citizenship becomes heritable capital, consolidated across generations. Generational perspective reveals the constraint as a kinship-coordination mechanism that preserves family power.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: DEMOCRATIC ASSEMBLY / REFORM COALITION (SCAFFOLD) — From the perspective of democratic reformers across generations, Pericles' law is a temporary coordination failure to be remedied by expanding the demos. The constraint is experienced as a sunset — the natural trajectory of democratic development is toward broader inclusion. The law persists through institutional inertia and entrenched family power, but it is not viewed as permanent.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: LEGITIMATION RITUAL (PITON) — The formal theory of Athenian citizenship presents it as meritocratic and open (Pericles' Funeral Oration celebrates the demos and law, not blood). The actual law enforces strict lineage. The gap between narrative and practice is substantial theater. The law persists partly through this theatrical legitimation — the demos convinced itself that citizenship is what meritocracy produces, not what blood obtains.
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL BOUNDARY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the boundary between insider and outsider is presented as a natural feature of political organization: every polity must define membership, every demos must have a border, exclusion is inherent to inclusion. However, this reading naturalizes a contingent historical choice. The engine will detect this as a false summit — the constraint is not about the existence of boundaries but about the specific closure mechanism (blood) and its tightening (Pericles' restriction).
constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusionary_base__citizen_privilege_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exclusionary_base__citizen_privilege_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exclusionary_base__citizen_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exclusionary_base__citizen_privilege_reading, TR),
    TR >= 0.70.

:- end_tests(exclusionary_base__citizen_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The law restricts access to material benefits (jury pay, assembly stipends, military advancement) that grew substantially during the fifth century. The restriction is not total coercion but strategic scarcity — the demos benefits from coordination of legal participation while extracting from those excluded. The trajectory shows increasing extractiveness (0.35 → 0.52) as empire deepened and the rents to citizenship expanded. Suppression (0.68): Moderate-high. The law is enforced through genealogical verification, citizenship challenges, and exclusion from juries and assembly. But suppression is not total — metics maintain property rights and can conduct trade. The suppression mechanism is active legal enforcement (courts, challenges) rather than violent coercion, which reduces the raw suppression score but increases theater. Theater ratio (0.35): Moderate-low. The law is justified through narrative of purity (blood, legitimacy) rather than explicit rent-seeking, which creates some theater. But the mechanism is material (benefit concentration) not purely performative, so theater remains moderate. The trajectory shows increasing theater as the purity narrative is repeatedly invoked to defend a mechanism whose real function (benefit concentration) becomes harder to obscure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The existing citizen body experiences Rope — coordination of democratic institutions and protection of membership value. Propertied families experience Rope at generational scale — heritable capital consolidation. Mixed-descent residents experience Snare — total exclusion from belonging and civic participation. Metics experience Tangled Rope — some benefits from legal order coordination, substantial extraction from civic exclusion. Democratic reformers experience Scaffold — a temporary coordination failure with a sunset trajectory toward broader inclusion. The legitimation apparatus experiences Piton — maintaining a narrative (merit-based citizenship) that the actual law (blood-based) contradicts. The analytical observer risks reading Mountain — seeing the boundary itself as natural law — but the structural data (identifiable beneficiaries, active enforcement, tightening mechanism) reveals false summit dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the constraint. The existing citizen body and propertied families are beneficiaries with arbitrage options (they could dissolve citizenship restrictions but choose not to) — low d, negative effective extraction. Mixed-descent residents are victims with no exit — high d, maximum experienced extraction. Metics are victims with constrained exit (property rights exist, but civic participation is foreclosed) — moderate-high d. Democratic reformers are organized agents with constrained exit (they can advocate but cannot unilaterally revise the law) — moderate d. The theater apparatus experiences arbitrage (it derives authority from claiming merit while enforcing blood) — low d. The analytical observer is neutral but risks naturalizing the constraint as mountain — d derived from canonical fallback (analytical = 0.73).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint is one reading of a contested kernel, not a single natural phenomenon with one true classification. The mandatrophy is resolved by recognizing that the citizen_privilege reading generates a tangled_rope classification (mixing coordination of the demos with extraction of excluded residents) that is structurally distinct from — and compatible with — the empire-based and slavery-based readings. All three readings can produce different constraint types while referring to the same historical phenomenon. The analytical observer's mountain (natural boundary) is a false summit that the citizen_privilege reading's structural data refutes through beneficiary and enforcement declarations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    blood_purity_enforcement_mechanism,
    'Was Pericles'' law enforced through genealogical verification, community attestation, or selective prosecution?',
    'Examination of court records (logographer speeches), citizenship challenges, and historical documentation of enforcement capacity and patterns',
    'If strictly enforced: suppression is higher, extraction more systematic. If sporadically enforced: theater_ratio increases, constraint approaches piton. Enforcement pattern determines whether the constraint is active coercion or performative gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blood_purity_enforcement_mechanism, empirical, 'Enforcement mechanism and stringency of genealogical verification').

omega_variable(
    female_citizenship_exclusion_structural_dependency,
    'Does the citizen_privilege_reading adequately account for the structural dependency on the exclusion of women from citizenship, or is female exclusion a prior, more fundamental constraint?',
    'Chronological analysis of when citizenship restrictions were enacted (female exclusion predates or coexists with Pericles'' law); structural analysis of whether the law targets mixed-descent to strengthen female exclusion or independently restricts dual descent.',
    'If female exclusion is prior and foundational: this reading may be a secondary effect of a deeper constraint (male-kinship-control). If Pericles'' law independently restricts dual descent: this reading stands as structurally autonomous. Affects the reading''s placement in the kernel network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_citizenship_exclusion_structural_dependency, conceptual, 'Relationship between citizenship restriction and female exclusion').

omega_variable(
    reading_naturalness_vs_contingency,
    'Is this reading treating the existence of citizenship boundaries as ''natural law'' (mountain) when in fact the specific exclusion mechanism is contingent historical choice?',
    'False summit detection: If beneficiaries exist and suppression is active, the mountain classification is a naturalization. Comparison with alternate membership systems (e.g., resident-based, property-based) shows that double-descent restriction is one choice among many, not an immutable constraint.',
    'Reclassification from mountain to tangled_rope in the analytical perspective. The constraint''s extractiveness is not inherent to ''having borders'' but to ''the specific closure and tightening mechanism chosen''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_naturalness_vs_contingency, conceptual, 'Whether mountain classification naturalizes contingent historical choice').

omega_variable(
    citizen_body_benefit_aggregation,
    'Do all existing citizens benefit equally from the restriction, or does the law amplify power for propertied families while imposing costs on non-propertied citizens?',
    'Distributional analysis: who controls jury slots, military command, and political offices post-Pericles? Does the restriction correlate with wealth concentration?',
    'If benefits are unequal: the beneficiary set should be narrowed to ''propertied families'', and non-propertied citizens should be reclassified as secondary victims. Extraction is masked within the beneficiary group''s internal power hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_body_benefit_aggregation, empirical, 'Distributional effects of citizenship restriction within citizen body').

omega_variable(
    slave_economy_counterfactual_tension,
    'Does this reading compete with or depend on the slave_economy_dependency reading? Is the citizen privilege constraint autonomous, or does it exist to protect the extraction of slave labor by concentrating citizen benefits?',
    'Timeline analysis: did restrictions on citizenship correlate with increased reliance on slave labor? Did slave-economy expansion require tighter citizen-body boundaries to allocate rents?',
    'If dependent: this reading is a derivative constraint. If autonomous: it is a structurally parallel reading of the same kernel. Affects network relationship (influences vs coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slave_economy_counterfactual_tension, conceptual, 'Relationship between citizen privilege and slave economy readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusionary_base__citizen_privilege_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(excl_priv_tr_t0, exclusionary_base__citizen_privilege_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(excl_priv_tr_t20, exclusionary_base__citizen_privilege_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(excl_priv_tr_t50, exclusionary_base__citizen_privilege_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(excl_priv_be_t0, exclusionary_base__citizen_privilege_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(excl_priv_be_t20, exclusionary_base__citizen_privilege_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(excl_priv_be_t50, exclusionary_base__citizen_privilege_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(excl_priv_su_t0, exclusionary_base__citizen_privilege_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(excl_priv_su_t20, exclusionary_base__citizen_privilege_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(excl_priv_su_t50, exclusionary_base__citizen_privilege_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusionary_base__citizen_privilege_reading, identity_coordination).
narrative_ontology:affects_constraint(exclusionary_base__citizen_privilege_reading, exclusionary_base__imperial_tribute_reading).
narrative_ontology:affects_constraint(exclusionary_base__citizen_privilege_reading, exclusionary_base__slave_economy_dependency_reading).

% DUAL FORMULATION NOTE:
% The 'exclusionary_base' kernel has three structurally distinct constraint stories corresponding to three readings: citizen_privilege (this story, focusing on membership restriction and benefit concentration), imperial_tribute (downstream of hegemonic fiscal extraction), and slave_economy (downstream of slave labor extraction). Each reading generates its own constraint with its own epsilon and perspectives. They coexist as competing causal explanations of the same historical institutional commitment. Link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
