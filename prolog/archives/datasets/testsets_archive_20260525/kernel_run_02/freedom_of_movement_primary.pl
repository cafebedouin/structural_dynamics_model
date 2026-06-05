% ============================================================================
% CONSTRAINT STORY: freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_of_movement_primary, []).

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
 *   constraint_id: freedom_of_movement_primary
 *   human_readable: Freedom of Movement as Fundamental Right vs. Border Control Authority
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'border_control_legitimacy': the reading that posits freedom of movement
 *   as a fundamental human right that supersedes (or limits) state
 *   territorial sovereignty. This reading enters direct logical conflict with
 *   the 'sovereignty_primary' reading, which holds that state sovereignty
 *   entails unqualified authority to exclude. The
 *   'jurisdictional_sovereignty' reading occupies a middle position: states
 *   control who receives rights and benefits once physically present, but the
 *   right to be present is not unlimited. The freedom_of_movement_primary
 *   reading makes displaced persons, labor migrants, and refuge seekers into
 *   victims of state exclusion authority. It reframes the state enforcement
 *   apparatus itself as a victim — forced to choose between upholding border
 *   closure authority (sovereignty doctrine) and honoring the freedom of
 *   movement right. This creates the tangled rope structure: genuine
 *   coordination functions (managing cross-border labor and resource flows)
 *   coexist with asymmetric extraction (states and wealthy citizens extract
 *   protection from border closure while displaced persons bear the cost).
 *   The constraint exhibits the full range of classification across
 *   perspectives precisely because the underlying kernel reading is
 *   contested: different institutional actors (states, humanitarian
 *   organizations, migrants themselves, wealthy citizens) operate from
 *   different readings of what legitimacy requires.
 *
 * KEY AGENTS:
 *   - Displaced Persons: Primary victims (powerless/trapped) — face maximum extraction through border closure denying exit from violence, persecution, economic collapse. No exit option; unable to organize collectively.
 *   - Labor Migrants: Secondary victims (moderate/constrained) — benefit from cross-border mobility but face asymmetric extraction through wage depression, status hierarchies, and exploitative work conditions enabled by irregular migration status.
 *   - Sovereign State Apparatus: Primary beneficiary (institutional/arbitrage) — extracts legitimacy, control, and resource management through border enforcement. Can adjust policy and negotiate treaties. Perceives border control as legitimate governance.
 *   - Resident Labor Markets: Mixed actor (moderate/constrained) — experiences both coordination benefit (labor supply management) and extraction (wage pressure, job competition). Exit options constrained.
 *   - Wealthy Mobile Citizens: Incidental beneficiary (powerful/mobile) — freedom of movement largely works for them; border closure is theatrical rather than binding. Visa regimes, passport reciprocity, digital nomad status.
 *   - International Humanitarian Organizations: Organized mediator (organized/constrained) — see both coordination need and extraction mechanism. Can advocate and litigate but cannot supersede state sovereignty. Constrained exit.
 *   - Analytical Observer: Witness (analytical/analytical) — risks naturalizing contingent sovereignty doctrine as immutable natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_of_movement_primary, 0.62).
domain_priors:suppression_score(freedom_of_movement_primary, 0.68).
domain_priors:theater_ratio(freedom_of_movement_primary, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_of_movement_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(freedom_of_movement_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(freedom_of_movement_primary, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_of_movement_primary, tangled_rope).
narrative_ontology:human_readable(freedom_of_movement_primary, "Freedom of Movement as Fundamental Right vs. Border Control Authority").
narrative_ontology:topic_domain(freedom_of_movement_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(freedom_of_movement_primary, formalized).
narrative_ontology:cs_authority_grounding(freedom_of_movement_primary, lineage).
narrative_ontology:cs_interpretation_layer_present(freedom_of_movement_primary).
narrative_ontology:cs_kernel_id(freedom_of_movement_primary, border_control_legitimacy).
narrative_ontology:cs_reading_relation(freedom_of_movement_primary, sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation(freedom_of_movement_primary, jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom(freedom_of_movement_primary, foundational, freedom_of_movement_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding(freedom_of_movement_primary, freedom_of_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom(freedom_of_movement_primary, foundational, sovereignty_not_unlimited_exclusion_authority).
narrative_ontology:cs_axiom_status(sovereignty_not_unlimited_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding(freedom_of_movement_primary, sovereignty_not_unlimited_exclusion_authority, deontological).
narrative_ontology:cs_reference_frame(freedom_of_movement_primary, universal_human_rights_framework).
narrative_ontology:cs_drift_state(freedom_of_movement_primary, contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, labor_mobility_seekers).
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, refuge_seekers).
narrative_ontology:constraint_victim(freedom_of_movement_primary, state_enforcement_apparatus).
narrative_ontology:constraint_victim(freedom_of_movement_primary, resident_labor_markets).
narrative_ontology:constraint_victim(freedom_of_movement_primary, sovereignty_doctrine_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PERSON (SNARE) — Faces maximum extraction from the constraint. Border closure denies exit from oppression, economic collapse, or violence. No exit option exists; the person cannot reorganize or negotiate. The right is declared but the enforcement mechanism (sovereignty doctrine) nullifies it. Pure extraction — the person bears the full cost of border closure while beneficiaries (sovereign states) extract legitimacy from it.
constraint_indexing:constraint_classification(freedom_of_movement_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR MARKET / RESIDENT WORKERS (TANGLED ROPE) — Experiences mixed coordination and extraction. Border controls coordinate labor supply and wage pressure (genuine coordination function); they also enable extraction of surplus from migrant workers through wage depression and status asymmetry. Residents face constrained exit — organizing against migrant labor is possible but costly. Both coordination benefit and extractive cost coexist structurally.
constraint_indexing:constraint_classification(freedom_of_movement_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVEREIGN STATE APPARATUS (ROPE) — Sees border closure as pure coordination: managing population, labor supply, resource distribution, and security. The state has arbitrage options (can enforce borders, revise policy, negotiate treaties). From this perspective, the constraint is functional coordination with minimal extraction. The state extracts legitimacy and control from border enforcement but perceives this as legitimate governance, not rent-seeking.
constraint_indexing:constraint_classification(freedom_of_movement_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY MOBILE CITIZENS (PITON) — The freedom of movement right functions at the wealthy level: passports, visa waivers, digital nomad visas enable near-frictionless movement across borders. For high-income individuals, border closure is largely theatrical — it affects poor and displaced persons, not them. The constraint's enforcement is theatrical for the powerful (visa reciprocity networks maintain the performance) while brutal for the powerless. Theater ratio high from this perspective.
constraint_indexing:constraint_classification(freedom_of_movement_primary, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN / LABOR RIGHTS ORGANIZATIONS (TANGLED ROPE) — See the constraint as simultaneously a coordination problem (states must cooperate on migration governance) and an extractive mechanism (border closure enables abuse of migrants). Organizations have constrained exit — they can advocate, litigate, build alternative pathways, but cannot dissolve state sovereignty. Both functions exist: genuine need for cross-border coordination AND asymmetric extraction from migrants.
constraint_indexing:constraint_classification(freedom_of_movement_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY AS NATURAL LAW (MOUNTAIN) — From this perspective, the border closure authority is an immutable feature of the Westphalian state system. Territorial integrity and population control are presented as irreducible properties of political organization. However, this perspective naturalizes what is actually a contested institutional arrangement — the engine's false summit detector will flag this mountain as constructed, not natural.
constraint_indexing:constraint_classification(freedom_of_movement_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_of_movement_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_of_movement_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_of_movement_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(freedom_of_movement_primary, TR),
    TR >= 0.70.

:- end_tests(freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts significantly from displaced persons and migrants (restricted exit, asymmetric labor terms, vulnerability to exploitation). But extractiveness is not maximal (0.85+) because: (1) the coordination function is genuine — cross-border movement does require coordination; (2) some states have opened borders partially (Schengen, labor mobility agreements), showing the constraint is not immutable; (3) humanitarian advocacy has created alternative pathways (refugee resettlement, asylum law). The measure reflects sustained but not total extraction. Suppression (0.68): High. Barriers to movement are substantial: visa requirements, border enforcement, documentation burdens, cost of crossing, legal liability for irregular migration, human-trafficking networks extracting rent from desperate persons. Suppression has increased over the interval as securitization rhetoric and enforcement budgets expanded. Theater ratio (0.58): Moderate-high. Significant performative content: visa reciprocity networks maintain an appearance of controlled access that is selective by wealth and nationality. Documentation theater (passports, visas, travel permits) performs state control while irregular crossing networks demonstrate that actual barriers are penetrable. The performance maintains legitimacy for border closure while the reality is that borders are selectively enforced based on power asymmetries.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence. The displaced person sees snare (pure extraction, no exit, maximum cost). The sovereign state sees rope (coordination function, legitimate governance). The humanitarian organization sees tangled rope (both coordination and extraction). Wealthy citizens see piton (the constraint is largely theatrical for them, maintained through inertia). The analytical observer risks mountain (naturalizing sovereignty as inherent to political organization). The resident labor market sees tangled rope (benefits from wage coordination, costs from wage depression). The gap reveals that the same institutional arrangement (border closure) is perceived as immutable natural law by some, as coordination mechanism by others, as pure extraction by the powerless, and as theater by the privileged. This perspectival divergence is not measurement error — it reflects genuine structural differences in how the constraint operates across power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural relationship to the extraction flow. Displaced persons have d ≈ 0.98 (full target, trapped exit) → high f(d) → high experienced extraction. Sovereign states have d ≈ 0.05 (beneficiary, arbitrage exit) → negative f(d) → negative experienced extraction (they experience the constraint as enabling their power). Organized humanitarian networks have d ≈ 0.60 (mixed, constrained exit) → f(d) ≈ 0.80 → moderate extraction. Wealthy citizens have d ≈ 0.10 (incidental beneficiary, mobile exit) → low f(d) → the constraint barely constrains them. The engine derives these d values from the beneficiary/victim declarations and exit options. No overrides needed — the structural derivation captures the real asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: The mandatrophy is resolved by recognizing that this constraint is one reading of a contested kernel, not a universally true classification. The snare classification (from the displaced person perspective) is legitimate within this reading's framework. The rope classification (from the state perspective) is legitimate within the sovereignty_primary reading's framework. The mountain classification (from the civilizational analytical perspective) naturalizes what is actually a reading-contingent institutional arrangement — the engine's false summit detector will flag this as constructed, not natural. The constraint's analytical utility is to document which reading is being instantiated, what institutional actors are endorsing it, and what the structural consequences are for different power positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_right_enforceability,
    'Is freedom of movement a fundamental right in the absence of an enforcement mechanism that supersedes state sovereignty?',
    'Analysis of which institutional actors recognize the right as binding (UN conventions, regional courts, national constitutions) and which mechanisms enforce it against state resistance. Track compliance rates and penalty structures.',
    'If enforceability is limited to voluntary state compliance: the right is performative and the constraint remains snare/tangled-rope. If supranational enforcement mechanisms mature: classification shifts toward rope/scaffold as coordination mechanisms strengthen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_right_enforceability, conceptual, 'Enforceability of freedom of movement against state sovereignty claims').

omega_variable(
    economic_extraction_mechanisms,
    'How much of border control''s suppression function is structural (material barriers to crossing) versus performative (visa regimes, documentation theater)?',
    'Comparative analysis of crossing costs: legal visa procedures vs. irregular border crossing vs. bribery costs vs. human trafficking networks. Measure actual barrier magnitude vs. institutional claim.',
    'If primarily structural: suppression is irreducible and snare classification is robust. If primarily performative: theater_ratio should increase and classification shifts toward piton or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_extraction_mechanisms, empirical, 'Structural vs. performative nature of border control barriers').

omega_variable(
    kernel_reading_contest,
    'Which reading of the border control legitimacy kernel is institutionally dominant: freedom of movement, sovereignty primacy, or jurisdictional sovereignty?',
    'Track institutional recognition: constitutions, international law, regional court decisions, state practice. Measure prevalence of each reading across nation-states and over time.',
    'Dominant reading determines which constraint story becomes the baseline. Other readings become sibling perspectives. Current institutional dominance is sovereignty primacy; freedom of movement is aspirational/emergent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Institutional dominance of border control legitimacy readings').

omega_variable(
    open_borders_coordination_feasibility,
    'Could open-borders regimes coordinate labor, resources, and security without border closure enforcement?',
    'Empirical study of open-labor-mobility zones (EU Schengen pre-2015, Gulf Cooperation Council labor mobility). Analyze whether coordination failed due to open borders or due to other factors.',
    'If feasible: freedom of movement perspective is robust rope/scaffold (coordination without closure). If infeasible: closure is necessary coordination mechanism and snare classification indicates displacement of cost onto the powerless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_borders_coordination_feasibility, empirical, 'Feasibility of coordination without border closure').

omega_variable(
    reading_axiom_empirical_ground,
    'Is the foundational axiom of freedom of movement (persons have inherent right to relocate) empirically grounded or deontologically grounded?',
    'Philosophical analysis of the axiom''s grounding within human-rights traditions. Empirically contingent axioms (grounded in consequences of movement restrictions) are subject to empirical override if evidence shows restrictions improve outcomes. Deontological axioms are not.',
    'If empirically contingent: axiom becomes overridden if evidence shows restriction benefits (axiom_overriding drift in cs_structure). If deontological: axiom remains holdable regardless of empirical consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_axiom_empirical_ground, conceptual, 'Grounding type of the freedom-of-movement axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_of_movement_primary, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(free_tr_t0, freedom_of_movement_primary, theater_ratio, 0, 0.42).
narrative_ontology:measurement(free_tr_t2, freedom_of_movement_primary, theater_ratio, 2, 0.5).
narrative_ontology:measurement(free_tr_t4, freedom_of_movement_primary, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(free_be_t0, freedom_of_movement_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(free_be_t2, freedom_of_movement_primary, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(free_be_t4, freedom_of_movement_primary, base_extractiveness, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(freedom_of_movement_primary, labor_market_wage_regulation).
narrative_ontology:affects_constraint(freedom_of_movement_primary, international_refugee_law_enforcement).
narrative_ontology:affects_constraint(freedom_of_movement_primary, visa_reciprocity_networks).

% DUAL FORMULATION NOTE:
% Freedom_of_movement_primary is one reading of border_control_legitimacy kernel. Sibling constraints instantiating other readings (sovereignty_primary, jurisdictional_sovereignty) model the same institutional phenomenon from different normative commitments. All three stories decompose the same domain phenomenon (state border control) into its structurally distinct readings. Network linkage: freedom_of_movement_primary affects labor_market_wage_regulation (open borders would change wage dynamics) and international_refugee_law_enforcement (refugee law attempts to operationalize the freedom of movement right). It is influenced by visa_reciprocity_networks (the actual mechanism enabling selective freedom of movement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
