% ============================================================================
% CONSTRAINT STORY: qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_sovereignty, []).

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
 *   constraint_id: qualified_sovereignty
 *   human_readable: Qualified State Sovereignty: Border Control as Constrained Authority
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The qualified sovereignty reading asserts that states retain legitimate
 *   authority to control borders but must exercise that authority
 *   proportionately to genuine security and welfare interests and
 *   consistently with binding human rights obligations (non-refoulement,
 *   non-discrimination, family unity, due process). This reading emerges from
 *   post-WWII international law (Universal Declaration, Refugee Convention)
 *   and becomes institutionalized through regional regimes (EU asylum
 *   directives, American Convention on Human Rights). It produces a
 *   structural hybrid: states retain sovereign discretion (coordination
 *   function) but that discretion is legally constrained (extraction
 *   mechanism). The constraint creates an adjudication burden: states must
 *   justify rejections as proportionate, and that justification is subject to
 *   international review. The extractiveness value (0.58) reflects this
 *   hybrid: states gain coordination benefits from border control (security,
 *   welfare system integrity, national identity maintenance) but lose
 *   absolute discretion, bearing instead the cost of demonstrating
 *   proportionality. The theater ratio (0.48) indicates moderate
 *   performativity: proportionality doctrine exists in law and is invoked in
 *   asylum decisions, but implementation gaps and adjudication capacity
 *   limits mean the constraint's bite varies widely across jurisdictions. The
 *   suppression value (0.65) reflects that excluded migrants face severe
 *   barriers (legal prohibition + enforcement + no meaningful appeal in many
 *   contexts) despite the qualification doctrine.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victim (powerless/trapped) — face absolute legal prohibition on entry with no meaningful qualification exception
 *   - Displaced Persons (IDPs): Secondary victim (moderate/constrained) — experience mixed extraction and coordination from state border apparatus
 *   - Nation-States: Primary beneficiary (institutional/arbitrage) — retain sovereign discretion while qualification creates coordination benefit (legitimacy, rule of law)
 *   - Wealthy Mobile Individuals: Secondary beneficiary (powerful/mobile) — benefit from coordination and have exit options (visa networks, capital mobility) despite extractive elements
 *   - International Human Rights Coalition: Organized observer (organized/constrained) — building institutional pathways to reduce state discretion (sunset logic)
 *   - Analytical Realist: Civilizational observer (analytical/analytical) — risks naturalizing state sovereignty as immutable when it is historically contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_sovereignty, 0.58).
domain_priors:suppression_score(qualified_sovereignty, 0.65).
domain_priors:theater_ratio(qualified_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(qualified_sovereignty, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(qualified_sovereignty, "Qualified State Sovereignty: Border Control as Constrained Authority").
narrative_ontology:topic_domain(qualified_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(qualified_sovereignty, formalized).
narrative_ontology:cs_authority_grounding(qualified_sovereignty, lineage).
narrative_ontology:cs_interpretation_layer_present(qualified_sovereignty).
narrative_ontology:cs_kernel_id(qualified_sovereignty, border_normative_status).
narrative_ontology:cs_reading_relation(qualified_sovereignty, sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation(qualified_sovereignty, freedom_primary, influences).
narrative_ontology:cs_axiom(qualified_sovereignty, foundational, state_discretion_bounded_by_proportionality).
narrative_ontology:cs_axiom_status(state_discretion_bounded_by_proportionality, holdable).
narrative_ontology:cs_axiom_grounding(qualified_sovereignty, state_discretion_bounded_by_proportionality, deontological).
narrative_ontology:cs_axiom(qualified_sovereignty, foundational, human_rights_override_sovereign_discretion).
narrative_ontology:cs_axiom_status(human_rights_override_sovereign_discretion, holdable).
narrative_ontology:cs_axiom_grounding(qualified_sovereignty, human_rights_override_sovereign_discretion, deontological).
narrative_ontology:cs_reference_frame(qualified_sovereignty, international_human_rights_authority).
narrative_ontology:cs_drift_state(qualified_sovereignty, contemporary_migration_crisis, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_sovereignty, nation_states_institutional_capacity).
narrative_ontology:constraint_beneficiary(qualified_sovereignty, security_apparatus).
narrative_ontology:constraint_victim(qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(qualified_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(qualified_sovereignty, stateless_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute barrier to entry regardless of material circumstance, family separation, or persecution risk. Suppression is maximal: the constraint operates through legal prohibition enforced by military/police force. No exit alternative exists; the trapped agent cannot arbitrage or relocate within the constraint system. Maximum experienced extractiveness: the state extracts mobility, opportunity, and dignity while bearing no reciprocal obligation under this reading.
constraint_indexing:constraint_classification(qualified_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED PERSON / IDP (TANGLED ROPE) — Bears costs of border control (geographic confinement, resource scarcity) but also benefits from the same state apparatus that controls borders — access to courts, welfare systems, schools. The constraint both extracts (confines movement, controls resource distribution) and coordinates (enables public goods). Exit is constrained by economic dependency and legal status, not trapped. Moderate experienced extraction reflects mixed benefit and cost.
constraint_indexing:constraint_classification(qualified_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATION-STATE (ROPE) — Experiences border control as pure coordination: organizing population, collecting taxes, providing security and welfare, maintaining territorial integrity. The constraint solves the collective action problem of establishing who belongs to the political community and who has claims on state resources. No external prohibition prevents the state from controlling borders; arbitrage exists globally across sovereign jurisdictions with varying enforcement. The state sees the constraint as legitimate coordination, not extraction.
constraint_indexing:constraint_classification(qualified_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY MOBILE INDIVIDUAL (TANGLED ROPE) — Has significant agency within border control systems. Capital mobility, visa networks, citizenship options, educational credentials create mobile exit options. Yet experiences extraction in the form of visa requirements, border processing delays, differential treatment based on nationality. Benefits from some coordination functions (stable institutions, rule of law in receiving states). The extraction is real but mitigated by exit options; coordination benefits are genuine.
constraint_indexing:constraint_classification(qualified_sovereignty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups, NGOs, and states committed to human rights see qualified sovereignty as a temporary constraint with a sunset clause. The coordination problem is how to harmonize state security interests with human dignity and non-discrimination. The rise of international human rights law, asylum treaties, and proportionality doctrine represents the sunset pathway — gradually constraining state discretion through reciprocal obligation. Low effective extraction because the coalition sees (and is building) an exit path.
constraint_indexing:constraint_classification(qualified_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REALIST THEORIST / NATURAL LAW VIEW (MOUNTAIN) — From a structural realism view, state border control is an immutable feature of the international system: without sovereign territorial control and population management, the state-system itself dissolves. The analytical observer may see qualified sovereignty as a constraint imposed on this natural law, or may see border control authority as entirely natural and qualification as artificial. This perspective risks naturalizing a historically contingent institutional arrangement (the Westphalian state system) as a law of nature.
constraint_indexing:constraint_classification(qualified_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates asymmetry in adjudication burden: states can reject asylum claims if they invoke 'proportionality,' and most states' rejections are not reviewed by independent international courts. The power to define proportionality is an extractive power. However, the constraint is not pure extraction because legitimate state interests (security, welfare system sustainability) genuinely require some coordination mechanism. The 0.58 value reflects that states extract significant discretion while bearing non-trivial compliance costs (adjudication, documentation, justification). Suppression (0.65): High. Excluded migrants face legal prohibition backed by force (police, military), with limited appeal mechanisms and high information/resource barriers to contesting rejection. Yet suppression is not total (0.70+) because some appeals succeed, some states honor asylum obligations, and international norms create pressure. The trajectory shows rising suppression over time: as migration pressure increases and security concerns rise, states tighten borders and reduce proportionality accommodation. Theater ratio (0.48): Moderate. Proportionality doctrine exists and is genuinely invoked in decisions, but significant implementation gap exists — many rejections proceed without rigorous proportionality analysis, and state security determinations are rarely independently reviewed. Rising over time: as international law hardens, states engage in more elaborate justification theater to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental perspectival divergence between agents. The excluded migrant sees a Snare: pure extraction with no coordination benefit and no exit option. The nation-state sees Rope: pure coordination with no extraction, solving the legitimate problem of establishing a political community. The human rights coalition sees a Scaffold: a temporary constraint with a sunset clause as international law gradually restricts state discretion. The analytical observer may see either a Tangled Rope (the reading this story instantiates) or a Mountain (if naturalizing sovereignty), with the false-summit risk being that sovereignty is treated as immutable when it is actually a contingent institutional arrangement. The perspectival gap reflects genuine disagreement not about facts but about what counts as 'legitimate state interest' and what counts as 'proportionate constraint.' This disagreement is encoded in the kernel contestation (the border_normative_status kernel has multiple readings, and this reading's legitimacy is itself contested).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value (d) for each perspective is derived from their structural position relative to the constraint. Excluded migrants are structurally victims (d~0.95, full target) with trapped exit and powerless position: f(d)~1.42, maximum experienced extractiveness. Nation-states are structurally beneficiaries (d~0.15, near-full beneficiary) with arbitrage exit and institutional power: f(d)~-0.01, negative experienced extractiveness (they benefit from the constraint). Wealthy mobile individuals are hybrid (d~0.50, symmetric) with mobile exit: f(d)~0.65, moderate experienced extractiveness despite constraints. The coalition is organized with constrained exit and victim-proxy status: d~0.55, f(d)~0.75. The scope modifier σ(S) increases effective extraction at global scope (σ=1.2) because border control's verification burden scales globally — states must maintain borders across multiple jurisdictions, increasing complexity and asymmetry. The multiplication χ = ε × f(d) × σ(S) produces the observed classification gap: powerless agents experience higher χ (Snare), institutional beneficiaries experience negative χ (Rope), analytical observer experiences moderate χ (Tangled Rope or Mountain depending on framing).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for qualified sovereignty is resolved by recognizing that the constraint is genuinely a Tangled Rope: it has both a real coordination function (establishing political community, securing public goods) AND asymmetric extraction (states exercise discretion that excludes vulnerable populations). The temptation to collapse into pure Rope (denying extraction) or pure Snare (denying coordination) both represent boundary-crossing errors. The Tangled Rope classification holds because: (1) beneficiaries exist (states gain sovereignty and legitimacy), (2) victims exist (excluded migrants, displaced persons), (3) active enforcement is required (legal prohibition + police/military + adjudication systems), and (4) the constraint solves a genuine collective action problem (political community definition, resource distribution) while imposing asymmetric costs. The theater ratio (0.48) confirms moderate (not high) performativity — proportionality doctrine is not pure window-dressing; it creates real constraints on state discretion in some contexts. The extractiveness trajectory (rising from 0.42 to 0.58) reflects that as migration pressure increases, states tighten borders and reduce proportionality accommodation, shifting the constraint toward higher-extraction territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What constitutes ''proportionate'' exercise of border control when legitimate state interests (security, welfare state sustainability, cultural integration) genuinely conflict with human rights obligations (non-refoulement, family unity, non-discrimination)?',
    'Case law analysis across international courts (ECJ, ECtHR, ICJ): extract proportionality tests applied to border decisions. Identify divergence in how different jurisdictions weight competing interests. Empirical measurement: ratio of rejected asylum claims with humanitarian grounds to total rejections, correlated with security incident data.',
    'If ''proportionality'' favors state interest: qualification becomes rhetorical (the state justifies any rejection as proportionate). Extractiveness rises to 0.75+, classification shifts toward Snare. If ''proportionality'' favors human rights: states face genuine constraint on discretion. Extractiveness remains ~0.58, Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Interpretation of ''proportionate'' when state interests conflict with human rights obligations').

omega_variable(
    adjudication_burden_allocation,
    'Who bears the burden and cost of adjudicating qualification claims? If states judge their own proportionality, is qualification legally binding?',
    'Institutional analysis: examine which actors (states, international courts, hybrid tribunals) have authority to review border decisions. Track what fraction of rejections are actually reviewed and reversed. Compare extractiveness under domestic-only review vs. international review regimes.',
    'If domestic review only: qualification is aspirational theater (Piton). If robust international review: qualification imposes real constraint on state discretion (Tangled Rope confirmed). If mixed: classification depends on agent''s access to review mechanisms (wealthy/organized agents see different types than powerless).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudication_burden_allocation, empirical, 'Who has adjudication authority over proportionality claims, and is it binding').

omega_variable(
    reading_contestation_kernel_status,
    'Is the ''border normative status'' kernel genuinely stabilized, or is the reading contestation itself the constraint?',
    'Historical and institutional analysis: track legal instruments (UN Convention on Migrants Rights, 1990; Regional asylum directives; ILO standards). Document state acceptance rates, reservation patterns, enforcement gaps. If >30 states formally reject or reserve qualification axioms, the kernel is not stabilized — the reading is minority position.',
    'If kernel is stabilized: this reading (qualified_sovereignty) has legitimate authority. If kernel remains contested: the constraint is actually the reading competition itself (a different, higher-level constraint: ''border_normative_contestation''). If reading is minority: qualified_sovereignty describes aspirational law, not implemented constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_kernel_status, empirical, 'Whether the ''border normative status'' kernel is stabilized or remains contested').

omega_variable(
    state_capacity_enforcement_gap,
    'What fraction of rejected asylum claims that meet international human rights standards are rejected anyway due to state administrative capacity limits, not deliberate violation?',
    'Audit of asylum decision-making institutions: track error rates in security vetting, case processing delays, resource allocation to adjudicators. Compare states with high institutional capacity (Canada, Germany, Sweden) to low-capacity states. Empirical measure: ratio of reversed decisions (on appeal) due to procedure/capacity vs. substantive law.',
    'If gap is large (>40% of rejections due to capacity, not law): qualification constraint is aspirational (Scaffold). If gap is small (<15%): constraint is genuinely binding (Tangled Rope). Gaps reveal where suppression operates structurally vs. institutionally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_enforcement_gap, empirical, 'Gap between qualified sovereignty standards and state administrative capacity to implement them').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_sovereignty, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qsov_tr_t0, qualified_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qsov_tr_t8, qualified_sovereignty, theater_ratio, 8, 0.48).
narrative_ontology:measurement(qsov_tr_t16, qualified_sovereignty, theater_ratio, 16, 0.52).

% Extraction over time
narrative_ontology:measurement(qsov_be_t0, qualified_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(qsov_be_t8, qualified_sovereignty, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(qsov_be_t16, qualified_sovereignty, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(qualified_sovereignty, freedom_primary).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel generates three constraint stories with different ε values representing different readings' empirical implementations. qualified_sovereignty (ε=0.58, Tangled Rope) represents the institutionalized international law position. sovereignty_primary (ε≈0.42, Rope or Piton) represents absolute state discretion with minimal qualification. freedom_primary (ε≈0.65, Snare) represents the maximalist anti-extraction reading where any border control is illegitimate extraction. Each reading has its own structural data and perspectives; they are linked via network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_sovereignty, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
