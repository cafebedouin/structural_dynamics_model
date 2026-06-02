% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation: Commons Conservation Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) prohibits national
 *   appropriation of celestial bodies and their resources, declaring them the
 *   'province of all mankind.' The commons-conservation reading interprets
 *   this language as creating a binding constraint on both state and private
 *   actors: resource extraction is prohibited without multilateral
 *   authorization that ensures benefit distribution to non-spacefaring states
 *   and preservation of the epistemic commons. This reading contrasts with
 *   two sibling interpretations: the extraction-permissive reading, which
 *   treats Article II as prohibiting sovereignty claims but permitting
 *   resource use under national licensing; and the international-regime
 *   reading, which treats Article II as an incomplete framework requiring new
 *   formal regimes (like the Law of the Sea) before appropriation rules are
 *   binding. The commons-conservation reading creates a structural wall:
 *   first-mover extraction investments are stranded until multilateral
 *   negotiation produces authorization frameworks; non-spacefaring states
 *   retain veto power over enclosure; benefit distribution is built into the
 *   constraint rather than aspirational. This reading produces genuine
 *   tangled_rope structure: coordination function (how to allocate resources
 *   fairly), asymmetric extraction (spacefaring states have technological
 *   advantage), and active enforcement (treaty bodies must evaluate each
 *   extraction proposal). Suppression is moderate-high because the constraint
 *   relies on organizational capacity (COPUOS successor, treaty amendment
 *   process) that may fail to materialize, leaving the principle formally
 *   binding but functionally unenforced.
 *
 * KEY AGENTS:
 *   - Non-Spacefaring States: Primary victims (powerless/trapped) — excluded from resource domain by technological asymmetry; benefit from veto power under commons-conservation reading but face suppression if extraction-permissive reading prevails
 *   - First-Mover Mining Investors: Primary beneficiaries of extraction-permissive reading; constrained beneficiaries under commons-conservation (must negotiate authorization, cannot extract unilaterally)
 *   - Spacefaring Technological States: Dual role — can access resources but must participate in multilateral authorization process; asymmetric advantage over non-spacefaring states but constraint to capital-intensive negotiation vs. unilateral appropriation
 *   - International Negotiating Coalition (COPUOS, UN General Assembly): Institutional actor required to enforce commons-conservation — multilateral authorization body that resolves benefit distribution
 *   - Collective Epistemic Commons: Abstract beneficiary (scientific knowledge, open data, uncontaminated geology/planetology) — protected by non-appropriation principle which prevents proprietary enclosure of research domains
 *   - Analytical Observer: Views commons-conservation as natural law or as contingent institutional arrangement depending on the kernel reading adopted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.58).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.68).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation: Commons Conservation Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '2ba57c96-3d46-481a-955f-a5b9c7d0d9f8').
narrative_ontology:cs_kernel_codification('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', fixed_text).
narrative_ontology:cs_authority_grounding('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', lineage).
narrative_ontology:cs_interpretation_layer_present('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8').
narrative_ontology:cs_reading_relation('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', foundational, appropriation_requires_multilateral_authorization).
narrative_ontology:cs_axiom_status(appropriation_requires_multilateral_authorization, holdable).
narrative_ontology:cs_axiom_grounding('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', appropriation_requires_multilateral_authorization, deontological).
narrative_ontology:cs_axiom('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', foundational, benefit_distribution_to_non_spacefaring_states_is_mandatory).
narrative_ontology:cs_axiom_status(benefit_distribution_to_non_spacefaring_states_is_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', benefit_distribution_to_non_spacefaring_states_is_mandatory, deontological).
narrative_ontology:cs_reference_frame('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', common_heritage_non_appropriation).
narrative_ontology:cs_drift_state('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', contemporary_mining_capability_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2ba57c96-3d46-481a-955f-a5b9c7d0d9f8', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, collective_epistemic_commons).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_technological_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SPACEFARING STATES (SNARE) — Trapped by technological asymmetry and capital barriers. Without multilateral authorization requirement, first-movers extract lunar/asteroid resources while non-spacefaring states have no veto, no participation in benefit distribution, no exit. Maximum suppression: cannot even access the resource domain to negotiate.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPACEFARING STATES WITHOUT DOMINANT CAPITAL (SNARE) — Can technically participate but face severe capital and technological barriers. Constrained by the cost of space infrastructure and racing dynamics: first-mover advantage incentivizes rapid extraction before norms crystallize. Extraction runs toward dominant spacefaring powers.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FIRST-MOVER MINING INVESTORS (TANGLED ROPE) — Under commons-conservation reading, this agent is partially trapped: multilateral authorization requirement prevents unilateral extraction, but genuine coordination function exists (investors participate in negotiated benefit-sharing frameworks). Constrained because extraction requires negotiated authorization rather than technological capability alone. This agent benefits from resource access but must negotiate distribution with non-spacefaring states.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL NEGOTIATING COALITION (ROPE) — Under commons-conservation reading, multilateral authorization requirement creates genuine coordination function. Solving the problem: which extraction activities serve collective benefit? How should benefits distribute? Organized parties (UN COPUOS successor, treaty amendment coalition) see the constraint as enabling coordination rather than suppression. Mobile exit options because parties can exit negotiation if terms become intolerable.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLLECTIVE EPISTEMIC COMMONS (ROPE) — Non-appropriation principle protects the intellectual commons: lunar/asteroid geology, formation physics, comparative planetology must remain open access and scientifically uncontaminated by proprietary extraction incentives. This perspective sees the constraint as coordination for knowledge preservation. Arbitrage exit because the scientific community can establish norms (open data, publication rights) independently of legal regimes.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — Civilizational view might see non-appropriation as an immutable principle analogous to natural law: common heritage of humankind is a foundational commitment that cannot be violated without dissolving the entire treaty framework. However, the structural data reveals this as a false summit — the non-appropriation principle is a negotiated institutional arrangement, not a law of nature. Different readings (extraction-permissive, international-regime) would reclassify this as contingent.
constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ost_article_ii_non_appropriation__commons_conservation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the 30-year interval. Initial value (0.35) reflects that when Article II was signed, space mining was theoretical — minimal extraction was occurring. As technology matured (time=15), extractiveness increased (0.45) because first-movers could plausibly extract if the constraint were removed, creating opportunity cost. Current value (0.58) reflects that lunar ice, asteroid metals, and rare earth resources now have commercial value; the constraint's power depends entirely on enforcement. The increase models technological capability creating extraction pressure, making the constraint's role more consequential. Under extraction-permissive reading, extractiveness would stabilize at 0.35-0.45 (no constraint, routine commercial activity). Under commons-conservation, extractiveness remains high because the constraint prevents what would otherwise occur. Suppression (0.68): Moderate-high and increasing. The constraint requires multilateral authorization machinery (COPUOS reform, treaty amendment processes) that does not yet exist. Suppression increases over time as technological capability expands but authorization frameworks do not, creating a gap between the constraint's legal requirement and its enforcement capacity. Theater ratio (0.45): Low and declining. Commons-conservation reading emphasizes substantive coordination (benefit distribution negotiation) over performative compliance. As the reading solidifies, theater decreases because the focus shifts from rhetorical commitment to actually building authorization mechanisms. Under extraction-permissive reading, theater would be higher (public commitment to non-appropriation while permitting extraction under national licensing produces performative compliance).
 *
 * PERSPECTIVAL GAP:
 *   The commons-conservation reading produces maximal perspectival divergence. Non-spacefaring states (powerless/trapped) experience snare: the constraint protects them from being locked out of resource benefits, but they lack the capacity to enforce it or participate in extraction. Spacefaring states without dominant capital (moderate/constrained) also experience snare: they want to mine but face both technological barriers and negotiation requirements. First-mover investors (powerful/constrained) experience tangled_rope: the constraint denies them unilateral appropriation but offers genuine coordination benefit (negotiated access with guaranteed benefit-sharing). The international negotiating coalition (organized/mobile) experiences rope: solving the coordination problem is their primary function. The epistemic commons (institutional/arbitrage) experiences rope: non-appropriation preserves open access to scientific knowledge. The analytical observer (analytical/analytical) risks misclassifying this as mountain—a natural law of space commons—when it is actually a contested institutional arrangement. The perspectival gap reveals that the commons-conservation reading is not a neutral interpretation but a choice that redistributes power toward non-spacefaring states and away from first-mover technology leaders.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position relative to the resource extraction flow. Non-spacefaring states are full victims of extraction-without-authorization (d ≈ 0.95 under extraction-permissive reading; d ≈ 0.40 under commons-conservation, because the constraint protects them). First-mover investors are full beneficiaries of extraction-permissive (d ≈ 0.05); moderate beneficiaries of commons-conservation (d ≈ 0.35, because they must negotiate but still access resources). The international coalition is a neutral coordinator (d ≈ 0.50). The epistemic commons is a beneficiary of non-appropriation (d ≈ 0.10, protected from proprietary enclosure). The analytical observer has observer directionality (d ≈ 0.72). Under commons-conservation reading, the sigmoid f(d) produces moderate-high chi because the constraint genuinely changes the cost-benefit structure for first-movers—they cannot extract without authorization, raising their cost. Under extraction-permissive reading, chi would be lower because no cost is imposed on extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The commons-conservation reading resolves mandatrophy by clarifying that Article II creates genuine coordination (multilateral benefit distribution) alongside asymmetric extraction (spacefaring states retain technological advantage even under authorization). This is the defining signature of tangled_rope: both elements must be present. Extractiveness (0.58) exceeds pure coordination thresholds (ε ≤ 0.45 for rope) because spacefaring states gain from first-mover advantage and resource access. Suppression (0.68) exceeds coordination floor because the constraint requires enforcement machinery (authorization bodies) that may not materialize. But beneficiary/victim structure is clear: non-spacefaring states benefit from the veto; spacefaring states are partially constrained (but benefit from negotiated access). The constraint is not pure snare because coordination function is genuine—benefit distribution is not theater but a real negotiation process. It is not pure rope because the technological asymmetry produces asymmetric extraction even under authorization. Tangled_rope correctly captures the hybrid: 'both genuine coordination and measurable asymmetry.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appropriation_definition_boundary,
    'Does ''use or occupation'' in Article II create a workable boundary between permitted scientific research and prohibited resource appropriation, or is the boundary inherently contested and observable-dependent?',
    'Analysis of lunar exploration missions: distinguish scientific sampling (permitted) from commercial extraction (prohibited). Test cases: Chang''e-5 sample return, Artemis ice mining feasibility studies, Axiom Station robotic arms. Can treaty bodies consistently classify activities on one side or the other?',
    'If boundary is stable: non-appropriation principle is enforceable (supports tangled_rope classification). If boundary dissolves under pressure: ''use or occupation'' becomes unmonitored, extraction becomes hidden behind research framing (constraint degrades to piton or collapses to snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_definition_boundary, empirical, 'Enforceability of use vs. appropriation boundary in space resource extraction').

omega_variable(
    multilateral_authorization_feasibility,
    'Is a functioning multilateral authorization regime structurally achievable given veto dynamics, economic incentives for free-riding, and the absence of enforcement machinery in existing space law?',
    'Comparative institutional analysis: examine Antarctic Treaty enforcement, Law of the Sea nodule mining framework, and COPUOS decision-making patterns. Model veto-proof majority and unanimity requirements under different geopolitical configurations.',
    'If regime is achievable: commons-conservation reading produces genuine tangled_rope (coordination + asymmetric extraction resolved by negotiation). If regime is infeasible: constraint reduces to snare (first-movers extract while authorization is negotiated endlessly) or degrades to piton (formal authorization requirement becomes theater, extraction proceeds covertly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_authorization_feasibility, conceptual, 'Feasibility of multilateral authorization mechanism for space resource appropriation control').

omega_variable(
    reading_kernel_contingency,
    'Is the commons-conservation reading the default legal interpretation of Article II, or is it one contested reading among coequal alternatives (extraction-permissive, international-regime)?',
    'Jurisprudential analysis: examine ICJ advisory opinions, COPUOS expert panels, treaty amendment proposals, and state practice. Determine whether the reading has formal authority codification or is primarily advocated by specific coalitions.',
    'If commons-conservation is the default: the constraint''s authority_grounding is ''lineage'' (inherited from 1967 OST text interpretation). If coequal with alternatives: authority_grounding is ''distributed'' (multiple readings held by different state factions with no arbiter). This affects cs_structure authority_grounding and drift_state classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contingency, conceptual, 'Status of commons-conservation as default vs. contested interpretation of Article II').

omega_variable(
    benefit_distribution_mechanism,
    'What mechanism would distribute ''common heritage'' benefits to non-spacefaring and developing states under commons-conservation reading? Is benefit distribution automatic, negotiated per-extraction, or a long-term regime redesign?',
    'Treaty text analysis and negotiation history: examine proposals for common heritage of humankind regime (Moon Treaty, ISA precedent in UNCLOS). Determine whether benefit distribution is a structural requirement or an aspirational principle without enforcement.',
    'If automatic/negotiated: commons-conservation reading has genuine coordination function (tangled_rope classification holds). If aspirational only: constraint becomes snare (benefits flow to first-movers; distribution is unenforceable), or piton (formal requirement persists but is systematically evaded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_distribution_mechanism, preference, 'Enforceability and mechanism for benefit distribution under non-appropriation principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_aa2_cc_tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ost_aa2_cc_tr_t15, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ost_aa2_cc_tr_t30, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ost_aa2_cc_be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ost_aa2_cc_be_t15, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(ost_aa2_cc_be_t30, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ost_aa2_cc_su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ost_aa2_cc_su_t15, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(ost_aa2_cc_su_t30, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, lunar_resource_licensing_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, asteroids_act_compliance_framework).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation principle is a contested kernel with three structurally distinct readings producing different ε values and classification outcomes. Commons-conservation reading (this file): ε=0.58, tangled_rope. Extraction-permissive reading: ε=0.35-0.42, rope or tangled_rope (beneficiaries dominant, suppression lower). International-regime reading: ε=0.45-0.55, tangled_rope (requires new institutional machinery). These are separate constraint stories, not observables of a single constraint. Each reading has its own beneficiary/victim structure and represents a genuine legal-political position held by different coalitions of states. Link via network.affects_constraints to enable contamination analysis: if commons-conservation is institutionalized, it affects the viability of extraction-permissive and international-regime readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
