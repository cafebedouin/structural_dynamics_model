% ============================================================================
% CONSTRAINT STORY: codification_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_codification_asymmetry, []).

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
 *   constraint_id: codification_asymmetry
 *   human_readable: Codification Asymmetry in Indian Personal Law
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   India's constitutionally pluralist family law system permits Hindu,
 *   Muslim, Christian, Parsi, and Sikh communities to govern marriage,
 *   divorce, inheritance, and guardianship through separate personal law
 *   codes while remaining within a single secular constitutional framework.
 *   This 75-year coexistence represents either a successful negotiation of
 *   pluralist accommodation or a structural constraint that fragments
 *   individual legal personality and perpetuates community-based gender
 *   asymmetries — depending on the observer's structural position. The
 *   constraint exhibits genuine coordination functions (respecting religious
 *   autonomy, reducing secular court burden) alongside asymmetric extraction
 *   (women's rights vary by religion, individual status is fragmented, state
 *   apparatus retains boundary-setting power). The codification asymmetry
 *   creates a theater effect: reform commissions, PIL cases, and
 *   constitutional reinterpretation circulate regularly, yet the underlying
 *   fragmentation persists. This suggests institutional inertia (piton
 *   perspective) or structural resistance (mountain view) rather than
 *   functional coordination (rope) or negotiated temporary solutions
 *   (scaffold).
 *
 * KEY AGENTS:
 *   - Hindu, Muslim, Christian, Parsi, Sikh Community Leadership: Institutional beneficiaries (institutional/arbitrage) — personal law regimes preserve religious autonomy in family governance; experience constraint as legitimate coordination
 *   - Women in Minority Personal Law Regimes: Primary victims (powerless/identity_locked) — structurally mobile within secular law, identity-locked within religious law; disproportionate vulnerability in divorce, property, custody
 *   - Individual Across Multiple Legal Identities: Secondary victims (moderate/constrained) — legal status fragmented; cannot achieve unified recognition across property, inheritance, marriage, custody
 *   - Secular State Authority: Institutional actor (institutional/constrained) — benefits from reduced family law caseload, extraction through jurisdiction boundary-setting; constrained by constitutional pluralism and electoral politics
 *   - Reformist Legal Institutions: Institutional actor (institutional/arbitrage) — maintain formal commitment to unified family law but have failed substantively across 75+ years; persist through performative legal activism (theater)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent feature of pluralist democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(codification_asymmetry, 0.58).
domain_priors:suppression_score(codification_asymmetry, 0.62).
domain_priors:theater_ratio(codification_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(codification_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(codification_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(codification_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(codification_asymmetry, tangled_rope).
narrative_ontology:human_readable(codification_asymmetry, "Codification Asymmetry in Indian Personal Law").
narrative_ontology:topic_domain(codification_asymmetry, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(codification_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(codification_asymmetry, '144a36d1-4899-49b1-b1a8-a151ddcd1aaa').
narrative_ontology:cs_created_at('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', '').
narrative_ontology:cs_kernel_codification('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', formalized).
narrative_ontology:cs_authority_grounding('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', lineage).
narrative_ontology:cs_interpretation_layer_present('144a36d1-4899-49b1-b1a8-a151ddcd1aaa').
narrative_ontology:cs_reading_relation('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', personal_law_hindu_reading, coexists_with).
narrative_ontology:cs_reading_relation('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', personal_law_muslim_reading, coexists_with).
narrative_ontology:cs_reading_relation('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', personal_law_christian_reading, coexists_with).
narrative_ontology:cs_reading_relation('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', personal_law_secular_state_reading, influences).
narrative_ontology:cs_reading_relation('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', personal_law_rights_based_reading, influences).
narrative_ontology:cs_axiom('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', foundational, religious_community_autonomy_necessary).
narrative_ontology:cs_axiom_status(religious_community_autonomy_necessary, holdable).
narrative_ontology:cs_axiom_grounding('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', religious_community_autonomy_necessary, deontological).
narrative_ontology:cs_axiom('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', foundational, pluralist_constitution_mandates_accommodation).
narrative_ontology:cs_axiom_status(pluralist_constitution_mandates_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', pluralist_constitution_mandates_accommodation, conventional).
narrative_ontology:cs_axiom('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', secondary, unified_family_law_requires_religious_prioritization).
narrative_ontology:cs_axiom_status(unified_family_law_requires_religious_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', unified_family_law_requires_religious_prioritization, empirically_contingent).
narrative_ontology:cs_reference_frame('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', pluralist_accommodation_framework).
narrative_ontology:cs_drift_state('144a36d1-4899-49b1-b1a8-a151ddcd1aaa', contemporary_rights_based_challenge, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(codification_asymmetry, religious_community_leadership).
narrative_ontology:constraint_beneficiary(codification_asymmetry, secular_state_apparatus).
narrative_ontology:constraint_victim(codification_asymmetry, individual_agency_across_frameworks).
narrative_ontology:constraint_victim(codification_asymmetry, women_in_minority_regimes).
narrative_ontology:constraint_victim(codification_asymmetry, epistemically_vulnerable_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN IN MINORITY REGIME (SNARE) — Structurally mobile within the secular legal framework (can divorce, inherit) but identity-locked within her religious community's codified law. Exit from the community law requires abandoning religious identity, family ties, and social recognition. The constraint extracts disproportionate marital and inheritance vulnerability: triple talaq, unequal testimony in property, loss of custody on remarriage. High suppression — formal legal alternatives exist but are experienced as identity death rather than exit.
constraint_indexing:constraint_classification(codification_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL NAVIGATING MULTIPLE FRAMEWORKS (TANGLED ROPE) — Moderate power. An individual whose marriage, divorce, inheritance, and guardianship status depend on which framework applies in which jurisdiction produces genuine coordination (personal law creates predictability for family matters within communities) alongside asymmetric extraction (the person's legal status is fragmented — simultaneously married and divorced, or heir and non-heir, depending on which law is invoked). Constrained exit: switching frameworks carries cascading legal consequences across property, custody, and inheritance claims.
constraint_indexing:constraint_classification(codification_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY LEADERSHIP (ROPE) — Institutional beneficiary. Personal law regimes enable community self-governance on family matters — coordination function that keeps internal disputes out of secular courts and preserves religious autonomy in defining marriage, divorce, inheritance, and guardianship. The leadership experiences the constraint as legitimate coordination: it solves the problem of how pluralist states can respect religious autonomy while maintaining secular constitutional frameworks. Effective extraction is low from this perspective — the leadership sees fair benefit-sharing (their community gets jurisdiction, they accept secular oversight).
constraint_indexing:constraint_classification(codification_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR STATE AUTHORITY (TANGLED ROPE) — The state has genuine coordination interests: personal law regimes reduce family law caseloads and respect constitutional pluralism (Articles 25-28 of Indian Constitution). But the state also extracts by retaining oversight authority, by picking which claims qualify as 'religious' (determining the boundary of personal law scope), and by using the fragmentation as a tool of political economy (can appeal to different communities at different times). Constrained exit: eliminating personal laws would trigger massive resistance and constitutional crisis. The state is both coordinator and extractor.
constraint_indexing:constraint_classification(codification_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST LEGAL INSTITUTIONS (PITON) — Courts, women's rights advocates, and secular legal reformers maintain formal commitment to unified family law (abolish personal law regimes, create secular Personal Law Code) but have failed to achieve substantive change across 75+ years despite multiple reform commissions. The reform effort persists through theater — high-profile cases, PIL (Public Interest Litigation), constitutional reinterpretation — without moving the underlying political equilibrium. The constraint persists through institutional inertia: reform language circulates, but the actual codification asymmetry is maintained because dismantling it would require coalition-building the reformist institutions lack. Theater ratio reflects this: much reformist activity, minimal functional change to the constraint's architecture.
constraint_indexing:constraint_classification(codification_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PLURALIST INEVITABILITY (MOUNTAIN) — From a global/civilizational perspective, the codification asymmetry appears as an inevitable feature of religiously plural states: any legally unified framework privileges some religious worldviews over others, and the only exit from discrimination is de facto pluralism via multiple personal laws. The mountain classification naturalizes this as an inherent structural feature of pluralist democracy. However, the structural data contradicts this: the constraint's beneficiaries (community leadership, state apparatus), victims (women, epistemically vulnerable groups), and extractive mechanisms (identity lock, fragmentation, constrained exit) are not laws of nature but contingent institutional arrangements. This perspective risks false summitry: treating a political choice (which communities get recognized, which women's interests are protected) as a logical necessity.
constraint_indexing:constraint_classification(codification_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(codification_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(codification_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(codification_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(codification_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(codification_asymmetry, TR),
    TR >= 0.70.

:- end_tests(codification_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from women in minority regimes and from individuals seeking unified legal recognition, while distributing coordination benefits to community leadership and state apparatus. The extraction is not maximal (snare-level) because genuine pluralist accommodation exists (communities do gain autonomy, individuals do have secular-law alternatives, state does maintain oversight). The value reflects that the constraint serves multiple functions simultaneously: coordination (pluralist accommodation) and extraction (gender asymmetry, legal fragmentation, state power). The extraction has increased over the 30-year interval (0.42 → 0.58) because reform attempts have failed to shift the power balance, meaning the constraint is increasingly recognized as extractive rather than merely accommodative. Suppression (0.62): High. Barriers to exit from personal law regimes include identity fusion with religious community, family and social pressure, legal complexity of switching frameworks, career and inheritance consequences. Suppression is particularly high for women in minority regimes where secular alternatives exist legally but are experienced as identity death. Theater ratio (0.65): Moderate-high. The sustained reform effort without substantive change suggests that reform language (constitutional reinterpretation, women's rights litigation, reform commission proposals) circulates while the underlying codification asymmetry persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival divide between institutional beneficiaries (community leadership, state apparatus) who see coordination and legitimate pluralism, and victims (women, individuals seeking unified status) who see extraction and fragmentation. Community leadership experiences rope — the constraint solves their coordination problem. The state experiences tangled_rope — it coordinates and extracts simultaneously. Women in minority regimes experience snare — maximum extraction, identity lock, minimal exit. The reformist institutions experience piton — performative activism without functional change. The analytical observer risks mountain — naturalizing pluralism as inevitable rather than political. This gap is not merely perspectival disagreement; it reflects different material positions: those who benefit from community autonomy (leadership) see coordination; those who lose power via fragmentation (women, individuals) see extraction. The perspectival gap is itself a diagnostic feature — where power and vulnerability align with classification, the constraint is likely not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. Community leadership benefits from the constraint with arbitrage-level exit (they can negotiate on their own terms with the state), producing low d → low/negative f(d) → they experience it as coordination. The state benefits while constrained (high institutional power but political constraints on unilateral action), producing moderate d. Women in minority regimes are victims with identity_locked exit (structurally mobile but cognitively trapped in religious framework), producing high d → high f(d) → they experience maximum extraction. The reformist institutions have arbitrage-level exit (they can pursue other legal causes) but maintain commitment to reform despite 75-year failure, suggesting identity_locked cognitive commitment rather than structural necessity. The analytical observer occupies the position of trying to see the whole structure from outside, but the mountain classification itself may be an instance of analytical identity lock: natural-law framing may be the analytical position's way of avoiding the political choice (whose pluralism gets privileged?).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not resolve the mandatrophy by choosing a single type. Instead, it demonstrates that mandatrophy is itself a feature of the constraint: the impossibility of unified classification reflects the underlying kernel contestation. When observed from the position of community leadership, it appears as rope (coordination). When observed from women's position, snare (extraction). When observed from the state's position, tangled_rope (mixed coordination-extraction). When observed from reformist institutions, piton (theater without function). When observed from the analytical position, mountain (inherent to pluralism). No single classification is false — each is the true perspective from its observational position. The mandatrophy is not a classification failure; it is a structural feature of the constraint itself: the constraint cannot be unified into a single type because the readings cannot be unified into a single legitimate frame. The engine should report this as resolved mandatrophy with identity classification: the constraint exhibits full perspectival variation as diagnostic evidence of kernel contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_coexisting_constraints,
    'Is codification asymmetry a single contested kernel (family law authority) read through multiple religious lenses, or genuinely distinct kernels (Hindu family authority, Islamic family authority, secular family authority) coexisting without unified normative foundation?',
    'Historical analysis of how each framework claims legitimacy (textual, traditional, constitutional); mapping of whether reform attempts treat personal laws as branches of a single unified authority or as separate systems requiring separate negotiation',
    'If single kernel: the constraint decomposes into 5 stories (one per reading). If coexisting kernels: the constraint is a network of 5 separate constraints with structural interdependencies. Classification remains tangled_rope either way, but the epistemology of reform differs fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_coexisting_constraints, conceptual, 'Whether codification asymmetry stems from a single contested kernel or multiple coexisting kernels').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For individuals in minority personal law regimes, is the suppression mechanism structural (legal barriers to exit the regime) or internalized (identity fusion that makes exit unthinkable)?',
    'Tracking post-exit behavior: do individuals who formally exit minority personal law frameworks (by secularizing, converting, or declaring themselves outside community jurisdiction) experience reduced suppression, or does suppression persist through internalized norms and family/community pressure that accompanies formal exit?',
    'If primarily structural: reform via legal unification is feasible. If primarily internalized: reform requires identity-frame shifting, not just legal change — the constraint persists even after formal exit because the bind is cognitive, not institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether suppression in personal law regimes is structural or internalized').

omega_variable(
    state_beneficiary_status_ambiguity,
    'Does the state benefit from maintaining codification asymmetry (reduced caseload, political flexibility), or does the state bear net costs (administrative burden of navigating multiple frameworks, constitutional tension)?',
    'Comparative analysis: do states with unified family law experience higher caseloads or lower efficiency in family law adjudication? Do states with fragmented personal law show evidence of intentional maintenance vs passive tolerance?',
    'If beneficiary: state is active enforcer of the constraint (tangled_rope classification confirmed). If cost-bearer: state is constrained agent whose extraction is extractive only relative to some citizens (women, minorities) — constraint may decompose into separate stories per affected group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_beneficiary_status_ambiguity, empirical, 'Whether the state benefits from or bears costs from codification asymmetry').

omega_variable(
    reform_failure_causation,
    'Why have 75+ years of reform attempts failed to achieve unified family law in India? Is it structural resistance (no viable coalition can form around any unified code), political economy (state benefits from status quo), or normative clash (no unified framework can satisfy pluralist accommodation)?',
    'Detailed historical analysis of failed reform commissions; mapping of which coalitions attempted unification and why they dissolved; comparative analysis with other pluralist states that achieved unification',
    'If structural resistance: the constraint is nearly immutable absent massive political upheaval — mountain tendency. If political economy: the constraint is maintained by beneficiaries — snare or tangled_rope structure confirmed. If normative clash: the constraint cannot be resolved without choosing which religious worldview to privilege — reveals kernel-level disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_failure_causation, conceptual, 'Causation of sustained reform failure in Indian personal law unification').

omega_variable(
    women_organization_capacity,
    'Can women''s movements across religious communities form a cross-cutting coalition powerful enough to reshape personal law codification, or does codification asymmetry structurally prevent such coalition by locking women into identity-specific frameworks?',
    'Tracking attempts at cross-community women''s organizing; analyzing whether personal law fragmentation is used as a political tool to prevent unified gender-based coalition formation',
    'If coalition is possible: women''s power (organized/mobile) could shift the constraint from snare/tangled_rope toward rope or scaffold. If coalition is structurally prevented: the identity lock on women in minority frameworks is an intentional feature, not accident — constraint is actively maintained extraction (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_organization_capacity, empirical, 'Capacity for cross-community women''s coalition formation against codification asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(codification_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(codif_tr_t0, codification_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(codif_tr_t15, codification_asymmetry, theater_ratio, 15, 0.52).
narrative_ontology:measurement(codif_tr_t30, codification_asymmetry, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(codif_be_t0, codification_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(codif_be_t15, codification_asymmetry, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(codif_be_t30, codification_asymmetry, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(codification_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(codification_asymmetry, women_inheritance_asymmetry).
narrative_ontology:affects_constraint(codification_asymmetry, religious_boundary_definition).
narrative_ontology:affects_constraint(codification_asymmetry, judicial_discretion_in_pluralist_law).

% DUAL FORMULATION NOTE:
% Codification asymmetry is the macro-constraint describing the overall fragmentation. It decomposes into three micro-constraints: (1) women_inheritance_asymmetry — differential property rights by religion (distinct ε); (2) religious_boundary_definition — state power to determine which communities qualify for personal law (distinct authority structure); (3) judicial_discretion_in_pluralist_law — judge's latitude in choosing which framework applies (distinct epistemic structure). Each story has its own extractiveness value and its own perspectives. Codification asymmetry links them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
