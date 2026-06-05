% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Secular Democratic Reading: Civilian Authority Over Military Subordination
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter's mandate for secular democratic institutions with
 *   military subordination to civilian authority represents one reading of a
 *   contested kernel about sovereign legitimacy in post-revolutionary state
 *   formation. This constraint embodies the tension between
 *   secular-nationalist and religious-populist visions of legitimate state
 *   authority. The secular democratic reading instantiates a framework in
 *   which sovereignty derives from the consent of a secular citizenry
 *   organized through democratic procedures, military power is
 *   instrumentalized as subordinate to civilian authority, and political
 *   Islam is excluded from legitimate sovereignty claims. The extractiveness
 *   trajectory (0.35 → 0.52) reflects accumulating institutional enforcement:
 *   initial flexibility in subordination doctrine hardens into enforced
 *   constraint as democratic consolidation deepens. The suppression
 *   requirement drops (0.72 → 0.58) as military professionalization
 *   internalizes subordination norms, reducing need for active coercive
 *   enforcement. Theater ratio rises (0.48 → 0.65) as the constraint's
 *   performative dimension expands — constitutional reaffirmation rituals,
 *   military parades with democratic themes, Constitutional Day readings —
 *   while substantive enforcement capacity fluctuates with security crises.
 *   This pattern distinguishes the secular democratic reading from its
 *   sibling readings: the guided nationalism reading would show rising
 *   military authority and falling suppression; the military custodian
 *   reading would show military extraction (negative chi for civilian
 *   authority) rather than civilian extraction from military autonomy.
 *
 * KEY AGENTS:
 *   - Secular Democratic Coalition: Institutional beneficiary (institutional/arbitrage) — captures legitimacy framework; experiences constraint as coordination. Includes urban civil society, professional classes, secular political parties, civil service reformers.
 *   - Political Islam Movements: Primary victim (organized → powerless trajectory) — excluded from sovereignty claims; faces active institutional suppression. Includes Jamaat-e-Islami, Brotherhood-affiliated organizations, religious scholars claiming state authority.
 *   - Military Institution: Victim and institutional actor (organized/constrained) — loses autonomous legitimacy claim; subordinated through constitutional mechanism. Career officers experience loss of institution-defining autonomy.
 *   - Civil Society / Citizenship: Moderate beneficiary-and-constrained (moderate/constrained) — gains procedural protections and rights framework; constrained by security apparatus and subordination enforcement.
 *   - State Security Apparatus: Bifurcated institutional actor (institutional/constrained) — subordinated to civilian authority but operationalized as enforcement mechanism for the constraint itself.
 *   - Constitutional Court / Oversight Mechanism: Organized enforcer (organized/constrained) — maintains civilian control architecture; experiences constraint as temporary scaffold requiring active institutional maintenance.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the secular-democratic legitimacy frame as immutable rather than recognizing it as one contested reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.52).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Reading: Civilian Authority Over Military Subordination").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'kernel-reading-july-charter-secular-democratic-2026-02-26').
narrative_ontology:cs_kernel_codification('kernel-reading-july-charter-secular-democratic-2026-02-26', formalized).
narrative_ontology:cs_authority_grounding('kernel-reading-july-charter-secular-democratic-2026-02-26', extraction).
narrative_ontology:cs_interpretation_layer_present('kernel-reading-july-charter-secular-democratic-2026-02-26').
narrative_ontology:cs_reading_relation('kernel-reading-july-charter-secular-democratic-2026-02-26', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('kernel-reading-july-charter-secular-democratic-2026-02-26', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('kernel-reading-july-charter-secular-democratic-2026-02-26', foundational, sovereign_legitimacy_derives_from_democratic_consent).
narrative_ontology:cs_axiom_status(sovereign_legitimacy_derives_from_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('kernel-reading-july-charter-secular-democratic-2026-02-26', sovereign_legitimacy_derives_from_democratic_consent, deontological).
narrative_ontology:cs_axiom('kernel-reading-july-charter-secular-democratic-2026-02-26', foundational, military_subordination_to_civilian_authority_is_structural_requirement).
narrative_ontology:cs_axiom_status(military_subordination_to_civilian_authority_is_structural_requirement, holdable).
narrative_ontology:cs_axiom_grounding('kernel-reading-july-charter-secular-democratic-2026-02-26', military_subordination_to_civilian_authority_is_structural_requirement, deontological).
narrative_ontology:cs_axiom('kernel-reading-july-charter-secular-democratic-2026-02-26', secondary, political_islam_excluded_from_sovereignty_claims).
narrative_ontology:cs_axiom_status(political_islam_excluded_from_sovereignty_claims, holdable).
narrative_ontology:cs_axiom_grounding('kernel-reading-july-charter-secular-democratic-2026-02-26', political_islam_excluded_from_sovereignty_claims, empirically_contingent).
narrative_ontology:cs_reference_frame('kernel-reading-july-charter-secular-democratic-2026-02-26', secular_democratic_sovereignty_frame).
narrative_ontology:cs_drift_state('kernel-reading-july-charter-secular-democratic-2026-02-26', contemporary_post_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('kernel-reading-july-charter-secular-democratic-2026-02-26', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_movements).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_institutional_autonomy).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, alternative_legitimacy_readings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL ISLAM EXCLUDED (SNARE) — Structurally trapped by constitutional exclusion from sovereignty claims. Cannot participate in legitimacy narratives; faces active suppression through institutional denial and legal constraint. No exit option within the democratic framework; any exit requires rejecting the charter itself. Maximum experienced extraction — victim group has no voice in the constraint's own justification.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY INSTITUTIONAL AUTONOMY (SNARE) — Organizational power to organize collectively, but constrained by legal subordination. Loses autonomous claim to legitimacy; must operate through civilian-authority framing. Career officers face institutional subordination; exit costs are high (professional identity tied to military hierarchy). Active suppression through civilian oversight boards and defense ministry control. Extraction runs toward civilian authority; military experiences the constraint as coercive.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECULAR DEMOCRATIC COALITION (ROPE) — Primary institutional beneficiary. Experiences the charter as coordination mechanism: establishes shared framework for legitimate governance, coordinates diverse secular actors around democratic procedures. Net beneficiary from institutional structure — civilian control operationalizes their legitimacy claim. Arbitrage options available (can reformulate governance while remaining within secular democratic frame). Experiences minimal suppression because the constraint aligns with their interests.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY / CITIZENSHIP (TANGLED ROPE) — Benefits from the secular democratic framing (protection of individual rights, procedural legitimacy) but constrained by the active enforcement apparatus (security apparatus, emergency powers subordinated to civilian authority but still operative). Genuine coordination function (civil society norms, citizenship rights) coexists with extraction (security exceptions, restrictions on alternative legitimacy claims). Moderate power; constrained exit (cost of rejecting democratic framework is social and political marginalization).
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SECURITY / CIVILIAN MINISTRY (TANGLED ROPE) — Bifurcated institutional actor: coordinating security functions (genuine coordination role) while subordinated to civilian political authority (enforced asymmetry). Benefits from operational resources and legal framework; constrained by civilian oversight and exclusion from sovereignty claims. Active enforcement of subordination (civilian cabinet appointees, legislative oversight, exclusion from budgetary autonomy). Both coordination and extraction present: secures the state while extracting from military autonomy.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL ENFORCEMENT (SCAFFOLD) — Organized mechanism (constitutional court, legislative oversight, civil service) that enforces civilian authority subordination. Experiences the constraint as temporary coordination structure with evolving sunset: as democratic norms mature and military professionalization deepens, the need for explicit constitutional enforcement diminishes. Theater ratio moderate (some performative constitutional review, but genuine institutional gatekeeping functions). Sunset clause implicit in democratic institutionalization — active enforcement becomes unnecessary as norms internalize.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: MILITARY INSTITUTIONAL NARRATIVE (PITON) — The charter's assertion of military subordination persists through institutional inertia and repeated formal reaffirmation even when enforcement capacity wanes during periods of democratic consolidation or security crisis. The narrative of 'professional military under civilian control' maintains ceremonial legitimacy (parades, Constitutional Day readings, officer training curricula) while substantive civilian oversight attenuates during emergencies. Theater ratio ≥0.70: the ritual of subordination persists regardless of practice. Exit option for military is arbitrage — can reformulate professional identity within the subordination frame without abandoning institutional form.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From civilizational perspective, the constraint reflects an irreducible structural fact: every modern state must resolve the sovereign authority question (who legitimately exercises state power?), and the secular-democratic resolution is one answer to this fundamental problem. The constraint appears as immutable because legitimacy itself is the enabling function of state authority — there is no 'outside' from which to contest the legitimacy framework. However, this perspective risks naturalizing what is actually a contingent reading of the charter. The engine will identify this as a false summit: identifiable beneficiaries (secular coalition) and victims (Islam-excluded movements) reveal that 'sovereign legitimacy' is not a law of nature but a constructed institutional arrangement.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__secular_democratic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, TR),
    TR >= 0.70.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, indicating genuine asymmetry. The secular democratic reading extracts from military institutional autonomy (subordination imposed) and from political Islam movements (exclusion from legitimacy). However, the reading also coordinates secular actors and establishes procedural legitimacy, so the extraction is hybrid, not pure. The rising trajectory reflects accumulating institutional enforcement: initial constitutional ambiguity permits both secular democratic and guided nationalism interpretations; over time, institutional development narrows the reading toward secular democratic subordination. Suppression (0.58): Moderate-high, raw structural suppression. The constraint actively suppresses military autonomy claims and Islam-political legitimacy. But suppression is not maximal (not 0.85+) because enforcement relies partly on internalized norms (military professionalization, secular middle class normalization) rather than pure coercive apparatus. The declining trajectory (0.72 → 0.58) suggests suppression mechanisms shift from active enforcement toward norm internalization as democratic consolidation deepens. Theater ratio (0.65): Moderate-high, indicating significant performative content. Constitutional review rituals, military ceremonies with democratic framing, and 'civilian control' declarations persist regardless of substantive enforcement gaps during crises. As theater rises, underlying enforcement may actually loosen — the rituals maintain legitimacy appearance while substantive civilian oversight fluctuates with political conditions. Claimed type (tangled_rope): Dual structure — genuine coordination function (secular democratic procedures, rule of law framework) coexists with asymmetric extraction (military subordination, Islam exclusion). Requires active enforcement: the constraint is not self-maintaining; constitutional court, defense ministry oversight, and civilian political dominance must actively police the boundary.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a stark perspectival chasm. Secular democratic coalition experiences rope — the charter coordinates diverse secular actors around shared institutional framework, establishes rule-of-law procedures, and prevents majoritarian tyranny. No significant extraction experienced; the constraint enables their political project. Political Islam experiences snare — excluded from sovereignty claims by constitutional mandate, faces active suppression through legal and institutional mechanisms, no exit option except rejecting the charter itself. Maximum extraction, no coordination benefit. Military experiences snare to tangled_rope range depending on enforcement intensity — subordination is experienced as coercive, but the subordination also enables military professionalization and integration into state apparatus, so some ambiguous benefit coexists. The false-summit risk is highest in the analytical perspective: from a civilizational vantage, sovereign legitimacy appears as an irreducible problem (where does state authority come from?) that must be resolved somehow, making the secular democratic resolution appear natural and necessary. But the structural data (identifiable beneficiaries extracting from identifiable victims) reveals the resolution as contested institutional arrangement, not natural law. The engine's false summit detector should fire here.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives d from structural position: beneficiary status, exit options, and power level feed the directionality computation chain. Secular coalition: beneficiary + arbitrage → low d (0.10-0.20) → negative effective extraction chi (they experience the constraint as beneficial coordination). Political Islam: victim + trapped → high d (0.90-0.95) → high f(d) (1.35+) → maximum experienced extraction. Military institution: victim + constrained → high d (0.75-0.85) → high f(d) (1.10-1.20) → high experienced extraction, but moderated by organized power status. Civil society: moderate position, both benefits and constraints → d ≈ 0.50 → f(d) ≈ 0.65 → moderate chi. State security: bifurcated institutional position (enforcer and subordinated) → d ≈ 0.55 → f(d) ≈ 0.72. Constitutional enforcement: organized power, constrained but functional → d ≈ 0.45 → f(d) ≈ 0.55. Analytical observer: canonical d ≈ 0.73 → f(d) ≈ 1.15, producing the high-chi mountain (false summit candidate). The perspectival gaps are structural: political Islam sees extraction where secular coalition sees coordination; military sees coercion where civil society sees protection; enforcement mechanism sees temporary scaffold where analysis sees immutable law. These gaps are not measurement artifacts — they reflect real differences in how agents experience the constraint's distribution of benefit and cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled_rope classification correctly captures both a genuine coordination function (establishing rule-of-law procedures, protecting secular civil society, coordinating state institutions) and genuine asymmetric extraction (suppressing military autonomy claims, excluding political Islam from legitimacy). The false summit risk in the analytical perspective is real and significant: the constraint naturalizes the secular-democratic resolution as 'what legitimacy requires' rather than recognizing it as one contested reading that benefits identifiable agents. The engine's false summit detector should reclassify this from mountain to tangled_rope or snare, revealing that naturalization as a cover story for institutional extraction. Mandatrophy is not fully resolved until the analytical observer acknowledges that 'sovereign legitimacy' is not answerable by physics or mathematics — it is a political question with multiple defensible answers, and the secular democratic answer is one such answer, not the only one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_neutrality_vs_secularization_enforcement,
    'Does the charter mandate state religious neutrality (mere absence of establishment) or active secularization (exclusion of religious authority from sovereignty claims)?',
    'Textual analysis of charter language (neutrality language vs exclusion language); historical record of enforcement intensity (passive neutrality vs active suppression of religious parties); comparison with other constitutional frameworks'' treatment of religious actors',
    'If neutrality: constraint is rope (coordination of diverse actors including religious). If secularization: constraint is tangled_rope or snare (active extraction of religious movements from legitimacy). Classification hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_neutrality_vs_secularization_enforcement, conceptual, 'Whether constraint mandates neutrality or active secularization').

omega_variable(
    military_subordination_enforcement_vs_norm,
    'Is civilian authority subordination enforced through active institutional mechanisms (legal constraint, oversight boards, budgetary control) or maintained through internalized professional norms?',
    'Examination of enforcement machinery: constitutional court intervention in military matters; frequency and consequence of civilian-military disputes; military budget autonomy vs legislative control; officer appointment procedures; emergency power invocation patterns',
    'If actively enforced: suppression stays at 0.58 (structural mechanisms). If norm-dependent: suppression drops to 0.35-0.40 (reliance on internalized compliance). This affects snare vs tangled_rope boundary for military perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_subordination_enforcement_vs_norm, empirical, 'Whether military subordination is actively enforced or norm-dependent').

omega_variable(
    guided_nationalism_reading_compatibility,
    'Can the secular democratic reading coexist with the guided nationalism reading (military as guarantor of national identity rather than subordinate institution) within a single constitutional framework?',
    'Analysis of military role language in charter: whether military has explicit legitimacy claim as guardian of national identity; whether civilian control and military nationalism can be simultaneously operationalized; historical periods when both readings claimed validity',
    'If coexistence is structurally possible: reading_relations declares coexists_with. If core premises contradict: reading_relations declares forecloses. Classification of sibling reading depends on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guided_nationalism_reading_compatibility, conceptual, 'Logical compatibility of secular democratic and guided nationalism readings').

omega_variable(
    political_islam_contingent_exclusion,
    'Is the political Islam exclusion a structural necessity of the secular democratic reading, or a contingent institutional choice that a secular democracy could theoretically accommodate?',
    'Comparison with other secular democracies'' treatment of religious parties (Tunisia, Turkey, European democracies); analysis of whether religious parties can accept secular democratic rules and civilian subordination; whether charter text explicitly requires religious exclusion or merely enables it',
    'If contingent: political Islam appears as victim but not as structural necessity — the reading could theoretically evolve. If structural necessity: Islam-exclusion is foundational axiom. Affects victim set characterization and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_islam_contingent_exclusion, conceptual, 'Whether political Islam exclusion is structural necessity or contingent choice').

omega_variable(
    false_summit_naturalness_claim,
    'Is the analytical observer''s mountain classification revealing a structural natural law (sovereign legitimacy is inherently indeterminate and must be resolved institutionally) or naturalizing a contingent institutional arrangement (this specific secular-democratic resolution)?',
    'Examination of whether other constitutional democracies resolve sovereign legitimacy differently while remaining ''democratic''; analysis of whether the secular-democratic frame is culturally universal or geopolitically contingent; historical record of whether charter''s legitimacy claim survives challenges',
    'If structural necessity: mountain classification stands. If contingent arrangement: false summit engine fires, reclassifies as tangled_rope or snare. Affects whether constraint is treated as immutable or contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness_claim, conceptual, 'Whether sovereignty legitimacy question has natural law status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcsdl_theater_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jcsdl_theater_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(jcsdl_theater_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(jcsdl_extract_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcsdl_extract_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jcsdl_extract_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jcsdl_suppress_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(jcsdl_suppress_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(jcsdl_suppress_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% The July Charter sovereign legitimacy kernel decomposes into three structurally distinct constraint stories, one per reading. Each reading instantiates a different ε value (secular democratic ε=0.52, guided nationalism ε=0.48, military custodian ε=0.65) because the readings differ in how they distribute extraction and coordination across agent groups. The three stories are linked via network.affects_constraints: the secular democratic reading (this file) influences both sibling readings by establishing the institutional framework within which alternative readings must operate. Each story has its own beneficiary/victim set, its own perspectives, and its own cs_structure axioms encoding the foundational normative distinction. Do not attempt to model the kernel itself as a single constraint — the kernel is the contested claim, and the three readings are the three ways that claim can be instantiated structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
