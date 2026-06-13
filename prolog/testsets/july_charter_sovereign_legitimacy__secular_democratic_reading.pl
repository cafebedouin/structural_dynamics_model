% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter Secular Democratic Mandate with Military Subordination
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates the SECULAR DEMOCRATIC READING of a
 *   contested constitutional kernel — the July Charter's mandate for state
 *   legitimacy and institutional structure. The kernel itself is the fixed
 *   text of the Charter, but its meaning and enforcement depends on which
 *   reading prevails. Under the secular democratic reading, the Charter
 *   mandates: (1) secular democratic institutions as the basis of state
 *   legitimacy, grounded in civil law and democratic procedure rather than
 *   religious identity; (2) military institutional subordination to civilian
 *   authority, foreclosing autonomous military custodial roles; (3)
 *   structural exclusion of political Islam from claiming state-building
 *   legitimacy. This reading benefits secular institutions, international
 *   liberal-order actors, and religious minorities while imposing costs on
 *   military autonomous authority and political Islam movements' legitimacy
 *   claims. The sibling readings—guided nationalism (Islamic identity as
 *   sovereign legitimacy) and military custodian (armed forces as permanent
 *   constitutional guardian)—represent competing interpretations of the same
 *   Charter text, each with different beneficiary/victim structures and
 *   different institutional consequences. This story models the constraint as
 *   instantiated by the secular democratic reading; the other readings are
 *   other constraints with different ε-invariance profiles.
 *
 * KEY AGENTS:
 *   - civilian_elected_leadership: institutional agenda-setter; interprets and enforces secular democratic reading; benefits from democratic legitimacy but depends on military compliance
 *   - military_institutional_leadership: institutional payer; bears cost of formal subordination; identity-locked to military autonomy concept; forced to accept civilian supremacy
 *   - political_islam_movements: organized victim; structurally excluded from legitimacy contest; identity-locked to religious sovereignty claim; cannot exit without ideological dissolution
 *   - secular_civil_society: moderate beneficiary; protected by secular institutional framework; mobile exit (political speech/voting); organized around secular democratic values
 *   - judges_constitutional_interpreters: institutional beneficiary/agenda-setter; enforce secular reading through case law; constrained exit (embedded in constitutional structure)
 *   - international_liberal_order: global institutional beneficiary; provides legitimacy and material support conditional on secular-democratic compliance; analytical seat (cannot directly enforce)
 *   - religious_minorities: powerless beneficiary; depend on secular reading for protection from majoritarian religious law; trapped in nation-state; survival depends on constraint persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.76).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter Secular Democratic Mandate with Military Subordination").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'a9638b16-0ff4-460b-8931-353ba685866c').
narrative_ontology:cs_kernel_codification('a9638b16-0ff4-460b-8931-353ba685866c', fixed_text).
narrative_ontology:cs_authority_grounding('a9638b16-0ff4-460b-8931-353ba685866c', lineage).
narrative_ontology:cs_interpretation_layer_present('a9638b16-0ff4-460b-8931-353ba685866c').
narrative_ontology:cs_reading_relation('a9638b16-0ff4-460b-8931-353ba685866c', july_charter_sovereign_legitimacy__july_charter_guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('a9638b16-0ff4-460b-8931-353ba685866c', july_charter_sovereign_legitimacy__july_charter_military_custodian_reading, influences).
narrative_ontology:cs_axiom('a9638b16-0ff4-460b-8931-353ba685866c', foundational, secular_democratic_legitimacy_ground).
narrative_ontology:cs_axiom_status(secular_democratic_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('a9638b16-0ff4-460b-8931-353ba685866c', secular_democratic_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('a9638b16-0ff4-460b-8931-353ba685866c', foundational, military_institutional_subordination).
narrative_ontology:cs_axiom_status(military_institutional_subordination, holdable).
narrative_ontology:cs_axiom_grounding('a9638b16-0ff4-460b-8931-353ba685866c', military_institutional_subordination, conventional).
narrative_ontology:cs_reference_frame('a9638b16-0ff4-460b-8931-353ba685866c', secular_democratic_constitutional_order).
narrative_ontology:cs_drift_state('a9638b16-0ff4-460b-8931-353ba685866c', contemporary_hybrid_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9638b16-0ff4-460b-8931-353ba685866c', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_institutions).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_authority_framework).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_movements).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint redistributes institutional authority and legitimacy from one set of actors to another. At 0.68 endpoint, this reflects substantial asymmetric redistribution: military loses autonomous authority, political Islam loses legitimacy as a state-building framework, civilians gain governing power, and secular institutions become primary. Extraction rises from 0.58 to 0.68 over the interval because the civilian leadership gradually hardens institutional enforcement of the secular reading—regulatory barriers to political Islam organizations, constitutional court rulings against military autonomy claims, civil law supremacy over religious law. Suppression rises from 0.65 to 0.76 (and plateaus) because maintaining the secular democratic reading requires active suppression: preventing military coups, barring political Islam parties from elections, arresting or monitoring religious organizing, controlling textbooks to teach secular history. Theater rises from 0.28 to 0.42 and plateaus because civilian leadership increasingly performs secular democracy through elections and human rights rhetoric while the actual governing capacity remains concentrated in civilian-aligned military units and judges—the elections are held but the military vetoes outcomes they dislike; rights are announced but selectively enforced against political Islam. The plateau in later periods suggests the constraint has reached an equilibrium: suppression cost is stable because military and political Islam have learned compliance or exile patterns; extractiveness plateaus because the redistribution has been locked in; theater plateaus because the performance level has normalized into routine institutional practice. Measurement grid is shared: every metric is authored at every time point (0, 8, 16, 25, 35, 50) preventing misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The civilian elected leadership seat and the military institutional leadership seat should compute radically different constraint types. From the civilian perspective, the constraint is genuine coordination (democratic procedure solves the post-revolutionary state-building problem; everyone benefits from stable secular institutions; military subordination is a rational institutional arrangement). From the military perspective, the constraint is pure extraction (they lose institutional autonomy they claim as constitutional right; they are forced into subordination by civilian decree; their institutional voice is excluded from the constitutional negotiation). From political Islam's perspective, the constraint is a snare (presented as neutral secular democracy but actually forecloses their legitimacy claim; they cannot participate in the state-building consensus because the consensus defines them as illegitimate). The engine's per-seat classification should diverge sharply: rope at the civilian seat, tangled_rope at judges, snare at political Islam, and forced-subordination-flavored extraction at military. These divergences are exactly the signal the framework exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality encodes each seat's structural relationship to THIS specific constraint. Civilian elected leadership: d near 0.0 (full beneficiary)—they gain governing legitimacy, institutional authority, and international support conditional on the secular reading. Military institutional leadership: d near 1.0 (full target)—they lose autonomous authority, face institutional subordination, must accept civilian command. Political Islam movements: d near 1.0 (full target)—they are excluded from legitimacy; their core identity claim (religious sovereignty) is foreclosed. Judges: d near 0.2 (beneficiary with modest cost)—they gain institutional power to interpret the Charter and rule on constitutional disputes, but must accept civilian-democratic legitimacy constraints on their authority. Secular civil society: d near 0.3 (beneficiary with minor cost)—they get their preferred institutional framework, face no substantial costs. International liberal order: d = 0.0 (pure beneficiary)—they gain a constitutional partner aligned with liberal-democratic values; no costs. Religious minorities: d near 0.2 (beneficiary with minor cost)—they gain protection through secular law, but depend on constraint persistence; trapped exit means any destabilization threatens them. These directionalities should be derived from the declared beneficiary/victim structure and exit options; no override needed here because the structural data correctly maps to the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is a real risk for this constraint. The founding problem was post-revolutionary state-building across religious/ideological divides. If the state achieves institutional stability and sectarian tensions decline, the mandate for military subordination and political Islam exclusion may outlive its functional purpose. The measurement series shows extractiveness and suppression both plateau in later periods (after t=25), which is consistent with either stable equilibrium (the constraint continues to be functional) or institutional inertia (the constraint persists despite declining functional need). A mandatrophy reading would say: the constraint achieved state consolidation; now it operates primarily to distribute power to civilian elites, maintain judicial independence, and exclude political Islam from the political process—functions that serve elite interests more than state stability. The civilian leadership would deny this (they frame continued suppression as necessary for preventing military coups and sectarian violence), while military and political Islam actors would affirm it (they see the constraint as purely extractive historical residue). The constraint is NOT YET mandatrophic by structural definition (the founding problem is contested as still live), but it is a candidate for mandatrophy if empirical conditions change: if military coups cease to be a threat, if political Islam becomes genuinely integrated into secular institutions, if sectarian tensions decline substantially, the suppression requirement could fall and the constraint would begin to look like pure inertial theater. The commentary should flag this as an empirical question rather than a current classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_reading_vs_nationalist_naturalization,
    'Is the secular democratic mandate in the Charter a foundational constitutional choice, or has it been naturalized post-hoc as an inevitable feature of state modernity by international liberal-order pressure?',
    'Historical comparison: compare the Charter''s original drafting records (what the constituent assembly explicitly debated and chose) against post-Charter liberal-order interventions (structural adjustment conditions, aid conditionality, international human rights monitoring). If the secular framing appeared primarily in post-Charter documents, the reading has been naturalized rather than foundational.',
    'If naturalized, the secular democratic reading is better understood as imposed from outside rather than as an internal constitutional commitment. This would suggest the constraint''s legitimacy depends on international power rather than domestic democratic choice, which reframes it from tangled_rope (with genuine domestic coordination function) toward snare (with external domination). If foundational, the reading''s legitimacy rests on the constituent assembly''s deliberate choice and domestic political alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_reading_vs_nationalist_naturalization, empirical, 'Whether secular democratic framing is intrinsic to the Charter or externally naturalized').

omega_variable(
    military_subordination_institutional_viability,
    'Can civilian institutions genuinely exercise control over military authority, or is military subordination a formal facade maintained by military choice to avoid coups?',
    'Test cases where civilian authority and military interests directly conflict (budget cuts, investigations, policy orders): observe whether civilians can enforce compliance against military resistance. If military can systematically block civilian decisions through coup threats or non-compliance, formal subordination is theater; if civilians can enforce against military resistance, subordination is substantive.',
    'If subordination is theater, the constraint is better classified as snare (military maintains autonomy under a democratic facade) or piton (inert formal subordination while military actual authority persists). If subordination is substantive, the tangled_rope classification holds because civilians genuinely pay coordination costs (delegation to military on security matters) while extracting power (control over military decisions). The suppression requirement would also be reinterpreted: either as genuine civilian enforcement of subordination, or as military tolerance of democratic forms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_subordination_institutional_viability, empirical, 'Whether military subordination is substantive or formal facade').

omega_variable(
    political_islam_identity_lock_mechanism,
    'Is the political Islam movements'' identity-lock to religious sovereignty claims genuine (they cannot exit because their foundational identity is religious), or is it a strategic position that could shift if institutional incentives changed?',
    'Counterfactual: if political Islam movements were offered formal institutional power within secular-democratic frameworks (cabinet positions, legislative influence, judicial appointments conditional on secular governance acceptance), would some factions accept and exit the identity-lock, or would all factions reject such offers as incompatible with their core claims?',
    'If identity-lock is genuine, political Islam movements are trapped victims of the constraint; exit is psychologically/ideologically impossible. If identity-lock is strategic, some movements could shift positions if incentives change, making them partially mobile targets rather than fully trapped victims. This affects the exit_options classification and the suppression requirement: genuine identity-lock requires more suppression (they will keep trying); strategic identity-lock allows for co-option and requires less active suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_islam_identity_lock_mechanism, empirical, 'Whether political Islam identity-lock is intrinsic or strategic').

omega_variable(
    reading_contest_as_power_struggle,
    'To what extent are the three Charter readings (secular democratic, guided nationalist, military custodian) genuinely distinct normative frameworks, versus surface rationalizations for institutional power contests between civilians, military, and religious movements?',
    'Discourse analysis: examine the three readings'' intellectual coherence, citations of the Charter text, and internal consistency. If each reading can defend itself on textual grounds, they are genuine frameworks; if readings are invoked opportunistically (different governments cite different readings to justify whichever policies benefit them), the readings are post-hoc rationalizations for power struggles.',
    'If readings are genuine normative frameworks, the constraint is a real constitutional dispute about state legitimacy with substantive content. If readings are power rationalizations, the constraint is fundamentally about military/civilian/religious power competition disguised in constitutional language—it would reframe as snare (power-holding elites controlling the terms of constitutional interpretation) rather than tangled_rope (genuine coordination with asymmetric extraction). Suppression would be understood as defending the chosen reading against challenge, not as enforcing a genuinely agreed-upon framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_as_power_struggle, conceptual, 'Whether the three readings are genuine normative frameworks or post-hoc rationalizations for power struggles').

omega_variable(
    international_liberal_order_conditionality,
    'How much of the secular democratic reading''s persistence depends on international aid conditionality, sanctions threats, and investment conditions, versus internal domestic support?',
    'Counterfactual: if international support (aid, investment, diplomatic recognition) were suddenly made unconditional on the secular reading, would domestic political forces continue enforcing it, or would military or nationalist readings gain traction as governments reduce compliance costs?',
    'If persistence depends heavily on international conditionality, the constraint''s stability is fragile and dependent on external maintenance—it functions as a mechanism of neo-liberal control rather than as internally legitimate constitutional choice. This would shift classification from tangled_rope (with domestic coordination function) toward snare (with external coercion). If persistence is robust to removal of international incentives, it indicates genuine domestic alignment around the secular reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_liberal_order_conditionality, empirical, 'Whether the secular reading persists through domestic support or international conditionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(july_tr_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(july_tr_t50, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(july_be_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(july_be_t50, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 16, 0.74).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(july_su_t35, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 35, 0.75).
narrative_ontology:measurement(july_su_t50, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_military_custodian_reading).

% DUAL FORMULATION NOTE:
% The July Charter sovereign legitimacy kernel decomposes into three constraint stories, each instantiating a different constitutional reading of the same text. The secular_democratic_reading claims secular institutions and democratic procedure ground state legitimacy, foreclosing religious-identity sovereignty and military autonomous authority. The guided_nationalism_reading claims Islamic identity and sharia-informed law constitute legitimate state grounds, coexisting with but structurally competing against the democratic reading. The military_custodian_reading claims military institutions serve as permanent constitutional guardians, influences both other readings by demanding military voice in constitutional interpretation. All three readings interpret the same Charter text but produce different beneficiary/victim structures, different suppression requirements, and different ε-values. They are not the same constraint viewed from different angles—they are three distinct constraints with distinct extraction profiles, bound together by the common kernel but diverging in structural implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
