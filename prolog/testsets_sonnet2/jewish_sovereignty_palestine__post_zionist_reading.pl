% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Ethnic-National Statehood Framework Post-Achievement (Post-Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the post-Zionist reading of the
 *   jewish_sovereignty_palestine kernel: it accepts that the Zionist project
 *   achieved a legitimate historical goal — sovereign statehood as refuge
 *   from persecution — but holds that the ethnic-national legal and symbolic
 *   framework built to achieve that goal has outlived its emancipatory
 *   function and now operates as an entrenched privilege structure
 *   obstructing civic equality for Israeli Palestinians and blocking regional
 *   integration. This is NOT the settler-colonial reading (which denies the
 *   founding claim's legitimacy from the outset) nor the liberal-nationalist
 *   reading (which affirms the framework's ongoing legitimacy). The
 *   distinguishing move here is temporal: the founding coordination function
 *   is granted as historically real and successfully completed, while the
 *   continued operation of the same ethnic-national scaffolding past the
 *   point of completion is read as the extractive residue.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.62).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.58).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Ethnic-National Statehood Framework Post-Achievement (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'f998ef4e-5bae-40ce-b936-75b72f352086').
narrative_ontology:cs_kernel_codification('f998ef4e-5bae-40ce-b936-75b72f352086', distributed).
narrative_ontology:cs_authority_grounding('f998ef4e-5bae-40ce-b936-75b72f352086', distributed).
narrative_ontology:cs_reading_relation('f998ef4e-5bae-40ce-b936-75b72f352086', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('f998ef4e-5bae-40ce-b936-75b72f352086', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('f998ef4e-5bae-40ce-b936-75b72f352086', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('f998ef4e-5bae-40ce-b936-75b72f352086', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('f998ef4e-5bae-40ce-b936-75b72f352086', foundational, founding_task_completed_at_statehood).
narrative_ontology:cs_axiom_status(founding_task_completed_at_statehood, holdable).
narrative_ontology:cs_axiom_grounding('f998ef4e-5bae-40ce-b936-75b72f352086', founding_task_completed_at_statehood, empirically_contingent).
narrative_ontology:cs_axiom('f998ef4e-5bae-40ce-b936-75b72f352086', foundational, post_completion_ethnic_architecture_is_extractive).
narrative_ontology:cs_axiom_status(post_completion_ethnic_architecture_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('f998ef4e-5bae-40ce-b936-75b72f352086', post_completion_ethnic_architecture_is_extractive, conventional).
narrative_ontology:cs_reference_frame('f998ef4e-5bae-40ce-b936-75b72f352086', statehood_as_completed_emancipatory_achievement).
narrative_ontology:cs_drift_state('f998ef4e-5bae-40ce-b936-75b72f352086', post_1993_oslo_collapse_and_2018_nation_state_law, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f998ef4e-5bae-40ce-b936-75b72f352086', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jews_with_return_rights).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__post_zionist_reading, jewish_collective_self_determination_achieved).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold automatic citizenship and land-access advantages under the Law of Return and associated land-allocation bodies; participate fully in the state's civic, military, and political life; the ethnic-national framework that once served as an emancipatory project now operates as their baseline of ordinary civic status, largely invisible to them as a structure requiring active defense.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel, beneficiary,
    organized, generational, mobile, national).

% Hold a standing right of immigration and citizenship to Israel regardless of residence, providing a form of geopolitical insurance and identity anchor without bearing the constraint's ongoing enforcement costs; can activate the right or not as circumstances warrant.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jews_with_return_rights, beneficiary,
    moderate, generational, arbitrage, global).

% Hold formal citizenship but face land-allocation bodies, state symbols, and a legally codified 'nation-state' framework that formally reserves national self-determination to Jews alone; navigate a civic status structurally subordinate to the ethnic-national core despite equal voting rights; exit means leaving the only homeland they have.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, payer,
    moderate, generational, trapped, national).

% Live under military administration and settlement expansion justified in part by the same founding ethno-national claim to the land; lack citizenship, voting rights in the governing state, or unrestricted movement; bear the sharpest edge of the framework's territorial logic.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Live under blockade and periodic military campaigns; their situation is shaped by a decades-long unresolved status flowing from the same founding partition and displacement history the framework has never civically resolved; exit is nearly foreclosed by border control on all sides.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians, payer,
    powerless, immediate, trapped, regional).

% Administer citizenship law, land authorities, military governance, and the constitutional 'nation-state' framework; could in principle reform toward civic equality but treat the ethnic-national character as foundational and non-negotiable, actively defending it through legislation and enforcement rather than treating it as a settled historical achievement open to revision.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, civilizational, analytical, national).

% Would participate in a de-Zionized, civically integrated regional order but are structurally outside the domestic conversation about Israel's internal constitutional character; regional integration proposals condition normalization on resolution of exactly the ethnic-national asymmetries this reading identifies.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_arab_states, excluded,
    organized, generational, constrained, regional).

% Document the gap between formal statehood achievement and civic equality; advocate for de-Zionization of state institutions (removing ethnic preference from citizenship, land, and symbolic law) while affirming the historical legitimacy of the state's existence; face domestic political marginalization for the position.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, post_zionist_scholars_and_civil_society, observer,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The founding framework once solved a genuine and urgent coordination problem: providing a secure political refuge and collective self-determination structure for a stateless, persecuted people, achieved through statehood in 1948.
% TRANSFER_FUNCTION: Land-allocation authority, automatic citizenship-and-return rights, and symbolic-constitutional status move preferentially to Jewish citizens and diaspora Jews, while civic parity, land access, and in the occupied territories basic political rights are withheld from Palestinian residents and citizens under the same governing structures.
% ABSENT_VOICES: Israeli Palestinian citizens and occupied Palestinians are formally present in some venues (Knesset representation, courts) but structurally unable to alter the ethnic-national constitutional core through ordinary politics; regional Arab states are excluded from any say in Israel's internal civic architecture despite bearing its regional consequences.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework were dissolved into a civic-equality model overnight, land authorities, citizenship law, and constitutional symbolism would need wholesale reconstruction; Jewish citizens would lose codified demographic and legal advantages, Israeli Palestinians would gain formal civic parity, and the basis for continued occupation-era distinctions would be substantially undermined — the state's entire internal legal architecture is built around the distinction this reading targets.
% FOUNDING_PROBLEM: Statelessness and persecution of Jews in Europe and elsewhere, culminating in the Holocaust, created an urgent need for a sovereign refuge where Jewish self-determination could not be revoked by a host state.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state institutions and most Jewish citizens attest the founding problem remains partially live (regional hostility, antisemitism, need for a Jewish-majority refuge). Independent post-Zionist scholars, international human rights bodies, and Israeli Palestinian civil society organizations — none of whom are beneficiaries of the ethnic-privilege structure — attest that statehood as such solved the founding problem decades ago and that the continuing ethnic-national legal architecture now serves a different, unaddressed function: maintaining demographic and land privilege rather than securing refuge.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at moderate-to-high (0.62) because Jewish citizens and diaspora Jews receive codified legal, land, and symbolic advantages under a framework that Israeli Palestinian citizens and occupied populations experience as structurally exclusionary, but the extraction is bounded by the fact that Israeli Palestinian citizens retain formal citizenship, courts, and electoral representation — unlike a pure snare. Suppression (0.58) reflects active legal and institutional defense of the ethnic-national core (Basic Law: Israel as the Nation-State of the Jewish People, land-authority statutes, military administration in the territories) rather than passive inertia. Theater ratio rose over the interval (0.15 to 0.48) as the founding refuge-function became less proximate to daily governance while symbolic and legal reaffirmation of the ethnic-national character intensified, particularly post-1967 and post-2018 Nation-State Law.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Israeli state institutions) and the beneficiary seat (Jewish citizens), the framework often computes as ordinary, necessary nation-state architecture, largely invisible as an active constraint. From the payer seats (Israeli Palestinian citizens, occupied populations), the same legal architecture computes as an actively enforced exclusion. This divergence is the structural datum this reading exists to name — it is not resolved by choosing one seat's perception as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens and diaspora Jews are beneficiaries: they hold return rights, land-access advantages, and full civic-national status without needing to defend the framework actively — it is their default condition. Israeli Palestinian citizens are targets: they pay through structurally subordinated civic status despite formal citizenship, with trapped exit since Israel is their only homeland. Occupied and Gaza Palestinians are the sharpest-edge targets: powerless, trapped, bearing the framework's territorial logic without any citizenship buffer. Israeli state institutions are the agenda-setter, administering and actively defending the framework rather than treating statehood as complete and civically neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-Zionist reading is explicitly a mandatrophy claim: it argues the founding mandate (secure a stateless people) was achieved and its continued operation as an ethnic-preference legal architecture is now serving a different function (demographic/land privilege maintenance) than the one that justified its creation. This is distinct from claiming the mandate was illegitimate from inception (settler-colonial reading) or claiming it remains fully live (liberal-nationalist and religious-zionist readings). Classifying this as tangled_rope rather than snare preserves the historically genuine coordination function (statehood-as-refuge) while registering the asymmetric extraction that now rides on the same structure — collapsing it to snare would erase the founding achievement this reading itself affirms; collapsing it to rope would erase the documented civic asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_completion_vs_ongoing_necessity,
    'Was the security/refuge function of the ethnic-national framework fully discharged by the achievement of statehood in 1948 (and reinforced by subsequent wars), such that its continuation now serves only privilege-maintenance — or does ongoing regional hostility and antisemitism mean the founding problem remains at least partially live, making the framework still substantially coordinative?',
    'Comparative analysis of whether formal civic-equality reforms (removing ethnic preference from citizenship and land law while retaining a demographic Jewish-majority state through non-legal means) would measurably increase Jewish physical insecurity, versus historical/comparative evidence from post-apartheid and multi-ethnic federal states about whether formal equality undermines minority-group security in practice.',
    'If the founding problem is judged fully discharged, this reading''s tangled_rope classification is stable and arguably understates present extraction; if judged still substantially live, the coordination component is larger than authored here and the classification would shift toward a more genuinely mixed rope/tangled_rope balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_completion_vs_ongoing_necessity, empirical, 'Whether the founding security rationale for the ethnic-national framework is discharged or still partially operative.').

omega_variable(
    reading_indexed_kernel_disagreement_location,
    'Where exactly does this reading''s disagreement with the liberal_nationalist_reading and settler_colonial_reading live — is it a factual dispute about historical intent and effect, a normative dispute about how long a coordination-derived privilege structure may persist before it becomes extractive, or an unresolvable framing choice about which observable (founding moment vs. present operation) is the correct referent for ε?',
    'This is inherently a conceptual/normative dispute not resolvable by additional data alone; documenting it here per the ε-invariance principle rather than attempting to average or reconcile it across the sibling readings, each of which is authored as its own separate constraint story.',
    'Clarifies that the four sibling readings are not competing measurements of one constraint but four structurally distinct constraints sharing a kernel; this story''s ε (0.62) is not in tension with the liberal_nationalist_reading''s presumably lower ε or the settler_colonial_reading''s presumably higher ε — each is a different reading''s own referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexed_kernel_disagreement_location, conceptual, 'Locating the source of kernel-level disagreement: factual, normative-threshold, or framing-referent.').

omega_variable(
    reversibility_of_de_zionization,
    'Is de-Zionization of state institutions (removing ethnic preference from citizenship, land, and symbolic law while preserving the state) a coherent reformist path, or does the demographic/political reality make it functionally equivalent to dissolution of the state''s founding character, which would make this reading''s proposed remedy indistinguishable from what the settler_colonial_reading calls for?',
    'Comparative constitutional analysis of civic-nationalist reform proposals actually advanced by post-Zionist scholars and organizations, assessed against what demographic and political conditions would need to hold for such reform to preserve a recognizable, functioning state.',
    'If de-Zionization collapses into de facto dissolution, this reading''s claim to be distinct from the settler_colonial_reading weakens considerably; if a stable civic-nationalist reform path exists, the reading''s distinctiveness (accepting the state''s legitimacy while rejecting its ethnic architecture) is structurally sound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_de_zionization, conceptual, 'Whether the reading''s reform proposal is distinguishable in practice from the settler-colonial reading''s abolitionist demand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(jewi_tr_t1980, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.48).
narrative_ontology:measurement(jewi_be_t1980, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1980, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the jewish_sovereignty_palestine kernel, decomposed per the ε-invariance principle: the natural-language concept 'Zionism/Jewish sovereignty in Palestine' covers structurally distinct claims (cultural renaissance without sovereignty requirement, liberal collective self-determination, post-achievement civic-equality critique, settler-colonial displacement pattern, and theological territorial claim) with different ε values, different beneficiary/victim structures, and different classifications. This post-Zionist reading authors moderate-to-high extractiveness (0.62) and classifies as tangled_rope (genuine founding coordination function plus ongoing asymmetric extraction), distinct from what the liberal_nationalist_reading (likely rope or lower-ε tangled_rope, framework treated as still-legitimate ongoing coordination) and the settler_colonial_reading (likely snare, higher ε, no genuine coordination function conceded even at founding) would author for the same underlying institutional facts. All five readings should be linked bidirectionally in production.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
