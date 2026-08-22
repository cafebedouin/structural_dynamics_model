% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination as Statehood Right (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the liberal nationalist reading of the
 *   jewish_sovereignty_palestine kernel: Jewish people hold a collective
 *   self-determination right analogous to other national groups' rights under
 *   liberal international-order doctrine, and statehood in the ancestral
 *   homeland is a legitimate — indeed remedial — exercise of that right given
 *   documented persecution. Structurally, this reading is distinguished from
 *   its siblings by treating Palestinian self-determination as a CO-EQUAL
 *   claim the framework itself generates: the liberal nationalist premise,
 *   applied consistently, requires partition or a binational solution, not
 *   exclusive title. The extractiveness authored here (0.42, moderate)
 *   reflects this reading's own internal logic: territorial compromise is
 *   expected and normatively required, and the gap between that requirement
 *   and the occupation's actual operation is the reading's central internal
 *   tension, not something this story resolves in Israel's favor by
 *   construction. The dip in extractiveness around Oslo (1993) and its
 *   subsequent rise reflect this reading's own account of a moment when
 *   partition-consistent practice was closest to being realized, followed by
 *   settlement expansion the reading itself treats as inconsistent with its
 *   foundational premise.
 *
 * KEY AGENTS:
 *   - jewish_israeli_citizens: primary beneficiary of realized statehood
 *   - jewish_diaspora_seeking_refuge: beneficiary of standing refuge right
 *   - palestinian_refugees_1948: bears displacement cost the reading treats as requiring redress
 *   - palestinian_residents_occupied_territories: bears occupation cost the reading treats as inconsistent with its own premise
 *   - israeli_settler_movement: administers the on-the-ground deformation of the partition-consistent outcome
 *   - regional_arab_states: contest and increasingly normalize the underlying legitimacy claim
 *   - international_legal_institutions: analytical observer adjudicating competing claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.55).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination as Statehood Right (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '66f2d6eb-ca59-4e49-b1b0-411be218a317').
narrative_ontology:cs_kernel_codification('66f2d6eb-ca59-4e49-b1b0-411be218a317', distributed).
narrative_ontology:cs_authority_grounding('66f2d6eb-ca59-4e49-b1b0-411be218a317', distributed).
narrative_ontology:cs_reading_relation('66f2d6eb-ca59-4e49-b1b0-411be218a317', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('66f2d6eb-ca59-4e49-b1b0-411be218a317', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('66f2d6eb-ca59-4e49-b1b0-411be218a317', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('66f2d6eb-ca59-4e49-b1b0-411be218a317', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('66f2d6eb-ca59-4e49-b1b0-411be218a317', foundational, national_groups_hold_symmetric_self_determination_claims).
narrative_ontology:cs_axiom_status(national_groups_hold_symmetric_self_determination_claims, holdable).
narrative_ontology:cs_axiom_grounding('66f2d6eb-ca59-4e49-b1b0-411be218a317', national_groups_hold_symmetric_self_determination_claims, deontological).
narrative_ontology:cs_axiom('66f2d6eb-ca59-4e49-b1b0-411be218a317', foundational, statehood_is_remedial_not_exclusive_title).
narrative_ontology:cs_axiom_status(statehood_is_remedial_not_exclusive_title, holdable).
narrative_ontology:cs_axiom_grounding('66f2d6eb-ca59-4e49-b1b0-411be218a317', statehood_is_remedial_not_exclusive_title, conventional).
narrative_ontology:cs_reference_frame('66f2d6eb-ca59-4e49-b1b0-411be218a317', post_holocaust_liberal_national_self_determination_order).
narrative_ontology:cs_drift_state('66f2d6eb-ca59-4e49-b1b0-411be218a317', post_oslo_collapse_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('66f2d6eb-ca59-4e49-b1b0-411be218a317', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_residents_occupied_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_settler_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise the collective self-determination right through a functioning state with its own military, law, and immigration policy (Law of Return). From this reading's premises, statehood is the legitimate remedy to millennia of statelessness and persecution, most recently catastrophically demonstrated. Bear real security costs and international isolation but hold sovereign institutions and citizenship.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_israeli_citizens, beneficiary,
    institutional, generational, constrained, regional).

% Hold a standing right of return and refuge grounded in the state's founding purpose as a haven of last resort. Their claim to the arrangement's benefit is largely potential rather than exercised, but the state's legitimacy story is built substantially around their historical vulnerability.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, biographical, mobile, global).

% Displaced during the state's founding and denied return under this arrangement's operative immigration and property regime. This reading acknowledges their loss as a genuine cost of partition-era conflict and treats it as the central item requiring redress or compensation in any final settlement, without conceding it delegitimizes the founding claim itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under military occupation and settlement expansion that, in this reading, are a deformation of the legitimate self-determination claim rather than its expression — the liberal nationalist premise requires a parallel, co-equal Palestinian self-determination outcome (partition or federation) that the occupation actively forecloses. They bear the day-to-day cost of a right this reading says should already have been reciprocated.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Actively expands settlement infrastructure in territories this reading holds should be available for Palestinian self-determination, effectively administering a policy that this reading's own logic treats as illegitimate overreach beyond the 1948/1967 state. Politically powerful enough to shape enforcement of land and permitting regimes on the ground.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_settler_movement, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_settler_movement, beneficiary).

% Historically rejected the partition premise and fought wars contesting the state's legitimacy; more recently some have normalized relations. Their acceptance or rejection of the liberal nationalist framing shapes the regional legitimacy conditions this reading depends on, but Palestinian negotiators are frequently sidelined in state-to-state normalization processes.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, regional_arab_states, observer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, regional_arab_states, excluded).

% Adjudicate competing self-determination and occupation-law claims (UN resolutions, ICJ opinions, ICC referrals). Provide the external evidentiary record this reading's corroboration draws on — often finding the occupation itself unlawful while treating 1948 statehood as an accomplished fact of international law.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Jewish people, following documented statelessness and genocide, a sovereign structure capable of self-defense, self-governance, and unrestricted immigration — solving the collective-action problem of protecting a dispersed, historically persecuted people without dependence on host-state goodwill.
% TRANSFER_FUNCTION: Moves land, water, and political sovereignty from the pre-1948 Arab population of Mandatory Palestine (and subsequently from Palestinians in the territories occupied in 1967) to the Jewish collective's state institutions and settlement infrastructure; moves security and self-governance capacity to Jewish Israelis and diaspora Jews holding return rights.
% ABSENT_VOICES: Palestinian self-determination claimants are structurally present as co-equal rights-holders in this reading's own normative logic, but are functionally absent from the territorial and political arrangements enforced on the ground — the partition or binational outcome the reading's premises require has not been delivered in fifty-plus years of occupation.
% DISAPPEARANCE_RATIONALE: If Jewish sovereign statehood disappeared overnight, Israeli Jewish citizens would lose the state apparatus this reading treats as the necessary remedy to historical persecution, and diaspora Jews would lose a standing refuge; Palestinians under occupation would gain removal of the military and settlement apparatus constraining them, but stateless status for millions of Israeli Jews would itself be a rearrangement of comparable magnitude. Whether the world 'rearranges' or 'unwinds a wrong' depends on which claimant's self-determination is treated as prior — exactly the contest this reading exists within.
% FOUNDING_PROBLEM: Sustained European antisemitic persecution culminating in the Holocaust demonstrated that diaspora existence under host-state sovereignty offered Jewish people no reliable protection; the founding problem was statelessness itself as an existential vulnerability, to be solved by a sovereign homeland with independent defense capacity.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historiography and international refugee law scholarship (largely non-Jewish institutional sources — UN archives, postwar tribunals) corroborate the founding problem's historical reality. Palestinian historians and international law bodies, from outside the beneficiary set, corroborate that the remedy as implemented displaced a resident population that had not caused the founding harm, and that the parallel Palestinian self-determination claim the liberal nationalist reading itself requires remains unresolved — making the founding problem 'live' for one claimant and 'unaddressed' for the other simultaneously.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 (moderate, not low) because this reading's own normative structure anticipates territorial compromise as the legitimate outcome — the moderate score reflects a right whose exercise properly requires sharing sovereignty, not one that licenses exclusive title. Suppression (0.55) is authored higher than extractiveness because maintaining the current territorial status quo (as opposed to the partition-consistent outcome the reading's own logic calls for) requires ongoing military and administrative enforcement — checkpoints, permit regimes, settlement infrastructure — that this reading's internal premises do not straightforwardly license. Resistance is authored high (0.75) because the arrangement, as actually operated, meets sustained resistance both from Palestinians bearing its costs and from within segments of the Jewish liberal-nationalist tradition itself that regard the occupation as a betrayal of the founding liberal premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israeli citizens and diaspora Jews are beneficiaries: the constraint's coordination function (sovereign self-defense, refuge) operates directly for them. Palestinian refugees and territorial residents are victims: they bear displacement and occupation costs that, crucially, this reading's OWN normative logic does not license as legitimate exercise of the Jewish self-determination right — the reading requires a reciprocal Palestinian outcome it has not delivered. This is the specific structural signature of the liberal nationalist reading versus its siblings: it authors extraction as a departure from its premises rather than (as the religious reading might) a fulfillment of them, or (as the settler-colonial reading does) an inherent structural feature.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish statelessness and persecution — is corroborated as historically live by sources outside the beneficiary set (Holocaust historiography, refugee law scholarship). But the specific REMEDY as currently operated (fifty-plus years of occupation without the partition or binational settlement the reading's own logic requires) shows signs of mandate drift: a founding problem that was dead-and-solved by 1948 statehood itself has been used to justify continuing territorial arrangements the founding logic does not itself sanction. This is precisely the mismatch the R5 apparatus is built to flag — founding_problem_status is authored as contested because whether the ORIGINAL problem justifies the CURRENT operation is exactly what is disputed between this reading and the post-zionist reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_delivery_gap,
    'Does the liberal nationalist reading''s own internal logic remain coherent once fifty-plus years have passed without delivering the co-equal Palestinian self-determination outcome it requires — or does the persistent gap itself falsify the reading''s claim to be currently operative rather than merely aspirational?',
    'Track whether concrete steps toward partition or binational co-sovereignty accumulate or regress over a defined future interval; a sustained multi-decade absence of any such steps despite stated commitment would support treating the reading as functioning primarily as legitimation rhetoric rather than operative principle.',
    'If the gap is judged unresolvable in practice, the liberal nationalist reading structurally converges toward the post_zionist_reading''s critique — the founding premise becomes decoupled from the arrangement it is invoked to justify, which is exactly the founding_problem_status=contested signal this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_delivery_gap, conceptual, 'Whether decades of undelivered partition falsifies or merely delays the reading''s own normative completion.').

omega_variable(
    id_1948_displacement_causal_weight,
    'How much of the 1948 Palestinian displacement was a foreseeable and structurally necessary consequence of establishing a Jewish-majority state in a territory with an Arab demographic majority, versus a contingent product of the specific war fought?',
    'Historical demographic and military-planning archival research (already substantially contested among historians of the 1948 war) establishing whether displacement was a designed outcome, an anticipated but unplanned consequence, or a war-contingent event separable from the statehood claim itself.',
    'A finding of structural necessity would push this reading toward acknowledging a much higher intrinsic extractiveness baked into the founding act itself, independent of the later occupation; a finding of contingency would support treating 1948 displacement as a tragic but severable historical event that does not indict the self-determination claim''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(id_1948_displacement_causal_weight, empirical, 'Whether 1948 displacement was structurally necessary to or contingently separable from the statehood claim.').

omega_variable(
    self_determination_symmetry_ambiguity,
    'Is the liberal nationalist reading''s commitment to Palestinian co-equal self-determination a genuine structural feature of the reading, or a rhetorical concession that the reading''s actual political coalitions have never operationalized?',
    'Compare stated doctrine (party platforms, international-law submissions invoking liberal self-determination principles) against enacted policy (settlement expansion, annexation votes, negotiating positions in peace processes) over the interval.',
    'If doctrine and enacted policy diverge sharply and persistently, the ''liberal nationalist reading'' as authored here may be better described as an idealized framework rarely instantiated in the actual political entities claiming it — which would mean this story''s beneficiary/victim structure describes an aspiration rather than the arrangement actually operating on the ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_symmetry_ambiguity, conceptual, 'Whether declared commitment to Palestinian co-equal self-determination is structurally real or rhetorical within the reading''s actual political instantiations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.38).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.1).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five linked readings of the jewish_sovereignty_palestine kernel. Each reading authors its own epsilon, beneficiary/victim structure, and type from a distinct normative starting point; none is a measurement of a shared 'true' epsilon under different observables. The liberal_nationalist_reading is distinguished from settler_colonial_reading by denying the settler-colonial analogy applies (this reading treats the claim as symmetric national self-determination, not a displacement pattern regardless of intent); from religious_zionist_reading by grounding the claim in secular rights doctrine rather than theological promise (and therefore treating territorial compromise as required, not merely permitted); from cultural_zionist_reading by requiring political sovereignty and demographic self-governance rather than treating cultural presence as sufficient; from post_zionist_reading by holding that the founding premise remains sound even where its current operation has drifted from it, rather than treating the ethno-national form itself as obsolete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
