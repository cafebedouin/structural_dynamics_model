% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute ICC Jurisdiction — Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's complementarity mechanism (Article 17) creates a
 *   hybrid constraint: the ICC has jurisdiction over core crimes but may only
 *   exercise it when national courts are genuinely unwilling or unable. This
 *   reading holds that complementarity is an admissibility filter, not a
 *   jurisdictional limit — the ICC's jurisdiction exists universally (by
 *   treaty consent of states parties) but its operation is deferred to
 *   national systems. The constraint extracts cooperation and resources from
 *   states parties while its enforcement depends on those same states.
 *   Powerful non-party states are structurally excluded from obligations but
 *   influence outcomes through Security Council politics. The claimed type
 *   (tangled_rope) reflects genuine coordination (ending impunity) fused with
 *   asymmetric extraction (selective enforcement, disproportionate burdens on
 *   weaker states parties).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.55).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute ICC Jurisdiction — Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '536e371e-bd94-4773-9f69-b87e0f8edf5a').
narrative_ontology:cs_kernel_codification('536e371e-bd94-4773-9f69-b87e0f8edf5a', formalized).
narrative_ontology:cs_authority_grounding('536e371e-bd94-4773-9f69-b87e0f8edf5a', lineage).
narrative_ontology:cs_interpretation_layer_present('536e371e-bd94-4773-9f69-b87e0f8edf5a').
narrative_ontology:cs_reading_relation('536e371e-bd94-4773-9f69-b87e0f8edf5a', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('536e371e-bd94-4773-9f69-b87e0f8edf5a', rome_statute_jurisdiction__sovereigntist_reading, influences).
narrative_ontology:cs_axiom('536e371e-bd94-4773-9f69-b87e0f8edf5a', foundational, complementarity_is_admissibility_not_jurisdiction).
narrative_ontology:cs_axiom_status(complementarity_is_admissibility_not_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('536e371e-bd94-4773-9f69-b87e0f8edf5a', complementarity_is_admissibility_not_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('536e371e-bd94-4773-9f69-b87e0f8edf5a', foundational, universal_aspiration_creates_residual_authority).
narrative_ontology:cs_axiom_status(universal_aspiration_creates_residual_authority, holdable).
narrative_ontology:cs_axiom_grounding('536e371e-bd94-4773-9f69-b87e0f8edf5a', universal_aspiration_creates_residual_authority, deontological).
narrative_ontology:cs_reference_frame('536e371e-bd94-4773-9f69-b87e0f8edf5a', nuremberg_lineage_complementarity_compromise).
narrative_ontology:cs_drift_state('536e371e-bd94-4773-9f69-b87e0f8edf5a', contemporary_selective_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('536e371e-bd94-4773-9f69-b87e0f8edf5a', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_institution).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties_signaling_commitment).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_cooperating_situations).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties_bearing_disproportionate_cooperation_costs).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_non_cooperating_situations).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, perpetrators_facing_selective_prosecution).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, complementarity_principle_as_admissibility_not_jurisdiction).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_aspiration_creates_residual_authority_beyond_consent).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, nuremberg_lineage_grounds_international_criminal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ICC sets the agenda for international criminal justice through its prosecutor's discretion, judicial interpretations of complementarity, and requests for cooperation. It collects budgetary contributions from states parties and legitimacy from the universal justice aspiration. Its exit is constrained by treaty obligations and institutional survival imperatives — it cannot dissolve itself without states parties' consensus.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_institution, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_institution, beneficiary).

% States that ratify to signal human rights commitment but face low risk of ICC scrutiny (no active conflicts, no nationals accused). They benefit from the legitimacy signal with minimal cooperation costs. They can exit via Article 127 withdrawal with one year's notice — a real but reputational cost exit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties_signaling_commitment, beneficiary,
    organized, biographical, mobile, regional).

% States parties with active conflicts or weak judicial systems that must cooperate with ICC investigations (arrest surrendered persons, protect witnesses, provide evidence). They bear financial, political, and security costs while powerful non-party states face no equivalent obligation. Exit is constrained — withdrawal doesn't undo existing obligations and invites political retaliation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties_bearing_disproportionate_cooperation_costs, payer,
    moderate, biographical, constrained, regional).

% Victims of crimes in situations where the state cooperates with the ICC (e.g., DRC, Uganda, CAR referrals). They gain access to a judicial forum otherwise unavailable. They are trapped — no individual exit from victimhood, no alternative forum when national courts fail.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_cooperating_situations, beneficiary,
    powerless, biographical, trapped, local).

% Victims in situations where the state refuses cooperation (e.g., Sudan, Libya, Palestine/Israel, Afghanistan pre-2021). The ICC's jurisdiction exists on paper but enforcement fails. They pay the cost of the constraint's aspiration without its delivery — the universal promise extracts hope and attention while delivering nothing. No exit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_non_cooperating_situations, payer,
    powerless, biographical, trapped, local).

% Mid-level perpetrators from cooperating states or weak states who face ICC prosecution while principal architects from powerful states evade accountability. They bear the enforcement costs of a system that claims universality but operates selectively. Exit is constrained — they cannot leave the jurisdictional reach once targeted.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, perpetrators_facing_selective_prosecution, payer,
    moderate, immediate, constrained, local).

% Permanent Security Council members and other major powers that have not ratified (US, China, Russia) or have withdrawn/unsigned (US 2002, Russia 2016, Philippines 2019). They are structurally excluded from the constraint's obligations but shape its operation through Security Council referrals/blocking, bilateral immunity agreements, and political pressure. They arbitrage — using the ICC when convenient (referrals) and blocking it when threatened.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful_non_party_states, excluded,
    powerful, generational, arbitrage, global).

% The ICC's legislative and oversight body. It sets the budget, elects judges and prosecutor, and can amend the Statute. It observes the constraint's operation from the institutional governance seat — neither directly paying nor collecting, but structuring the rules.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, assembly_of_states_parties, observer,
    institutional, generational, analytical, global).

% The interpretive community that debates complementarity's scope, the ICC's legitimacy, and the treaty's meaning. They do not bear costs or collect benefits from the constraint's operation but shape the intellectual environment in which it is understood.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_scholars).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a permanent international forum to prosecute genocide, crimes against humanity, and war crimes when national courts are genuinely unable or unwilling — solving the impunity gap that ad hoc tribunals addressed only episodically.
% TRANSFER_FUNCTION: Moves sovereignty (jurisdictional primacy, cooperation obligations, budgetary contributions) from states parties to the ICC institution; moves accountability from domestic courts to an international forum; moves legitimacy from power-politics to legal process. The transfer is asymmetric: states parties transfer more than non-parties; powerful states transfer least.
% ABSENT_VOICES: Victims in non-cooperating situations (Syria, Myanmar, Yemen, Ethiopia) who have no forum and no voice in the ASP; future generations who inherit the precedent of selective enforcement; non-state armed groups bound by IHL but excluded from the Statute's drafting.
% DISAPPEARANCE_RATIONALE: If the Rome Statute vanished overnight, the ICC would dissolve, leaving no permanent international criminal court. Ad hoc tribunals would return for specific conflicts (as with Yugoslavia/Rwanda). Universal jurisdiction exercises by domestic courts would increase but remain politically fraught. The normative architecture of 'complementarity' — the principle that international justice supplements rather than supplants national justice — would lose its institutional anchor, though the principle might persist in customary law.
% FOUNDING_PROBLEM: The post-Cold War impunity gap: ad hoc tribunals (ICTY, ICTR) were slow, expensive, and politically contingent; no permanent mechanism existed to deter or punish core international crimes when states were unable or unwilling to act.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the 1998 Rome Conference record (120 states voting for adoption) and the ICTY/ICTR completion strategies. However, powerful states (US, China, Russia) contested whether the founding problem justified the sovereignty transfer — evidenced by their non-ratification and the US ASPA (2002). The ICC's own Assembly of States Parties acknowledges the problem persists but disputes whether the current institution solves it (Review Conference 2010, 2017).
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial: the constraint transfers sovereignty and resources from states parties to an institution that cannot enforce its own orders, while powerful states free-ride. Suppression (0.55) is moderate: states can withdraw (Article 127), non-parties are not bound, but once a situation is referred or proprio motu opened, the complementarity filter limits exit for targeted states. Theater ratio (0.45) is significant: many states parties perform cooperation (enact implementing legislation, pay dues) while obstructing actual surrenders (e.g., Al-Bashir's travels to states parties). Accessibility collapse (0.50) reflects that alternatives (ad hoc tribunals, universal jurisdiction, hybrid courts) exist but are politically costly and episodic. Resistance (0.70) is high: major powers resist, African Union has threatened mass withdrawal, even states parties resist specific cooperation requests.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's seat, the constraint is a rope with growing pains — a genuine coordination mechanism maturing toward universality. From states parties bearing costs, it is a snare — they pay for a system that protects them least. From victims in non-cooperating situations, it is a false summit — the mountain of universal justice appears but cannot be climbed. From powerful non-party states, it is a piton — a degraded institution they tolerate when useful, block when threatening. The engine computes these seat divergences from the structural data; the authored claim (tangled_rope) captures the hybrid structure visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC institution (agenda_setter/beneficiary) sits near the beneficiary end (d ~ 0.2) — it collects budget, legitimacy, and institutional survival from the constraint. States parties signaling commitment (beneficiary, mobile exit) sit at low d (~0.15). States parties bearing cooperation costs (payer, constrained exit) sit at high d (~0.75). Victims in cooperating situations (beneficiary, trapped) have low d but no exit — the constraint subsidizes them. Victims in non-cooperating situations (payer, trapped) have high d (~0.85) — they bear the aspiration's cost without its delivery. Perpetrators facing selective prosecution (payer, constrained) sit at high d (~0.8). Powerful non-party states (excluded, arbitrage) are outside the directionality calculation — they neither pay nor benefit directly but shape the field. The ASP and scholars (observers, analytical) sit at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending impunity for core crimes) remains live — atrocities continue, national courts still fail. But the constraint's current form may not solve it: complementarity defers to the very national systems that fail; the ICC lacks enforcement power; powerful states evade. This is mandatrophy not because the problem is solved, but because the solution has become a substitute for the solution — the ICC's existence lets states claim 'justice is handled' while impunity persists. The founding problem is contested (not dead) because parties dispute whether the ICC as constituted addresses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Rome Statute''s complementarity mechanism a single constraint with contested interpretation, or are the universalist, sovereigntist, and hybrid readings structurally distinct constraints sharing a label?',
    'Apply the epsilon-invariance test: if measuring the constraint''s extractiveness under the universalist reading (ICC as universal court) yields a different epsilon than under the sovereigntist reading (ICC as conditional forum), they are distinct constraints. The hybrid reading''s epsilon (0.65) should be compared against independently authored sibling stories.',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the classification must accommodate irreducible interpretive variance — which the current framework does not allow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s contested readings are one constraint or a constraint family.').

omega_variable(
    complementarity_nature,
    'Is complementarity (Article 17) a jurisdictional limit (the ICC lacks jurisdiction unless national courts fail) or an admissibility filter (the ICC has jurisdiction but defers to national courts)?',
    'Treaty interpretation: Article 1 text (''jurisdiction over persons for the most serious crimes'') vs Article 17 (''case is inadmissible''). The ICC''s jurisprudence (Katanga, Gaddafi, Al-Senussi) treats it as admissibility. State practice (non-cooperation arguments) treats it as jurisdictional. The resolution changes the constraint''s structural boundary.',
    'If jurisdictional, the constraint is a scaffold (transitional, sunset via universal ratification). If admissibility, it is a tangled_rope (permanent hybrid). This reading authors admissibility; the sovereigntist reading authors jurisdictional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_nature, conceptual, 'Whether complementarity defines the ICC''s jurisdiction or its exercise.').

omega_variable(
    enforcement_dependence_structural_or_contingent,
    'Is the ICC''s dependence on state cooperation for enforcement a structural feature of international law (no supranational police) or a contingent design choice that could be remedied (e.g., by a standing UN police force)?',
    'Counterfactual analysis: if the Rome Statute had created an enforcement mechanism (Art 43 UN Charter style), would the constraint''s extractiveness and suppression change? Historical record: the 1998 Conference rejected enforcement mechanisms as sovereignty violations. The dependence is structural to the treaty''s consent basis.',
    'If structural, the tangled_rope classification is stable — the coordination/extraction hybrid is inherent. If contingent, the constraint could evolve toward rope (with enforcement) or snare (without coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_dependence_structural_or_contingent, empirical, 'Whether the ICC''s enforcement gap is structural or remediable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_hybrid_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(rome_hybrid_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.22).
narrative_ontology:measurement(rome_hybrid_tr_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(rome_hybrid_tr_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(rome_hybrid_tr_t2015, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(rome_hybrid_tr_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(rome_hybrid_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(rome_hybrid_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(rome_hybrid_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.4).
narrative_ontology:measurement(rome_hybrid_be_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(rome_hybrid_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(rome_hybrid_be_t2015, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(rome_hybrid_be_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(rome_hybrid_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rome_hybrid_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement(rome_hybrid_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.4).
narrative_ontology:measurement(rome_hybrid_su_t2005, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(rome_hybrid_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(rome_hybrid_su_t2015, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(rome_hybrid_su_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(rome_hybrid_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.1).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_cooperation_regime).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_jurisdiction_domestic_courts).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, ad_hoc_tribunal_legacy).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_complementarity_reading of the rome_statute_jurisdiction kernel. The universalist_reading and sovereigntist_reading are sibling constraints. All three share the kernel's formal text but instantiate different constraints with different epsilon values, beneficiary/victim structures, and classifications. The hybrid reading's epsilon (0.65) reflects the operational reality of selective enforcement; the universalist reading's epsilon would be lower (coordination-dominant); the sovereigntist reading's epsilon would be higher (extraction-dominant from states' perspective).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.2).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerless, 0.85).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
