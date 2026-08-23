% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Control Regime (Freedom of Movement Primary Reading)
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   This constraint story represents the freedom_of_movement_primary reading
 *   of the border_control_legitimacy kernel. It assesses the
 *   actually-existing global border regime — the system of visa requirements,
 *   carrier sanctions, detention, deportation, pushbacks, and externalization
 *   — as a snare: pure extraction masquerading as coordination. The reading's
 *   foundational claim is that freedom of movement is a fundamental human
 *   right (UDHR Art. 13, ICCPR Art. 12) and territorial sovereignty entails
 *   only jurisdictional authority (regulating rights within territory), not
 *   exclusion authority. The regime's beneficiaries are state security
 *   apparatuses, enforcement agencies, political entrepreneurs, and detention
 *   contractors. Its victims are the displaced, asylum seekers, economic
 *   migrants, stateless persons, border communities, and mixed-status
 *   families. The regime has no sunset clause, requires active enforcement,
 *   and shows rising extractiveness and suppression over 76 years. The
 *   founding problem (post-war displacement management) is dead; the regime
 *   persists as zombie extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.87).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.91).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.87).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Control Regime (Freedom of Movement Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political/legal/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '6b3d8c6d-92c1-43cd-9929-ba40b48e0466').
narrative_ontology:cs_kernel_codification('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', formalized).
narrative_ontology:cs_authority_grounding('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', lineage).
narrative_ontology:cs_interpretation_layer_present('6b3d8c6d-92c1-43cd-9929-ba40b48e0466').
narrative_ontology:cs_reading_relation('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', foundational, freedom_of_movement_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', freedom_of_movement_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', foundational, territorial_sovereignty_jurisdictional_authority_only).
narrative_ontology:cs_axiom_status(territorial_sovereignty_jurisdictional_authority_only, holdable).
narrative_ontology:cs_axiom_grounding('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', territorial_sovereignty_jurisdictional_authority_only, deontological).
narrative_ontology:cs_axiom('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', secondary, non_refoulement_peremptory_norm).
narrative_ontology:cs_axiom_status(non_refoulement_peremptory_norm, holdable).
narrative_ontology:cs_axiom_grounding('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', non_refoulement_peremptory_norm, conventional).
narrative_ontology:cs_reference_frame('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', postwar_human_rights_settlement).
narrative_ontology:cs_drift_state('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', contemporary_border_externalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b3d8c6d-92c1-43cd-9929-ba40b48e0466', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, xenopolitical_entrepreneurs).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, detention_industry_operators).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, economic_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, stateless_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, border_community_residents).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, mixed_status_families).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, freedom_of_movement_as_fundamental_human_right).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, territorial_sovereignty_as_jurisdictional_authority_only).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, non_refoulement_as_peremptory_norm).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, human_dignity_as_border_invariant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fleeing persecution, conflict, or environmental collapse; blocked by visa regimes, carrier sanctions, and pushback operations; face detention, refoulement, or death at borders. No legal pathway exists for most; exit from the constraint means remaining in zones of danger.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons, payer,
    powerless, biographical, trapped, global).

% Present at borders claiming protection under Refugee Convention; met with deterrence policies (detention, offshore processing, safe third country transfers, meterling). The right to seek asylum exists in law but is structurally nullified by non-entrée policies.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Move for livelihood; channeled into irregular pathways by absence of work visas matching labor demand; exploited by employers who leverage deportability; remit billions while denied social protections. Exit options limited to dangerous irregular routes or staying in origin-country poverty.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, economic_migrants, payer,
    moderate, biographical, constrained, global).

% Denied nationality by any state; cannot enter, cannot stay legally, cannot leave. Border control renders them permanently rightless. The constraint operates on them as total exclusion — no jurisdiction claims them, all jurisdictions exclude them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, stateless_persons, payer,
    powerless, generational, trapped, global).

% Live in militarized border zones; subject to checkpoints, surveillance, racial profiling, property seizure for wall construction; cross-border kinship and economic ties severed. Bear costs of enforcement without political voice in its design.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_community_residents, payer,
    moderate, biographical, constrained, local).

% Citizen and non-citizen family members separated by enforcement; citizen children inherit parent's precarity; forced to choose between family unity and legal status. Identity-locked because exit means abandoning kin or citizenship.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, mixed_status_families, payer,
    powerless, biographical, identity_locked, national).

% Designs and administers border regime; controls visa policy, enforcement priorities, detention infrastructure, data systems. Justifies regime as security necessity; extracts budgetary resources and political legitimacy from border theater. Can reform or dismantle the constraint but chooses expansion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive expanding budgets, personnel, and authority; unionize to protect institutional interests; lobby for harsher laws that grow their mission. Career advancement tied to enforcement metrics (apprehensions, removals). Exit is easy — transfer to other security agencies — but they benefit from regime persistence.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies, beneficiary,
    organized, biographical, mobile, national).

% Politicians and media figures who leverage anti-migrant rhetoric for electoral gain and audience capture. Extract political capital from border enforcement spectacle; no operational role in the constraint but depend on its salience. Can exit the discourse at will but choose to amplify it.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, xenopolitical_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, global).

% Private contractors running immigration detention centers; paid per detainee-day; lobby for mandatory detention quotas and expanded enforcement. Direct financial extraction from the constraint's operation. Exit options include pivoting to carceral or security contracts.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, detention_industry_operators, beneficiary,
    organized, biographical, mobile, national).

% NGOs, lawyers, activists litigating and campaigning for rights; systematically locked out of policy design; their evidence dismissed as 'activism'; face criminalization of solidarity (search-and-rescue prosecutions, harboring laws). Would object to the regime's legitimacy but are not seated at the table.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migration_advocates, excluded,
    organized, biographical, mobile, global).

% Analyze the regime's conformity with international law; document the gap between treaty obligations and state practice; provide expert testimony. Neither collect nor pay; their seat is the analytical benchmark against which the constraint's claimed legitimacy is measured.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine. The regime claims to coordinate orderly migration, security, and sovereign equality — but the coordination story is cover. The actual function is exclusionary sorting: determining which lives are grievable and which are disposable. No collective-action problem is solved by preventing a Guatemalan mother from crossing the Rio Grande; the 'problem' is constructed to justify the sorting.
% TRANSFER_FUNCTION: Moves freedom of movement, bodily autonomy, life chances, and labor value from displaced persons, asylum seekers, economic migrants, stateless persons, and border communities to state security apparatus, border enforcement agencies, xenopolitical entrepreneurs, and detention industry operators. The transfer is enforced by walls, cages, drones, databases, and the threat of violence.
% ABSENT_VOICES: The displaced themselves — those who would move but cannot, those who die in the Sonoran Desert or the Mediterranean, those detained in Libya or Nauru, those born stateless in the Dominican Republic or Myanmar. They are structurally excluded by the very constraint that governs them; their exclusion is the constraint's operating principle. Also absent: future generations who will inherit a world partitioned by birthright lottery.
% DISAPPEARANCE_RATIONALE: If border closure authority vanished overnight, hundreds of millions would exercise freedom of movement within years. Global labor markets would reorganize around human need rather than birthplace privilege. Remittance flows would dwarf current aid. State legitimacy would need new foundations beyond territorial exclusion. The Westphalian order would face its deepest crisis since 1648 — but the rearrangement would be toward human rights fulfillment, not chaos.
% FOUNDING_PROBLEM: Post-WWII state system needed to manage 40+ million displaced Europeans and prevent conflict-driven population transfers; border control was framed as orderly migration management under the new UN system. The 1951 Refugee Convention and 1948 UDHR Article 13 established freedom of movement as the norm, with border control as a limited, temporary exception for post-war stabilization.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR historical records confirm the post-war displacement crisis was the stated justification for the temporary exception regime. Migration scholars (Massey, Castles, Koser, Czaika) document the regime's steady expansion far beyond its founding scope — from 2% of global population under control in 1950 to near-universal visa regimes today. No disinterested authority attests the current regime serves its founding purpose; even the World Bank acknowledges migration restrictions are the single largest source of global economic inefficiency.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.87) because the regime transfers the most fundamental liberty — movement — from the powerless to the powerful, with no compensation and no consent. Suppression is extreme (0.91) because the regime deploys military-grade force against unarmed civilians (walls, drones, patrol boats, detention camps) and its persistence depends entirely on suppressing exit (visa regimes, carrier sanctions, non-entrée policies). Theater ratio is moderate (0.38) because some enforcement activity genuinely interdicts trafficking and smuggling, but the vast majority serves exclusion. Accessibility collapse is high (0.82) because for most of humanity, the alternative (free movement) is not just difficult but legally impossible — the constraint has collapsed the alternative into unthinkability. Resistance is high (0.76) because migrants, advocates, and communities continuously resist through irregular movement, litigation, sanctuary, and protest — the regime must constantly escalate to maintain itself.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (displaced, stateless, mixed-status families) experience the constraint as total extraction with no coordination benefit — their classification is snare. The agenda_setter seat (state security apparatus) experiences it as necessary coordination with acceptable costs — its classification would be rope or tangled_rope. The beneficiary seats (enforcement agencies, detention contractors) experience it as pure gain — their classification is meaningless (they don't classify). The observer seat sees the structural asymmetry: the same physical infrastructure (walls, databases, laws) is experienced as cage by some and career by others. The engine computes this divergence from the declared power/exit/role structure; the authored claimed_type (snare) reflects the reading's assessment that the payer seat's experience is the truth of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced persons, asylum seekers, economic migrants, stateless persons, border communities, and mixed-status families are structural payers (d → 1.0): they bear the full cost of the regime with zero benefit and trapped/constrained exit. State security apparatus is agenda_setter (d → 0.0): it designs the regime, collects its budgetary and legitimacy rents, and can exit by reforming it. Border enforcement agencies and detention operators are beneficiaries (d → 0.1-0.2): they collect salaries, budgets, contracts from the regime with mobile exit. Xenopolitical entrepreneurs are beneficiaries (d → 0.0): they extract political capital with arbitrage exit. Migration advocates are excluded (d undefined): they would pay if included but are structurally locked out. Legal scholars are observers (d = 0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war European displacement) was real, time-bounded, and resolved by 1960. The regime not only persisted but expanded globally, converting a temporary exception into a permanent planetary system. The mandate atrophied; the extraction remained. The constraint now serves no coordination function that requires exclusion — Schengen proves coordination without exclusion works. The persistence is pure mandatrophy: the regime exists because the apparatus that benefits from it has the power to maintain it, and the victims lack the power to dismantle it. The 'sovereignty' justification is the cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right_ambiguity,
    'Is freedom of movement a genuine natural law (Mountain) that border control violates, or a constructed right whose violation merely reveals power?',
    'Cross-cultural historical analysis: if every society independently develops freedom-of-movement norms when not coerced, it tracks natural law. If it appears only in specific liberal-legal traditions, it is constructed. Also: does the right persist as a claim even when unenforceable (signature of Mountain)?',
    'If Mountain: the constraint is a false summit (natural law violated by construct) — FSM signature triggers, reclassification to tangled_rope. If constructed: the constraint is a snare whose extraction is measured against a contingent normative baseline — classification stands but referent shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right_ambiguity, conceptual, 'Whether the reading''s foundational axiom tracks natural law or liberal construction.').

omega_variable(
    coordination_function_existence,
    'Does the border regime perform ANY genuine coordination function (disease surveillance, trafficking interdiction, tax administration) that would survive open borders, or is the coordination story entirely fictive?',
    'Counterfactual analysis: which border functions require exclusion vs. which require only registration/regulation? Compare Schengen internal borders (coordination without exclusion) to external borders. Empirical test: do states with open internal borders lose coordination capacity?',
    'If genuine coordination exists: claimed_type shifts from snare to tangled_rope (hybrid). If entirely fictive: snare classification holds. Also affects Boltzmann coordination_type assignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_existence, empirical, 'Whether any non-extractive coordination function is structurally necessary to the regime.').

omega_variable(
    reading_relation_to_sovereignty_primary,
    'Does this reading FORECLOSE the sovereignty_primary reading (mutual logical impossibility in one framework), or merely COEXIST with it as competing political positions?',
    'Constitutional theory test: can a single legal system simultaneously hold ''freedom of movement is fundamental'' and ''states have absolute exclusion authority''? If the constitution guarantees both, the conflict is resolved by hierarchy (fundamental right trumps). If no hierarchy, the framework is incoherent — one reading must foreclose.',
    'If forecloses: cs_structure.reading_relations = forecloses; the kernel admits no stable compromise. If coexists_with: the kernel is permanently contested terrain with no structural resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_to_sovereignty_primary, conceptual, 'Structural relationship between freedom-of-movement-primary and sovereignty-primary readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the regime''s suppression structural (walls, laws, drones) or internalized (migrants self-excluding, aspiring migrants staying put because ''it''s not for people like me'')?',
    'Post-exit suppression trajectory: if suppression persists after physical barriers are removed (e.g., Schengen internal borders), reclassify as partially internalized. Survey aspiring migrants on perceived vs. actual barriers.',
    'If internalized component is large: effective suppression > measured structural suppression; the constraint''s reach extends beyond its enforcement apparatus. Classification may understate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in border control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t1948, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t1960, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t1975, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t1990, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t2001, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2001, 0.33).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t2015, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_tr_t2024, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t1948, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t1960, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t1975, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t2001, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2001, 0.78).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t2015, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_be_t2024, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2024, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t1948, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t1960, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t1975, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t2001, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t2015, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(border_control_legitimacy__freedom_of_movement_primary_su_t2024, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__freedom_of_movement_primary, 0.05).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This story, sovereignty_primary, and jurisdictional_sovereignty form the border_control_legitimacy constraint family. They share the kernel 'border_control_legitimacy' but instantiate different constraints with different ε values, different victim/beneficiary structures, and different classifications. This reading (freedom_of_movement_primary) has high ε (0.87), snare classification, displaced persons as victims. Sovereignty_primary has low ε (0.15), mountain/tangled_rope classification, state as beneficiary. Jurisdictional_sovereignty has medium ε (0.45), tangled_rope classification, mixed victims/beneficiaries. The ε-invariance principle requires separate stories because the same label 'border control' covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, institutional, 0.05).
constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, organized, 0.15).
constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, powerful, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
