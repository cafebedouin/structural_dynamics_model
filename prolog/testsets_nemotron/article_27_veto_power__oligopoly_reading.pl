% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council P5 Veto — Oligopoly Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council veto (Art. 27(3)) is conventionally framed as a
 *   coordination mechanism: by giving each permanent member a blocking vote,
 *   it ensures no resolution can compel a nuclear-armed great power into
 *   unwanted war, thereby keeping all P5 inside the collective security
 *   system. This constraint story instantiates the oligopoly_reading of the
 *   Art. 27 veto kernel — the reading that the veto's actual structural
 *   function is entrenching a 1945 geopolitical oligopoly. The veto extracts
 *   ongoing authority rents (agenda control, reform blockade, legitimizing
 *   cover) for the P5 while suppressing exit and reform pathways for the 188
 *   non-P5 members. The Charter's amendment formula (Art. 108) requires P5
 *   ratification, making the constraint self-entrenching. Measured extraction
 *   has risen steadily as the geopolitical distance between 1945 power
 *   realities and 2025 realities has widened — the veto's coordination cover
 *   has thinned while its extraction function has hardened.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.82).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council P5 Veto — Oligopoly Reading").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'b03cd5b3-53f4-4cde-9014-cb7ab103d508').
narrative_ontology:cs_kernel_codification('b03cd5b3-53f4-4cde-9014-cb7ab103d508', formalized).
narrative_ontology:cs_authority_grounding('b03cd5b3-53f4-4cde-9014-cb7ab103d508', extraction).
narrative_ontology:cs_interpretation_layer_present('b03cd5b3-53f4-4cde-9014-cb7ab103d508').
narrative_ontology:cs_reading_relation('b03cd5b3-53f4-4cde-9014-cb7ab103d508', article_27_veto_power__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('b03cd5b3-53f4-4cde-9014-cb7ab103d508', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('b03cd5b3-53f4-4cde-9014-cb7ab103d508', foundational, veto_as_oligopoly_entrenchment).
narrative_ontology:cs_axiom_status(veto_as_oligopoly_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('b03cd5b3-53f4-4cde-9014-cb7ab103d508', veto_as_oligopoly_entrenchment, empirically_contingent).
narrative_ontology:cs_axiom('b03cd5b3-53f4-4cde-9014-cb7ab103d508', foundational, charter_immutability_as_extraction_tool).
narrative_ontology:cs_axiom_status(charter_immutability_as_extraction_tool, holdable).
narrative_ontology:cs_axiom_grounding('b03cd5b3-53f4-4cde-9014-cb7ab103d508', charter_immutability_as_extraction_tool, empirically_contingent).
narrative_ontology:cs_axiom('b03cd5b3-53f4-4cde-9014-cb7ab103d508', secondary, great_power_participation_problem_dead).
narrative_ontology:cs_axiom_status(great_power_participation_problem_dead, holdable).
narrative_ontology:cs_axiom_grounding('b03cd5b3-53f4-4cde-9014-cb7ab103d508', great_power_participation_problem_dead, empirically_contingent).
narrative_ontology:cs_reference_frame('b03cd5b3-53f4-4cde-9014-cb7ab103d508', id_1945_concert_of_great_powers).
narrative_ontology:cs_drift_state('b03cd5b3-53f4-4cde-9014-cb7ab103d508', contemporary_multipolar_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b03cd5b3-53f4-4cde-9014-cb7ab103d508', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_foreign_ministries).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_defense_establishments).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_south_coalition).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, conflict_affected_populations).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, multilateral_reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the veto power individually and collectively control Charter amendment (Art. 108/109). Use the veto to block resolutions that threaten strategic interests, shape mandate language for peace operations, and extract diplomatic concessions in exchange for non-use. No exit cost — they are the constraint's authors and enforcers. Collect authority rents: agenda control, legitimizing cover for unilateral action, and institutional veto over reform.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% Operationalize the veto daily — draft veto threats, negotiate P5 statements, trade veto cover for concessions in other forums. Their bureaucratic interests align with preserving the veto as a career-structuring asset: the UN desk is a premier posting because the veto makes it matter. Exit is arbitrage-grade: they rotate to other capitals carrying the veto credential.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_foreign_ministries, beneficiary,
    institutional, biographical, arbitrage, global).

% The veto shields nuclear postures and force-projection doctrines from multilateral constraint. A resolution authorizing force against a P5 ally or interest is blocked at source. This is not coordination — it is a structural guarantee that the Security Council never becomes a vehicle for constraining P5 military autonomy. Exit is irrelevant; they are the hard-power backing of the constraint.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_defense_establishments, beneficiary,
    institutional, generational, arbitrage, global).

% Comprise 188 of 193 UN members. Bound by Chapter VII decisions they cannot veto and cannot amend the Charter to change (Art. 108 requires P5 ratification). Bear the costs: peacekeeping mandates shaped by P5 interests, sanctions regimes applied asymmetrically, protection responsibilities vetoed when P5 clients are involved. Exit is structurally blocked — withdrawal forfeits all UN platform access and treaty privileges; no alternative security architecture exists.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    organized, generational, trapped, global).

% G77, NAM, African Group, LDCs — coordinate reform demands (permanent seats, veto restraint, expansion) for decades. Their collective weight is numerical, not structural: the General Assembly has no binding authority over the Council. Exit options are constrained — they can withdraw from specific treaties or form parallel institutions (BRICS, Shanghai Cooperation Org), but the UN remains the only universal legitimacy forum. Their exclusion is the constraint's product: the veto exists to ensure their demands never become binding.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_south_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, global_south_coalition, excluded).

% Civilians in Syria, Ukraine, Gaza, Myanmar, Sudan — where P5 vetoes have blocked ceasefires, humanitarian access, accountability mechanisms, or peace operations. They do not participate in the UN; they suffer its paralysis. Exit is impossible — they are the human substrate the constraint extracts legitimacy from while failing to protect. Their situation is the empirical referent for the constraint's extraction.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, conflict_affected_populations, payer,
    powerless, immediate, trapped, local).

% Scholars, NGOs (Global Governance Forum, Stimson Center), former diplomats (Elders, Accountability, Coherence & Transparency Group) who document veto abuse and propose restraint codes (French/Mexican initiative, ACT Code). Their exit is mobile — they can shift to other issues — but their exclusion is structural: the veto power they critique is the same power that decides whether their proposals are ever voted on.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, multilateral_reform_advocates, excluded,
    moderate, biographical, mobile, global).

% Sees the full structure: a 1945 power settlement frozen into constitutional form, using the Charter's own amendment rules to make itself unamendable. The veto is not a coordination mechanism that degraded — it was designed as oligopoly insurance and has functioned as such for 80 years. The coordination story (preventing great-power war) is the cover; the extraction (blocking redistribution of authority) is the function.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint CLAIMS to coordinate great-power management by giving each nuclear-armed permanent member a blocking vote, ensuring no Security Council resolution can compel a P5 state into unwanted military confrontation. This is the coordination_reading's framing — the oligopoly_reading holds this is post-hoc cover.
% TRANSFER_FUNCTION: Moves institutional authority — agenda control, legitimizing cover for unilateral action, veto over reform — from the collective membership (193 states) to the P5 oligopoly (5 states). The transfer is structural and continuous: every veto exercised, every reform blocked, every mandate shaped by P5 consent redistributes authority upward. The Charter's amendment formula (Art. 108) makes the transfer irreversible without P5 consent.
% ABSENT_VOICES: The 188 non-P5 member states — especially the Global South majority — are structurally excluded from the veto power and from the amendment gate. Conflict-affected populations have no voice at all. Their absence is not accidental; the constraint's persistence depends on their exclusion. If they were in the room with equal voice, the veto would not survive.
% DISAPPEARANCE_RATIONALE: If the P5 veto vanished overnight, the Security Council would become a majority-rule body. Resolutions on Syria, Ukraine, Palestine, Myanmar would pass. Peace operations would deploy without P5 permission. The Charter amendment gate would open — expansion, veto restraint, new permanent seats would become achievable. The global security architecture would reorganize around majority legitimacy rather than great-power consent. The P5 would lose their structural guarantee against multilateral constraint.
% FOUNDING_PROBLEM: 1945: preventing a repeat of the League of Nations' failure by ensuring great powers would not leave or be compelled into war by a majority they could not control. The veto was the price of P5 participation — without it, the UN would have been stillborn or immediately irrelevant to the only states capable of global war.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — great-power non-participation — is dead. The P5 are deeply embedded in the UN system; none has left, none threatens to leave. The veto no longer secures participation; it secures dominance. Corroboration: the P5's own behavior (routine veto use on non-existential issues, veto threats as diplomatic currency) and the historical record (zero P5 withdrawals, zero vetoes cast to prevent great-power war since 1945 — vetoes are cast to protect clients, interests, and narratives). The coordination_reading and sovereignty_reading contest this status from within their own frameworks; no external corroboration supports 'live'.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.82) is high because the veto transfers binding authority from the collective to the oligopoly continuously — every veto exercised, every reform blocked, is a rent payment. Suppression (0.88) is higher: the constraint's persistence depends on actively blocking Charter amendment (Art. 108), suppressing reform coalitions (Uniting for Consensus vs. G4), and maintaining the normative fiction that the veto is a coordination necessity rather than an extraction mechanism. Theater (0.65) is substantial: the 'great-power war prevention' narrative is performed in every General Assembly debate on reform, but the veto's actual use pattern (protecting clients, shielding allies, blocking accountability) contradicts it. Accessibility collapse (0.92) is near-total: no non-P5 state has a path to veto power or Charter amendment without P5 consent. Resistance (0.78) is high and sustained: 80 years of reform proposals, veto restraint initiatives, General Assembly resolutions — all blocked by the same constraint they target.
 *
 * PERSPECTIVAL GAP:
 *   The coordination_reading and sovereignty_reading experience this constraint as mountain or rope — necessary, legitimate, stabilizing. The oligopoly_reading experiences it as snare — extractive, suppressive, self-entrenching. The engine computes per-seat classification from the structural data: the P5 seats will compute as beneficiaries (low χ), the non-P5 seats as payers (high χ), the conflict-affected as victims (maximum χ). The divergence IS the measurement — a single constraint producing diametrically opposed classifications across seats is the signature of a snare masquerading as a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 permanent members are structural beneficiaries (d near 0.0) — they collect the authority rents, control the amendment gate, and face zero exit cost. P5 foreign ministries and defense establishments are subsidiary beneficiaries (d ~ 0.1) — their institutional interests are served by the veto's maintenance. Non-P5 member states are structural targets (d near 1.0) — bound by Chapter VII, excluded from the veto, blocked from amendment. Global South coalitions are trapped targets with constrained exit (d ~ 0.9) — numerical majority, structural impotence. Conflict-affected populations are pure victims (d = 1.0) — they bear the human cost of veto paralysis with zero voice. Reform advocates are excluded observers (mobile exit) — they document the extraction but cannot access the lever that would change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (great-power participation) is dead; the arrangement persists and has expanded its extraction function. The veto no longer solves the problem it was built for — it solves a different problem: how to keep the 1945 winners as permanent institutional landlords. This is classic mandatrophy: the mandate has outlived its function, but the constraint remains because the beneficiaries control the amendment gate. The theater_ratio rise (0.25 → 0.65) tracks the cover story's thickening as the extraction becomes more visible. The constraint is not a degraded rope (piton) — it is an active, enforced snare whose extraction has grown over 80 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_use_vs_war_prevention,
    'How many P5 vetoes since 1945 were actually cast to prevent great-power war, versus protecting clients, interests, or narratives?',
    'Systematic coding of all veto episodes (1946-present) by stated justification and structural effect. Compare vetoes on Chapter VII enforcement actions vs. vetoes on condemnations, investigations, mandate renewals.',
    'If near-zero vetoes prevented great-power war, the coordination_reading''s foundational claim is empirically falsified — the veto''s actual use pattern is oligopolistic extraction, not war prevention. This would foreclose the coordination_reading''s axiom_grounding_type = empirically_contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_use_vs_war_prevention, empirical, 'Whether the veto''s empirical use pattern matches its coordination cover story.').

omega_variable(
    art108_amendment_gate_blockage,
    'Has any Charter amendment affecting P5 privileges ever passed without P5 consent? Has any reform proposal reached the Art. 108 ratification stage?',
    'Legal-historical review of all Charter amendments (1945-present) and all reform proposals that reached General Assembly vote. Document the P5 ratification veto at each stage.',
    'If zero amendments affecting P5 privileges have passed without P5 consent, the amendment gate is a structural suppression mechanism, not a procedural hurdle. This confirms the constraint''s self-entrenchment and elevates suppression from ''high'' to ''structural totality''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(art108_amendment_gate_blockage, empirical, 'Whether the Charter''s own amendment formula functions as a permanent P5 veto over reform.').

omega_variable(
    coordination_extraction_separability,
    'Could a Security Council without veto (or with qualified veto) still prevent great-power war? Is the coordination function separable from the oligopoly extraction?',
    'Counterfactual modeling: great-power war incidence in veto vs. non-veto scenarios; analysis of whether P5 participation requires veto or whether lesser incentives (prestige, platform, burden-sharing) suffice.',
    'If coordination is separable from veto, the extraction is pure rent — the oligopoly_reading''s extraction claim is validated. If inseparable, part of measured ε is the price of coordination (Tangled Rope territory). This is the core structural ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the veto''s coordination and extraction components are structurally separable.').

omega_variable(
    committer_frame_underdetermination,
    'Does the oligopoly_reading foreclose the coordination_reading within a single framework, or do they coexist as competing legitimizing narratives for different audiences?',
    'Analyze whether a single actor (e.g., a P5 foreign ministry) can simultaneously hold both readings as operational frames, or whether adopting the oligopoly_reading''s structural critique logically commits one to rejecting the coordination_reading''s foundational premise.',
    'If forecloses: the readings are mutually exclusive within a single commitment framework — a P5 diplomat who sees the veto as oligopoly extraction cannot simultaneously treat it as war-prevention coordination. If coexists_with: both readings operate as live positions in different institutional contexts (public rhetoric vs. internal assessment). This determines the reading_relations declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, conceptual, 'Structural relationship between the oligopoly_reading and its sibling readings — forecloses, coexists_with, or influences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a27_oligopoly_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(a27_oligopoly_tr_t1960, article_27_veto_power__oligopoly_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(a27_oligopoly_tr_t1975, article_27_veto_power__oligopoly_reading, theater_ratio, 1975, 0.45).
narrative_ontology:measurement(a27_oligopoly_tr_t1990, article_27_veto_power__oligopoly_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement(a27_oligopoly_tr_t2005, article_27_veto_power__oligopoly_reading, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(a27_oligopoly_tr_t2025, article_27_veto_power__oligopoly_reading, theater_ratio, 2025, 0.65).

% Extraction over time
narrative_ontology:measurement(a27_oligopoly_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(a27_oligopoly_be_t1960, article_27_veto_power__oligopoly_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(a27_oligopoly_be_t1975, article_27_veto_power__oligopoly_reading, base_extractiveness, 1975, 0.61).
narrative_ontology:measurement(a27_oligopoly_be_t1990, article_27_veto_power__oligopoly_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(a27_oligopoly_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(a27_oligopoly_be_t2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(a27_oligopoly_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(a27_oligopoly_su_t1960, article_27_veto_power__oligopoly_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(a27_oligopoly_su_t1975, article_27_veto_power__oligopoly_reading, suppression_requirement, 1975, 0.78).
narrative_ontology:measurement(a27_oligopoly_su_t1990, article_27_veto_power__oligopoly_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(a27_oligopoly_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.86).
narrative_ontology:measurement(a27_oligopoly_su_t2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_108_charter_amendment).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, security_council_reform_proposals).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, general_assembly_uniting_for_peace).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, veto_restraint_initiatives).

% DUAL FORMULATION NOTE:
% This story is the oligopoly_reading of the article_27_veto_power kernel. The coordination_reading (veto as war-prevention mechanism) and sovereignty_reading (veto as Westphalian consent instantiation) are sibling constraints with different ε, different beneficiary/victim structures, and different claimed_types. The oligopoly_reading's ε (0.82) derives from the same Charter text but a different structural assessment: the veto's actual operation, not its stated purpose. The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, organized, 0.85).
constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
