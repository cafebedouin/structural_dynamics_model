% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Heller/Bruen Line)
 *   domain: constitutional_law/firearms_policy
 *
 * SUMMARY:
 *   This constraint story captures the individual-right reading of the Second
 *   Amendment operative clause ('the right of the people to keep and bear
 *   arms shall not be infringed') as crystallized in District of Columbia v.
 *   Heller (2008), incorporated in McDonald v. Chicago (2010), and expanded
 *   in New York State Rifle & Pistol Association v. Bruen (2022). The reading
 *   treats the prefatory militia clause as merely announcing a purpose, not
 *   limiting the operative clause's guarantee. Personal self-defense —
 *   especially in the home — is the core protected activity. The constraint
 *   is the legal rule-set that flows from this reading: categorical
 *   protection for common-use arms, history-and-tradition test for
 *   regulations, strong presumption against permitting regimes. It is a
 *   tangled rope: genuine coordination (provides a stable, enforceable floor
 *   for self-defense rights, resolves circuit splits) coexists with
 *   asymmetric extraction (categorical disarmament of felons and domestic
 *   abusers without individualized hearings, externalization of mortality
 *   risk onto gun violence victims, regulatory capture of state police power
 *   by judicial history test).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Heller/Bruen Line)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a').
narrative_ontology:cs_kernel_codification('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', fixed_text).
narrative_ontology:cs_authority_grounding('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', lineage).
narrative_ontology:cs_interpretation_layer_present('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a').
narrative_ontology:cs_reading_relation('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', foundational, individual_self_defense_core).
narrative_ontology:cs_axiom_status(individual_self_defense_core, holdable).
narrative_ontology:cs_axiom_grounding('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', individual_self_defense_core, deontological).
narrative_ontology:cs_axiom('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', foundational, operative_clause_independent).
narrative_ontology:cs_axiom_status(operative_clause_independent, holdable).
narrative_ontology:cs_axiom_grounding('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', operative_clause_independent, conventional).
narrative_ontology:cs_reference_frame('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', heller_mcdonald_framework).
narrative_ontology:cs_drift_state('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', post_bruen_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f4cb8f95-d16c-4a61-b5fa-37b7d9a0e26a', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_orgs).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_possessors_felons).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_possessors_domestic_abusers).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, residents_in_restrictive_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, gun_violence_victims).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, individual_self_defense_right).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, operative_clause_independence_from_militia).
narrative_ontology:constraint_vindicates(second_amendment_text__individual_right_reading, textual_originalism_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally protected right to possess firearms for self-defense in the home (Heller) and carry in public (Bruen). Benefit from judicial invalidation of bans and restrictive permitting. Exit options limited by state-level variation — can relocate but face federal floor. Organized through advocacy groups (NRA, GOA, FPC) that litigate and lobby.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Litigate to expand the right, fund test cases, grade legislators, mobilize voters. Capture the institutional agenda on firearms policy — set the terms of debate, choose the courts, frame the history. Collect membership dues and donations that scale with perceived threat level. Exit is arbitrage-grade: can shift venues (state vs federal), forums (legislative vs judicial), and narratives.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_orgs, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, gun_rights_advocacy_orgs, agenda_setter).

% Commercial beneficiary of expanded legal markets — every invalidated restriction increases addressable market. Funds advocacy directly and through trade associations (NSSF). Protected by PLCAA from most tort liability. Exit is mobile: can shift product lines, distribution channels, and marketing emphasis across jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, mobile, national).

% Categorically barred from firearm possession under federal law (18 U.S.C. § 922(g)(1)) and state analogues, regardless of offense nature, time elapsed, or rehabilitation. Violation carries mandatory minimums. No individualized relief mechanism in most jurisdictions. Trapped by status — exit requires executive clemency or judicial restoration, both rare and politicized.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_possessors_felons, payer,
    powerless, biographical, trapped, national).

% Barred under 18 U.S.C. § 922(g)(8)-(9) based on protective orders or misdemeanor convictions. Rahimi (2024) upheld the prohibition but narrowed to dangerousness findings. Still categorical in practice — no individualized hearing in many states. Trapped by relationship status and court orders they cannot unilaterally modify.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_possessors_domestic_abusers, payer,
    powerless, biographical, trapped, national).

% Subject to permitting regimes (may-issue → shall-issue post-Bruen, but sensitivity/good-character clauses persist), assault weapon bans, magazine limits, waiting periods, registration. Bear compliance costs, delays, denials. Exit is constrained: can move but face employment, family, housing ties. Bruen's history-and-tradition test creates litigation pressure but compliance burden remains during pendency.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, residents_in_restrictive_jurisdictions, payer,
    moderate, biographical, constrained, regional).

% Bear the externalized harm of expanded legal access — shootings, suicides, domestic homicides, accidental discharges. No standing in the constitutional calculus of the right; their interests appear only in state police-power justifications that courts increasingly discount. Exit is structural: cannot opt out of the risk environment created by pervasive legal carry and ownership.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_violence_victims, payer,
    powerless, biographical, trapped, national).

% Enact and defend regulations (permitting, bans, sensitivity zones) under Bruen's history-and-tradition framework. Must analogize modern laws to founding-era analogues — a judicial test that constrains legislative creativity. Constrained exit: can repeal or amend but face primary challenges, preemption suits, and judicial invalidation.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_legislatures_restrictive, agenda_setter,
    institutional, biographical, constrained, regional).

% Adjudicates the scope of the right via history-and-tradition test (Bruen). Lower courts apply; Supreme Court sets the framework. Not a beneficiary in the extraction sense — collects no rents — but structurally administers the constraint. Exit is analytical: can only be changed by appointments, constitutional amendment, or court-packing.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Study firearm injury epidemiology, policy effects, prevention. Dickey Amendment legacy suppressed funding; post-2019 appropriations restored some. Provide evidence that the constraint's beneficiaries and agenda-setters routinely discount. Analytical exit — can publish, testify, but cannot compel policy uptake.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_health_researchers, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a judicially enforceable floor for individual firearm possession and carry, resolving the pre-Heller ambiguity where circuits split on individual vs collective right. Coordinates expectations of owners, police, prosecutors, and legislatures around a core self-defense entitlement.
% TRANSFER_FUNCTION: Transfers regulatory authority from legislatures to courts — the history-and-tradition test (Bruen) moves the locus of firearms policy from democratic majorities to judicial historical analogy. Transfers risk from state (disarmed populace) to individuals (gun violence victims, prohibited persons). Transfers compliance costs to residents in restrictive jurisdictions and prohibited possessors.
% ABSENT_VOICES: Gun violence victims and their communities — disproportionately Black and Brown urban populations — are structurally excluded from the constitutional calculus. The right's beneficiaries (organized gun owners) are predominantly white, rural/suburban, male. Future generations who will inherit the mortality burden have no voice. Public health apparatus was actively silenced (Dickey Amendment) for two decades.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, the collective security reading would become controlling. Permitting regimes would revert to may-issue or be eliminated entirely (no individual right to carry). Assault weapon bans, magazine limits, universal background checks, and registration would face no Second Amendment barrier. The firearms industry would lose its constitutional shield. Police would lose the 'armed citizen' assumption in encounter protocols. The entire regulatory architecture of the last 15 years would invert.
% FOUNDING_PROBLEM: Founding-era fear that the new federal government would disarm the state militias by prohibiting individual arms ownership, leaving states defenseless against federal tyranny and citizens unable to participate in the militia system. The right was understood as protecting the individual's capacity to keep and bear arms for militia service and personal self-defense as a corollary.
% FOUNDING_PROBLEM_CORROBORATION: Originalist historians outside the gun-rights movement (Jack Rakove, Saul Cornell, Carol Anderson) argue the founding problem was collective — ensuring state militia viability — not individual self-defense. The Heller majority's historical narrative has been contested in peer-reviewed journals (Fordham Law Review, Yale Law Journal) and in Bruen dissents. No consensus among historians; the corroborating sources for the individual-right framing are predominantly law-office history produced by advocates.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the constraint's dual nature: it provides real coordination value (clear right, judicial enforceability) but extracts heavily from prohibited possessors (lifetime bans without individualized review) and residents of restrictive jurisdictions (compliance costs, delays, denials). The mortality externality on gun violence victims is a structural extraction not priced into the constraint. Suppression (0.72) is high because the constraint's persistence depends on active judicial enforcement — lower courts applying Bruen's history-and-tradition test, striking laws, enjoining enforcement. Theater (0.38) is moderate: the history-and-tradition test has performative elements (cherry-picked analogues, law-office history) but produces real invalidations. Accessibility collapse (0.62) reflects that alternatives (moving, non-firearm self-defense, legislative reform) exist but are constrained. Resistance (0.75) is high: the reading faces sustained political, scholarly, and state-level resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (gun owners, advocacy orgs), the constraint appears as a genuine rope — a coordination mechanism securing a natural right against majoritarian erosion. From the payer seats (prohibited possessors, gun violence victims), it appears as a snare — categorical extraction enforced by state power with no exit. The agenda-setter seats (judiciary, restrictive legislatures) experience it as a tangled rope: they must administer a framework they did not choose, under rules (history-and-tradition) that constrain their discretion. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and advocacy orgs are structural beneficiaries (d ≈ 0.15-0.25): they collect the right's protection, control the litigation agenda, face minimal compliance burden. Firearms industry is a beneficiary with mobile exit (d ≈ 0.2). Prohibited possessors (felons, domestic abusers) are full targets (d ≈ 0.9-1.0): trapped by status, bear categorical bans with no relief. Residents in restrictive jurisdictions are constrained payers (d ≈ 0.6-0.7): bear compliance costs but have some mobility and political recourse. Gun violence victims are trapped payers (d ≈ 0.95): bear externalized mortality risk with zero voice in the constitutional calculus. State legislatures are constrained agenda-setters (d ≈ 0.5): they administer regulations but under judicial straitjacket. Federal judiciary is the analytical/administering seat (d ≈ 0.5). The derivation chain from beneficiary/victim declarations + exit options produces these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal disarmament of militias) is historically dead — the militia system has been replaced by the National Guard and standing military. Yet the arrangement persists and expands. The mandatrophy is not resolved: the constraint has colonized new domains (public carry, assault weapons, magazine limits) far beyond the founding concern. The coordination function (individual self-defense floor) is real but the extraction function has grown disproportionately. The history-and-tradition test acts as a ratchet: each new regulation must find a founding-era analogue, which becomes harder as technology and society diverge, so the constraint's extractive reach expands over time. This is classic mandatrophy — the original mandate is dead but the constraint not only persists but metastasizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the individual-right reading a genuine discovery of the Amendment''s original meaning, or a constructed doctrine serving contemporary political interests?',
    'Convergence of independent historical scholarship (not law-office history) on the founding-era understanding of ''the people,'' ''keep and bear arms,'' and the militia clause''s syntactic function.',
    'If constructed, the constraint''s claimed natural-law status (deontological axiom) collapses to conventional/instrumental — reclassifying the extraction as political choice rather than rights vindication.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading reflects original meaning or modern construction.').

omega_variable(
    naturalness_ambiguity,
    'Does the right protected by this reading exist as a pre-political natural right, or is it a positive-law creation of the constitutional text and judicial interpretation?',
    'Cross-jurisdictional comparison: do jurisdictions without a Second Amendment analogue (UK, Japan, Germany) lack the self-defense protections this reading treats as fundamental?',
    'If natural right, the constraint is a mountain (emerges_naturally) misclassified as tangled_rope. If positive law, the extraction metrics are correctly calibrated and the claimed_type is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_ambiguity, conceptual, 'Natural-right vs positive-law ontology of the protected activity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by prohibited possessors and restrictive-jurisdiction residents primarily structural (legal penalties, enforcement) or internalized (chilling effect, compliance culture, identity foreclosure)?',
    'Post-Bruen natural experiment: measure compliance rates, chilling effects, and self-censorship in jurisdictions where laws remain on books but are enjoined or unenforced.',
    'If substantially internalized, the constraint''s effective suppression exceeds the legal-penalty measure — the constraint operates through identity and norm internalization, not just state force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanisms for payer seats.').

omega_variable(
    history_tradition_test_coherence,
    'Is the Bruen history-and-tradition test a coherent, neutrally applicable legal standard, or an indeterminate method that produces results aligned with the judiciary''s policy preferences?',
    'Inter-circuit consistency analysis: do different circuits applying the same test reach the same results on identical regulations? Measure variance in historical analogue selection.',
    'If incoherent, the constraint''s coordination function degrades — the ''floor'' becomes a moving target, increasing theater_ratio and decreasing legitimacy. The constraint shifts toward piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(history_tradition_test_coherence, empirical, 'Coherence of the judicial test that administers the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 2008, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ir_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(sa_ir_tr_t2010, second_amendment_text__individual_right_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(sa_ir_tr_t2016, second_amendment_text__individual_right_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(sa_ir_tr_t2020, second_amendment_text__individual_right_reading, theater_ratio, 2020, 0.34).
narrative_ontology:measurement(sa_ir_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.37).
narrative_ontology:measurement(sa_ir_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(sa_ir_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(sa_ir_be_t2010, second_amendment_text__individual_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(sa_ir_be_t2016, second_amendment_text__individual_right_reading, base_extractiveness, 2016, 0.48).
narrative_ontology:measurement(sa_ir_be_t2020, second_amendment_text__individual_right_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(sa_ir_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement(sa_ir_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_ir_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.55).
narrative_ontology:measurement(sa_ir_su_t2010, second_amendment_text__individual_right_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(sa_ir_su_t2016, second_amendment_text__individual_right_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(sa_ir_su_t2020, second_amendment_text__individual_right_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(sa_ir_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement(sa_ir_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, federal_firearms_regulatory_framework).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, state_permit_regimes).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, red_flag_laws).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, assault_weapon_bans).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, universal_background_check_mandates).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the second_amendment_text constraint family. The three readings decompose the Amendment's colloquial label into structurally distinct claims with different ε, different beneficiary/victim sets, and different types. The individual_right_reading has the highest extractiveness (0.58) because it categorically protects a broad class of arms and carries, disarms prohibited possessors without individualized review, and externalizes mortality risk. The collective_security_reading would have near-zero extractiveness (mountain-like) but is foreclosed by current precedent. The originalist_civic_virtue_reading sits between — lower extraction than individual_right (civic-duty framing implies collective responsibility) but higher than collective_security.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, institutional, 0.3).
constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
