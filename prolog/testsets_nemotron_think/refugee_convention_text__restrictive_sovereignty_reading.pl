% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The restrictive sovereignty reading of the 1951 Refugee Convention treats
 *   the treaty as a minimum floor that preserves maximum state discretion
 *   over admission and protection. It requires individualized persecution
 *   proof for 'well-founded fear', limits 'particular social group' to
 *   immutable characteristics with state awareness, excludes generalized
 *   violence and non-state persecution, and enables high-threshold
 *   admissibility screening and offshore processing. This reading has become
 *   dominant in state practice since the 1990s, driven by asylum deterrence
 *   policies. The constraint is the legal standard as applied — not the
 *   Convention text itself but this specific interpretive framework that
 *   narrows the protected class and empowers sovereign exclusion.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda_setter and beneficiary (institutional/arbitrage) — sets and benefits from the restrictive standard
 *   - asylum_seekers_excluded (five sub-groups): Primary payers/victims (powerless/trapped) — bear costs of denial with no exit
 *   - asylum_seekers_included: Beneficiary (powerless/constrained) — narrow group that receives protection under the reading
 *   - unhcr: Observer (institutional/analytical) — monitors and advocates but cannot enforce
 *   - destination_state_courts: Secondary agenda_setter (institutional/analytical) — operationalizes the reading in adjudication
 *   - offshore_processing_operators: Beneficiary (organized/mobile) — profits from enforcement architecture
 *   - civil_society_ngos: Excluded (organized/constrained) — advocates against the reading but structurally excluded from decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, snare).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '2c83c46d-3fd4-46ad-a37e-50f4a6bab207').
narrative_ontology:cs_kernel_codification('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', fixed_text).
narrative_ontology:cs_authority_grounding('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', lineage).
narrative_ontology:cs_interpretation_layer_present('2c83c46d-3fd4-46ad-a37e-50f4a6bab207').
narrative_ontology:cs_reading_relation('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', foundational, sovereign_discretion_primacy).
narrative_ontology:cs_axiom_status(sovereign_discretion_primacy, holdable).
narrative_ontology:cs_axiom_grounding('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', sovereign_discretion_primacy, conventional).
narrative_ontology:cs_axiom('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', foundational, individualized_persecution_requirement).
narrative_ontology:cs_axiom_status(individualized_persecution_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', individualized_persecution_requirement, conventional).
narrative_ontology:cs_axiom('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', foundational, immutable_characteristics_psg_limitation).
narrative_ontology:cs_axiom_status(immutable_characteristics_psg_limitation, holdable).
narrative_ontology:cs_axiom_grounding('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', immutable_characteristics_psg_limitation, conventional).
narrative_ontology:cs_reference_frame('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', state_centric_sovereignty_framework).
narrative_ontology:cs_drift_state('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', contemporary_migration_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c83c46d-3fd4-46ad-a37e-50f4a6bab207', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_operators).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_nonstate_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_gender_based).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_lgbtq).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_clan_based).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_included).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_primacy_in_migration).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, individualized_persecution_standard).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, immutable_characteristics_psg_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States interpret and implement the Convention through domestic asylum systems, setting admissibility criteria, conducting status determination, and operating removal/deportation machinery. They benefit from maximum discretion to control borders and limit protection obligations. Exit from the constraint would require treaty withdrawal or fundamental legal reform, which is politically costly but structurally available.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, beneficiary).

% Fleeing war, state collapse, or widespread violence but not individually targeted persecution. Under this reading, their claims are inadmissible because they cannot show individualized persecution. They bear the full cost of exclusion: return to danger, indefinite detention, or irregular onward movement. No effective exit from the constraint's reach.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Persecuted by non-state actors (gangs, militias, families, traffickers) where the state is unable or unwilling to protect. This reading requires state awareness/acquiescence for persecution to count, so their claims fail. They bear costs of denial with no alternative protection pathway.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_nonstate_persecution, payer,
    powerless, immediate, trapped, global).

% Women and girls fleeing gender-based violence (domestic abuse, honor violence, forced marriage, FGM) where the persecutor is private and the state fails to protect. The immutable-characteristics-with-state-awareness test excludes most gender-based claims. They bear costs of return to life-threatening situations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_gender_based, payer,
    powerless, immediate, trapped, global).

% LGBTQ+ individuals persecuted due to sexual orientation/gender identity in contexts where the state does not directly persecute but tolerates or fails to prevent private violence. The state-awareness requirement and narrow PSG construction exclude many. They bear extreme costs of denial.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_lgbtq, payer,
    powerless, immediate, trapped, global).

% Individuals targeted due to clan, tribal, or ethnic affiliation in non-state conflicts where persecution is collective but not state-directed. The individualized proof requirement and state-awareness test render their claims inadmissible. They bear costs of exclusion with no viable exit.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_clan_based, payer,
    powerless, immediate, trapped, global).

% The narrow group who meet the individualized persecution standard with state-aware persecutors and immutable-characteristic PSG claims. They receive protection under this reading, but their access depends on high-threshold adjudication and is vulnerable to further restriction. Exit from the constraint's protection is possible only by winning status, which is uncertain.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_included, beneficiary,
    powerless, biographical, constrained, global).

% The UN refugee agency advocates for expansive interpretation, monitors state compliance, and provides operational guidance. It has no enforcement power but shapes normative discourse. Its analytical seat sees the full structural pattern of exclusion. Exit is analytical — it cannot leave the regime but can document its drift.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr, observer,
    institutional, generational, analytical, global).

% Domestic courts adjudicate asylum claims applying the restrictive reading. They operationalize the individualized proof requirement, PSG limits, and state-awareness test. Some courts resist through purposive interpretation, but institutional pressure favors restriction. Their exit is analytical — they interpret within the framework.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, destination_state_courts, agenda_setter,
    institutional, biographical, analytical, national).

% Private contractors and host states running offshore processing centers (e.g., Nauru, Manus Island, Rwanda scheme). They receive state funding to detain and process asylum seekers excluded by the restrictive reading. They benefit financially from the constraint's enforcement architecture. Exit is mobile — they can bid for other contracts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_operators, beneficiary,
    organized, biographical, mobile, regional).

% Human rights NGOs, refugee legal aid providers, and advocacy groups that challenge the restrictive reading in courts and public discourse. They would object to the narrow victim set and offshore processing but are structurally excluded from decision-making. Their exit is constrained — they can litigate and advocate but cannot change the reading directly.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, civil_society_ngos, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a minimum legal floor for refugee protection that states can uniformly apply, preventing a race to the bottom where no state offers any protection. Coordinates burden-sharing expectations at a minimal level.
% TRANSFER_FUNCTION: Transfers protection obligations from states to excluded asylum seekers: states retain sovereign discretion to deny entry/status, while asylum seekers fleeing generalized violence, non-state persecution, and gender/LGBTQ+/clan-based harm bear the costs of denial (return to danger, detention, irregular migration). Offshore processing operators receive state funds to manage the excluded population.
% ABSENT_VOICES: The excluded asylum seeker groups themselves (generalized violence, non-state persecution, gender-based, LGBTQ+, clan-based) are physically absent from the adjudicative spaces where the restrictive reading is authored and applied. Their voices are filtered through NGOs and UNHCR but never directly shape the interpretive standard. States that would adopt expansive readings are pressured by restrictive peers.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished overnight, states would lose the legal cover for narrow adjudication and offshore processing. Asylum systems would face pressure to adopt broader protection standards, expanding the protected population dramatically. Offshore processing contracts would lose legal basis. The global migration governance architecture would reorganize around a higher protection floor.
% FOUNDING_PROBLEM: Post-WWII need for a predictable, state-consented framework to manage displaced persons without imposing unlimited obligations on sovereigns. The Convention was designed as a compromise: a minimum floor (non-refoulement, defined refugee) that preserved maximum state discretion on admission, integration, and burden-sharing.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Convention's drafting history (travaux préparatoires) which shows states explicitly rejecting unlimited obligations. However, UNHCR, human rights treaty bodies, and numerous domestic courts attest that the founding problem (state consent) has been weaponized to erode the Convention's humanitarian object and purpose. The corroboration is split: states cite the founding problem to justify restriction; international bodies cite the same history to argue for dynamic interpretation.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading systematically denies protection to large categories of forced migrants while states retain the Convention's legitimacy benefits. Suppression (0.85) is very high because the constraint's persistence depends on active enforcement: border controls, accelerated procedures, detention, offshore processing, and jurisprudential gatekeeping that excludes expansive interpretations. Theater ratio (0.42) reflects that the Convention's humanitarian language and non-refoulement core are maintained performatively while the restrictive reading hollows out their application. Accessibility collapse (0.72) is high because once the restrictive standard is understood, alternatives (expansive interpretation, complementary protection, regional frameworks) are legally and politically suppressed. Resistance (0.68) is substantial from UNHCR, courts, NGOs, and some states but has not reversed the drift.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign state seat, the reading appears as legitimate coordination (managing flows, preventing abuse). From the excluded asylum seeker seats, it operates as pure extraction (denial of protection with no alternative). The engine computes this divergence: the same legal text produces snare-class extraction for the excluded and rope-class coordination for states. The claimed_type 'snare' reflects the authoring seat's structural judgment — the coordination function is minimal and the extraction is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are structural beneficiaries (d near 0.0): they collect discretion, legitimacy, and reduced obligations. Asylum seekers excluded by the reading are full targets (d near 1.0): they bear extraction (denied protection) with trapped exit. Asylum seekers included are near-symmetric (d ~0.5): they get protection but face high adjudicatory barriers. UNHCR and courts are analytical (d ~0.5): they see the structure but operate within it. Offshore operators are beneficiaries (d low) but mobile. NGOs are excluded — their objection is the constraint's validation signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state consent for a minimum floor) is dead in its original form — the post-WWII displacement context is gone — but the arrangement persists and has intensified. The mandate has atrophied into a tool for sovereign exclusion. The constraint is not a piton (inertial remnant) because it is actively enforced and expanded (offshore processing, safe third country, accelerated procedures). It is a snare: the humanitarian cover story enables active extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_text_vs_state_practice,
    'Is the restrictive reading a faithful interpretation of the Convention text or a constructed constraint that benefits sovereign states?',
    'Comparative analysis of travaux préparatoires, subsequent state practice, and treaty body jurisprudence. If state practice consistently narrows the text beyond its ordinary meaning, the reading is constructed.',
    'If constructed, the constraint is a snare with states as beneficiaries; if faithful, it may be a rope (coordination on minimal terms) or mountain (treaty text as fixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_text_vs_state_practice, conceptual, 'Whether the restrictive reading reflects the Convention''s actual terms or a state-benefiting reinterpretation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of excluded asylum seekers primarily structural (border enforcement, legal bars) or internalized (deterrence, self-exclusion, normalization of denial)?',
    'Post-policy-change tracking: if suppression persists after legal barriers are removed (e.g., court rulings expanding PSG), the internalized component is significant.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint''s reach extends beyond formal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the asylum deterrence regime.').

omega_variable(
    psg_immutability_boundary,
    'Where is the line drawn between ''immutable characteristics'' and ''fundamental aspects of identity'' in state practice, and does state awareness require active persecution or mere failure to protect?',
    'Systematic coding of domestic and international PSG jurisprudence across jurisdictions over time.',
    'A narrower line increases extraction; a broader line (including gender, sexuality, clan) moves the reading toward tangled_rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(psg_immutability_boundary, conceptual, 'The precise boundaries of the immutable-characteristics-with-state-awareness test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1967, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1980, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t1995, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2001, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2015, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_tr_t2024, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1967, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1980, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t1995, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2001, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2015, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_be_t2024, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1967, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1980, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t1995, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2001, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2015, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(refugee_convention_text__restrictive_sovereignty_reading_su_t2024, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, eu_asylum_acquis).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, us_asylum_law).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, australia_offshore_processing).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, uk_rwanda_asylum_partnership).

% DUAL FORMULATION NOTE:
% This reading, the expansive_humanitarian_reading, and the procedural_integrity_reading form a constraint family decomposing the kernel 'refugee_convention_text'. Each reading instantiates a different constraint with distinct ε, beneficiary/victim structures, and classifications. The restrictive reading extracts from excluded asylum seekers to benefit sovereign discretion; the expansive reading coordinates broad protection; the procedural reading coordinates fair process. They are linked by shared treaty text but are structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__restrictive_sovereignty_reading, institutional, 0.1).
constraint_indexing:directionality_override(refugee_convention_text__restrictive_sovereignty_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
