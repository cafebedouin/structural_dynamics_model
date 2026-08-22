% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Birth-Threshold Personhood — Moral Standing from Birth
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the birth-threshold reading of the
 *   personhood_boundary kernel — the commitment that personhood begins at
 *   live birth and extends to all born humans without exception. It is one of
 *   three structurally distinct readings of the same kernel (the others being
 *   fitness-contingent and potential-based readings). The constraint operates
 *   as a rope: it solves a genuine coordination problem (who counts as a
 *   person?) with a bright-line, administrable threshold that minimizes
 *   coercive overhead. Its extraction is low (0.08) because it primarily
 *   protects rather than extracts; its suppression is low (0.15) because the
 *   threshold is largely self-enforcing through social consensus and legal
 *   institutionalization. The theater ratio is minimal (0.05) — the
 *   constraint's operation matches its stated function. The primary
 *   structural tension is the exclusion of viable fetuses, which creates a
 *   victim class at the boundary. This exclusion is not a bug but the
 *   necessary counterpart of the bright line — the birth threshold's
 *   coordination value depends on its being a sharp, observable boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.08).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.15).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth-Threshold Personhood — Moral Standing from Birth").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'aa551dfa-adff-4807-b246-4fcb0e6e2bd7').
narrative_ontology:cs_kernel_codification('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', formalized).
narrative_ontology:cs_authority_grounding('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', lineage).
narrative_ontology:cs_interpretation_layer_present('aa551dfa-adff-4807-b246-4fcb0e6e2bd7').
narrative_ontology:cs_reading_relation('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', foundational, birth_confers_full_moral_standing).
narrative_ontology:cs_axiom_status(birth_confers_full_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', birth_confers_full_moral_standing, deontological).
narrative_ontology:cs_axiom('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', foundational, no_state_authority_to_exclude_born_humans).
narrative_ontology:cs_axiom_status(no_state_authority_to_exclude_born_humans, holdable).
narrative_ontology:cs_axiom_grounding('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', no_state_authority_to_exclude_born_humans, deontological).
narrative_ontology:cs_reference_frame('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', universal_human_dignity_from_birth).
narrative_ontology:cs_drift_state('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', contemporary_bioethics_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aa551dfa-adff-4807-b246-4fcb0e6e2bd7', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, all_born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, legal_systems_recognizing_birth_personhood).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, medical_ethics_committees).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, newborn_infants).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, viable_fetuses_excluded_from_personhood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, newborn_infants).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity_from_birth).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, prohibition_on_infanticide).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_moral_standing_of_all_born_persons).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, state_obligation_to_protect_all_born_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person born alive receives full moral standing and legal protection under this reading. They benefit from the absolute prohibition on killing born humans and the state obligation to protect their life. Exit from this protection is impossible — it is constitutive of their legal personhood.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, all_born_humans, beneficiary,
    moderate, biographical, constrained, global).

% Newborns are the primary subjects of the constraint's protection — they cannot exit, advocate, or resist. They bear the full weight of any failure of the constraint (neglect, infanticide, state abandonment) while receiving its full benefit when it operates. Their situation is one of radical dependency on the constraint's enforcement.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, newborn_infants, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, newborn_infants, beneficiary).

% This reading explicitly includes them as full persons from birth, unlike the fitness-contingent and potential-based readings which may exclude them. They bear the risk of being reclassified if the reading erodes, but currently receive full protection. Their situation is the sharpest test of the reading's universality claim.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, severely_disabled_infants, payer,
    powerless, biographical, trapped, global).

% Late-gestation fetuses with capacity for extrauterine survival are structurally excluded from personhood under this reading. They receive no moral standing or legal protection as persons. Their exclusion is the necessary counterpart of the birth threshold — the line must be drawn somewhere, and this reading draws it at birth.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, viable_fetuses_excluded_from_personhood, excluded,
    powerless, immediate, trapped, global).

% These legal systems (post-UDHR constitutional orders, civil law codes recognizing birth as personhood inception) administer and enforce the constraint. They define homicide law, child protection statutes, and medical ethics guidelines around the birth threshold. They can modify the threshold through legislation or constitutional amendment but face high political and legitimacy costs for doing so.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_systems_recognizing_birth_personhood, agenda_setter,
    institutional, generational, arbitrage, global).

% Hospital ethics committees, bioethics boards, and professional medical associations interpret and apply the birth threshold in clinical decisions — resuscitation of extremely preterm infants, withdrawal of life support for severely disabled newborns, prenatal diagnosis counseling. They operate within the constraint's framework but shape its practical boundaries.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_ethics_committees, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, medical_ethics_committees, observer).

% Academic philosophers of bioethics (Tooley, Singer, McMahan, Kamm, Harman, among others) analyze the birth threshold's coherence, its alternatives, and its implications. They do not administer the constraint but their work shapes the intellectual environment in which legal and medical actors operate. Their debates are the analytical surface of the kernel contest.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, philosophical_bioethicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, publicly verifiable threshold for the onset of full moral and legal personhood that requires no contested assessments of cognitive capacity, potential, or social utility. Solves the coordination problem of 'who counts as one of us' by anchoring personhood to the observable, universal event of live birth — a threshold every society can administer without specialized expertise.
% TRANSFER_FUNCTION: Moves the burden of justification from the born human (who need not demonstrate fitness, potential, or utility to claim protection) onto any actor or institution that would deny protection. The constraint transfers the cost of personhood disputes from the most vulnerable (newborns, disabled infants) to the powerful (states, medical authorities, philosophical frameworks that would exclude them).
% ABSENT_VOICES: The viable fetus excluded from personhood — it would object to being denied standing if it could speak, but its exclusion is structural to the birth threshold. The severely disabled infant under alternative readings — under fitness-contingent or potential-based readings, these infants would be excluded or downgraded; their protection under this reading means their voices are present here but absent in the sibling readings. Historical victims of infanticide and 'euthanasia' programs — their absence is the constraint's founding witness.
% DISAPPEARANCE_RATIONALE: If the birth-threshold constraint vanished overnight, legal systems would immediately face the question: what replaces it? Fitness-contingent and potential-based criteria would compete for adoption. Homicide law, child protection, neonatology protocols, and prenatal policy would all become contestable. The absolute prohibition on killing born humans would lose its constitutional anchor. States that currently criminalize infanticide would need new justifications. The world would rearrange — not into chaos, but into a different, contested personhood regime.
% FOUNDING_PROBLEM: The historical problem was the widespread, legally sanctioned killing of newborns — infanticide as population control, gender selection, disability elimination, and economic necessity — across pre-modern and early modern societies. The birth threshold emerged as the minimal universal commitment that could criminalize infanticide without requiring contested metaphysical agreement about ensoulment, rationality, or potential.
% FOUNDING_PROBLEM_CORROBORATION: Historical demographers (Langer, Boswell, Scheper-Hughes) document infanticide as a near-universal pre-modern practice. Legal historians trace the criminalization of infanticide to the 18th-19th century in European law, with the birth threshold as the operative line. The Universal Declaration of Human Rights (1948) and subsequent human rights treaties (ICCPR Art. 6, CRC Art. 6) codify the right to life from birth — this is corroboration from outside any single philosophical tradition. However, contemporary bioethicists (Singer, Tooley, Giubilini & Minerva) argue the founding problem is mis-specified: they claim the real problem is not infanticide per se but the absence of a principled criterion for personhood, and that the birth threshold is an arbitrary line. The status is therefore contested — the problem the constraint was built to solve (infanticide) is historically attested, but whether the birth threshold is the *right* solution is disputed by the sibling readings.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The birth threshold functions as a coordination mechanism (rope) rather than an extraction mechanism because: (1) it imposes negligible costs on the powerful — no state, medical authority, or philosophical framework is forced to sacrifice significant resources; (2) its primary effect is to protect the powerless (newborns, disabled infants) from exclusion; (3) it requires minimal active enforcement — infanticide prohibition is embedded in homicide law and social taboo; (4) alternatives (fitness, potential assessments) would require continuous, invasive evaluation of every newborn, creating massive coordination costs and extraction opportunities. The slight uptick in suppression_requirement at the end of the interval (0.15 in 2024 vs 0.10 in 2010) reflects growing philosophical and legal challenges to the birth threshold from both the potential-based direction (fetal personhood movements) and the fitness-contingent direction (post-birth abortion arguments), requiring more active legal defense of the threshold.
 *
 * PERSPECTIVAL GAP:
 *   From the newborn's seat, the constraint is absolute protection (mountain-like in its fixity). From the viable fetus's seat, it is an arbitrary exclusion (snare-like in its boundary violence). From the legal system's seat, it is an administrable bright line (rope). From the fitness-contingent philosopher's seat, it is an unprincipled compromise (tangled rope — coordination without adequate justification). The engine will compute these divergences from the structural data; the authored claim (rope) reflects the constraint's *primary* operational character as a coordination mechanism, not the experience of every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   All born humans are beneficiaries (d near 0.0) — they receive protection without paying for it. Newborns and severely disabled infants are also payers in a structural sense (d near 1.0) — they bear the full consequences of any constraint failure while having zero exit capacity. This dual positioning (beneficiary + payer) for the most vulnerable is the constraint's defining structural feature: it protects them by making their protection non-negotiable, but that very non-negotiability means they cannot opt out or advocate for themselves. Viable fetuses are excluded (d = 1.0 structurally) — they receive no protection and have no voice. Legal systems and medical ethics committees are agenda_setters with arbitrage/mobile exit — they administer the constraint and could change it, but face high legitimacy costs. Philosophical bioethicists are analytical observers with zero structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (infanticide) remains live in the sense that infanticide still occurs and would increase without the constraint, but the *intellectual* founding problem — the need for a universally administrable personhood threshold that avoids metaphysical dispute — is contested. The sibling readings argue the birth threshold is not a solution but an evasion of the real problem (what grounds moral status?). The constraint has not undergone mandatrophy — its coordination function is still actively performed — but it faces persistent revision pressure from both sibling readings. The mandate has not outlived its function; rather, its function is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_threshold_vs_viability_boundary,
    'Is the birth threshold structurally necessary for the constraint''s coordination function, or could a viability threshold (e.g., 24 weeks gestation) serve the same coordination role with less exclusionary violence at the boundary?',
    'Comparative analysis of jurisdictions using viability thresholds for legal personhood (none currently exist for full personhood, but some use viability for fetal homicide law). Counterfactual modeling of a viability-threshold regime: would it require continuous gestational-age assessment, creating new coordination costs and extraction opportunities?',
    'If viability could serve as an equally administrable bright line, the birth threshold''s exclusion of viable fetuses becomes a contingent choice rather than a structural necessity — weakening the reading''s claim to be the *minimal* coordination solution. If birth is uniquely administrable (universally observed, no medical assessment required), the exclusion is the price of coordination purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_threshold_vs_viability_boundary, conceptual, 'Whether the birth threshold''s boundary exclusion is structurally necessary or contingently chosen.').

omega_variable(
    disabled_infant_protection_vs_fitness_creep,
    'Does the birth threshold''s explicit inclusion of severely disabled infants actually hold in practice, or does clinical practice (selective non-resuscitation, withdrawal of nutrition/hydration) create a de facto fitness-contingent layer beneath the formal birth threshold?',
    'Empirical study of neonatology protocols and ethics committee decisions for infants with severe disabilities (trisomy 13/18, anencephaly, extreme prematurity with severe IVH). Comparison of stated policy (birth threshold) vs. actual treatment patterns.',
    'If clinical practice systematically excludes severely disabled infants from full protection, the constraint''s extraction profile shifts — the formal rope conceals a de facto fitness-contingent snare operating at the implementation layer. This would require decomposing the constraint into a formal layer (rope) and an implementation layer (snare/tangled rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disabled_infant_protection_vs_fitness_creep, empirical, 'Whether the birth threshold''s universal protection holds in clinical practice or is undermined by fitness-contingent implementation.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the personhood_boundary kernel best framed as a single threshold question (when does personhood begin?) or as a dual-threshold structure (when does protection begin? when does full moral standing begin?)?',
    'Analysis of legal regimes that distinguish ''legal personhood'' (birth) from ''fetal protections'' (viability-based homicide protections, prenatal injury torts). If such dual-threshold regimes are stable and widespread, the single-threshold kernel framing may be analytically inadequate.',
    'If the kernel is better modeled as dual-threshold, the three readings are not competing answers to one question but competing allocations across two thresholds. This would restructure the constraint family: birth_threshold_reading would become one component of a two-constraint system, and the sibling readings would map onto the second threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s framing as a single threshold is analytically adequate or obscures a dual-threshold structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_tr_t1948, personhood_boundary__birth_threshold_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_tr_t1970, personhood_boundary__birth_threshold_reading, theater_ratio, 1970, 0.06).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_tr_t1990, personhood_boundary__birth_threshold_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_tr_t2010, personhood_boundary__birth_threshold_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_tr_t2024, personhood_boundary__birth_threshold_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_be_t1948, personhood_boundary__birth_threshold_reading, base_extractiveness, 1948, 0.12).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_be_t1970, personhood_boundary__birth_threshold_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_be_t1990, personhood_boundary__birth_threshold_reading, base_extractiveness, 1990, 0.08).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_be_t2010, personhood_boundary__birth_threshold_reading, base_extractiveness, 2010, 0.07).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_be_t2024, personhood_boundary__birth_threshold_reading, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_su_t1948, personhood_boundary__birth_threshold_reading, suppression_requirement, 1948, 0.25).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_su_t1970, personhood_boundary__birth_threshold_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_su_t1990, personhood_boundary__birth_threshold_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_su_t2010, personhood_boundary__birth_threshold_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(personhood_boundary_birth_threshold_reading_su_t2024, personhood_boundary__birth_threshold_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__birth_threshold_reading, 0.06).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, homicide_law_universal_prohibition).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, child_protection_statutes).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, neonatal_resuscitation_protocols).

% DUAL FORMULATION NOTE:
% This constraint is the birth_threshold_reading of the personhood_boundary kernel. It decomposes the colloquial 'personhood debate' into structurally distinct constraints: (1) birth_threshold_reading — universal protection from birth (rope, low extraction); (2) fitness_contingent_reading — protection conditional on demonstrated capacities (tangled rope/snare, higher extraction via assessment infrastructure); (3) potential_based_reading — protection grounded in developmental potential (tangled rope, extraction via potential-assessment infrastructure). The three readings share the kernel but have different ε values, different victim sets, and different coordination functions. They are linked via network.affects_constraints. The birth threshold reading is the upstream constraint — historically earlier, more widely institutionalized, and the default against which the siblings define themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, powerless, 0.95).
constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
