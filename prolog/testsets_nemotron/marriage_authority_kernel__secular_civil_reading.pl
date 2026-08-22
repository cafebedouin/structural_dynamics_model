% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 (SMA) provides a secular civil marriage
 *   option grounded in constitutional individual rights, enabling interfaith
 *   and nonreligious marriage without religious conversion. It is the only
 *   Indian marriage law that applies uniformly regardless of religion, with
 *   gender-equitable divorce, maintenance, and inheritance provisions. The
 *   constraint is the SMA's authority structure: civil courts adjudicate,
 *   constitutional rights ground legitimacy, and community religious
 *   authority is displaced. Beneficiaries are interfaith couples, women
 *   seeking equity, and nonreligious persons. Victims are community religious
 *   authorities and families enforcing endogamy who lose jurisdictional
 *   control. The claimed type is rope: genuine coordination (solves
 *   interfaith marriage problem) with modest extraction (procedural burdens,
 *   notice period) that falls primarily on the couples it serves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.32).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.28).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'a559cd45-ecaa-465a-90f3-265cc06b2d29').
narrative_ontology:cs_kernel_codification('a559cd45-ecaa-465a-90f3-265cc06b2d29', formalized).
narrative_ontology:cs_authority_grounding('a559cd45-ecaa-465a-90f3-265cc06b2d29', lineage).
narrative_ontology:cs_interpretation_layer_present('a559cd45-ecaa-465a-90f3-265cc06b2d29').
narrative_ontology:cs_reading_relation('a559cd45-ecaa-465a-90f3-265cc06b2d29', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('a559cd45-ecaa-465a-90f3-265cc06b2d29', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a559cd45-ecaa-465a-90f3-265cc06b2d29', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a559cd45-ecaa-465a-90f3-265cc06b2d29', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('a559cd45-ecaa-465a-90f3-265cc06b2d29', foundational, constitutional_rights_ground_marriage_authority).
narrative_ontology:cs_axiom_status(constitutional_rights_ground_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('a559cd45-ecaa-465a-90f3-265cc06b2d29', constitutional_rights_ground_marriage_authority, deontological).
narrative_ontology:cs_axiom('a559cd45-ecaa-465a-90f3-265cc06b2d29', foundational, civil_courts_adjudicate_marriage_disputes).
narrative_ontology:cs_axiom_status(civil_courts_adjudicate_marriage_disputes, holdable).
narrative_ontology:cs_axiom_grounding('a559cd45-ecaa-465a-90f3-265cc06b2d29', civil_courts_adjudicate_marriage_disputes, conventional).
narrative_ontology:cs_axiom('a559cd45-ecaa-465a-90f3-265cc06b2d29', secondary, gender_equality_is_nonnegotiable_in_marriage_law).
narrative_ontology:cs_axiom_status(gender_equality_is_nonnegotiable_in_marriage_law, holdable).
narrative_ontology:cs_axiom_grounding('a559cd45-ecaa-465a-90f3-265cc06b2d29', gender_equality_is_nonnegotiable_in_marriage_law, deontological).
narrative_ontology:cs_reference_frame('a559cd45-ecaa-465a-90f3-265cc06b2d29', constitutional_individual_rights_framework).
narrative_ontology:cs_drift_state('a559cd45-ecaa-465a-90f3-265cc06b2d29', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a559cd45-ecaa-465a-90f3-265cc06b2d29', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equitable_rights).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, nonreligious_individuals).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, constitutional_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, community_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, families_enforcing_endogamy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, state_marriage_registrars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Couples from different religious backgrounds who need a legal framework that doesn't require conversion or community permission. The SMA provides the only civil pathway to marriage without religious gatekeepers. Exit from community law brings severe social ostracism and sometimes violence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).

% Women who benefit from the SMA's equal divorce grounds, maintenance provisions, and inheritance rights that override discriminatory personal laws. However, accessing these rights often means severing family and community ties, making exit from community law identity-locked for many.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equitable_rights, beneficiary,
    moderate, biographical, constrained, national).

% Atheists, agnostics, and those who reject religious personal law entirely. The SMA is their only marriage option. They face social invisibility and procedural hurdles (30-day notice period, publication requirements) that religious marriages don't face.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, nonreligious_individuals, beneficiary,
    powerless, biographical, constrained, national).

% High courts and the Supreme Court that interpret and expand the SMA's reach through progressive judgments (e.g., recognizing live-in relationships, striking down discriminatory provisions). They administer the constraint and benefit from its legitimating connection to constitutional rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Religious leaders and community bodies (qazis, priests, khap panchayats, caste councils) who lose jurisdictional authority over marriage when couples choose the SMA. Their power to enforce endogamy and gendered personal law erodes. They experience this as extraction of their traditional authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, community_religious_authorities, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, community_religious_authorities, payer).

% Extended families and kinship networks that enforce religious/caste endogamy through social pressure, economic coercion, and honor violence. The SMA enables children to marry outside the group without community consent, which families experience as loss of control over lineage and social reproduction.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, families_enforcing_endogamy, payer,
    organized, biographical, identity_locked, local).

% Bureaucrats who administer the SMA's notice period, objections process, and registration. They bear administrative burden and face pressure from both couples (for speed) and communities (to delay/obstruct). Their role is procedural but they are the constraint's frontline enforcers.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, state_marriage_registrars, agenda_setter,
    institutional, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, state_marriage_registrars, payer).

% Lawyers and NGOs who help couples navigate the SMA's procedural hurdles and defend against family/community retaliation. They see the constraint's operation from the beneficiary side but also witness its procedural gaps and the gap between formal rights and effective access.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, legal_aid_lawyers, observer,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religion-neutral, state-administered marriage framework that solves the coordination problem of interfaith and nonreligious marriage without requiring conversion or community consent, while establishing gender-equitable rights as the default.
% TRANSFER_FUNCTION: Transfers jurisdictional authority over marriage formation and dissolution from religious communities to civil courts; transfers exit costs from individuals (who would face conversion or exclusion) to communities (who lose control); transfers procedural burden to the state (30-day notice, registration).
% ABSENT_VOICES: Couples who want the SMA's protections but cannot survive the 30-day notice period's public exposure (especially in honor-violence contexts); LGBTQ+ couples for whom the SMA's heterosexual framing excludes access; migrant workers and undocumented persons who cannot meet documentary requirements; rural women who lack legal literacy to navigate civil courts.
% DISAPPEARANCE_RATIONALE: If the SMA vanished overnight, interfaith couples would lose their only civil marriage pathway — forced into conversion, religious marriage under one partner's law, or cohabitation without legal protection. Women's gender-equitable divorce and maintenance rights under the SMA would revert to discriminatory personal laws. Constitutional courts would lose a key statutory anchor for progressive family law jurisprudence. Community authorities would regain de facto monopoly over marriage.
% FOUNDING_PROBLEM: The colonial legacy of religion-based personal laws that denied interfaith marriage, entrenched gender inequality, and made religious identity legally compulsory. The SMA was enacted in 1954 to give citizens a constitutional alternative grounded in individual rights rather than community membership.
% FOUNDING_PROBLEM_CORROBORATION: The Constituent Assembly debates (non-beneficiary source) record the intent to provide a uniform civil code option. The Law Commission reports (1970s-present, independent body) document the SMA's procedural defects and the gap between its promise and operation. Women's rights organizations outside the direct beneficiary set (e.g., AIDWA, Majlis) corroborate that the founding problem of religious personal law discrimination persists and the SMA remains an incomplete solution.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects the SMA's procedural costs (30-day public notice, documentary requirements, court delays) that extract time, privacy, and safety from couples — especially women and interfaith pairs — but the core marriage right is not monetized. Suppression (0.28) is modest: the constraint doesn't actively coerce; rather, community actors deploy extra-legal suppression (honor violence, social boycott) that the state fails to prevent. Theater ratio (0.15) is low: the SMA's function is real and used, though procedural gaps create performative compliance (notice period often weaponized). Accessibility collapse (0.42) is moderate: alternatives (religious marriage, conversion, cohabitation) exist but carry high identity costs. Resistance (0.38) reflects ongoing litigation, community opposition, and state neglect of procedural reform.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (couples, women), the SMA is a fragile lifeline — real coordination function but high procedural and social friction. From the agenda-setter seats (courts, registrars), it is a constitutional mandate imperfectly implemented. From the payer/victim seats (community authorities, families), it is an illegitimate state intrusion that extracts their traditional jurisdiction. The engine computes these divergent effective extractions from the structural data; the claimed type (rope) reflects the author's assessment that coordination function is genuine and primary, though extraction has accumulated over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Interfaith couples, women seeking equity, and nonreligious individuals are beneficiaries (d ~ 0.2-0.3): they gain rights but bear procedural and social costs. Constitutional courts and registrars are agenda_setters (d ~ 0.1-0.2): they administer and gain legitimacy. Community authorities and endogamy-enforcing families are payers/victims (d ~ 0.7-0.8): they lose authority and control. Legal aid lawyers are observers (d ~ 0.5): analytical seat. Exit options differentiate: couples are constrained (social costs of exit from community), not trapped (legal exit exists); community authorities are identity_locked (their role is constituted by the authority they lose).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious personal law discrimination) is contested as live: personal laws remain discriminatory, but the SMA has not become the universal civil code the framers envisioned. It remains a niche option (~0.5% of marriages) due to procedural barriers and social costs. The mandate has not atrophied — the problem persists — but the solution has not scaled. This is not mandatrophy (which requires the problem to be dead while the arrangement persists); it is incomplete realization. The arrangement would be a piton if it existed only theatrically; instead it is actively used and litigated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sma_procedural_gaps_as_extraction,
    'Are the SMA''s procedural requirements (30-day notice, publication, objection period) genuine coordination necessities or do they function as extraction mechanisms that deter use?',
    'Comparative analysis with marriage registration systems in other jurisdictions; empirical study of objection outcomes and their demographic correlates; legislative history of procedural amendments.',
    'If procedures are extraction, the constraint trends toward tangled_rope (coordination + asymmetric extraction on beneficiaries). If coordination-necessary, rope classification holds. Affects ε and suppression scoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sma_procedural_gaps_as_extraction, empirical, 'Whether procedural friction is functional or extractive.').

omega_variable(
    community_suppression_vs_state_suppression,
    'Is the suppression experienced by SMA users attributable to the constraint itself (state enforcement) or to community extra-legal retaliation that the state fails to prevent?',
    'Case law analysis of state protection orders for interfaith couples; police FIR data on honor violence vs. SMA registrations; ethnographic work on couples'' threat perception sources.',
    'If suppression is primarily community-based and the constraint provides legal remedy, suppression score should be lower and the constraint is more rope-like. If the constraint''s notice period enables community suppression, it is structurally complicit — higher suppression, more tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(community_suppression_vs_state_suppression, conceptual, 'Attribution of suppression to constraint design vs. external actors.').

omega_variable(
    kernel_reading_boundary_hindu_codified,
    'Does the hindu_codified_reading foreclose, coexist with, or influence the secular_civil_reading? The HMA 1955 was enacted alongside the SMA and shares constitutional courts as interpreters.',
    'Doctrinal analysis of whether Hindu law reform (HMA) and the secular option (SMA) were designed as complementary or competing frameworks; judicial citation patterns; legislative debates.',
    'If forecloses: the readings cannot be held in one framework (unlikely — they operate in parallel). If coexists_with: parallel regimes with different jurisdictional triggers (default vs. opt-in). If influences: HMA reforms (e.g., 2005 amendment) raise the floor for SMA gender equity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_hindu_codified, conceptual, 'Structural relationship to the hindu_codified_reading sibling.').

omega_variable(
    kernel_reading_boundary_muslim_shariat,
    'Does the muslim_shariat_reading foreclose the secular_civil_reading for Muslims? Muslim personal law boards claim exclusive jurisdiction; the SMA is an opt-out.',
    'Case law on whether Muslims can validly marry under the SMA (they can, per Supreme Court); fatwa literature on apostasy consequences; empirical data on Muslim SMA usage.',
    'If forecloses within Muslim community framework: the readings are logically incompatible for a Muslim party. If coexists_with: Muslims can choose SMA but face community sanctions. The engine computes per-seat type; this omega documents the structural boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_muslim_shariat, empirical, 'Whether Muslim personal law forecloses the secular option for Muslims.').

omega_variable(
    sma_as_transitional_scaffold,
    'Was the SMA intended as a transitional scaffold toward a Uniform Civil Code (UCC) rather than a permanent rope? The Constituent Assembly debated UCC as a directive principle.',
    'Constituent Assembly debates; Law Commission reports on UCC; political history of UCC non-enactment; whether the SMA has sunset or review clauses (it does not).',
    'If scaffold: the constraint''s justification is transition, not steady state; claimed type should be scaffold with has_sunset_clause (but it lacks one — tension). If rope: permanent coordination mechanism. This bears on mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sma_as_transitional_scaffold, conceptual, 'Whether the SMA was founded as a transitional measure toward UCC.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(marr_tr_t1976, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1976, 0.08).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(marr_be_t1976, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(marr_su_t1976, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1976, 0.22).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1985, 0.24).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1995, 0.26).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2005, 0.27).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, uniform_civil_code_directive_principle).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, special_marriage_act_procedural_reform).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel. The kernel decomposes into five constraint stories (one per reading) because each reading instantiates a different authority structure with different beneficiaries, victims, courts, and equity outcomes — different ε values. They are linked via affects_constraints. The secular_civil_reading has the highest gender equity and only interfaith access, but highest social exit costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, organized, 0.75).
constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
