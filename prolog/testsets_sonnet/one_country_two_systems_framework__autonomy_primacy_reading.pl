% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems — Autonomy-Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   Following the 1997 handover, the Sino-British Joint Declaration and the
 *   Basic Law were presented internationally as a binding fifty-year
 *   guarantee of Hong Kong's distinct legal system, civil liberties, and
 *   limited democratic development, insulated from mainland Chinese
 *   governance. This story authors the reading under which that guarantee is
 *   real and operative: mainland actions that narrow judicial independence,
 *   press freedom, or protest rights are treaty violations, not lawful
 *   exercises of sovereign discretion. Under this reading the 2020 National
 *   Security Law and subsequent prosecutions of legislators, journalists, and
 *   activists register as extraction beyond what the framework authorizes — a
 *   measurable departure from the coordination bargain, not its normal
 *   operation.
 *
 * KEY AGENTS:
 *   - hong_kong_residents: primary beneficiary class of the guarantee, powerless individually, constrained exit
 *   - hong_kong_judiciary: institutional agenda-setter whose independence is the treaty's enforcement mechanism on this reading
 *   - prodemocracy_activists and civil_society_organizations: primary bearers of extraction under the post-2020 security apparatus
 *   - central_peoples_government: excluded from this reading's premises — its own sovereignty account is the sibling reading
 *   - international_treaty_partners: external verifiers whose six-monthly reporting operationalizes treaty enforceability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.28).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.35).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems — Autonomy-Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, 'dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3').
narrative_ontology:cs_kernel_codification('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', fixed_text).
narrative_ontology:cs_authority_grounding('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', lineage).
narrative_ontology:cs_interpretation_layer_present('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3').
narrative_ontology:cs_reading_relation('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', foundational, autonomy_as_enforceable_treaty_right).
narrative_ontology:cs_axiom_status(autonomy_as_enforceable_treaty_right, holdable).
narrative_ontology:cs_axiom_grounding('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', autonomy_as_enforceable_treaty_right, conventional).
narrative_ontology:cs_axiom('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', foundational, judicial_review_binds_central_authority_within_hk).
narrative_ontology:cs_axiom_status(judicial_review_binds_central_authority_within_hk, holdable).
narrative_ontology:cs_axiom_grounding('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', judicial_review_binds_central_authority_within_hk, conventional).
narrative_ontology:cs_axiom('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', secondary, civil_liberties_baseline_is_treaty_fixed_not_politically_revisable).
narrative_ontology:cs_axiom_status(civil_liberties_baseline_is_treaty_fixed_not_politically_revisable, holdable).
narrative_ontology:cs_axiom_grounding('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', civil_liberties_baseline_is_treaty_fixed_not_politically_revisable, deontological).
narrative_ontology:cs_reference_frame('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', joint_declaration_fifty_year_guarantee).
narrative_ontology:cs_drift_state('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', post_2020_national_security_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd6dd1ca-7f2a-4ec5-9880-cfab792e47b3', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_sector).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prodemocracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_independent_press).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, basic_law_constitutional_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, judicial_independence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a legal and civic order that, on this reading, guarantees common-law courts, a free press, and protest rights distinct from mainland practice. They rely on the Basic Law and Joint Declaration as guarantees enforceable against Beijing, not mere administrative concessions. Exit means emigration; staying means betting on the treaty holding.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    powerless, biographical, constrained, regional).

% Common-law courts, staffed partly by foreign judges under the Basic Law, adjudicate disputes including those testing the boundary of central government power. On this reading, judicial review is a genuine check binding the executive and, in principle, Beijing's delegated organs — its independence is the treaty's operative mechanism, not decorative.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary).

% The UK (as co-signatory to the Sino-British Joint Declaration), the UN, and other states monitor compliance, issue six-monthly reports, and treat departures from autonomy as treaty violations with diplomatic and trade consequences. They benefit from a stable, rules-based reading that lets them hold Beijing to an external legal standard rather than an internal political one.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners, beneficiary).

% International firms and local capital depend on the perception that Hong Kong's legal system, currency, and regulatory autonomy are genuinely insulated from mainland political interference. This reading underwrites the premium Hong Kong commands as a separate jurisdiction for finance and arbitration.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_sector, beneficiary,
    organized, biographical, mobile, global).

% Organizers, elected legislators, and protest leaders who acted on the premise that the treaty guaranteed a pathway toward greater democratic representation and protected dissent. Many now face prosecution under the National Security Law; from the autonomy-primacy reading, this prosecution is itself the treaty violation the framework was meant to prevent, not a lawful exercise of delegated authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prodemocracy_activists, payer,
    powerless, biographical, trapped, regional).

% Unions, student groups, and advocacy NGOs that operated under an assumption of protected assembly and association rights. Many have dissolved under pressure since 2020; their disappearance is read, on this account, as direct evidence of treaty breach rather than a legitimate narrowing of local latitude.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations, payer,
    moderate, biographical, constrained, regional).

% Newsrooms that relied on press freedom guarantees distinct from mainland censorship norms. Several have closed or relocated staff abroad following raids and arrests. On this reading their closure is a measurable erosion of the guaranteed civil liberties baseline, not a matter of internal editorial risk management.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_independent_press, payer,
    moderate, biographical, trapped, regional).

% Beijing's own account of its authority — sovereignty as the source from which autonomy is delegated — is not part of THIS reading's premises; it is the seat that the sovereignty_primacy_reading centers, and appears here only as the excluded voice this reading treats as bound rather than as final authority. It would object that the autonomy-primacy account treats delegated power as an independent legal ceiling on sovereign authority, which it does not accept.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, central_peoples_government, excluded,
    institutional, civilizational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internationally verifiable division of legal systems that let Hong Kong retain common-law commercial infrastructure and civil liberties distinct from the mainland after 1997, enabling continued participation in global finance and preserving a negotiated transition promise to residents.
% TRANSFER_FUNCTION: On this reading, nothing should flow from Hong Kong's autonomy to central authority beyond the reserved sovereign functions (defense, foreign affairs) named in the Basic Law; any further transfer of authority, prosecutorial reach, or civic space away from Hong Kong institutions constitutes extraction beyond the treaty's terms.
% ABSENT_VOICES: The Central People's Government's own constitutional theory of unitary sovereignty is structurally excluded from this reading's premises — it is the seat the sovereignty_primacy_reading gives voice to. Within Hong Kong, disqualified legislators and jailed activists are also absent from the current legislature and courts that would otherwise test these claims.
% DISAPPEARANCE_RATIONALE: If the autonomy-primacy reading were abandoned entirely, treaty partners argue Hong Kong's separate customs, currency, and legal status would lose their justificatory basis and could be challenged internationally; Beijing and its allies argue nothing observable would change because the reading was never operative in practice. The parties dispute whether the reading is currently being enforced, eroded, or was always aspirational.
% FOUNDING_PROBLEM: Reconciling the 1997 handover of sovereignty to the PRC with preserving Hong Kong's distinct legal, economic, and civil institutions long enough for a negotiated 50-year transition, without triggering capital flight, population exodus, or diplomatic rupture with the UK and other Joint Declaration guarantors.
% FOUNDING_PROBLEM_CORROBORATION: UN human rights bodies, the UK Foreign Office's six-monthly Hong Kong reports, and independent international law scholars outside both the Hong Kong government and pro-democracy movement attest that the founding problem (preserving a distinct rights-bearing legal order) is being actively undermined; the Hong Kong and central governments dispute this characterization and assert the framework's core commitments remain fully honored.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.28) rather than low, because even under the autonomy-primacy reading some baseline transfer to central authority (defense, foreign affairs) is treaty-legitimate; the rise from 0.10 at handover to 0.30 around 2020 reflects the specific, contested departures (NSL prosecutions, disqualifications, press closures) this reading treats as violations rather than legitimate operation. Suppression tracks a similar arc, peaking with the 2020 NSL enactment and settling at an elevated post-2020 plateau. Theater ratio rises steadily as institutional forms (elections, judicial proceedings, press licensing) increasingly perform continuity while substantive independence narrows — this is the diagnostic signature the reading treats as evidence of drift away from its founding commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents, the judiciary, treaty partners, and the business sector are coded as beneficiaries because the reading's entire premise is that the framework subsidizes them with rights and stability beyond bare sovereign discretion. Prodemocracy activists, civil society organizations, and the independent press are coded as victims because they are the specific parties who acted on the guarantee's promised scope and have borne concentrated, identifiable costs (imprisonment, dissolution, exile) when that scope narrowed. The central government is excluded rather than scored as a directional party in this reading, because its sovereignty claim is not a premise this reading admits — it is the rival reading's foundation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy misclassification by keeping the founding problem (a rights-preserving legal transition) explicitly separable from its current operation: the founding_problem_status is authored 'contested' rather than 'dead', because from this reading's own premises the treaty commitments remain legally live even where practically eroded — the frame is not claiming the arrangement has become pure inertia, but that active enforcement is currently violating a still-binding bargain. This prevents the reading from collapsing either into a Panglossian 'still working fine' Rope or a fatalistic 'was never real' Snare; it holds the tangled_rope classification because a genuine coordination function (legal-system dualism supporting Hong Kong's role) persists alongside identifiable, enforced extraction from a specific victim class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_after_handover_period,
    'Is the Sino-British Joint Declaration still binding on the PRC as a matter of international law after the UK''s diplomatic recognition role effectively ended, or was it always understood by Beijing as a historical instrument without ongoing enforceability?',
    'International Court of Justice referral (unlikely to occur given PRC''s non-acceptance of compulsory jurisdiction on this matter) or a definitive multilateral state practice showing whether third states continue treating the Declaration as creating present obligations.',
    'If the Declaration lacks live enforceability, this reading''s core claim that mainland intervention constitutes ''treaty violation'' loses its legal anchor and the constraint''s classification could shift toward accepting the sovereignty_primacy_reading''s premises as operative fact, independent of which reading is normatively preferred.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_enforceability_after_handover_period, conceptual, 'Whether the Joint Declaration remains a live international legal constraint or has become a dead letter.').

omega_variable(
    judicial_independence_actual_versus_formal,
    'Do Hong Kong courts, particularly under National Security Law cases, retain the substantive independence this reading asserts as the treaty''s enforcement mechanism, or has independence become largely formal in politically sensitive matters?',
    'Systematic outcome analysis of NSL and politically sensitive cases compared to base-rate judicial outcomes in comparable non-political litigation, plus tracking of foreign judge resignations from the Court of Final Appeal as a revealed-preference signal.',
    'If independence is substantively intact outside a narrow security carve-out, the extraction measured here is bounded and the tangled_rope classification is well-calibrated; if independence has collapsed generally, the true extraction is higher than authored and the constraint may function closer to a snare from the payer stakeholders'' seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_actual_versus_formal, empirical, 'Whether judicial independence remains a live constraint on executive/central power or has become symbolic.').

omega_variable(
    reading_selection_versus_ground_truth,
    'Is the choice to author the autonomy-primacy reading here a defensible representation of the treaty''s original legal intent, or does it import an outside (largely Western liberal-legal) framing that Hong Kong''s own Basic Law drafters did not universally share even in 1997?',
    'Comparative analysis of Basic Law drafting committee records and contemporaneous PRC/UK negotiating positions to establish whether ''substantive autonomy with enforceable checks'' was the shared understanding at signing or a later interpretive gloss.',
    'If the autonomy-primacy reading was never the shared founding understanding, its extractiveness measurements describe departure from an aspirational reading rather than a genuinely breached bargain — a conceptual distinction that changes how the founding_problem_status finding should be weighted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_versus_ground_truth, conceptual, 'Whether this reading reflects the treaty''s actual founding consensus or a retrospective interpretive construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2047).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(one__tr_t2025, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.1).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2003, 0.14).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.2).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.24).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.3).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2023, 0.28).
narrative_ontology:measurement(one__be_t2025, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.12).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2003, 0.18).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.28).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.35).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2023, 0.36).
narrative_ontology:measurement(one__su_t2025, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law_enforcement).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judicial_independence_basic_law).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the one_country_two_systems_framework kernel, each authored as a separate ε-invariant constraint per the decomposition rule: autonomy_primacy_reading (this story, tangled_rope — genuine coordination plus contested extraction against a defined victim class), sovereignty_primacy_reading (delegated/revocable autonomy — expected to classify closer to a legitimate exercise of reserved sovereign power with much lower victim-coded extraction), and balanced_coexistence_reading (negotiated boundary, expected to sit as a rope or scaffold depending on whether the 2047 terminus is read as a genuine sunset). The three share no single ε; each reading's ε is a fact about that reading's own premises, not a measurement of 'the framework' viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
