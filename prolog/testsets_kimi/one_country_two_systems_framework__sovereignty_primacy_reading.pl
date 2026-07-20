% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems â Sovereignty Primacy Reading
 *   domain: constitutional/political/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_primacy_reading of the
 *   contested one_country_two_systems_framework kernel. The reading holds
 *   that Hong Kong's autonomy is delegated by and revocable through PRC
 *   sovereign authority, with national security and territorial integrity
 *   overriding local autonomy upon conflict. Post-2020, this reading has been
 *   operationalized through the National Security Law, direct mainland
 *   security operations in Hong Kong, and the subordination of local judicial
 *   independence to central political prerogatives. The constraint
 *   coordinates territorial integrity (preventing secession) while extracting
 *   civil liberties, local institutional autonomy, and political opposition
 *   capacity from Hong Kong residents and institutions.
 *
 * KEY AGENTS:
 *   - PRC central government: agenda_setter (institutional/arbitrage) â asserts final sovereignty, enacts NSL, overrides local law
 *   - HK Chief Executive: beneficiary/agenda_setter (institutional/constrained) â implements central directives, gains political backing from Beijing
 *   - HK pro-democracy activists: primary payer (powerless/trapped) â criminalized by NSL, targeted by mainland security
 *   - HK judiciary: payer (institutional/constrained) â loses independence on security cases, faces political pressure
 *   - HK civil liberties sector: payer (moderate/constrained) â media, NGOs, unions facing self-censorship and legal risk
 *   - International legal observers: observer (institutional/analytical) â monitors and documents without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems â Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '9dd678fd-a4ed-4284-bc2e-475c5b1b9970').
narrative_ontology:cs_kernel_codification('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', fixed_text).
narrative_ontology:cs_authority_grounding('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', lineage).
narrative_ontology:cs_interpretation_layer_present('9dd678fd-a4ed-4284-bc2e-475c5b1b9970').
narrative_ontology:cs_reading_relation('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', foundational, unitary_state_sovereignty_absolute).
narrative_ontology:cs_axiom_status(unitary_state_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', unitary_state_sovereignty_absolute, conventional).
narrative_ontology:cs_axiom('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', foundational, national_security_overrides_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_overrides_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', national_security_overrides_local_autonomy, instrumental).
narrative_ontology:cs_reference_frame('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', unitary_sovereignty_framework).
narrative_ontology:cs_drift_state('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', post_nsl_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('9dd678fd-a4ed-4284-bc2e-475c5b1b9970', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_chief_executive).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_liberties_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims final authority over Hong Kong through constitutional supremacy and the Basic Law as a national law; enacts and enforces the National Security Law directly in Hong Kong; overrides local legislation and judicial review when sovereignty or security are invoked; benefits politically from elimination of separatist challenge and consolidation of territorial control.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Heads the Hong Kong government with authority derived from Beijing's approval; implements the National Security Law locally, appoints national security judges, and advances mainland policy objectives in the local administration; benefits from Beijing's political backing but lacks an independent electoral mandate and cannot deviate from the sovereignty-primacy line without removal.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_chief_executive, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_chief_executive, agenda_setter).

% Subject to arrest, prosecution, and imprisonment under the National Security Law for political speech, assembly, election organizing, and overseas advocacy; mainland security agencies operate against them within Hong Kong; exit options are reduced to exile or silence; former elected legislators and activists have been detained or have fled.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Retains institutional form but loses substantive independence on national security cases; the chief executive appoints national security judges; defendants can be transferred to mainland jurisdiction; courts cannot review the compatibility of the NSL with the Basic Law or Bill of Rights; judges face political pressure and reputation risk if they rule against the government.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, payer,
    institutional, biographical, constrained, local).

% Comprises media organizations, NGOs, trade unions, and academic institutions whose civil liberties protections have been overridden by national security prerogatives; faces self-censorship, funding freezes, leadership arrests, and organizational dissolution; cannot openly advocate for autonomy or accountability without legal risk.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_liberties_sector, payer,
    moderate, biographical, constrained, local).

% Monitors and documents the erosion of Hong Kong's autonomy under international human rights law and the Sino-British Joint Declaration; issues reports and recommendations but lacks enforcement authority; observes the structural transformation without being subject to the constraint's direct coercion.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_legal_observers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Chinese territorial integrity and prevents Hong Kong from becoming a base for secession or subversion by subordinating local legal autonomy to central sovereign authority when national security is invoked.
% TRANSFER_FUNCTION: Moves criminal jurisdiction, adjudicative authority, and political speech regulation from Hong Kong local institutions to PRC central and mainland security institutions; moves civil liberty protections from Hong Kong residents to state security prerogatives.
% ABSENT_VOICES: Pro-independence advocates, full universal suffrage campaigners, and international human rights monitors are structurally excluded from the legal framework; their positions are criminalized under the NSL rather than accommodated in political or legal process.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primacy framework disappeared overnight, Hong Kong's legal system would regain full jurisdiction over national security matters, mainland security agencies would withdraw, political speech and assembly would no longer trigger criminal prosecution under the NSL, the judiciary would recover independence, and the Basic Law's autonomy provisions would operate as substantive constraints on central intervention rather than delegable permissions.
% FOUNDING_PROBLEM: Post-1997 governance of Hong Kong required a mechanism to reconcile Chinese territorial sovereignty with Hong Kong's distinct legal and economic system; the framework was built to prevent secession and foreign interference while preserving enough local autonomy to maintain international confidence and economic function.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government attests the founding problem is live, citing the 2019 protests as evidence. Hong Kong pro-democracy activists, UK government officials (signatories to the Joint Declaration), and UN human rights bodies attest the founding problem is solved by force and the arrangement now serves political consolidation rather than reconciliation; these sources sit outside the PRC beneficiary set and corroborate the shift in function.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint decouples political speech and assembly from legal protection and transfers criminal jurisdiction to mainland-influenced institutions. Suppression is higher (0.88) because the arrangement requires active enforcement: mainland security agencies operating in Hong Kong, arrests of activists, disqualification of legislators, and media closures. Theater ratio is substantial (0.58) because the forms of Hong Kong autonomy (separate courts, local legislature, common law rhetoric) persist as performance while substance has migrated to Beijing. Accessibility collapse is very high (0.90) because the legal space for opposition, independence advocacy, or even critical reporting has effectively closed. Resistance is moderate (0.55) because international condemnation and diaspora activism continue but are structurally unable to reverse the constraint within Hong Kong.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (PRC central government, HK Chief Executive) experience the constraint as restoration of legitimate sovereign order and prevention of territorial fragmentation. The payer seats (activists, judiciary, civil society) experience the same structure as elimination of civil liberties, rule of law, and local self-governance. The engine computes this divergence from identical structural facts via directionality: beneficiaries receive low effective extraction (the constraint subsidizes their political control) while trapped, identity-exposed targets receive high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government is the structural beneficiary (low directionality): the constraint subsidizes its territorial control and eliminates political challenge. The HK Chief Executive is a secondary beneficiary (low-to-moderate directionality): gains office and backing but is also constrained by Beijing's agenda. Pro-democracy activists are full targets (high directionality): they bear the direct costs of criminalization, surveillance, and exclusion. The judiciary is a constrained target (high directionality despite institutional power): its professional identity and institutional role are locked to Hong Kong, and its victim status under the constraint overrides its nominal power. Civil liberties sector is a moderate target: less individually exposed than activists but bears diffuse costs of self-censorship and organizational pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this constraint as pure extraction (snare) because a genuine coordination function exists: preventing secession and foreign interference in a highly contested territorial context. It also prevents mislabeling it as pure coordination (rope) because the extraction is asymmetric and substantial â civil liberties and judicial independence are costs borne by identifiable victims, not voluntary participants. The active enforcement requirement (true) and the presence of both beneficiaries and victims satisfy the tangled_rope gate. If the national security function were purely performative with no real secession threat, the constraint would degrade toward snare; if the enforcement were withdrawn and local autonomy genuinely respected, it would approach rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_scope_creep,
    'Does the National Security Law''s application remain confined to secession, subversion, terrorism, and collusion with foreign forces, or has it expanded to cover ordinary political opposition, journalism, and civil society advocacy?',
    'Systematic review of prosecutions and indictments under the NSL: if the majority of cases involve non-violent political speech or association rather than conduct meeting international definitions of terrorism or secession, scope creep is confirmed.',
    'If scope creep is confirmed, the constraint''s extractiveness exceeds even its own sovereignty-primacy framing and the coordination function (genuine security) is further decoupled from the extraction function (political suppression), pushing classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_scope_creep, empirical, 'Empirical boundary of NSL enforcement scope').

omega_variable(
    judicial_residual_independence,
    'Does the Hong Kong judiciary retain meaningful independence in non-national-security cases, or has mainland political influence penetrated the entire docket?',
    'Comparative analysis of judicial outcomes in commercial, administrative, and ordinary criminal cases for correlation with mainland political interests; tracking of judicial appointments and promotions for political criteria.',
    'If residual independence exists only in non-political cases, the constraint is tightly scoped; if influence has penetrated commercial and administrative review, the constraint''s scope is broader than its formal classification and effective extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_residual_independence, empirical, 'Depth of mainland influence on HK judiciary').

omega_variable(
    kernel_reading_contest,
    'Is the sovereignty-primacy reading now the operative legal reality to the complete exclusion of the autonomy-primacy reading, or does the autonomy reading retain residual institutional purchase in Hong Kong legal culture or international law?',
    'Monitoring of domestic judicial reasoning for continued citation of autonomy principles; international tribunal and treaty-body findings on Joint Declaration obligations; persistence of autonomy rhetoric in HK legislative and bureaucratic practice.',
    'If the autonomy reading retains residual purchase, the kernel is genuinely contested rather than foreclosed; if sovereignty primacy is total, the forecloses relation to the autonomy reading is fully realized and the kernel has collapsed to a single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel remains contested or has collapsed to one reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(one__tr_t24, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(one__tr_t30, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(one__be_t24, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(one__be_t30, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(one__su_t24, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(one__su_t30, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.1).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the one_country_two_systems_framework kernel, which decomposes into three structurally distinct constraints: autonomy_primacy_reading (treaty-based autonomy), balanced_coexistence_reading (negotiated division), and sovereignty_primacy_reading (unitary sovereignty with delegated autonomy). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through their shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
