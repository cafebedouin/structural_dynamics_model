% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country Two Systems Framework â Autonomy Primacy Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy_primacy_reading of the
 *   one_country_two_systems_framework kernel. Under this reading, the
 *   Sino-British Joint Declaration and Basic Law create a treaty-anchored
 *   constitutional order in which Hong Kong possesses substantive autonomy
 *   with meaningful legal checks on mainland interference; civil liberties
 *   and judicial independence are internationally guaranteed and remain
 *   low-extraction for most residents. The standing arrangement under contest
 *   is the post-1997 governance framework as it has evolvedâparticularly
 *   post-2020ânot the reading's endorsed alternative of full autonomy
 *   restoration. The story authors structural data showing that while the
 *   framework retains genuine coordination functions (economic and legal
 *   continuity), it also operates as asymmetric extraction against
 *   pro-democracy political actors through the National Security Law and
 *   institutional overrides, making it a tangled_rope despite the reading's
 *   rope-like self-presentation. This constraint is one of a family of three
 *   readings (autonomy_primacy, sovereignty_primacy, balanced_coexistence)
 *   that share the same institutional kernel but assign opposite normative
 *   and directional properties to the identical arrangement.
 *
 * KEY AGENTS:
 *   - beijing_central_government: Agenda setter (institutional/constrained) â formally bound by the framework, but captures political gains by overriding autonomy when it conflicts with regime security
 *   - hong_kong_sar_government: Secondary agenda setter (institutional/constrained) â implements Beijing's policy within local institutions, narrowing autonomy
 *   - hong_kong_general_public: Primary beneficiary (organized/constrained) â retains commercial and personal freedoms, loses political voice
 *   - pro_democracy_camp: Primary payer (powerless/trapped) â bears the concentrated costs of NSL prosecution and political disqualification
 *   - hong_kong_judiciary: Beneficiary/institutional anchor (institutional/identity_locked) â retains formal independence but faces NPCSC override
 *   - international_business_community: Secondary beneficiary (powerful/mobile) â benefits from rule of law but can exit if it erodes
 *   - united_kingdom: Excluded observer (institutional/analytical) â treaty party with no enforcement mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.52).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.68).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country Two Systems Framework â Autonomy Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, 'e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0').
narrative_ontology:cs_kernel_codification('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', fixed_text).
narrative_ontology:cs_authority_grounding('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', lineage).
narrative_ontology:cs_interpretation_layer_present('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0').
narrative_ontology:cs_reading_relation('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', foundational, joint_declaration_binding_obligation).
narrative_ontology:cs_axiom_status(joint_declaration_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', joint_declaration_binding_obligation, conventional).
narrative_ontology:cs_axiom('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', foundational, autonomy_as_substantive_check_on_sovereignty).
narrative_ontology:cs_axiom_status(autonomy_as_substantive_check_on_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', autonomy_as_substantive_check_on_sovereignty, deontological).
narrative_ontology:cs_reference_frame('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', treaty_based_autonomous_constitutional_order).
narrative_ontology:cs_drift_state('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', post_national_security_law_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e0b381ba-9e2e-40c9-9301-c1b21e4c4bd0', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_camp).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains constitutional authority over foreign affairs, defense, and national security; is formally bound by the Sino-British Joint Declaration and Basic Law to respect Hong Kong's high degree of autonomy, though under this reading its unilateral interventions constitute treaty violations. Could abrogate the framework but at severe reputational, economic, and internal-governance cost.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, beijing_central_government, agenda_setter,
    institutional, generational, constrained, national).

% Administers Hong Kong under the Basic Law; responsible for local legislation, law enforcement, and public order. Since 2020 has implemented the National Security Law and electoral overhauls under Beijing's direction, progressively narrowing the scope of local autonomy while maintaining the institutional facade of self-administration.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_sar_government, agenda_setter,
    institutional, biographical, constrained, regional).

% The majority of Hong Kong residents who continue to benefit from the separate economic, legal, and immigration systemsâfreedom of movement, contract enforcement under common law, commercial freedoms, and low taxationâwhile facing increasingly constrained political expression, press freedom, and civil liberties.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_general_public, beneficiary,
    organized, biographical, constrained, regional).

% Pro-democracy politicians, activists, journalists, trade unionists, and civil society organizers who have faced prosecution under the National Security Law, disqualification from public office, dissolution of organizations, asset freezes, and suppression of peaceful assembly. They bear the concentrated extraction of the framework's political suppression mechanism.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_camp, payer,
    powerless, immediate, trapped, regional).

% Courts and judges retain formal authority to interpret the Basic Law and adjudicate rights claims under the common law tradition; however, the National Security Law creates parallel procedures, designates mainland security organs as overseers, and the NPCSC retains power to issue binding interpretations that override local final adjudication, progressively constraining the judiciary's independence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary,
    institutional, biographical, identity_locked, regional).

% Multinational corporations, financial institutions, and investors who rely on Hong Kong's separate legal and regulatory systemâparticularly commercial contract enforcement, capital mobility, free convertibility of the currency, and intellectual property protectionsâto access China and Asia-Pacific markets.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community, beneficiary,
    powerful, biographical, mobile, global).

% Co-signatory to the Sino-British Joint Declaration; asserts a continuing legal and moral interest in Hong Kong's autonomy. Systematically excluded from meaningful monitoring or consultation by the PRC, which characterizes the Joint Declaration as a historical document with no continuing validity.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, united_kingdom, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, beijing_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continuity of Hong Kong's capitalist legal and economic system within PRC sovereignty, providing a governance framework that preserves distinct institutions after the 1997 handover and prevents immediate systemic disruption while allowing Beijing to reclaim sovereignty over the territory.
% TRANSFER_FUNCTION: Transfers limited autonomy and delegated sovereignty from Beijing to Hong Kong institutions (legislative, executive, judicial) under a 'high degree of autonomy' guarantee, while Beijing retains control over foreign affairs, defense, andâunder contested interpretationânational security; the transfer is partial and asymmetric.
% ABSENT_VOICES: The United Kingdom as co-signatory to the Joint Declaration; UN Human Rights monitors and special rapporteurs; exiled pro-democracy leaders and diaspora organizations; Taiwanese constitutional observers who would contest the 'internal affair' framing. They are excluded by diplomatic stonewalling, travel bans, rejection of third-party jurisdiction, and the PRC's characterization of the issue as purely domestic.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, Hong Kong would be governed directly under mainland law and the PRC constitution, ending its separate customs territory, common law system, independent currency-peg regime, and visa-free access arrangements; the economic and legal distinction that makes Hong Kong a global financial center would collapse, and the constitutional order would revert to standard provincial status with immediate capital flight and institutional rupture.
% FOUNDING_PROBLEM: How to transfer sovereignty over Hong Kong to the PRC without destroying the economic confidence, legal continuity, and way of life that had developed under British administration, while bridging the incompatible ideological and legal systems of capitalist/liberal governance and socialist party-state rule.
% FOUNDING_PROBLEM_CORROBORATION: The PRC government and HK SAR government attest the problem is solved and the framework functions as designed. The UK Foreign Office, UN human rights treaty bodies, and Hong Kong pro-democracy civil society attest the founding problem has been superseded by a new problemâpreserving autonomy against encroachmentâand the original arrangement has been functionally displaced. International legal scholars outside the PRC beneficiary set corroborate the contested status, noting the post-2020 divergence from the Joint Declaration's original terms.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is authored at moderate level because the framework simultaneously coordinates genuine economic/legal continuity and extracts political submission: the NSL creates criminal offenses for subversion, secession, and collusion that target democratic political activity, while commercial freedoms remain largely intact. Suppression (0.68) reflects the active legal machinery required to sustain this dual operationâelectoral vetting, prosecutions, media closures, and the threat of NPCSC interpretationâthat suppresses the alternative of democratic self-governance. Theater_ratio (0.38) captures the growing gap between the autonomy rhetoric in official discourse and the narrowed operational reality. Accessibility_collapse (0.62) acknowledges that meaningful alternatives (full independence, genuine electoral competition, unfiltered judicial review) have been structurally foreclosed since 2020. Resistance (0.58) registers persistent civil society opposition, diaspora advocacy, and diplomatic pressure despite suppression. The measurement series trace a sharp inflection after 2020, when the NSL and electoral overhaul ratcheted extraction and suppression simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   From the Beijing seat, the framework is a necessary concession to historical circumstance that preserves Hong Kong's economic utility while preventing separatist contagion; from the general public seat, it is a familiar way of life with slowly narrowing political margins; from the pro-democracy seat, it is a collapsing constitutional shelter where the roof leaks exactly on those who need it most. The engine computes these divergences from the same structural factsâdirectionality varies by role and exit options, not by separate constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/payer split is asymmetric by political position rather than class or wealth. The hong_kong_general_public and international_business_community sit near the beneficiary end: their daily economic and commercial activity is protected by the framework's coordination function, giving them low directionality. The pro_democracy_camp sits at the high-target end: the same framework that protects commercial autonomy is actively weaponized against their political expression through NSL enforcement. Beijing sits ambiguouslyâit is formally the agenda setter and capturer of political gains (gain_flow), but under this reading it is also the party constrained by treaty obligations. The automatic derivation treats Beijing as beneficiary given its agenda_setter role and gain_flow capture, which is structurally accurate for the extraction dimension but understates the sovereignty cost the autonomy reading claims the framework imposes. No override is applied because the capturer-of-gains dimension dominates the measurable structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problemâpreventing systemic disruption during the 1997 handover while preserving Hong Kong's distinctivenessâwas substantially solved by the early 2000s. What persists now is not the original coordination challenge but a transformed structure that uses the inherited institutional shell to enforce political conformity. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags this: if the constraint vanished, the world would rearrange (proving it is not inert), yet the original problem it was built to solve is contested/dead, indicating mandatrophy. The framework has not become a pure piton because the extraction is too concentrated and the beneficiaries too clear; it is a tangled_rope where the coordination strand has atrophied and the extraction strand has thickened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_obligation_vs_sovereign_will,
    'Is the Sino-British Joint Declaration a binding international treaty creating enforceable obligations, or a declaratory statement of intent subordinate to PRC constitutional sovereignty?',
    'International court advisory opinion or third-party arbitral ruling on the Joint Declaration''s legal status; or a durable shift in PRC government position acknowledging treaty obligations.',
    'If binding, the autonomy_primacy reading gains legal foundation and the framework''s extractiveness from Beijing is structurally anchored; if declaratory only, the reading collapses toward the sovereignty_primacy position and the constraint reclassifies toward extraction-dominated types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_obligation_vs_sovereign_will, conceptual, 'Legal status of the Joint Declaration as binding treaty or political declaration').

omega_variable(
    judicial_review_efficacy_threshold,
    'At what point does NPCSC interpretation power render Hong Kong judicial review a formalistic rather than substantive check on executive and mainland interference?',
    'Empirical analysis of NSL case outcomes: proportion of cases where local courts deferred to mainland security organs or where defendants were denied bail; comparison with pre-2020 judicial independence metrics.',
    'If judicial review is now formalistic, the autonomy_primacy reading''s claim of meaningful checks is empirically falsified and the constraint''s coordination function is largely theatrical; if substantive review persists in non-NSL domains, the reading retains partial validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_efficacy_threshold, empirical, 'Whether Hong Kong courts still provide substantive constitutional review').

omega_variable(
    civil_liberties_scope_ambiguity,
    'Do civil liberties protections under the autonomy_primacy reading apply to political expression and democratic participation, or only to commercial and personal freedoms?',
    'Comparative analysis of Basic Law Article 23 and NSL implementation: prosecution rates for speech, assembly, and press versus property and commercial disputes; tracking which rights domains remain protected.',
    'If protections have narrowed to commercial/personal freedoms only, the ''low-epsilon for most residents'' claim holds but the reading''s democratic dimension is dead; if political rights are still protected in practice, the reading retains broader validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civil_liberties_scope_ambiguity, empirical, 'Scope of civil liberties protection under the framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 27, 0.62).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 27, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 27, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the one_country_two_systems_framework constraint family. The autonomy_primacy_reading, sovereignty_primacy_reading, and balanced_coexistence_reading are distinct constraints linked by their shared kernel but separated by Îµ-invariance: each reading evaluates the same institutional arrangement through a different normative lens, producing different beneficiary/victim structures and classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
