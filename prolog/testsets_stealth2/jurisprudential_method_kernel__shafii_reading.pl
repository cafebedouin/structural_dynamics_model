% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Four-Tier Source Hierarchy with Transmission Arbitration
 *   domain: religious/legal/institutional-history
 *
 * SUMMARY:
 *   In the late eighth and ninth centuries, Islamic legal practice was
 *   regionally fragmented: Kufan jurists extended revelation through
 *   disciplined reasoning, Medinan jurists deferred to the city's continuous
 *   practice, and each center treated its own method as faithful to the
 *   Prophet. Al-Shafi'i's methodological standardization replaced this
 *   pluralism with a single derivational procedure — Qur'an, then
 *   authenticated transmitted reports, then consensus, then tightly bounded
 *   analogy — in which verified transmission outranks both local practice and
 *   juristic preference whenever they conflict. The arrangement solves a real
 *   collective-action problem (portable, consistent, defensible rulings
 *   across a continental polity) while transferring source-authority to the
 *   specialists who perform authentication. The claim/metric independence
 *   rule applies: claimed_type records the structural reading (tangled_rope —
 *   genuine coordination plus asymmetric extraction through the same
 *   mechanism); the metrics record the arrangement's observed operation and
 *   are not tuned to the claim.
 *
 * KEY AGENTS:
 *   - hadith_transmission_scholars: Primary beneficiary (organized/mobile) — authentication verdicts arbitrate what counts as law beyond the Qur'an
 *   - usul_methodology_establishment: Agenda setter (institutional/identity_locked) — administers, teaches, and reproduces the tier-order
 *   - transregional_judges: Secondary beneficiary (institutional/mobile) — gain a portable, defensible decision procedure
 *   - medinan_customary_practice_defenders: Primary target (organized/constrained) — living practice demoted from source to evidence
 *   - juristic_reasoning_extenders: Target (organized/constrained) — discretionary analogy narrowed to fourth-tier bounded form
 *   - lay_muslim_communities: Diffuse beneficiary and bearer of costs (moderate/identity_locked)
 *   - local_custom_bearers: Excluded party (powerless/trapped) — practice judged without a seat in the proceedings
 *   - caliphal_administration: Analytical observer (institutional/analytical) — watches the self-administering legal class take shape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.62).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.45).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Four-Tier Source Hierarchy with Transmission Arbitration").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "religious/legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '73f09bb6-0977-4c8f-9ae8-7c71c0c38d06').
narrative_ontology:cs_kernel_codification('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', fixed_text).
narrative_ontology:cs_authority_grounding('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', lineage).
narrative_ontology:cs_interpretation_layer_present('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06').
narrative_ontology:cs_reading_relation('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', jurisprudential_method_kernel__hanafi_reading, influences).
narrative_ontology:cs_reading_relation('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', foundational, authenticated_reports_override_local_practice).
narrative_ontology:cs_axiom_status(authenticated_reports_override_local_practice, holdable).
narrative_ontology:cs_axiom_grounding('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', authenticated_reports_override_local_practice, theological).
narrative_ontology:cs_axiom('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', foundational, single_ranked_source_hierarchy_required).
narrative_ontology:cs_axiom_status(single_ranked_source_hierarchy_required, holdable).
narrative_ontology:cs_axiom_grounding('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', single_ranked_source_hierarchy_required, instrumental).
narrative_ontology:cs_reference_frame('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', four_tier_transmission_arbitrated_hierarchy).
narrative_ontology:cs_drift_state('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', post_classical_madhhab_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73f09bb6-0977-4c8f-9ae8-7c71c0c38d06', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, usul_methodology_establishment).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, transregional_judges).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, lay_muslim_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, medinan_customary_practice_defenders).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, juristic_reasoning_extenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, lay_muslim_communities).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, isnad_transmission_reliability).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, single_ranked_source_hierarchy_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Specialize in verifying reports attributed to the Prophet through chain (isnad) and text (matn) criticism. Under the new derivational order, their authentication verdicts settle disputes that cities and jurists previously settled by appealing to their own practice or reasoning. Students, patronage, and judicial appointments flow toward their seminars, and their verdicts travel with the merchants and judges who rely on them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars, beneficiary,
    organized, generational, mobile, continental).

% Teach the derivational procedure in mosques and study circles, train the judges, write the commentaries that apply the tier-order to new cases, and staff the councils where disputed questions are settled. Their careers, students, and institutional posts exist inside the procedure they administer; stepping outside it would mean forfeiting the standing that lets them speak to the law at all.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, usul_methodology_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, usul_methodology_establishment, beneficiary).

% Appointed to provincial courts far from their home cities, they need rulings that survive scrutiny at the center. The fixed tier-order gives them a defensible answer for every conflict between a report and local usage, and appointment boards increasingly expect demonstrated fluency in the procedure.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, transregional_judges, beneficiary,
    institutional, biographical, mobile, continental).

% Inherit a continuous civic practice in Medina that their teachers treated as authoritative where reports were silent or conflicting. The new order reclassifies that practice: it no longer stands as a source on its own but must be corroborated report-by-report or yield. Elders spend their remaining years re-deriving inherited rulings from texts their grandfathers never needed to cite.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, medinan_customary_practice_defenders, payer,
    organized, generational, constrained, regional).

% Jurists trained in Kufa and Basra whose reasoned extensions — preference-adjusted analogy, equity adjustments — carried weight in courts. The new order confines analogy to a fourth-tier, tightly bounded form and treats preference-based adjustment as unauthorized legislation. Their discretionary room contracts to what the procedure explicitly permits.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, juristic_reasoning_extenders, payer,
    organized, biographical, constrained, continental).

% Worship, marry, trade, and inherit under rules they receive from judges and scholars. Predictable, portable rulings let contracts and marriages hold across regions. Where their inherited local custom conflicts with an authenticated report, the custom loses, and they have no standing to challenge the authentication itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, lay_muslim_communities, beneficiary,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, lay_muslim_communities, payer).

% Village and urban communities whose daily practice predates or runs alongside the scholarly literature — irrigation customs, market usages, family arrangements. When their practice reaches a judge trained in the procedure, it is weighed against reports they have never seen and cannot produce; no council deciding source-status includes a seat for them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, local_custom_bearers, excluded,
    powerless, generational, trapped, regional).

% Governors, the chief-justice apparatus, and the fiscal bureaucracy watch an increasingly self-administering legal class take shape. A uniform procedure would simplify oversight and appeals, but in this period the state neither authors the hierarchy nor staffs its enforcement; it observes and occasionally arbitrates jurisdictional quarrels.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, caliphal_administration, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_transmission_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single derivational procedure for divine law: a fixed priority order among sources and a verification standard (transmission criticism) for resolving conflicts between an authenticated report and established local practice, so jurists in different regions reach the same ruling from the same materials.
% TRANSFER_FUNCTION: Moves legal source-authority — the standing to say what the law is when texts and practice conflict — from holders of living customary practice and discretionary juristic reasoning to specialists in report authentication and the scholars who administer the derivational procedure; interpretive labor correspondingly shifts from maintaining local practice to verifying chains of transmission.
% ABSENT_VOICES: Communities whose inherited custom stands to be overruled had no seat where source-status was decided; nor did those without access to the transmission literature — notably non-elites and women whose family-law customs were evaluated against reports they could not cite or contest. Their objection would be that authenticity verdicts about their lives are rendered by a class they cannot examine.
% DISAPPEARANCE_RATIONALE: Without the four-tier hierarchy, the shared grammar of Sunni legal reasoning collapses: courts lose the tie-breaker between report and practice, the schools lose their common procedure, and derivation fragments back into regional practice or bare literalism; legal education, judicial appointment, and fatwa authority all reorganize around whatever replaces the arbitration standard.
% FOUNDING_PROBLEM: After rapid conquest fused diverse regions into one polity, divine law was one in principle but many in practice — Kufa, Medina, and Syria ruled differently on the same questions. The founding problem: how to derive a single, verifiable statement of God's law from revelation when transmitted reports and established regional practice disagree.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: modern reformist jurists who reject the school settlement nonetheless affirm the derivation problem as unresolved; academic historians of Islamic law working outside the tradition document the fragmentation crisis the methodology answered; and intra-Muslim critics of hadith-centrism concede the original inconsistency problem was real while disputing the remedy.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness settles at 0.62: the arbitration mechanism genuinely transfers source-standing from custom and reason to transmission specialists, and the transfer widens as the framework consolidates (series rises 0.45 to 0.62), but a large share of the structure's operation is the irreducible cost of any verification regime — hence medium-high rather than severe. Suppression is 0.45 as a RAW structural property (unscaled by power or scope): the framework suppresses rival methodologies argumentatively — branding preference-based adjustment as unauthorized legislation, demoting practice to evidence — and entrenches through curricula and career gates rather than coercive machinery. Theater_ratio is low-to-moderate (0.25 at interval end): isnad criticism is real scholarship doing real filtering, but a growing share of authentication activity functions as credential performance inside the schools (series 0.08 to 0.25). Accessibility_collapse is 0.7: within the framework, alternatives collapse almost completely — once the tier-order is accepted, deriving law from uncorroborated practice is simply unavailable — though meta-level exit (affiliating with another school) persists, keeping it below mountain-grade. Resistance is 0.6: the documented Baghdad and Egypt disputes show organized pushback from both targeted seats, and the final classical settlement absorbed concessions rather than annihilating rivals. All three metric series share one time grid (points 0, 15, 30, 45, 60, 75) so no metric is sampled against another's end-state. The suppression_requirement series is deliberately non-monotonic: enforcement effort peaks mid-interval during the open polemical contests, then declines as the framework becomes self-enforcing through education and appointment incentives — the story tracks enforcement-capacity change, which is why the series is authored at all.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats, the hierarchy is the necessary grammar of revelation — without it, divine law is unverifiable opinion; the establishment experiences the arrangement as the discovery of law's true structure, not as a choice among alternatives. From the payer seats, the same mechanism operates as dispossession: a distant philological elite now decides, by criteria the locals cannot examine, whether the law their grandparents practiced is law at all. Transregional judges experience it as professional convenience and protection. The engine computes these per-seat classifications from the structural data; the divergence between the establishment's self-understanding and the targets' experience is the perspectival fact this story encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for hadith_transmission_scholars (the gains accrue to their seat — hence gain_flow names them), usul_methodology_establishment (agenda_setter with secondary beneficiary position), and transregional_judges; lay_muslim_communities sit near symmetric (predictability gained, custom lost where it conflicts). Victim declarations drive high directionality for medinan_customary_practice_defenders and juristic_reasoning_extenders, amplified by constrained exit: their expertise and standing are denominated in the very sources the hierarchy demotes. Exit modulation matters: the establishment is identity_locked (its members' scholarly selves are constituted by the procedure), which anchors it at the beneficiary end regardless of formal power, while the transmission scholars' mobility spreads their gains continent-wide. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct per-seat relationships, and the override surface is keyed by power atom, which would misfire across this set's heterogeneous seats sharing power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regional inconsistency in divine law — remains live, so the arrangement is not a piton maintained by inertia, and it carries no sunset clause, so it is not a scaffold. The genuine coordination function (portable, verifiable adjudication) blocks a snare mislabel: the extraction rides on real coordination, not on a cover story. The asymmetric transfer of source-authority blocks a rope mislabel: identifiable seats pay through the same mechanism that coordinates. Reading the constraint as tangled_rope keeps both facts visible simultaneously — which is exactly what the payer and beneficiary seats dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the jurisprudential_method_kernel; would instantiating a sibling reading instead (one that recognizes living custom as an independent source, or legitimizes reason-extension) change the victim set and epsilon?',
    'Compile the four sibling stories and compare computed classifications; locate the disagreement in the source-status assigned to customary practice and to the permitted breadth of analogy.',
    'Under a practice-weighted sibling reading, medinan_customary_practice_defenders leave the victim set and extraction relocates onto whichever seat the sibling demotes; the epsilon of the SAME kernel label varies by reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story instantiates one of four readings of a shared kernel; classification is indexical to the reading.').

omega_variable(
    shafii_attribution_authenticity,
    'Is the four-tier systematization authentically al-Shafi''i''s own composition (the Risala as traditionally received), or a later school crystallization retrojected onto him?',
    'Manuscript philology and redaction history of the early usul literature: date the stratification of the Risala''s recensions against the school''s formative commentaries.',
    'If the systematization is a later crystallization, the agenda_setter seat dates decades later than authored, the founding-problem genealogy compresses, and the early-interval measurements describe a proto-arrangement rather than the operating constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shafii_attribution_authenticity, empirical, 'Historical attribution of the methodology to its named founder.').

omega_variable(
    arbitration_cost_vs_rent,
    'Is the authority transferred to transmission specialists a necessary cost of ANY verifiable arbitration mechanism between report and practice, or a gatekeeping rent captured by controlling authentication?',
    'Compare consistency and appeal-survivability of rulings under transmission-arbitrated versus practice-weighted regimes, and test whether authentication standards tightened where they protected incumbents versus where reliability demanded it.',
    'If the transfer is a necessary coordination cost, effective extraction falls toward the rope boundary; if it is gatekeeping rent, the constraint trends toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitration_cost_vs_rent, empirical, 'Whether the authority concentration is coordination overhead or captured rent.').

omega_variable(
    amal_independent_source_status,
    'Did Medinan living practice ever operate as a genuinely independent source of law before the standardization, or was it always implicit application of texts?',
    'Trace pre-standardization Medinan legal responsa for rulings issued where no report existed and practice alone was invoked as warrant.',
    'If practice was never independent, the hierarchy clarified rather than expropriated, and measured extraction drops; if it was independent, the hierarchy stripped a real source from its holders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amal_independent_source_status, conceptual, 'Whether the demoted victim-source was real before the hierarchy, determining the size of the transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t15, jurisprudential_method_kernel__shafii_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(juri_tr_t15, observed).
narrative_ontology:measurement(juri_tr_t30, jurisprudential_method_kernel__shafii_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(juri_tr_t30, observed).
narrative_ontology:measurement(juri_tr_t45, jurisprudential_method_kernel__shafii_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement_basis(juri_tr_t45, observed).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__shafii_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(juri_tr_t60, observed).
narrative_ontology:measurement(juri_tr_t75, jurisprudential_method_kernel__shafii_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(juri_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t15, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement_basis(juri_be_t15, observed).
narrative_ontology:measurement(juri_be_t30, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(juri_be_t30, observed).
narrative_ontology:measurement(juri_be_t45, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 45, 0.59).
narrative_ontology:measurement_basis(juri_be_t45, observed).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(juri_be_t60, observed).
narrative_ontology:measurement(juri_be_t75, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(juri_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t15, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(juri_su_t15, observed).
narrative_ontology:measurement(juri_su_t30, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(juri_su_t30, observed).
narrative_ontology:measurement(juri_su_t45, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement_basis(juri_su_t45, observed).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement_basis(juri_su_t60, observed).
narrative_ontology:measurement(juri_su_t75, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement_basis(juri_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, information_standard).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how Islamic law derives from revelation' covers four structurally distinct arrangements, one per reading of the jurisprudential_method_kernel. This file instantiates the shafii_reading only — a strict ranked hierarchy with transmission authentication as arbiter — with its own epsilon, beneficiary set (transmission specialists), and victim set (customary practice and reason-extension as independent sources). The sibling files assign source-status differently and therefore carry different epsilon and victim sets; the family is linked through affects_constraints so contamination and comparison analyses traverse all four readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
