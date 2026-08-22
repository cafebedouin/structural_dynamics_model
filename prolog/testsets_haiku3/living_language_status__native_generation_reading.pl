% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native Generational Transmission as Living Language Standard
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generation reading of living language status asserts that
 *   linguistic vitality requires native speakers to transmit a language as a
 *   mother tongue in daily use, and that liturgical-only transmission, no
 *   matter how continuous or structured, preserves a 'corpse' rather than a
 *   living language. This reading emerged from secular nationalist movements
 *   seeking to ground linguistic sovereignty in sociological fact rather than
 *   political will. It has become institutional policy in state-education
 *   systems and language-revival bureaucracies. However, it delegitimizes
 *   centuries of successful liturgical transmission (Hebrew in Jewish
 *   practice, Latin in Catholic ritual, Quranic Arabic in Islamic
 *   scholarship) and marginalizes diaspora communities whose
 *   heritage-transmission modes do not match the daily-use environment the
 *   standard presupposes. The constraint is ONE READING of a contested
 *   kernel: the question 'What makes a language living?' admits multiple
 *   coherent answers (native generational transmission, productive literary
 *   creation, continuous liturgical recitation, linguistic structure
 *   independent of speakers). This story instantiates only the
 *   native-generation reading, not the sibling readings. The author's
 *   assessed extractiveness reflects the institutional asymmetry: the
 *   standard legitimates nationalist and state-education institutions while
 *   delegitimizing religious and diaspora communities. The claim/metric
 *   independence is deliberate: this constraint is CLAIMED as tangled_rope
 *   (coordination around vitality measurement + asymmetric institutional
 *   transfer) and the metrics describe substantially extractive, actively
 *   suppressed operation.
 *
 * KEY AGENTS:
 *   - Secular nationalist movements: set the agenda; define vitality; direct resources to secular education; benefit from sovereignty legitimacy.
 *   - Language-revival institutions: collects prestige and funding; operates schools aligned with native-generational standard; benefits from institutional alignment.
 *   - Liturgical-only communities: identity-locked to religious transmission; treated as deficient under the standard; excluded from definitional authority.
 *   - Diaspora heritage speakers: powerless; constrained to multilingual, minority-language contexts; classified as insufficient transmitters.
 *   - Religious institutional authorities: structurally excluded; would argue the standard conflates political sovereignty with linguistic vitality; maintain alternative transmission modes.
 *   - Secular academic linguists: analytical seat; measure outcomes; assess whether native-generation transmission actually produces the claimed vitality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.61).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.72).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native Generational Transmission as Living Language Standard").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'c55fd81b-ca3c-48ef-9a97-3d3a7ee45643').
narrative_ontology:cs_kernel_codification('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', distributed).
narrative_ontology:cs_authority_grounding('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', extraction).
narrative_ontology:cs_reading_relation('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', foundational, native_generational_transmission_criterion).
narrative_ontology:cs_axiom_status(native_generational_transmission_criterion, holdable).
narrative_ontology:cs_axiom_grounding('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', native_generational_transmission_criterion, empirically_contingent).
narrative_ontology:cs_axiom('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', foundational, daily_use_environment_requirement).
narrative_ontology:cs_axiom_status(daily_use_environment_requirement, holdable).
narrative_ontology:cs_axiom_grounding('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', daily_use_environment_requirement, deontological).
narrative_ontology:cs_reference_frame('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', linguistic_nationalist_sovereignty_framework).
narrative_ontology:cs_drift_state('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', contemporary_diaspora_and_religious_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c55fd81b-ca3c-48ef-9a97-3d3a7ee45643', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, language_revival_institutions).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_heritage_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, indigenous_language_revitalization_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define language vitality through native generational transmission; invest in state-backed education, media, and cultural policy to privilege mother-tongue daily use over liturgical or literary contexts. Benefit from the definition because it grounds political legitimacy (sovereignty through living language) and justifies resource allocation to secular institutions over religious ones.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive funding, curriculum authority, and prestige from the native-generation standard. Operate immersion schools, youth programs, and media production aligned with daily-use transmission. Their institutional success and growth depend on demonstrating progress toward the native-generational benchmark.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, language_revival_institutions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, language_revival_institutions, agenda_setter).

% Maintain language vitality through centuries of liturgical study, prayer, and ritual transmission (e.g., Hebrew in Jewish religious practice, Aramaic in Christian liturgy, Quranic Arabic in Islamic scholarship). Under the native-generation standard, their transmission mode is classified as preserving a 'corpse,' delegitimizing their practice and reducing institutional support. They cannot exit identity_locked to the liturgical community without severing religious identity.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, biographical, identity_locked, local).

% Transmit heritage language through family, community ritual, and religious practice in multilingual environments where local vernaculars dominate. The native-generation standard, which requires daily-use environments with full institutional support, treats their intergenerational transmission as insufficient or deficient. They lack the resources to create mother-tongue immersion environments and face pressure to assimilate to dominant local languages.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_heritage_speakers, payer,
    powerless, biographical, constrained, local).

% Conduct empirical research on language vitality, transmission mechanisms, and community-specific outcomes. They occupy an analytical seat from which to measure whether the native-generation standard's claimed outcomes (intergenerational vitality) actually obtain, and whether alternative transmission modes produce measurable language persistence.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_academic_linguists, observer,
    institutional, generational, analytical, global).

% Maintain liturgical languages and regulate their transmission through religious education, ritual practice, and scriptural study. They would argue that the native-generation standard falsely equates linguistic vitality with secular political sovereignty and delegitimizes millennia of religious transmission. Their exclusion from defining vitality is structural to the constraint: the constraint's force depends on ruling their definition out of order.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, religious_institutional_authorities, excluded,
    powerful, civilizational, trapped, global).

% Use the native-generation standard to justify immersion education, family-transmission programs, and institutional support for endangered languages. The standard legitimates their focus on mother-tongue daily use and helps secure funding and policy alignment. They experience the constraint as enabling, though their capacity to create full daily-use environments often remains limited.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, indigenous_language_revitalization_groups, beneficiary,
    organized, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified measurement standard for language vitality that enables resource-allocation coordination: governments, educational institutions, and language-revival organizations can align funding and policy around the native-generation metric without negotiating separate definitions for each language community.
% TRANSFER_FUNCTION: Transfers institutional legitimacy, funding, curriculum authority, and policy priority from religious/liturgical transmission modes and diaspora heritage communities toward secular state-education systems, nationalist movements, and immersion-education institutions. The transfer is justified by the native-generation standard as directing resources to 'living' languages, while treating liturgical and diaspora transmission as 'corpse preservation.'
% ABSENT_VOICES: Religious institutional authorities (priests, rabbis, Islamic scholars, liturgical experts) who maintain centuries-long transmission traditions are structurally excluded from definitional processes — they would argue that the native-generation standard falsely equates linguistic vitality with political sovereignty and ignores the success of liturgical transmission. Diaspora heritage speakers lack institutional voice to contest the standard's applicability to multilingual minority environments.
% DISAPPEARANCE_RATIONALE: If the native-generation standard disappeared, institutional investment would rebalance immediately: funding and policy authority would flow toward liturgical education and diaspora heritage transmission alongside secular immersion programs. Religious institutional authorities would claim equal definitional standing. Diaspora communities would no longer be classified as deficient transmitters. The allocation of resources to language preservation would reorganize around competing definitions of vitality rather than a single nationalist standard.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, as nationalist movements sought to ground linguistic sovereignty in sociological fact rather than historical contingency, the question arose: which languages are genuinely 'living' and thus worthy of state recognition and sovereignty support? How do we distinguish languages with vital speaker communities from dead languages, elite preserves, or historical curiosities maintained by small populations? The native-generation reading answered: a language is living if and only if native speakers transmit it as a mother tongue in daily-life environments, generating new speakers who use the language actively. This standard would enable objective measurement of language vitality and justify resource allocation to truly living languages.
% FOUNDING_PROBLEM_CORROBORATION: Secular nationalist movements and state-education institutions attest the founding problem remains live — language choice is tied to sovereignty claims and resource allocation is contested. Religious scholars and liturgical experts attest the founding problem was misframed: it conflated linguistic vitality (a structural property of language transmission) with political legitimacy (a question about which institutions should hold authority). The native-generation standard answers the political question, not the linguistic one — and it was adopted precisely because it concentrated authority in secular institutions. Diaspora language activists attest that the standard does not apply to minority-language contexts and misrepresents multilingual, community-based transmission as deficient rather than adaptive.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.61 at interval end) because the constraint redistributes institutional legitimacy and resources from religious to secular structures without genuine neutrality — the native-generation standard is presented as a scientific description of vitality but functions to concentrate resources in state and nationalist institutions. Suppression is elevated (0.72) because the constraint's persistence depends on actively downgrading alternative transmission modes as 'corpse preservation' and marginalizing religious institutional authorities from definitional processes. Theater is moderate (0.48): the standard does rest on real sociolinguistic data (native-generation transmission does produce observable intergenerational vitality), but a growing share of institutional activity defends the standard's exclusivity rather than measuring actual language outcomes. The measurement series show extractiveness and suppression rising sharply in the first 15–20 years (phase of institutional consolidation and nationalist movement growth), then plateauing as the standard becomes entrenched and opposition solidifies. Theater ratio rises more gently and plateaus earlier, indicating the standard's performative maintenance becomes more pronounced as time passes. All metrics are authored on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the secular nationalist position, the native-generation standard is a neutral scientific description that correlates language vitality with speaker demographics and daily use. From the liturgical community position, the standard is a political redefinition of vitality designed to delegitimize religious transmission and concentrate resources in secular institutions. From the diaspora perspective, the standard misrepresents multilingual minority transmission as deficient rather than adaptive. The engine computes this divergence: the seat-specific classifications will differ because the structural relationship to the constraint differs (beneficiary vs. target vs. excluded). The authored claim (tangled_rope) asserts both coordination (unified measurement enabling resource allocation) and extraction (asymmetric institutional transfer). The authored metrics describe substantially extractive operation; the engine determines whether the measured values support or diverge from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements are the structural beneficiary (d approaches 0.0 — low extraction, institutional arbitrage, mobile exit): they benefit from resource concentration and sovereignty legitimacy. Language-revival institutions are near-beneficiary (d ~0.1–0.2): they collect resources and prestige, though they are subordinate to nationalist movements. Liturgical communities are targets (d approaches 1.0 — high extraction, identity-locked exit, constrained alternatives): they lose institutional legitimacy and resources while being unable to exit the religious identity that sustains their transmission mode. Diaspora heritage speakers are targets (d ~0.8 — high extraction, powerless position, constrained alternatives): they bear the cost of institutional misalignment without capacity to reshape the standard. Religious institutional authorities are structurally excluded rather than classified on the d axis — their exclusion IS the enforcement mechanism. Academic linguists occupy the analytical seat (d = 0.5 — symmetric; they measure but do not collect or pay).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'Which languages deserve sovereignty support and state resources?' — was genuine in the early 20th century when nationalist movements sought to ground linguistic claims in sociological fact rather than historical contingency. However, the founding problem's status is now contested: secular nationalists attest it remains live (language choice still affects sovereignty and policy), while religious scholars and diaspora activists attest the problem was misframed and the native-generation standard answers a political question disguised as a linguistic one. The constraint would resolve mandatrophy if the founding problem were dead AND the arrangement persisted anyway; here, the founding problem's status is contested but the arrangement persists with substantial institutional support. Mandatrophy is not yet resolved, but the contest over the founding problem's status is precisely where the constraint's extractiveness registers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_vs_political_vitality,
    'Is the native-generation standard a description of linguistic vitality (structural property of language-transmission systems), or a political definition of which languages deserve state resources and sovereignty support?',
    'Comparative analysis of language outcomes: do languages with native-generation transmission show measurably greater structural stability, vocabulary innovation, and grammatical transmission than liturgical or literary traditions? Or do liturgical and literary transmission modes produce equivalent linguistic outcomes through different institutional channels?',
    'If the standard is primarily political (defines resource distribution, not vitality), the constraint reclassifies from tangled_rope (coordination + asymmetric transfer) toward snare (pure extraction under a false coordination narrative). If the standard is primarily linguistic (native-generation transmission measurably correlates with vitality), then the extraction component is justified by real coordination differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(linguistic_vs_political_vitality, empirical, 'Whether the native-generation standard describes linguistic structure or political resource allocation.').

omega_variable(
    liturgical_transmission_efficacy,
    'Do liturgical-only transmission modes, over centuries-long timescales, produce equivalent linguistic outcomes to mother-tongue daily use — stable vocabulary, productive morphology, transmission to new generations within the religious community?',
    'Historical linguistic analysis of liturgical languages: Hebrew, Latin, Arabic, Aramaic across 500+ year timescales. Measure: vocabulary innovation, grammatical complexity, community competence, intergenerational transmission rates within the liturgical context.',
    'If liturgical transmission proves efficacious (languages remain structurally intact, communities maintain competence, new generations acquire the language through liturgy), then the native-generation standard''s characterization of liturgical language as a ''corpse'' is falsified and the constraint becomes extractive misrepresentation. If liturgical transmission produces measurably degraded outcomes (dead language, community incompetence, no real transmission to new generations), then the standard''s classification is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_transmission_efficacy, empirical, 'Whether centuries-long liturgical transmission produces linguistically viable outcomes.').

omega_variable(
    diaspora_transmission_adaptivity,
    'In multilingual diaspora contexts where daily-use immersion in the heritage language is structurally impossible, do alternative transmission modes (family speech, community ritual, educational supplements) produce measurable intergenerational language retention that differs from the standard''s prediction?',
    'Longitudinal study of diaspora language outcomes: measure second- and third-generation heritage speaker competence across diaspora communities with varying transmission intensity. Compare outcomes to the native-generation standard''s expectation (should fail to transmit; diaspora heritage speakers should not remain competent without daily-use immersion).',
    'If diaspora transmission produces measurable competence retention despite lack of daily-use environment, the standard misclassifies diaspora language vitality and misallocates resources. If diaspora communities consistently lose competence and the standard''s prediction holds, then the standard accurately describes diaspora language dynamics, though it may still be extractive in delegitimizing adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_transmission_adaptivity, empirical, 'Whether diaspora heritage transmission can produce language retention without daily-use immersion.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint (native-generation reading) a coherent linguistic claim about language vitality, or is it a political reading of the contested kernel ''living language status'' that conflates linguistic vitality with nationalist institutional interests?',
    'Historical analysis of the reading''s emergence: did the native-generation standard arise from linguistic research independently, or was it adopted/promoted by nationalist movements seeking to ground political claims in scientific authority? What role did religious institutional resistance play in shaping the standard''s rhetorical opposition to liturgical transmission?',
    'If the standard arose from independent linguistic research, the constraint is primarily a linguistic claim with secondary political consequences. If the standard was adopted by nationalist movements specifically to delegitimize religious transmission and centralize language authority, the constraint is primarily political and the linguistic justifications are post-hoc cover story. This resolves what cs_structure.reading_relations should register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the native-generation reading is a scientific linguistic claim or a political-nationalist reading of the kernel.').

omega_variable(
    suppression_internalization_depth,
    'Is the suppression of alternative transmission modes (measured at 0.72) structurally enforced through institutional policy and exclusion, or has the standard become internalized such that diaspora and minority speakers spontaneously devalue their own heritage transmission?',
    'Post-exit suppression trajectory: if a diaspora speaker achieves successful heritage-language transmission outside the native-generation institutional context, does the suppression diminish or persist? Do diaspora communities that institutionally support heritage transmission (independent heritage schools, liturgical education) show reduced suppression despite lacking state backing?',
    'If suppression is primarily structural, removing institutional enforcement (state backing withdrawal, open funding for alternative transmission modes) would substantially reduce it. If suppression is primarily internalized, diaspora communities would continue devaluing their own transmission even after institutional barriers are removed. Mixed result (both mechanisms present) would suggest the constraint carries internalized suppression that would persist after structural barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Structural vs. internalized component of suppression of alternative transmission modes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(livi_tr_t5, living_language_status__native_generation_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(livi_tr_t10, living_language_status__native_generation_reading, theater_ratio, 10, 0.43).
narrative_ontology:measurement(livi_tr_t15, living_language_status__native_generation_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(livi_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(livi_tr_t25, living_language_status__native_generation_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(livi_tr_t30, living_language_status__native_generation_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(livi_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(livi_be_t5, living_language_status__native_generation_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(livi_be_t10, living_language_status__native_generation_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(livi_be_t15, living_language_status__native_generation_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(livi_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(livi_be_t25, living_language_status__native_generation_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(livi_be_t30, living_language_status__native_generation_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(livi_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(livi_su_t5, living_language_status__native_generation_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(livi_su_t10, living_language_status__native_generation_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(livi_su_t15, living_language_status__native_generation_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(livi_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(livi_su_t25, living_language_status__native_generation_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(livi_su_t30, living_language_status__native_generation_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(livi_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The 'living language status' kernel admits three structurally distinct constraint stories: (1) native_generation_reading (this file) — linguistic vitality defined by native-speaker generational transmission; (2) liturgical_preservation_reading — vitality sustained through continuous religious ritual and sacred study; (3) literary_continuity_reading — vitality grounded in productive literary and intellectual creation. Each reading instantiates a different constraint with different beneficiary/victim structures, different ε values, and different types. The readings coexist in public discourse — different communities hold each simultaneously — but they compete over institutional resources and definition authority. Each constraint story is linked via network.affects_constraints to document the kernel structure; the readings are NOT perspectives on a single constraint, but three separate constraints that share a kernel and compete for legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
