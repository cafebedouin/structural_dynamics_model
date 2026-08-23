% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: NSL as Sovereign Security Instrument Restoring Constitutional Order
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law (NSL) enacted June 30, 2020 by NPCSC Standing
 *   Committee is read here as a legitimate exercise of sovereign authority to
 *   restore constitutional order after the 2019 protests created a governance
 *   crisis. This reading — the sovereignty_restoration_reading — claims the
 *   NSL coordinates a genuine security function (public order, sovereign
 *   integrity) while extracting asymmetrically from political opposition. The
 *   constraint targets organizers, legislators, press, and civil society
 *   (victims) while benefiting CPG authority, SAR government, and pro-Beijing
 *   establishment (beneficiaries). General population receives mixed
 *   coordination benefit (restored order) and extraction cost (lost
 *   liberties). The claimed_type is tangled_rope: real coordination function
 *   exists but asymmetric extraction and active enforcement are structural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.45).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.65).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "NSL as Sovereign Security Instrument Restoring Constitutional Order").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'f0f87f26-995b-4bb8-b058-18a75794f332').
narrative_ontology:cs_kernel_codification('f0f87f26-995b-4bb8-b058-18a75794f332', formalized).
narrative_ontology:cs_authority_grounding('f0f87f26-995b-4bb8-b058-18a75794f332', lineage).
narrative_ontology:cs_interpretation_layer_present('f0f87f26-995b-4bb8-b058-18a75794f332').
narrative_ontology:cs_reading_relation('f0f87f26-995b-4bb8-b058-18a75794f332', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0f87f26-995b-4bb8-b058-18a75794f332', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('f0f87f26-995b-4bb8-b058-18a75794f332', foundational, sovereign_security_primacy).
narrative_ontology:cs_axiom_status(sovereign_security_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f0f87f26-995b-4bb8-b058-18a75794f332', sovereign_security_primacy, conventional).
narrative_ontology:cs_axiom('f0f87f26-995b-4bb8-b058-18a75794f332', secondary, constitutional_order_restoration_legitimacy).
narrative_ontology:cs_axiom_status(constitutional_order_restoration_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f0f87f26-995b-4bb8-b058-18a75794f332', constitutional_order_restoration_legitimacy, conventional).
narrative_ontology:cs_reference_frame('f0f87f26-995b-4bb8-b058-18a75794f332', npcsc_interpretive_authority).
narrative_ontology:cs_drift_state('f0f87f26-995b-4bb8-b058-18a75794f332', post_2020_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0f87f26-995b-4bb8-b058-18a75794f332', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_beijing_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, general_hong_kong_population).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, general_hong_kong_population).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereign_security_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, constitutional_order_restoration).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, npcsc_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted NSL via NPCSC decision bypassing Hong Kong legislature; defines security offenses and asserts jurisdiction over 'complex' cases. Collects direct control over Hong Kong's security architecture and legislative veto. Faces no meaningful exit constraint — the constraint is its own instrument.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Implements NSL through local police, prosecution, and Committee for Safeguarding National Security. Gains restored executive authority after 2019 paralysis but operates under Beijing's direct oversight. Cannot exit the arrangement without losing legitimacy; constrained by dependence on central approval.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hong_kong_sar_government, beneficiary).

% Business elites, professional associations, and rural committees aligned with Beijing. Gain stabilized operating environment, privileged access to mainland markets, and political patronage. Can redirect capital and operations to mainland if Hong Kong becomes untenable — arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_beijing_establishment, beneficiary,
    powerful, generational, arbitrage, regional).

% Face prosecution for subversion, secession, terrorism, collusion for organizing protests, primaries, or advocacy. Professional and personal identities fused to democratic resistance; exit means abandoning life-project and community. Prison sentences of 10+ years create structural trap; identity lock makes psychological exit near-impossible.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    organized, biographical, identity_locked, local).

% Apple Daily shuttered, Stand News closed, journalists arrested for sedition. Remaining outlets self-censor or relocate. Exit means moving operations overseas (costly, loses local reach) or closing. Advertising revenue collapsed under political pressure — constrained but not fully trapped.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_press, payer,
    moderate, biographical, constrained, local).

% Unions, student unions, rights groups disbanded or leaders arrested (Hong Kong Confederation of Trade Unions, Civil Human Rights Front, Amnesty HK). Assets frozen, bank accounts closed. Can reincarnate overseas or underground but lose local operational capacity and legitimacy.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, civil_society_organizations, payer,
    organized, biographical, constrained, local).

% Disqualified en masse 2020; remaining resign en masse. 47 activists charged with subversion for unofficial primary. Political career path eliminated; no electoral path to return. Prison sentences remove them physically; those at large face arrest. Exit only via exile — trapped in place or forced out.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators, payer,
    moderate, biographical, trapped, local).

% Experience restored public order and MTR reliability (benefit) but lose protest rights, legislative representation, and judicial independence (cost). Emigration wave (200k+ 2020-2023) shows exit is possible but costly — family ties, assets, career anchor. BNO pathway offers UK route for ~3M eligible.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, general_hong_kong_population, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, general_hong_kong_population, payer).

% UN human rights bodies, foreign governments, legal scholars monitor compliance with ICCPR, Sino-British Joint Declaration. Issue reports, sanctions, travel advisories. No enforcement power; analytical seat only. Their assessments shape diplomatic pressure but cannot alter constraint directly.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores public order and sovereign authority after 2019 protests paralyzed governance, enabling Hong Kong to function as a stable special administrative region under 'one country, two systems'.
% TRANSFER_FUNCTION: Moves political contestation space and legislative veto power from opposition forces to central authority; transfers security jurisdiction from common law courts to designated judges and NPCSC interpretation; moves organizational survival of civil society from autonomous registration to political loyalty review.
% ABSENT_VOICES: Hong Kong voters never consulted on NSL — NPCSC enacted it directly. The 600k+ who voted in 2020 pro-democracy primary are criminalized for participation. International legal community (ICCPR committee, common law jurists) excluded from interpretation; NPCSC reserves final interpretive power.
% DISAPPEARANCE_RATIONALE: If NSL vanished overnight: protest dynamics would resume within weeks; legislative veto would return to opposition; Beijing's direct enforcement apparatus (Office for Safeguarding National Security) would withdraw; SAR government would lose its primary tool for disqualifying opposition; civil society would reconstitute. The 2019 governance vacuum would reopen.
% FOUNDING_PROBLEM: 2019 anti-extradition protests escalated into citywide paralysis, legislative siege, university occupations, and alleged foreign interference — creating a security vacuum the SAR government could not fill under Basic Law Article 23's failed 2003 attempt.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and SAR government attest the security crisis remains live, citing ongoing 'soft resistance' and foreign interference. International legal scholars (Bingham Centre, ICJ), Hong Kong democrats in exile, and UN Human Rights Committee attest the founding problem was manufactured/pretextual — protest demands were democratic, not secessionist, and Article 23 legislation was deliberately stalled by Beijing after 2003.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).
:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45 reflects targeted extraction — opposition bears concentrated costs (prison, exile, organizational death) while general population bears diffuse costs. Not a snare because coordination function (public order restoration) is genuine and acknowledged even by critics. Suppression 0.65 reflects criminalization of previously protected activities (primaries, slogans, reporting) and NPCSC interpretation overriding common law. Theater 0.35: security apparatus performs 'law-based governance' rhetoric while designated judges and closed trials undermine procedural legitimacy. Accessibility_collapse 0.55: emigration and self-censorship close exits but BNO pathway and overseas operations preserve partial alternatives. Resistance 0.75: mass resignations, primaries, international sanctions, and ongoing covert resistance show high active opposition.
 *
 * PERSPECTIVAL GAP:
 *   From CPG/SAR seats: constraint is rope — genuine coordination solving 2019 chaos. From opposition seats: constraint is snare — cover for criminalizing dissent. From general population: tangled rope — real order restored but at real liberty cost. From international observers: constraint violates ICCPR/Sino-British Declaration — illegitimate. The engine computes these divergences from structural data; the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   CPG authority sits at beneficiary pole (d ~0.05) — constraint is its instrument, collects sovereignty dividends. SAR government d ~0.25 — administers constraint but constrained by Beijing. Pro-Beijing establishment d ~0.15 — benefits with arbitrage exit. Opposition activists d ~0.95 — identity-locked targets facing life sentences. Independent press d ~0.8 — constrained but can relocate. Civil society d ~0.75 — organizations can reconstitute overseas. Opposition legislators d ~0.9 — trapped, no electoral return path. General population d ~0.5 — symmetric (order benefit vs liberty cost). International observers d ~0.0 (analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (restoring order) is live — 2019 paralysis was real. But the extraction function has expanded beyond founding scope: NSL now polices 'soft resistance' (commemoration, social media, education), not just violence/secession. The arrangement persists because CPG authority extracts sovereign control dividends; fixing cost is prohibitive for SAR government (would require Beijing's consent). Mandatrophy not resolved — coordination mandate has been extended into permanent political control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the sovereignty_restoration_reading represent a genuine structural reading of the NSL, or is it a post-hoc legitimating frame for what is structurally a democratic enclosure?',
    'Compare enforcement patterns: if >80% of NSL cases target non-violent political expression (primaries, slogans, journalism) rather than violence/terrorism, the security restoration frame loses empirical support.',
    'If security frame is pretextual, the constraint reclassifies from tangled_rope to snare — coordination function collapses, extraction becomes primary. This reading''s claimed_type would be falsified by its own enforcement data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, empirical, 'Whether the sovereignty restoration claim matches enforcement reality').

omega_variable(
    common_law_erosion_vs_restoration,
    'Does NPCSC interpretation authority under NSL Article 65 restore constitutional order (Basic Law supremacy) or erode common law autonomy (judicial independence, fair trial)?',
    'Track designated judge appointments, closed trial frequency, NPCSC interpretation overrides of CFA rulings. Measure divergence from pre-2020 common law norms.',
    'If common law erosion is structural, jurisdictional_capture_reading gains empirical ground; sovereignty_restoration_reading''s vindicated_proposition ''constitutional_order_restoration'' becomes contested. The constraint family''s network influence shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_erosion_vs_restoration, conceptual, 'Whether NPCSC interpretive power restores or captures constitutional order').

omega_variable(
    general_population_extraction_diffusion,
    'Is the general Hong Kong population a net beneficiary (restored order) or net payer (lost liberties) of the NSL, and does the BNO emigration wave indicate revealed preference?',
    'Longitudinal surveys on security vs liberty tradeoffs; emigration rates by demographic; capital flow data. Compare revealed preference (feet) vs declared preference (polls under constraint).',
    'If population is net payer, the constraint''s coordination function weakens — tangled_rope requires genuine beneficiaries. If net beneficiary, tangled_rope classification strengthens. Affects directionality derivation for general_population stakeholder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(general_population_extraction_diffusion, empirical, 'Whether general population experiences net benefit or net extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_sov_rest_tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(nsl_sov_rest_tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(nsl_sov_rest_tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement(nsl_sov_rest_tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.35).
narrative_ontology:measurement(nsl_sov_rest_tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.35).
narrative_ontology:measurement(nsl_sov_rest_tr_t2025, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(nsl_sov_rest_be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(nsl_sov_rest_be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(nsl_sov_rest_be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.44).
narrative_ontology:measurement(nsl_sov_rest_be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(nsl_sov_rest_be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(nsl_sov_rest_be_t2025, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nsl_sov_rest_su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(nsl_sov_rest_su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(nsl_sov_rest_su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement(nsl_sov_rest_su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.65).
narrative_ontology:measurement(nsl_sov_rest_su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement(nsl_sov_rest_su_t2025, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the nsl_legal_text constraint family. All three share the same legal text (kernel) but instantiate different constraints with different ε, beneficiary/victim structures, and types. This reading (sovereignty_restoration) claims tangled_rope with moderate ε=0.45 targeting opposition; democratic_enclosure_reading claims snare with high ε targeting all dissent; jurisdictional_capture_reading claims tangled_rope/snare hybrid with ε targeting legal autonomy. The upstream claim (sovereignty restoration) is cited by Beijing to legitimize the downstream effects (enclosure, capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, institutional, 0.05).
constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, organized, 0.85).
constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
