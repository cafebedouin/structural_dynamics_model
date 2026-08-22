% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems — Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primacy reading of the One
 *   Country, Two Systems kernel: Hong Kong's autonomy is a delegation from,
 *   not a limit on, PRC sovereign authority, and is revocable or overridable
 *   wherever the central government determines national security or
 *   territorial integrity is at stake. Under this reading, the 1997 handover
 *   created a functioning coordination arrangement (preserving Hong Kong's
 *   commercial and legal distinctiveness while reincorporating the
 *   territory), but the 2020 National Security Law and its enforcement
 *   apparatus convert that arrangement into one that also actively extracts
 *   political compliance from Hong Kong's population, judiciary, and civil
 *   society — coordination and extraction riding the same structure, which is
 *   the tangled-rope signature. This is one of three sibling readings of the
 *   same kernel (autonomy_primacy_reading and balanced_coexistence_reading
 *   are separate constraint files); this story does not average across them
 *   or describe their contest internally — see kernel_context and the omegas
 *   below for how the contest is routed.
 *
 * KEY AGENTS:
 *   - prc_central_government: agenda_setter (institutional/arbitrage) — sets and revises the boundary of autonomy
 *   - mainland_state_security_apparatus: beneficiary/agenda_setter (institutional/arbitrage) — gains direct operational jurisdiction inside Hong Kong via NSL
 *   - hong_kong_pro_beijing_establishment: beneficiary (powerful/mobile) — career and market access tied to alignment
 *   - hong_kong_pro_democracy_activists: payer (powerless/trapped) — bears prosecution and exile
 *   - hong_kong_independent_judiciary: payer (moderate/constrained) — loses final adjudication on security matters
 *   - hong_kong_press_and_civil_society: payer (moderate/constrained) — forced closures and self-censorship
 *   - hong_kong_general_population: payer/beneficiary (powerless/constrained) — retains commercial law benefits, bears narrowed political sphere
 *   - international_treaty_partners: excluded (institutional/analytical) — object without domestic enforcement standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.86).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'a44f9683-6936-4f70-89aa-d1873a2e8578').
narrative_ontology:cs_kernel_codification('a44f9683-6936-4f70-89aa-d1873a2e8578', fixed_text).
narrative_ontology:cs_authority_grounding('a44f9683-6936-4f70-89aa-d1873a2e8578', extraction).
narrative_ontology:cs_interpretation_layer_present('a44f9683-6936-4f70-89aa-d1873a2e8578').
narrative_ontology:cs_reading_relation('a44f9683-6936-4f70-89aa-d1873a2e8578', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a44f9683-6936-4f70-89aa-d1873a2e8578', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('a44f9683-6936-4f70-89aa-d1873a2e8578', foundational, sovereignty_is_indivisible_and_prior).
narrative_ontology:cs_axiom_status(sovereignty_is_indivisible_and_prior, holdable).
narrative_ontology:cs_axiom_grounding('a44f9683-6936-4f70-89aa-d1873a2e8578', sovereignty_is_indivisible_and_prior, conventional).
narrative_ontology:cs_axiom('a44f9683-6936-4f70-89aa-d1873a2e8578', foundational, national_security_determination_is_nonjusticiable_locally).
narrative_ontology:cs_axiom_status(national_security_determination_is_nonjusticiable_locally, holdable).
narrative_ontology:cs_axiom_grounding('a44f9683-6936-4f70-89aa-d1873a2e8578', national_security_determination_is_nonjusticiable_locally, instrumental).
narrative_ontology:cs_reference_frame('a44f9683-6936-4f70-89aa-d1873a2e8578', unitary_sovereign_reserve_power).
narrative_ontology:cs_drift_state('a44f9683-6936-4f70-89aa-d1873a2e8578', post_2020_national_security_law_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a44f9683-6936-4f70-89aa-d1873a2e8578', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, indivisible_state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, national_security_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the power to interpret the Basic Law, enact national laws applicable to Hong Kong (via Annex III and the 2020 National Security Law), and remove or disqualify officials and legislators. Frames autonomy as a grant that flows from, and remains conditioned on, unbroken sovereignty and territorial integrity. Sets the terms of what counts as a national security threat and adjusts institutional arrangements when it judges autonomy has produced instability.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Local political, business, and administrative figures whose positions, appointments, and business access depend on demonstrated alignment with central authority. Gain career advancement, security, and market access as autonomy is subordinated to sovereignty; can exit into mainland-aligned opportunities if local legitimacy costs rise.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment, beneficiary,
    powerful, biographical, mobile, regional).

% The National Security Law establishes an Office for Safeguarding National Security staffed partly by mainland personnel, operating with reduced local judicial oversight in defined categories of cases and able to have designated cases tried on the mainland. Gains direct operational presence and jurisdiction inside Hong Kong that did not exist before 2020.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, agenda_setter).

% Face prosecution, disqualification from office, asset freezes, and in many cases imprisonment or exile under national security provisions applied retroactively to speech and organizing that was previously lawful. Exit means permanent emigration and loss of local standing; remaining means constrained speech under threat of arrest.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists, payer,
    powerless, biographical, trapped, regional).

% Common-law judges retain authority over most matters but national security cases can be assigned to a vetted list of judges, tried without jury, and in defined circumstances removed to mainland jurisdiction; the Standing Committee's interpretive power sits above final local adjudication. Judges who resist alignment face reputational and institutional marginalization rather than formal removal in most instances.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_independent_judiciary, payer,
    moderate, generational, constrained, regional).

% Independent outlets and NGOs have been forced to close, deregister, or self-censor following raids, funding freezes, and prosecutions tied to national security provisions. Continued operation requires avoiding coverage or organizing that could be characterized as threatening national security, a category whose boundaries are set unilaterally by the enforcement authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_and_civil_society, payer,
    moderate, biographical, constrained, regional).

% Retain most everyday civil and commercial legal protections and continue to benefit from Hong Kong's distinct common-law commercial system, but experience a narrowed sphere of permissible political expression, assembly, and association, and bear the reputational and economic costs of capital and talent outflow triggered by the security law's chilling effect.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_general_population, beneficiary).

% States that were signatories or guarantors of the Sino-British Joint Declaration raise objections to the erosion of the '50-year' autonomy commitment but have no enforcement mechanism inside PRC domestic law; their objections are treated by the central government as interference in internal sovereign affairs and carry no binding force on the arrangement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_treaty_partners, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable framework for reincorporating a territory with a distinct legal, economic, and social system into the sovereign state without immediate wholesale legal unification, allowing continuity of Hong Kong's commercial law and international financial role while resolving, in the central government's favor, any conflict between local autonomy and matters it designates as touching sovereignty or national security.
% TRANSFER_FUNCTION: Moves the power to define the boundary of Hong Kong's autonomy — and to reallocate enforcement, judicial, and political authority within that boundary — from Hong Kong's local institutions and population to the PRC central government and its security apparatus, converting a previously locally-contested space of political speech, assembly, and judicial independence into a nationally-adjudicated one.
% ABSENT_VOICES: Hong Kong residents who would prefer the treaty-based autonomy reading enforced by international mechanisms have no forum with binding authority; the UK and other Joint Declaration co-signatories issue statements but hold no enforcement lever recognized in PRC domestic law. Detained or exiled activists cannot participate in the domestic political process that would revise the arrangement.
% DISAPPEARANCE_RATIONALE: If sovereignty-primacy enforcement (the National Security Law apparatus and the interpretive supremacy it rests on) disappeared overnight, Hong Kong's judiciary would revert to unqualified final local adjudication in security-adjacent cases, closed civil society organizations and press outlets would have grounds to reopen, prosecuted activists' convictions would be subject to review, and the local legislature would regain de facto space for political contestation that currently cannot occur without risk of prosecution.
% FOUNDING_PROBLEM: The 1997 handover required a mechanism to reincorporate Hong Kong into PRC sovereignty while preserving the capitalist economic system and common-law institutions that gave the territory its financial value, and later, after 2019's mass protests, to give the central government a legal tool to suppress what it characterized as separatist and foreign-instigated unrest that local law enforcement and the existing Basic Law framework had not contained.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government and Hong Kong establishment figures attest the security problem was real and unaddressed by pre-2020 law, citing the 2019 protests' scale and disruption. Independent international legal scholars, UN human rights bodies, and former Hong Kong judges who have since resigned attest that the pre-existing Basic Law and common-law system already provided adequate public-order tools, and that the National Security Law's post-2020 application to speech, assembly, and press activity substantially exceeds what containing the 2019 unrest required — corroboration exists on both sides of the status question, from sources outside the PRC government's own institutions.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply from 0.25 at handover to 0.78 by 2027, tracking the 2020 NSL's introduction of mainland enforcement jurisdiction and the post-2019 crackdown on speech and assembly — under the sovereignty-primacy reading, this is not drift within a stable arrangement but the reading's own account of the arrangement being exercised as designed once the central government judged autonomy had produced instability. Suppression (0.86) is authored higher than extractiveness because the reading's own logic requires active, hardening enforcement (vetted judges, closed-door national security trials, extraterritorial reach) to hold the sovereignty-primacy boundary in place against continued local resistance; suppression is not scaled by scope or power in the engine's computation, only extractiveness is. Theater ratio (0.42) reflects that a substantial share of enforcement activity retains real coordination function (commercial law, judicial administration for non-security matters) alongside a growing performative layer (loyalty oaths, patriotic education requirements) that does not itself contain security risk. Accessibility collapse (0.71) is high because, once the NSL frames national security as trumping local autonomy, the range of political alternatives previously available (opposition organizing, independent press coverage of security matters) collapses nearly completely for those inside the jurisdiction; resistance (0.62) remains substantial because Hong Kong civil society, the legal profession, and diaspora networks continue to contest the arrangement despite its costs.
 *
 * PERSPECTIVAL GAP:
 *   From the central government's agenda-setting seat, this reading experiences itself as legitimate exercise of an always-latent sovereign reserve power — coordination that was merely dormant until security conditions activated it. From the payer seats (activists, judiciary, press), the identical structural facts register as extraction dressed in coordination language: a delegation that can be revoked at will was never a limit on power in the first place. The engine computing divergent per-seat types from this same structural data is the intended test; this story does not resolve the divergence, it authors the data honestly for both ends.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government and its security apparatus sit at the full-beneficiary end: they set the terms of the delegation and collect the compliance and territorial-integrity guarantee the arrangement produces. The pro-Beijing establishment benefits derivatively through career and market access, with genuine mobility (mainland-aligned exit options) that damps their effective extraction relative to trapped local dissidents. Activists and closed civil-society organizations sit at the full-target end — trapped exit, direct prosecution risk, no coalition leverage recognized within the domestic legal system. The judiciary and press occupy an intermediate position: moderate power, constrained rather than trapped exit (professional emigration is possible but costly), bearing institutional rather than purely personal extraction. The general population's dual role (beneficiary of continued commercial law, payer of narrowed political space) is deliberately captured with a secondary role rather than collapsed into one direction, since the reading's own logic holds that autonomy is preserved economically while being subordinated politically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly reincorporation of a distinct legal/economic system, later joined by a public-order problem following 2019) has partially dissolved into a different function: the arrangement no longer primarily solves a reincorporation-transition problem (that problem was substantially resolved by the mid-2000s) but persists and has intensified as a live political-control mechanism. The founding_problem_status is authored 'contested' rather than 'dead' because the PRC government maintains the security threat remains live, while independent corroborating sources outside the benefiting parties assess the NSL's scope as exceeding what containing 2019-era unrest required. This divergence between claimed function and corroborated function is exactly the tangled-rope signature the classification is built to surface, rather than either accepting the sovereignty framing at face value or dismissing the coordination function (commercial law continuity) that genuinely persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_vs_original_grant_ambiguity,
    'Is Hong Kong''s autonomy structurally a revocable delegation from an always-prior unitary sovereignty (this reading''s premise), or was it a negotiated grant with independent international-treaty force that sovereignty cannot unilaterally revoke (the autonomy-primacy reading''s premise)? The Basic Law and Joint Declaration text is genuinely read differently by different legal traditions.',
    'No neutral international tribunal has binding jurisdiction to resolve the conflict between PRC constitutional doctrine (National People''s Congress Standing Committee interpretive supremacy) and the Sino-British Joint Declaration''s treaty-registration status at the UN; resolution would require either a change in PRC domestic constitutional practice or an internationally recognized adjudicative body neither party currently accepts as binding.',
    'If the delegation premise is correct, the current enforcement trajectory is simply sovereignty being exercised as it always could be; if the treaty-grant premise is correct, the same NSL enforcement constitutes a unilateral breach of an internationally binding commitment, which would reclassify much of what this story authors as legitimate coordination cost into pure extraction with no coordination cover at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(delegation_vs_original_grant_ambiguity, conceptual, 'Whether HK autonomy is a revocable domestic delegation or an internationally binding treaty grant — the central premise dividing this reading from autonomy_primacy_reading.').

omega_variable(
    national_security_threat_genuineness,
    'Was there a genuine, otherwise-uncontainable national security threat in Hong Kong by 2019-2020 that justified the scope of NSL enforcement actually deployed, or was ''national security'' a label applied to political dissent that pre-existing law and ordinary policing could have addressed?',
    'Comparative analysis of prosecutions under the NSL against the conduct actually charged (peaceful assembly, publishing, electoral candidacy) versus conduct that would meet a threat threshold in comparable common-law jurisdictions'' national security statutes; independent judicial or human-rights-body review of case records.',
    'If the threat was genuine and proportionately addressed, the coordination function (public order, territorial integrity) would be stronger than authored here and extractiveness somewhat lower; if the label substantially exceeded any genuine threat, the coordination story is closer to pure cover and the constraint moves further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_threat_genuineness, empirical, 'Whether the national security framing tracks a real threat or functions primarily as cover for political suppression.').

omega_variable(
    sovereignty_primacy_reading_as_committer_frame,
    'This story documents that the kernel one_country_two_systems_framework admits at least three structurally distinct readings (sovereignty_primacy, autonomy_primacy, balanced_coexistence), each instantiating a different constraint with a different ε and different victim set. Which reading a given legal actor holds is itself a contested, non-neutral fact — courts, legislatures, and populations within the same jurisdiction hold different readings simultaneously.',
    'No single resolution mechanism exists; this is the committer-structure itself, not an empirical or conceptual gap resolvable by data. The three sibling files jointly document the contest; none is privileged as ''the'' correct account of One Country, Two Systems.',
    'Analysts using only one reading (e.g. only sovereignty_primacy) risk treating a contested kernel as settled; the corpus should be read across all three sibling files, with network.affects_constraints links maintained, to preserve the contest rather than resolve it prematurely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primacy_reading_as_committer_frame, conceptual, 'Documents that this story is one committer-frame reading among three siblings of a contested kernel, per Rule 2 routing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.15).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2027, 0.42).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.25).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2003, 0.3).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2023, 0.76).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2027, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2014, 0.48).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2023, 0.85).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2027, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of the one_country_two_systems_framework kernel, decomposed per the ε-invariance principle: sovereignty_primacy_reading (this file, ε=0.78, tangled_rope), autonomy_primacy_reading (treaty-guaranteed autonomy, expected lower ε, likely rope or tangled_rope depending on enforcement history authored there), and balanced_coexistence_reading (negotiated non-hierarchical division, expected intermediate ε). Each sibling authors its own beneficiary/victim structure and classification independently; none averages over the others. Network edges here mark the kernel-sibling relationship, not causal downstream influence in the family-decomposition sense used for e.g. the BGS constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
