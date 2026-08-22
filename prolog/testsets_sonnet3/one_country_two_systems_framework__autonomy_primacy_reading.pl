% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems — Autonomy-Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the autonomy-primacy reading of the One Country,
 *   Two Systems kernel: the claim that Hong Kong's autonomy, civil liberties,
 *   and judicial independence are treaty-guaranteed commitments under the
 *   Sino-British Joint Declaration and the Basic Law, internationally
 *   enforceable and not subject to unilateral mainland revision. Under this
 *   reading, mainland actions that curtail local political freedom (the 2020
 *   National Security Law, the 2021 electoral 'improvements' restricting who
 *   may stand for office, disqualifications of elected legislators, and the
 *   2019 extradition bill that triggered mass protest) are read as breaches
 *   of a binding arrangement, not as lawful exercises of retained sovereign
 *   authority. This is the reading most associated with the pro-democracy
 *   camp, much of the international legal community, and the original British
 *   negotiating position. The sibling readings — sovereignty_primacy_reading
 *   (autonomy is delegated and revocable by Beijing) and
 *   balanced_coexistence_reading (an ongoing negotiated accommodation with no
 *   legal supremacy either way) — are separate constraints with their own ε
 *   and stakeholder structures, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - hong_kong_residents: primary diffuse beneficiary (moderate/constrained) — inherits rights infrastructure without individually bargaining for it
 *   - hong_kong_judiciary: institutional beneficiary and agenda-setter (institutional/constrained) — the guarantee's chief institutional expression
 *   - pro_democracy_political_actors: primary target (powerless/trapped) — bears the cost when the guarantee is tested and found not to hold in practice
 *   - hong_kong_civil_society_organizations and hong_kong_press: secondary targets (powerless-to-moderate/trapped-to-constrained) — organizational casualties of the gap between claim and enforcement
 *   - prc_central_government: excluded from this reading's frame by construction — its own sovereignty theory is the premise this reading argues against
 *   - international_treaty_observers: analytical/institutional observers with moral but not binding enforcement authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.42).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.55).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems — Autonomy-Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '0cbbd927-e9a6-4a25-bb7e-a7240da82b02').
narrative_ontology:cs_kernel_codification('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', fixed_text).
narrative_ontology:cs_authority_grounding('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', lineage).
narrative_ontology:cs_interpretation_layer_present('0cbbd927-e9a6-4a25-bb7e-a7240da82b02').
narrative_ontology:cs_reading_relation('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', foundational, joint_declaration_creates_binding_international_obligation).
narrative_ontology:cs_axiom_status(joint_declaration_creates_binding_international_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', joint_declaration_creates_binding_international_obligation, conventional).
narrative_ontology:cs_axiom('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', foundational, basic_law_guarantees_constrain_npc_standing_committee_action).
narrative_ontology:cs_axiom_status(basic_law_guarantees_constrain_npc_standing_committee_action, holdable).
narrative_ontology:cs_axiom_grounding('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', basic_law_guarantees_constrain_npc_standing_committee_action, conventional).
narrative_ontology:cs_axiom('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', secondary, judicial_review_binds_executive_and_central_authority_alike).
narrative_ontology:cs_axiom_status(judicial_review_binds_executive_and_central_authority_alike, holdable).
narrative_ontology:cs_axiom_grounding('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', judicial_review_binds_executive_and_central_authority_alike, conventional).
narrative_ontology:cs_reference_frame('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', joint_declaration_treaty_bargain_1984).
narrative_ontology:cs_drift_state('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', post_national_security_law_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0cbbd927-e9a6-4a25-bb7e-a7240da82b02', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_political_actors).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_press).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration_treaty_status).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, basic_law_constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a common-law system with courts, press freedoms, and rights protections structurally distinct from the mainland, guaranteed for fifty years under the Joint Declaration and Basic Law. Under this reading, that guarantee is treaty-backed and internationally enforceable, giving residents a baseline of civil liberty most did not have to fight for individually. Exit means emigration, which many pursue but which is costly and severs local ties.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, regional).

% Retains formal independent adjudicative power, common-law precedent, and a final court of appeal with foreign judges under this reading, positioned as a genuine check on both local executive and mainland overreach. Its authority rests on the Basic Law's guarantees continuing to be honored; each instance of respected judicial review reinforces the autonomy claim.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter).

% Uses Hong Kong as a jurisdiction with rule-of-law guarantees, independent courts, and capital mobility distinct from the mainland, pricing risk on the premise that the autonomy guarantee holds. Firms can relocate capital and headquarters if the guarantee is seen to fail, giving them real if costly exit.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community, beneficiary,
    organized, biographical, mobile, global).

% Organize, run for office, and litigate on the premise that the Basic Law's promised democratic development and civil liberties are enforceable commitments, not mainland-revocable grants. Many have been disqualified, prosecuted under national security law, or forced into exile when the mainland treated their activity as a security threat rather than protected political speech. Their trapped position — prosecution risk if they stay, statelessness or exile if they flee — is the primary site where this reading's central claim (treaty enforceability) is tested against events on the ground.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_political_actors, payer,
    powerless, biographical, trapped, regional).

% Unions, student groups, and NGOs that organized protest and advocacy on the assumption that assembly and association rights were treaty-guaranteed. Many have dissolved under National Security Law pressure since 2020. Their situation directly tests whether 'internationally enforceable' guarantees translate into actual protection when Beijing treats organizing as subversion.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations, payer,
    powerless, biographical, trapped, regional).

% Independent outlets operated on the premise that press freedom was structurally guaranteed and distinct from mainland media controls. Closures, arrests of editors, and raids under national security law have tested this premise directly; some outlets relocated operations abroad (constrained but not fully trapped exit, unlike the political actors).
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_press, payer,
    moderate, biographical, constrained, regional).

% Not excluded from the story generally, but excluded from THIS reading's frame: under autonomy-primacy, Beijing's interventions (National Security Law imposition 2020, electoral overhaul 2021, extradition bill 2019) are read as violations of the arrangement rather than as expressions of a sovereignty that was always superior to autonomy. Beijing's own account of its authority — that autonomy is delegated and revocable — is the sovereignty-primacy reading's premise, treated here as the thing this reading's structure must argue against.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, excluded,
    institutional, civilizational, analytical, national).

% The UK government (as co-signatory to the Joint Declaration), UN human rights bodies, and foreign parliaments monitor compliance and issue reports characterizing mainland actions as breaches. They have no direct enforcement mechanism beyond diplomatic pressure, sanctions, and declaratory statements — the treaty has no independent tribunal with binding jurisdiction over China.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legally distinct jurisdiction that lets a former colonial territory transition to Chinese sovereignty while preserving the common-law commercial and civil infrastructure that gave Hong Kong its economic function — solving the genuine coordination problem of integrating two systems with radically different legal traditions without destroying the value of either.
% TRANSFER_FUNCTION: Under this reading, the arrangement is meant to transfer very little coercively: residents retain rights, businesses retain legal predictability, and Beijing gains formal sovereignty without administrative absorption. Where the reading breaks down empirically, the actual transfer has been political voice and associational freedom moving from residents and civil society to the central government's security apparatus.
% ABSENT_VOICES: Beijing's own constitutional theory (that all local autonomy in a unitary state is delegated and revocable) is structurally absent from this reading's frame — it is treated as the position being argued against rather than as a legitimate rival premise. Ordinary residents who neither engage in politics nor run international capital — the majority — are underrepresented in accounts that focus on the judiciary and dissidents; their experience of accessibility collapse is lower than either pole stakeholder's.
% DISAPPEARANCE_RATIONALE: If the autonomy-primacy reading's guarantees (as opposed to the arrangement itself) disappeared overnight, pro-democracy actors and civil society would lose their strongest available legal and rhetorical resource for resisting mainland action, but the underlying arrangement (Basic Law, courts, capital markets) would persist in altered form under the sovereignty-primacy reading — this is precisely what commentators dispute: whether that alteration is a rearrangement of a distinct system or the unmasking of what was always the real one.
% FOUNDING_PROBLEM: Britain's 1997 handover required a legal mechanism that would let Hong Kong's capitalist, common-law economy continue functioning under Chinese sovereignty without triggering capital flight, without violating the 'one China' principle Beijing insisted on, and while giving residents (and the international community) enough assurance to keep investing and living there.
% FOUNDING_PROBLEM_CORROBORATION: The UK government, UN human rights treaty bodies, and independent legal scholars (e.g., the Hong Kong Bar Association pre-2020, international law academics) attest from outside Beijing's benefiting position that the original bargain included binding, internationally enforceable guarantees, and that post-2019 measures constitute breach. Beijing's own legal scholars and the National People's Congress Standing Committee, from inside the position that benefits from expanded control, attest that no breach has occurred because sovereignty was never actually constrained. No fully neutral corroborating party exists — every institutional voice sits on one side of the underlying kernel contest, which is itself evidence for the 'contested' status.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.42 at interval end, not low and not severe: under this reading the arrangement is fundamentally a genuine coordination structure (preserving a distinct legal-economic system) that has been increasingly instrumentalized to extract political compliance from a specific class of residents (organizers, journalists, opposition politicians) while leaving the median resident's civil and commercial life comparatively undisturbed. Suppression is higher (0.55) and theater ratio substantial (0.40) reflecting that a considerable share of enforcement activity since 2020 — national security prosecutions, disqualifications, licensing actions against media — is better read as targeted political suppression than as even-handed rule application; the security framing performs even-handedness while operating asymmetrically. Accessibility collapse is moderate (0.35): most residents retain real, if narrowing, room to live, work, and litigate; the collapse is severe and near-total specifically for the payer stakeholders. Resistance is high (0.70) — courts, the Bar Association (pre-2020), foreign governments, and civil society have all actively contested the narrowing, which is itself evidence this is not simply a settled natural arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the ordinary-resident and business seats, the arrangement still substantially reads as coordination — courts function, contracts are enforced, capital moves. From the pro-democracy and civil-society seats, the same structural facts (Basic Law text, treaty language) coexist with lived experience of prosecution and dissolution, producing a tangled-rope reading rather than a pure-rope one: genuine coordination function for most, asymmetric extraction concentrated on a politically identifiable class, requiring active enforcement (national security apparatus) to hold. This divergence is the seat-level computation the engine performs from the structural data; the claimed_type here (tangled_rope) already reflects that divergence rather than averaging it away.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordinary residents and the judiciary sit near the beneficiary end: the guarantee, under this reading, subsidizes their continued access to a distinct legal-civil order they did not individually negotiate. International business sits near-beneficiary with arbitrage-adjacent exit (mobile capital). Pro-democracy actors, civil society organizations, and press sit near the full-target end: trapped or constrained exit, and the specific site where the reading's central claim (enforceable guarantee) is tested against enforcement practice and found, in the post-2019 record, to have failed for that particular class of agent. The central government is deliberately excluded from directionality computation in THIS reading's frame, since its sovereignty claim is the contested premise, not a stakeholder position within the autonomy-primacy account.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a workable legal bridge for the 1997 transition preserving Hong Kong's economic function) is genuinely contested as to whether it remains live: the reading holds it is still live and the guarantee-enforcement gap is a breach to be remedied, not evidence the founding bargain has been superseded. Classifying this as tangled_rope rather than snare avoids mislabeling the entire arrangement as pure extraction — most residents and the commercial economy still derive real, non-illusory coordination benefit from a legal system distinct from the mainland's; but classifying it as tangled_rope rather than rope avoids papering over that a specific class of agent bears concentrated, enforcement-backed costs that the coordination story does not explain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_ambiguity,
    'Is the Sino-British Joint Declaration actually internationally enforceable in a legal sense, or is it a diplomatic instrument whose only ''enforcement'' is reputational and political pressure with no binding tribunal?',
    'Examination of whether any international body has ever exercised binding jurisdiction to compel PRC compliance with Joint Declaration terms, versus the historical record of purely declaratory UK/UN statements without material consequence.',
    'If no binding enforcement mechanism exists and never has, the autonomy-primacy reading''s central structural claim (treaty-guaranteed, internationally enforceable) is itself aspirational rather than descriptive, which would push the computed classification for the payer seats closer to snare (extraction dressed as an unenforceable coordination promise) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_ambiguity, empirical, 'Whether the treaty guarantee this reading rests on has ever had real enforcement teeth.').

omega_variable(
    which_reading_is_the_true_kernel,
    'Is autonomy-primacy the reading the original 1984-1997 negotiators actually intended and that the legal text supports, or is it a reading that gained prominence because it served Hong Kong democratic actors'' and Western governments'' interests, with sovereignty-primacy being the reading actually consistent with PRC constitutional theory (a unitary state with no genuine federal subdivision) all along?',
    'Comparative analysis of the negotiating history (declassified UK and PRC records), the Basic Law''s own drafting history, and PRC constitutional scholarship contemporaneous with 1990 (pre-dating the current dispute) versus post-2019 retrospective justification.',
    'If sovereignty-primacy was the operative legal reality from the start and autonomy-primacy was always aspirational rhetoric rather than binding structure, this reading''s classification as tangled_rope (genuine coordination plus targeted extraction) would need reconsideration toward a reading where the entire ''guarantee'' framing was cover from inception — though that reconsideration would itself be a different constraint (the sovereignty-primacy reading), not a revision of this one, per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel, conceptual, 'Genuine interpretive indeterminacy about which reading the kernel''s founding text actually supports.').

omega_variable(
    post_2020_trajectory_permanence,
    'Is the post-2020 tightening (National Security Law, electoral overhaul) a temporary security response to a specific crisis (2019 protests) that will relax once the immediate threat perception passes, or a permanent structural recalibration establishing sovereignty-primacy as the operative reality going forward?',
    'Longitudinal tracking of prosecution rates, press closures, and electoral participation over the 2025-2035 period; comparison to post-crisis security relaxation patterns in other jurisdictions.',
    'A relaxation would support the autonomy-primacy reading''s claim that this is a temporary breach rather than the arrangement''s true character; continued or intensifying suppression would support the sovereignty-primacy reading and suggest the tangled_rope classification here is itself a transitional snapshot en route to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2020_trajectory_permanence, empirical, 'Whether current suppression levels are a temporary spike or the new steady state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2003, 0.12).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2023, 0.42).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2027, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.15).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2003, 0.2).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.3).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2027, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.2).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2003, 0.25).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.35).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2023, 0.58).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2027, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the one_country_two_systems_framework kernel, decomposed per the epsilon-invariance principle: the same natural-language label ('One Country, Two Systems') covers structurally distinct claims about where legal supremacy sits. autonomy_primacy_reading treats the guarantee as binding and enforceable (this file); sovereignty_primacy_reading treats autonomy as delegated and revocable; balanced_coexistence_reading treats the boundary as perpetually negotiated with no legal supremacy on either side. Each carries independent ss, beneficiary/victim structure, and claimed_type. They are linked here rather than merged because measuring 'the same constraint' under each reading's own lights produces materially different ss values (this reading: 0.42; sovereignty-primacy reading would author civil-liberties extraction as near-zero by definition since no breach is structurally possible; balanced-coexistence would author a lower, more diffuse ss reflecting ongoing negotiation rather than either breach or lawful exercise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
