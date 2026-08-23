% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__democratic_pluralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__democratic_pluralist_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__democratic_pluralist_reading
 *   human_readable: Democratic-Pluralist Reading of AI Governance Legitimacy
 *   domain: political theology / technology governance
 *
 * SUMMARY:
 *   Contested kernel: what makes AI governance legitimate. This file
 *   instantiates the democratic-pluralist reading — legitimacy flows from
 *   inclusive deliberation and consent of the governed; no tradition,
 *   clerical or technical, holds interpretive monopoly; the encyclical speaks
 *   as one voice among many. The standing arrangement under contest, and
 *   therefore the referent for epsilon, is the emerging
 *   deliberative-participation regime in AI governance (citizens' assemblies,
 *   statutory consultation, judicial review of deployment decisions) assessed
 *   by this reading's own lights. The interval 0-15 maps approximately
 *   2011-2026 at one unit per year. Sibling readings are separate constraint
 *   files linked through the network section; their epsilon values differ
 *   because each reading assesses its own referent arrangement. KEY AGENTS
 *   (by structural relationship): democratic_institutions — agenda-setting
 *   beneficiary (institutional/constrained), convenes and ratifies, collects
 *   the legitimacy yield; civil_society_organizations — beneficiary
 *   (organized/mobile), staffs the deliberative economy;
 *   minority_rights_holders — beneficiary (moderate/constrained), procedural
 *   protection against majoritarian and expert override;
 *   digitally_excluded_populations — primary target (powerless/trapped),
 *   governed by inputs they could not give; authoritarian_regime_populations
 *   — primary target (powerless/trapped), no operative channel under
 *   rejecting states; global_south_ai_subjects — excluded seat
 *   (powerless/trapped), subject to rules made in other polities'
 *   deliberations; magisterial_authorities — positional payer
 *   (institutional/identity_locked), voice without interpretive privilege;
 *   technocratic_expert_bodies — positional payer
 *   (institutional/constrained), influence without final word;
 *   deliberative_theorists — analytical observer auditing function versus
 *   performance.
 *
 * KEY AGENTS:
 *   - democratic_institutions: agenda-setting beneficiary (institutional/constrained) — convenes assemblies, mandates consultation, ratifies rules; collects the legitimacy yield
 *   - civil_society_organizations: beneficiary (organized/mobile) — convenes publics, translates technical material, staffs participatory bodies
 *   - minority_rights_holders: beneficiary (moderate/constrained) — relies on inclusion guarantees and judicial review against override
 *   - digitally_excluded_populations: primary target (powerless/trapped) — bears decisions citing input they could not give
 *   - authoritarian_regime_populations: primary target (powerless/trapped) — governed by deployments validated under standards their states reject
 *   - global_south_ai_subjects: excluded seat (powerless/trapped) — subject to exported rules from foreign deliberations
 *   - magisterial_authorities: positional payer (institutional/identity_locked) — contributes a voice, forfeits interpretive monopoly
 *   - technocratic_expert_bodies: positional payer (institutional/constrained) — keeps agenda influence, loses final word
 *   - deliberative_theorists: analytical observer — documents who was heard and what changed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).
domain_priors:suppression_score(ai_governance_legitimacy__democratic_pluralist_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__democratic_pluralist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_governance_legitimacy__democratic_pluralist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__democratic_pluralist_reading, scaffold).
narrative_ontology:human_readable(ai_governance_legitimacy__democratic_pluralist_reading, "Democratic-Pluralist Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__democratic_pluralist_reading, "political theology / technology governance").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:has_sunset_clause(ai_governance_legitimacy__democratic_pluralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__democratic_pluralist_reading, '73413995-3fb8-43e7-ba39-ab075de12a67').
narrative_ontology:cs_kernel_codification('73413995-3fb8-43e7-ba39-ab075de12a67', distributed).
narrative_ontology:cs_authority_grounding('73413995-3fb8-43e7-ba39-ab075de12a67', practice).
narrative_ontology:cs_interpretation_layer_present('73413995-3fb8-43e7-ba39-ab075de12a67').
narrative_ontology:cs_reading_relation('73413995-3fb8-43e7-ba39-ab075de12a67', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('73413995-3fb8-43e7-ba39-ab075de12a67', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_reading_relation('73413995-3fb8-43e7-ba39-ab075de12a67', ai_governance_legitimacy__market_libertarian_reading, coexists_with).
narrative_ontology:cs_axiom('73413995-3fb8-43e7-ba39-ab075de12a67', foundational, legitimacy_requires_consent_of_governed).
narrative_ontology:cs_axiom_status(legitimacy_requires_consent_of_governed, holdable).
narrative_ontology:cs_axiom_grounding('73413995-3fb8-43e7-ba39-ab075de12a67', legitimacy_requires_consent_of_governed, deontological).
narrative_ontology:cs_axiom('73413995-3fb8-43e7-ba39-ab075de12a67', foundational, no_single_tradition_holds_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_single_tradition_holds_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('73413995-3fb8-43e7-ba39-ab075de12a67', no_single_tradition_holds_interpretive_monopoly, deontological).
narrative_ontology:cs_reference_frame('73413995-3fb8-43e7-ba39-ab075de12a67', deliberative_consent_of_the_governed).
narrative_ontology:cs_drift_state('73413995-3fb8-43e7-ba39-ab075de12a67', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73413995-3fb8-43e7-ba39-ab075de12a67', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, digitally_excluded_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_authorities).
narrative_ontology:constraint_victim(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_expert_bodies).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, deliberative_democracy_theory).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__democratic_pluralist_reading, public_reason_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, election commissions, and constitutional courts convene citizens' assemblies, write consultation requirements into AI bills, and review contested deployments. They decide when deliberation has been adequate and ratify the resulting rules. The arrangement yields them an authorization they cannot manufacture internally: AI decisions signed off through visible public process carry an endorsement no internal memo provides. Stepping off the deliberative path would leave them authorizing consequential technology on bare administrative say-so.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions, beneficiary).

% Receive formal consultative standing, convene panels, translate technical documents for publics, and staff the secretariats of participatory bodies. Their relevance and much of their funding ride on the deliberative economy. They can and do also operate outside it through campaigns and litigation when formal channels stall.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, civil_society_organizations, beneficiary,
    organized, biographical, mobile, national).

% Rely on guaranteed inclusion and judicial review to keep AI decisions from overriding them by majority vote or by expert fiat. The protection is procedural: it pays off whenever deliberation genuinely runs and evaporates when participation turns nominal. Their recourse is slow — petitions, impact litigation, coalition-building.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, minority_rights_holders, beneficiary,
    moderate, generational, constrained, national).

% Lack the connectivity, digital literacy, language access, or free evening hours that participation presupposes. Decisions about automated benefits triage, predictive policing, and platform moderation proceed citing public input they never gave and could not have given. There is no exit: the rules govern them wherever they live within the polity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, digitally_excluded_populations, payer,
    powerless, immediate, trapped, national).

% Live under governments that reject consent-based legitimacy altogether, deploying AI surveillance and social scoring with no deliberative pretense. The participatory standards validated in democracies offer them no operative channel; cross-border deployments arrive already endorsed elsewhere. Their recourse is exile, silence, or underground networks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, authoritarian_regime_populations, payer,
    powerless, generational, trapped, national).

% Are subject to AI systems designed, trained, and regulated under other polities' deliberative processes — content moderation rules, credit scoring models, aid-allocation algorithms. They are not constituents of the assemblies that produced those rules and hold no seat in them; they encounter the outputs as finished products.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, global_south_ai_subjects, excluded,
    powerless, generational, trapped, continental).

% Contribute the encyclical's teaching as one intervention among many in hearings and consultations, without the interpretive privilege their own framework claims. The cost they bear is positional: accumulated teaching authority does not convert into decision rights here. Exiting the conversation would mean conceding that their social doctrine has nothing to say about technology, which their institutional identity cannot accommodate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, magisterial_authorities, payer,
    institutional, civilizational, identity_locked, global).

% Standards institutes, lab safety teams, and statistical agencies must render their judgments in publicly accessible reasons and submit to lay review panels. They retain agenda-setting influence over what gets studied and proposed, but the final word migrates to accountable institutions. The cost is speed and autonomy; the alternative — claiming authority from expertise alone — no longer clears the legitimacy bar in consolidated democracies.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, technocratic_expert_bodies, payer,
    institutional, biographical, constrained, global).

% Political theorists and participation scholars audit whether assemblies and consultations realize the public-reason ideal they articulate: who was invited, whose input changed the draft, whether reasons were accessible. They hold no decision rights and collect no proceeds; their leverage is documentation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__democratic_pluralist_reading, deliberative_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__democratic_pluralist_reading, democratic_institutions).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__democratic_pluralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed values and knowledge across deep pluralism into AI governance decisions that losers can accept as fairly reached. It solves the problem of authorizing binding rules when no tradition, expert body, or market outcome commands universal assent, providing a shared procedure — inclusive deliberation, transparent reasons, majority decision with rights protection — that converts value conflict into legitimate collective choice.
% TRANSFER_FUNCTION: Moves authorization for AI governance from self-authorizing elites (ecclesial, corporate, technical) to accountable democratic institutions; moves public attention, meeting time, and civic resources into participation infrastructure; moves decision costs onto the deliberative calendar as delay, and onto those whose voices the process fails to reach.
% ABSENT_VOICES: The digitally excluded within democracies, non-citizen subjects of exported AI systems, future generations, and populations under authoritarian regimes are absent from the deliberations that produce rules governing them; they appear only as aggregate statistics or affected-stakeholder categories. Dissenting traditional authorities are present by design but hold no veto — their objection is heard, not binding.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, AI governance authorization would default to whichever claimant moved fastest — corporate deployment, agency expertise, or clerical authority where influential. The assemblies, consultation mandates, and judicial review hooks would dissolve; the legitimacy contest among the four readings would reopen with no agreed arbiter; and minority protections tied to procedural inclusion would weaken first.
% FOUNDING_PROBLEM: AI capabilities began making consequential decisions faster than any existing authority structure could credibly claim the consent of those affected. Corporate labs, security agencies, and religious authorities each asserted interpretive privilege over AI's governance without a procedure the governed could recognize as their own.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: OECD and UNESCO adoption processes documented public trust deficits and demanded participation mechanisms; independent academic audits of participation gaps show consultation inputs routinely arriving after design decisions are locked; digital-rights litigation attests exclusion in specific deployments; dissidents under authoritarian regimes attest the total absence of any channel. No official within authoritarian states corroborates — they deny the problem exists, which is itself signal.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__democratic_pluralist_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__democratic_pluralist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__democratic_pluralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__democratic_pluralist_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).
:- end_tests(ai_governance_legitimacy__democratic_pluralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.40 (mid-band of the expected 0.35-0.45): the arrangement delivers real coordination — a usable authorization procedure across pluralism — while imposing genuine costs: mandatory deliberation taxes decision speed, participation burdens fall unevenly, and the process claims universality while excluding the disconnected, the foreign, and the unfree. Suppression is moderate (0.35) and structural rather than censorious: the constraint's coercive edge runs through judicial review voiding non-deliberative authorization and statutory participation mandates, while advocacy for rival readings remains legally protected — suppressing a legitimacy claim is not the same as suppressing its speech. Theater ratio 0.40 reflects the documented growth of participation-washing: consultations convened after decisions are effectively made, panels whose input is filed unread. Accessibility collapse is low (0.30): understanding the consent requirement does not eliminate alternatives — the three sibling readings remain fully operable in other jurisdictions, which is precisely why this is a scaffold and not a mountain. Resistance is substantial (0.55): authoritarian states reject the standard outright, technocratic bodies resist deliberative delay, industry resists participation mandates, and religious authorities resist demotion from interpretive privilege. The powerless victim seats retain coalition potential through transnational digital-rights networks and impact litigation, which is the main force keeping resistance and theater in check. Coordination type is declared enforcement_mechanism: the constraint's dominant function is operating a governance-legitimacy structure with dedicated enforcement infrastructure (courts, electoral accountability, statutory mandates). All three metric series are authored on one shared time grid (points 0, 3, 6, 9, 12, 15) so the engine samples every metric at every examined time point; trajectories are monotonic rather than cyclical — accumulation, not oscillation, is this constraint's dynamic. The claimed type (scaffold) and the metrics were authored independently: the claim asserts transitional participatory infrastructure with a real sunset; the metrics describe its actual mixed operation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the trapped target seats (digitally excluded, authoritarian-regime populations), the arrangement operates as a gate that legitimizes decisions made without them — high effective extraction amplified by zero exit. From the beneficiary seats, the same structure is subsidy: standing, funding, protection. The agenda-setting seat experiences it as obligation that pays: deliberative duty in exchange for authorization it cannot self-issue. The positional payers (magisterial, technocratic) experience loss of rank rather than material extraction — their exit options differ sharply (identity_locked versus constrained), so identical nominal power yields different computed directionalities. The excluded seat (global south) is recorded commentary-grade: it documents whose absence makes the unanimity of consenting publics possible, without driving classification overrides.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for democratic_institutions, civil_society_organizations, and minority_rights_holders — the constraint subsidizes them, and their exits (constrained to mobile) keep them short of the target end. Victim declarations drive high directionality for digitally_excluded_populations and authoritarian_regime_populations; trapped exit pushes both toward the full-target end, since the constraint's costs land on them with no arbitrage available. The positional payers sit mid-to-high: they bear real costs (forfeited monopoly, forfeited final word) but retain influence and, in the technocratic case, partial mobility. Scope is national for most seats with continental reach for exported-rule effects, so verification difficulty scales moderately. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a scaffold with a declared sunset and a live founding problem, so the mandatrophy mismatch flag should not fire: founding_problem_status is live and disappearance_verdict is world_rearranges. The risk profile is forward-looking: the sunset clause is real only if ad hoc assemblies dissolve on schedule and ordinary electoral and administrative channels absorb their function. The theater_ratio series rises monotonically (0.25 to 0.40) — if the founding problem resolves while convened deliberation keeps expanding as ritual, the arrangement drifts toward inertial maintenance, and the piton signature (administrator-could-change-but-cost-exceeds-what-it-bears) becomes the live hypothesis. The omega on sunset routinization tracks exactly this fork. Classification prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the trapped targets it legitimizes decisions over; reading it as pure extraction ignores the authorization function no rival reading supplies at lower cost to the included.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation_scope,
    'This file instantiates one reading (democratic_pluralist) of the kernel ai_governance_legitimacy; the magisterial, technocratic, and market-libertarian readings are separate constraints with their own epsilon, beneficiaries, and victims. Which reading''s institutional form actually prevails in a given jurisdiction, and does this file''s classification travel?',
    'Jurisdiction-by-jurisdiction coding of the operative authorization basis for AI rules — statutory participation mandates versus doctrinal vetoes versus agency discretion versus market self-certification — with the sibling files carrying the corresponding stories.',
    'If a sibling reading prevails institutionally in a jurisdiction, this constraint''s beneficiary/victim structure and epsilon do not describe that jurisdiction''s arrangement; classification must be read as reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation_scope, conceptual, 'Reading-indexed scope of the classification within the four-reading kernel family.').

omega_variable(
    authoritarian_exclusion_channel,
    'Populations under authoritarian regimes are declared victims, but the reading''s remedy — deliberation — presupposes institutions their states forbid. Is their harm intrinsic to this arrangement or an artifact of its incomplete reach, and does any operative channel (diaspora assemblies, encrypted consultation, exiled review bodies) exist?',
    'Comparative tracking of transnational participation experiments and whether any of their inputs altered deployment decisions affecting those populations.',
    'If no channel exists, the reading''s universality claim fails for roughly a quarter of humanity and effective extraction on that population approaches the full-target end; a working channel would pull it back down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authoritarian_exclusion_channel, empirical, 'Whether the consent-based standard reaches populations whose states reject it.').

omega_variable(
    participation_washing_share,
    'What fraction of convened deliberation is functional rather than performative — did assembled input change the draft, the deployment, or the deadline?',
    'Input-to-outcome tracing across a sample of AI assemblies and consultations: diff drafts against testimony, log which recommendations were adopted, rejected, or unread.',
    'A washing share above half would push theater_ratio past 0.5 and date a Goodhart drift toward proxy compliance; below a quarter would support the scaffold reading and validate the sunset clause as load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_washing_share, empirical, 'Functional versus performative share of the deliberative activity the constraint mandates.').

omega_variable(
    sunset_routinization_trajectory,
    'Will the participatory scaffolding actually dissolve into routinized democratic accountability as its declared sunset anticipates, or harden into permanent standing consultation bureaucracy once the founding urgency fades?',
    'Track whether ad hoc AI assemblies dissolve on schedule and whether ordinary electoral and administrative channels absorb their function; watch for renewal-after-renewal patterns and secretariat entrenchment.',
    'Permanence without absorbed function converts the scaffold into inertial maintenance — the mandatrophy endpoint with theater as the visible symptom; on-time dissolution validates the sunset clause as real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_routinization_trajectory, empirical, 'Whether the declared sunset is operative or decorative.').

omega_variable(
    hybrid_authorization_blur,
    'Hybrid regimes layer democratic procedure over substantive limits — doctrinal vetoes, expert lock-ins, property absolutism. Do such hybrids belong to this reading, to a sibling, or to no single file, and does their growth blur the four-way decomposition?',
    'Classify hybrid constitutions by which authorization basis is decisive when the layers conflict: who wins when the assembly, the doctrine, the agency, and the market disagree.',
    'If hybrids code to siblings by decisive layer, this file''s victim set shrinks to pure-procedure jurisdictions; if hybrids code here, epsilon rises because layered vetoes tax the consent mechanism the reading depends on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_authorization_blur, conceptual, 'Boundary problem between this reading and its siblings under hybrid authorization regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__democratic_pluralist_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t0, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t0, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t3, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t3, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t6, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t6, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t9, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 9, 0.36).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t9, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t12, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t12, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_tr_t15, ai_governance_legitimacy__democratic_pluralist_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(ai_gov_legit_demplur_be_t0, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t0, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_be_t3, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t3, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_be_t6, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t6, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_be_t9, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 9, 0.38).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t9, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_be_t12, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t12, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_be_t15, ai_governance_legitimacy__democratic_pluralist_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_legit_demplur_su_t0, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t0, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_su_t3, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 3, 0.26).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t3, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_su_t6, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 6, 0.29).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t6, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_su_t9, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 9, 0.31).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t9, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_su_t12, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t12, observed).
narrative_ontology:measurement(ai_gov_legit_demplur_su_t15, ai_governance_legitimacy__democratic_pluralist_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(ai_gov_legit_demplur_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__democratic_pluralist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__democratic_pluralist_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label AI governance legitimacy covers four structurally distinct authorization arrangements, each authored as its own constraint with its own epsilon, beneficiaries, and victims. This file carries the democratic-pluralist instantiation. Edges run from this file to each sibling because the readings compete for the same institutional ground: this reading's procedural requirements reshape the operating environment of the others (experts must justify to publics; markets face participation mandates; doctrinal authority is demoted to one voice), and their persistence in other jurisdictions is the main reason this reading's accessibility_collapse stays low.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
