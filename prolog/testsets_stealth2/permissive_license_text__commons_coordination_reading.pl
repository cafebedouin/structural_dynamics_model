% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text as Commons Coordination Instrument (Commons Coordination Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   Permissive license texts (MIT, BSD, Apache-2.0) are standardized legal
 *   instruments that convert copyright's default exclusivity into a standing
 *   blanket permission: anyone may copy, modify, redistribute, and sublicense
 *   without negotiation, payment, or reciprocal disclosure. This story
 *   instantiates ONE reading of the contested kernel permissive_license_text
 *   — the commons_coordination_reading, under which the text is a
 *   coordination device that maximizes universal implementation freedom by
 *   minimizing legal friction. Per the committer frame, this file authors
 *   only this reading as a clean, epsilon-invariant constraint: one stable
 *   epsilon (0.09), one beneficiary structure (grantors and implementers, no
 *   victim set), one type. The sibling readings — corporate_moat_reading (the
 *   same text as enabler of uncompensated proprietary derivative building)
 *   and copyleft_counterfactual_reading (the same text as a reciprocity
 *   failure demanding viral correction) — are separate constraints linked
 *   through network.affects_constraints, not folded into this one. Claim and
 *   metrics are authored independently: the claimed type (rope) states what
 *   this reading holds structurally true, and the metrics state what is
 *   descriptively true of the arrangement's actual operation under this
 *   reading's own lights.
 *
 * KEY AGENTS:
 *   - - upstream_code_authors: Grantor/beneficiary ([moderate]/[mobile]) — converts copyright exclusivity into standing blanket permission; retains relicensing control over future versions
 *   - - commercial_implementers: Primary beneficiary ([institutional]/[arbitrage]) — embeds permissive components in proprietary products without negotiation, fees, or reciprocity
 *   - - independent_open_source_developers: Beneficiary ([moderate]/[mobile]) — frictionless reuse in personal and community projects
 *   - - downstream_end_users: Incidental beneficiary ([powerless]/[mobile]) — inherits abundant inexpensive software; bears no direct duties under the license
 *   - - copyleft_project_maintainers: Excluded voice ([organized]/[constrained]) — reciprocity-tradition stewards whose objection lives in a sibling reading, outside this reading's frame
 *   - - licensing_scholars: Analytical observer ([institutional]/[analytical]) — studies license-choice dynamics, flow symmetry, and commons sustainability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.09).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.04).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text as Commons Coordination Instrument (Commons Coordination Reading)").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "economic/technological/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '49db8d23-133a-46d6-8ef4-860808553ae8').
narrative_ontology:cs_kernel_codification('49db8d23-133a-46d6-8ef4-860808553ae8', fixed_text).
narrative_ontology:cs_authority_grounding('49db8d23-133a-46d6-8ef4-860808553ae8', practice).
narrative_ontology:cs_interpretation_layer_present('49db8d23-133a-46d6-8ef4-860808553ae8').
narrative_ontology:cs_reading_relation('49db8d23-133a-46d6-8ef4-860808553ae8', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('49db8d23-133a-46d6-8ef4-860808553ae8', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('49db8d23-133a-46d6-8ef4-860808553ae8', foundational, unconditional_grant_maximizes_implementation_freedom).
narrative_ontology:cs_axiom_status(unconditional_grant_maximizes_implementation_freedom, holdable).
narrative_ontology:cs_axiom_grounding('49db8d23-133a-46d6-8ef4-860808553ae8', unconditional_grant_maximizes_implementation_freedom, instrumental).
narrative_ontology:cs_axiom('49db8d23-133a-46d6-8ef4-860808553ae8', secondary, ex_ante_authorial_consent_settles_downstream_obligation).
narrative_ontology:cs_axiom_status(ex_ante_authorial_consent_settles_downstream_obligation, holdable).
narrative_ontology:cs_axiom_grounding('49db8d23-133a-46d6-8ef4-860808553ae8', ex_ante_authorial_consent_settles_downstream_obligation, deontological).
narrative_ontology:cs_reference_frame('49db8d23-133a-46d6-8ef4-860808553ae8', unconditional_reuse_grant_baseline).
narrative_ontology:cs_drift_state('49db8d23-133a-46d6-8ef4-860808553ae8', contemporary_ai_training_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('49db8d23-133a-46d6-8ef4-860808553ae8', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, upstream_code_authors).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, commercial_implementers).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, independent_open_source_developers).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_end_users).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, permissionless_reuse_doctrine).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, attribution_preserves_provenance_chain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and publish code, attaching a permissive license text that converts their copyright exclusivity into a standing blanket permission for all comers. What flows to them: adoption, ecosystem gravity, provenance credit through preserved attribution notices, hiring pipelines, and de facto standard-setting influence. What flows away: exclusivity — anyone may build proprietary products on their work without payment or reciprocity. Exit looks like retaining copyright, relicensing future versions, dual-licensing, or ceasing publication; past releases remain perpetually granted.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, upstream_code_authors, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, upstream_code_authors, agenda_setter).

% Embed permissively licensed components in proprietary products and internal infrastructure without negotiation, fees, or reciprocal disclosure obligations. They receive legal certainty from standardized, widely litigated text and can substitute vendors, fork, or rewrite around any component; nothing binds them to a particular supplier or upstream community.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, commercial_implementers, beneficiary,
    institutional, biographical, arbitrage, global).

% Reuse permissive libraries in personal and community projects without license-compatibility analysis or obligation tracking. They contribute fixes upstream when inclined and are never compelled to. Switching ecosystems or abandoning a dependency carries no legal residue.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, independent_open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Inherit abundant, inexpensive software built atop permissive foundations. They bear no direct duties under the license and rarely encounter its text; their benefit arrives mediated entirely through products they download or purchase.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_end_users, beneficiary,
    powerless, immediate, mobile, global).

% Steward reciprocal-licensed commons projects and argue publicly that unconditional grants let proprietary derivatives take from the commons without returning anything, gradually draining the shared infrastructure their projects depend on. They are bound by this arrangement only when they consume permissive dependencies, and their objections are articulated in a separate reading of the same license text; within this reading's frame they hold no seat in the conversation.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_project_maintainers, excluded,
    organized, generational, constrained, global).

% Study license-selection dynamics, contribution-flow asymmetry, and commons sustainability across the software ecosystem. They publish analyses that inform corporate open-source policy and foundation stewardship alike, and hold no stake in any particular license outcome.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, licensing_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pre-clears, in one standardized instrument, the permissions that copyright law otherwise requires negotiating bilaterally for every reuse: copying, modification, redistribution, sublicensing, and (under Apache-2.0) patent assertions. It solves the many-to-many reuse transaction problem once, centrally, instead of per implementing pair.
% TRANSFER_FUNCTION: Moves standing legal permission and reusable code from upstream copyright holders to the universal implementer pool without payment, reciprocity, or disclosure obligations; moves a trivial attribution-preservation duty from implementers back toward authors; returns adoption, provenance credit, and ecosystem influence to upstream authors.
% ABSENT_VOICES: Reciprocity-tradition maintainers and commons economists would object that unconditional grants permit uncompensated appropriation and gradual enclosure of collectively produced infrastructure; they hold seats in the sibling copyleft reading, not here. Original creators whose work is absorbed into proprietary derivatives without compensation are represented in this frame only by their ex ante consent — no seat speaks for their later, revealed preferences.
% DISAPPEARANCE_RATIONALE: If blanket permissive grants vanished overnight, every reuse touching a formerly permissive component would revert to copyright default: bilateral negotiation, license-compatibility audits, and infringement exposure across the software supply chain. Package registries, continuous-integration ecosystems, and product bills of materials would reorganize around licensed-or-rewritten components within months, and the volume of distributed open-source software would contract sharply.
% FOUNDING_PROBLEM: Academic and industrial code sharing in the 1980s faced copyright's default exclusivity: disseminating research software beyond the originating institution required bespoke negotiated agreements, chilling the dissemination mandates (notably Berkeley's federally funded distribution obligations) that motivated the work in the first place.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by software-law historiography of the BSD and MIT license origins, university technology-transfer records showing deliberate adoption of permissive terms to satisfy public-funding dissemination requirements, and package-registry metadata studies identifying negotiation avoidance as the dominant stated motive for permissive selection. Corporate counsel attests the same transaction-cost problem from the consuming side, but corporations are beneficiaries; the historiographic and registry evidence stands independently of them.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.09: the only residual obligations are attribution-notice preservation and (in Apache-2.0) a patent-termination condition on suit — burdens sitting just above the information_standard coordination floor (0.02), with no fee, no reciprocity requirement, and no field-of-use restriction. Suppression is 0.04 and is authored as a raw structural property (unscaled; only extractiveness is scaled by directionality and scope in the engine): nothing is coerced, and proprietary, copyleft, and dual-licensing alternatives remain fully available. Theater ratio is 0.10: the license text performs real legal work every day; the modest ceremonial share reflects the license-proliferation era's vanity licenses and approval rituals, not the core instruments. Accessibility collapse is 0.22: understanding the license collapses no alternatives — the choice architecture stays open, which is the opposite of a natural-law profile. Resistance is 0.15: episodic ideological contestation from reciprocity advocates and occasional corporate hesitancy, but no sustained opposition to the instrument itself. The temporal series run on one shared six-point grid (both metrics authored at every point): base extractiveness declines gently (0.14 to 0.09) with the retirement of the BSD advertising clause, consolidation on shorter texts, and Apache-2.0's explicit patent grant replacing patent uncertainty; theater ratio rises gently (0.06 to 0.10) with proliferation-era ceremony. No cyclical dynamics are present — the trends are monotone and shallow.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is muted inside this reading because every seated party is a beneficiary: from the commercial implementer seat the arrangement is a pure windfall (rope-like, near-zero d); from the upstream author seat it is a voluntary exchange of exclusivity for adoption, ecosystem gravity, and provenance credit (also low d, with relicensing leverage as exit). The sharpest divergence sits with the excluded seat: from the copyleft maintainer's position the same text reads as a leak in the commons — but that perception belongs to the sibling reading, and this file deliberately does not adjudicate it. The engine computes per-seat classifications from the structural data; the authored claim does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups derive low directionality: upstream_code_authors grant voluntarily and retain mobile exit (relicense future versions, dual-license, stop publishing); commercial_implementers hold arbitrage-grade exit (substitute, fork, rewrite) and sit nearest the beneficiary pole; independent_open_source_developers and downstream_end_users bear no duties worth naming. No victims are declared, so no high-d target exists within this reading — and that absence IS the reading's structural signature, and the precise point the copyleft sibling contests. Scope is global, which amplifies effective extraction modestly in the engine's arithmetic, but with epsilon this low the amplification operates on a near-floor base.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (transaction costs of reuse under copyright default) is live, and the arrangement still performs its function daily. The classification guards against two mislabels in both directions. First, it prevents reading the near-floor extraction as hidden rent: what little burden exists is the irreducible cost of a standardized legal instrument, consistent with the information_standard Boltzmann floor. Second, it prevents reading the absence of declared victims as proof of universal benefit: the excluded copyleft seat and the flow-symmetry omega keep open exactly the question — whether the commons as an uncounted party bears diffuse cost — that would move this story toward the sibling readings' picture. Receipt-surface placement is consistent: gain_flow='diffuse' (no named seat captures the residual extraction) combined with fixing_cost='cheap' (any grantor can unilaterally relicense future versions; no entrenched administrator defends the arrangement) lands in the transient-neglect cell, not the piton cell — a living rope, not an inertial shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the permissive license text best modeled as a pure coordination instrument (this reading), or does the same text instantiate extraction under the corporate-moat or reciprocity-failure readings?',
    'Cross-reading comparison within the constraint family: measure downstream flow symmetry (contribution-back rates, fix-propagation rates, sponsorship flows) and test which reading''s predicted signature matches observed flows.',
    'If flows prove strongly asymmetric with no compensating channel, the coordination-rope classification loses support and effective extraction rises toward the sibling readings'' estimates; if flows are roughly balanced, this reading''s low epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the permissive_license_text kernel this license text structurally instantiates.').

omega_variable(
    commons_flow_symmetry,
    'Does the permissive ecosystem sustain bidirectional value flow (bug fixes, hardening labor, standards work, sponsorship returning upstream), or is flow effectively unidirectional from the commons to proprietary consumers?',
    'Longitudinal contribution telemetry on high-traffic permissive packages; corporate open-source participation surveys; dependency-graph analysis of fix propagation from proprietary forks back upstream.',
    'Symmetric flow confirms the rope classification and the no-victim declaration; persistent unidirectional flow implies the commons as a whole bears uncompensated cost, raising effective extraction despite the absence of any named victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_flow_symmetry, empirical, 'Whether value returns upstream at rates consistent with a self-sustaining commons.').

omega_variable(
    author_regret_signal,
    'Do upstream authors'' later behaviors (relicensing flips, dual-licensing additions, public regret statements) reveal that ex ante consent understated the cost of unconditional grants?',
    'Track relicensing events and maintainer testimony across the interval; distinguish strategic monetization flips from protective reactions to perceived appropriation.',
    'A systematic regret pattern would weaken the consent-based axiom and introduce a latent victim set this reading currently declares absent; isolated strategic flips leave the reading intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(author_regret_signal, preference, 'Whether revealed author preferences contradict the sufficiency of ex ante consent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t6, permissive_license_text__commons_coordination_reading, theater_ratio, 6, 0.07).
narrative_ontology:measurement_basis(perm_tr_t6, observed).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__commons_coordination_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement_basis(perm_tr_t12, observed).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__commons_coordination_reading, theater_ratio, 18, 0.09).
narrative_ontology:measurement_basis(perm_tr_t18, observed).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__commons_coordination_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement_basis(perm_tr_t24, observed).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__commons_coordination_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(perm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t6, permissive_license_text__commons_coordination_reading, base_extractiveness, 6, 0.13).
narrative_ontology:measurement_basis(perm_be_t6, observed).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__commons_coordination_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement_basis(perm_be_t12, observed).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__commons_coordination_reading, base_extractiveness, 18, 0.1).
narrative_ontology:measurement_basis(perm_be_t18, observed).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__commons_coordination_reading, base_extractiveness, 24, 0.1).
narrative_ontology:measurement_basis(perm_be_t24, observed).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__commons_coordination_reading, base_extractiveness, 30, 0.09).
narrative_ontology:measurement_basis(perm_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__commons_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the kernel permissive_license_text, decomposed per the epsilon-invariance principle: the colloquial label 'permissive licensing' covers three structurally distinct claims, each with its own epsilon, beneficiary structure, and classification. This file authors the commons_coordination_reading (coordination instrument, epsilon ~0.09, no victim set). corporate_moat_reading authors the claim that the same text enables uncompensated extraction for proprietary derivative products (identifiable victims: uncompensated originators and the commons). copyleft_counterfactual_reading authors the claim that relaxation without reciprocity structurally enables exploitation and that viral reciprocity is the necessary alternative. The readings share one observable (the license text) but evaluate different arrangements under contest, so each warrants a separate story rather than one story with a measurement parameter. Edges here link this reading to both siblings; the upstream/downstream citation pattern runs from this reading outward, since the coordination framing is what corporate adopters cite as legitimacy and what copyleft advocates argue against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
