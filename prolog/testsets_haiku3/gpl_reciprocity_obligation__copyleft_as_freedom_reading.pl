% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity: Freedom-Preserving Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the GPL reciprocity
 *   obligation kernel: the freedom-preserving reading. Under this reading,
 *   viral licensing (the requirement that derivative works remain under GPL
 *   terms) is fundamentally a mechanism to prevent proprietary capture and
 *   ensure user software freedom persists across generations. The mechanism
 *   imposes costs on proprietary integrators, but those costs are justified
 *   as preventing a greater harm: the loss of user freedoms through
 *   downstream lock-in. The beneficiaries are downstream users who receive
 *   guaranteed access to improvements. The victims are proprietary vendors
 *   who cannot monetize GPL code through closed-source integration. This
 *   reading coexists with two siblings: the commons-institutional reading
 *   (which privileges institutional commons preservation over individual user
 *   freedom) and the restriction reading (which frames reciprocity as a
 *   constraint on business freedom). Each reading is a separate constraint
 *   story with its own ε, beneficiary structure, and classification.
 *
 * KEY AGENTS:
 *   - downstream_users: powerless beneficiaries who gain guaranteed access to source and modification rights
 *   - open_source_derivative_projects: moderate-power beneficiaries who inherit commons and must contribute back
 *   - proprietary_integrators: powerful victims constrained by license incompatibility
 *   - closed_source_vendors: institutional victims facing business model suppression
 *   - original_gpl_authors: institutional agenda-setters who maintain and enforce the license
 *   - competing_open_source_foundations: organized excluded parties who favor permissive licensing
 *   - free_software_movement: organized observers interpreting and advocating this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.72).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity: Freedom-Preserving Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d').
narrative_ontology:cs_kernel_codification('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', fixed_text).
narrative_ontology:cs_authority_grounding('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', extraction).
narrative_ontology:cs_interpretation_layer_present('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d').
narrative_ontology:cs_reading_relation('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', foundational, user_software_freedom_foundational).
narrative_ontology:cs_axiom_status(user_software_freedom_foundational, holdable).
narrative_ontology:cs_axiom_grounding('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', user_software_freedom_foundational, deontological).
narrative_ontology:cs_axiom('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', foundational, proprietary_capture_prevention_justified).
narrative_ontology:cs_axiom_status(proprietary_capture_prevention_justified, holdable).
narrative_ontology:cs_axiom_grounding('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', proprietary_capture_prevention_justified, empirically_contingent).
narrative_ontology:cs_reference_frame('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', user_software_freedom_framework).
narrative_ontology:cs_drift_state('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', contemporary_saas_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea6381cc-a1a3-4a00-8fc9-fc33ce782d6d', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_derivative_projects).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_derivative_projects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software built on GPL-licensed components with guaranteed access to source code and modification rights. Under this reading, they benefit from the prevention of proprietary lock-in: any GPL-derived work must remain freely available to them. They cannot exit the arrangement except by switching to proprietary alternatives, which they perceive as losing freedoms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Inherit GPL-licensed codebases and are required to contribute improvements back under the same license. They benefit from access to collective work and guarantees that downstream projects cannot monopolize their contributions. The constraint preserves the commons for their successors. They pay by being unable to privatize their own work built on GPL foundations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_derivative_projects, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_derivative_projects, payer).

% Wish to incorporate GPL code into proprietary products without disclosing source. The GPL reciprocity obligation requires them either to open-source their integration or avoid GPL components altogether. They bear the cost of license incompatibility and the loss of the option to build proprietary layers atop GPL work. Their exit is constrained because alternative permissive licenses forgo the specific network effects and maturity GPL communities provide.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Operate business models predicated on proprietary software as a competitive advantage and source of revenue through licensing fees and lock-in. GPL reciprocity directly suppresses their preferred integration paths and business model scalability. They must either license proprietary alternatives or pay legal/compliance costs to segregate GPL-licensed components from their proprietary work.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors, payer,
    institutional, generational, constrained, global).

% Set the GPL license terms and defend them through legal enforcement (cease-and-desist letters, litigation against GPL violators). They maintain and evolve the license text, define compliance boundaries, and adjudicate ambiguous cases. They benefit from ensuring the commons persists and from the legitimacy accrued through defining and protecting software freedom.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, original_gpl_authors, agenda_setter,
    institutional, generational, arbitrage, global).

% Manage alternative open-source licenses (Apache, MIT, BSD) that permit proprietary integration without reciprocity obligation. They would argue for the freedom to build proprietary value atop open work; their absence from GPL governance means their perspective on licensing flexibility is not centered in the design of copyleft mechanisms. They advocate for permissive licensing but are not party to GPL license-setting decisions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, competing_open_source_foundations, excluded,
    organized, generational, mobile, global).

% Interprets and advocates the freedom-preservation reading: that viral licensing's reciprocity obligation is a tool to prevent proprietary capture and ensure user software freedom persists. They engage in discourse, legal analysis, and community norm-setting to defend and propagate this framing. They observe the full structure and comment on its freedom-preservation function.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_movement, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, original_gpl_authors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of proprietary capture in software ecosystems: creates a mechanism where improvements to foundational code must be shared back, preventing any single vendor from monopolizing derivative work. Coordinates a global commons of shared software components on terms that guarantee downstream access and modification rights. The coordination problem is real: without reciprocity, communities that invest in maintaining shared code face the risk that vendors will incorporate that code, add proprietary improvements, and lock downstream users out of accessing the collective work.
% TRANSFER_FUNCTION: Moves the right to restrict software distribution from proprietary integrators to the commons: requires that any derivative work remain under GPL terms, transferring the ability to lock up software from individual vendors back to all users collectively. In monetary terms, proprietary vendors lose the option to capture licensing revenue from GPL-derived products; in freedom terms, users gain the guarantee that improvements will remain accessible.
% ABSENT_VOICES: Proprietary vendors and businesses whose models depend on closed-source software are structurally excluded from GPL governance and license-setting decisions. They would argue for the freedom to privatize improvements and build proprietary layers, but they are not represented in GPL foundation bodies. Permissive-license advocates who see GPL reciprocity as unnecessarily restrictive are also absent from GPL governance (they maintain competing licenses instead). Developing-world software consumers who lack resources to participate in GPL enforcement are also largely absent from governance structures.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity obligations vanished overnight, proprietary vendors would immediately incorporate GPL code into closed-source products without disclosing improvements. The collective commons would fragment into proprietary derivatives maintained behind corporate walls. Downstream users would lose guaranteed access to improvements and could be locked into proprietary software built on work they helped fund. The software development ecosystem would reorganize around vendor lock-in rather than user freedom, with network effects and developer communities captured by proprietary entities.
% FOUNDING_PROBLEM: In the early 1990s, the free-software movement identified a structural risk: foundational free software could be proprietarily captured. A company could take GPL'd code, add valuable improvements, and lock them behind proprietary licensing, while upstream communities and downstream users lost access to the accumulated work. This was not merely theoretical: the movement observed business strategies attempting exactly this (proprietary Unix systems incorporating BSD code, commercial software companies building on GNU tools without contributing back). GPL reciprocity was designed to make proprietary capture economically and legally impossible.
% FOUNDING_PROBLEM_CORROBORATION: Original GPL authors (Richard Stallman, Free Software Foundation) attest the problem remains live, citing ongoing integration attempts and attempts to find loopholes (TiVoization, GPL enforcement disputes). Empirical corroboration: recurring legal cases (Oracle v. Google on copyleft scope, AWS licensing disputes over separation of GPL-licensed and proprietary code), integration attempts, and reverse-engineering efforts demonstrate that vendors continue to seek to capture GPL'd work. Competing foundations and closed-source vendors dispute the problem's framing—they argue the problem is GPL's restriction, not proprietary capture—but they do not dispute the technical risk itself (that proprietary entities would incorporate GPL work and restrict downstream access if permitted). External scholarly analysis (LLM licensing studies, BSD vs. GPL ecosystem comparisons) provides independent corroboration that the founding problem (capture risk) remains empirically present.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28) because the benefit to downstream users is genuine (guaranteed source access, modification rights, prevention of proprietary lock-in) even though proprietary vendors bear costs. This is not pure extraction—it is coordination with asymmetric distribution. Under this reading, the asymmetry is justified: the constraint prevents a greater extraction (proprietary capture). Suppression is high (0.72) because the mechanism fundamentally prevents proprietary integrators from pursuing their preferred business model—they cannot legally incorporate GPL code into closed-source products. Alternatives (permissive licenses, proprietary alternatives) remain available but are framed by this reading as insufficient because they permit the capture that GPL prevents. Theater is low (0.18) because the GPL compliance and enforcement machinery directly serve the freedom-preservation function; there is relatively little performative overhead. Temporal measurements show extractiveness and suppression stabilizing after ~25 years: the enforcement infrastructure matured and reached steady state; the mechanism's costs and benefits have become routinized in the ecosystem. The shared time grid (0, 5, 10, 15, 25, 35) allows all three metrics to track together without grid-misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint computes very differently across seats. From the perspective of a downstream user or free-software advocate, the arrangement is protective: it preserves their freedom and the commons. From the perspective of a proprietary vendor, the same arrangement is extractive and restrictive: it prohibits their preferred integration path and forces them to accept a cost. From the perspective of a permissive-license advocate, the arrangement is overly rigid: it restricts downstream business flexibility unnecessarily. The engine computes per-seat classifications by deriving directionality from the stakeholder's power, exit options, and role. A proprietary vendor (powerful, constrained exit, payer role) sits at high d, experiencing high χ. A downstream user (powerless, mobile exit, beneficiary role) sits at low d, experiencing low/negative χ (subsidy). The analytical seat observes the structural asymmetry and recognizes that the same mechanism protects one party's freedom by constraining another's.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users are structural beneficiaries (low d → low/negative χ): they receive guaranteed access and modification rights without bearing a direct cost; they can exit only to proprietary alternatives, which they perceive as losing freedoms, so their exit is strongly constrained in perception even though technically mobile. Proprietary integrators are structural targets (high d → high χ): they bear the cost of license incompatibility, face legal prohibition of their preferred integration model, and have constrained alternatives (they can use permissive licenses, but those alternatives forgo the specific benefits GPL provides, so the choice is costlier). Open-source derivative projects are beneficiaries with secondary payer characteristics: they benefit from commons access but must contribute back, so they experience both roles. The GPL authors sit at agenda-setter: they set and enforce the terms; they have high power and maintain the license, so directionality depends on whether one treats them as enforcers of user freedom (beneficiary-aligned) or as boundary-keepers excluding proprietary vendors (target-aligned). This story treats them as aligned with the user-freedom reading (their motivation is stated as freedom preservation) so they approach beneficiary directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary capture risk) remains live as an empirical threat, corroborated by recurring integration attempts (TiVoization, AWS licensing disputes, Oracle litigation). The constraint persists because the coordination function (commons preservation) remains valuable to downstream communities. However, there is an emerging mandatrophy question: as cloud computing and SaaS business models decouple software use from distribution, the original GPL enforcement mechanism (requiring distribution of source code) has become less effective at preventing vendor lock-in—a vendor can now lock users in through service terms rather than through proprietary software. This reading's framing of GPL as freedom-preserving assumes the original enforcement mechanism remains adequate. If the mechanism degrades (vendors find loopholes), the constraint might persist for theater reasons (maintaining the appearance of commons control) while its actual freedom-preservation function atrophies. This is not yet mandatrophy (the founding problem is still actively addressed) but a emerging vulnerability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_separation,
    'Are the three readings (freedom, commons, restriction) genuinely distinct constraints, or are they the same constraint viewed from different seats?',
    'By the ε-invariance principle (DP-001), if the three readings assign different ε values to GPL reciprocity, they are structurally different constraints and should be decomposed as separate stories. If they share an ε but differ only in beneficiary framing, they are one constraint with multiple interpretations.',
    'This corpus treats them as three separate constraints because: (1) the freedom reading''s ε captures prevention of proprietary capture (protective, low-to-moderate extraction from users'' perspective); (2) the commons reading''s ε captures institutional preservation of collective ownership (different beneficiary structure); (3) the restriction reading''s ε captures suppression of proprietary business models (high extraction from vendor perspective). The ε values differ because the referents differ: what is extracted, from whom, for what purpose. If the readings shared an ε, decomposition would be unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_separation, conceptual, 'Whether the three kernel readings are structurally separate constraints or one constraint with rival interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 35, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 5, 0.21).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 35, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation kernel is contested among three readings. This constraint (freedom-preserving) treats viral licensing as preventing proprietary capture and preserving user software freedom. The sibling constraints address the same mechanism under different interpretations: commons-institutional (preserving collective ownership) and restriction (constraining proprietary business models). All three stories share the same kernel_id and describe the same GPL license; they differ in beneficiary set, axioms, and ε. They are linked via network.affects_constraints to indicate kernel membership and interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
