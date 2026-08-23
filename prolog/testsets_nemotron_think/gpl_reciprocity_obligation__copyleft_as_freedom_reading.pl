% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL reciprocity obligation (copyleft) requires that derivative works
 *   of GPL-licensed software be distributed under the same license terms,
 *   preserving the four freedoms for all downstream recipients. This reading
 *   — the 'copyleft as freedom' framing championed by the FSF — asserts that
 *   the viral mechanism is not a restriction but a liberation: it prevents
 *   proprietary capture that would otherwise deny users their freedoms. The
 *   constraint is claimed as a rope (pure coordination solving the enclosure
 *   problem), with beneficiaries identified as downstream users and victims
 *   as proprietary integrators who lose the option to enclose GPL code. High
 *   suppression reflects active enforcement through copyright law and the
 *   structural impossibility of proprietary relicensing once GPL code is
 *   incorporated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.35).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.75).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '36fe4118-4195-49c8-91d8-a954acfc9cdf').
narrative_ontology:cs_kernel_codification('36fe4118-4195-49c8-91d8-a954acfc9cdf', fixed_text).
narrative_ontology:cs_authority_grounding('36fe4118-4195-49c8-91d8-a954acfc9cdf', lineage).
narrative_ontology:cs_interpretation_layer_present('36fe4118-4195-49c8-91d8-a954acfc9cdf').
narrative_ontology:cs_reading_relation('36fe4118-4195-49c8-91d8-a954acfc9cdf', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('36fe4118-4195-49c8-91d8-a954acfc9cdf', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('36fe4118-4195-49c8-91d8-a954acfc9cdf', foundational, user_freedom_inalienable).
narrative_ontology:cs_axiom_status(user_freedom_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('36fe4118-4195-49c8-91d8-a954acfc9cdf', user_freedom_inalienable, deontological).
narrative_ontology:cs_axiom('36fe4118-4195-49c8-91d8-a954acfc9cdf', secondary, proprietary_capture_harms_users).
narrative_ontology:cs_axiom_status(proprietary_capture_harms_users, holdable).
narrative_ontology:cs_axiom_grounding('36fe4118-4195-49c8-91d8-a954acfc9cdf', proprietary_capture_harms_users, empirically_contingent).
narrative_ontology:cs_reference_frame('36fe4118-4195-49c8-91d8-a954acfc9cdf', four_freedoms_framework).
narrative_ontology:cs_drift_state('36fe4118-4195-49c8-91d8-a954acfc9cdf', cloud_saas_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('36fe4118-4195-49c8-91d8-a954acfc9cdf', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, four_freedoms_do_not_require_proprietary_permission).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_is_inalienable_not_conditional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the four freedoms (use, study, modify, distribute) on all GPL-licensed software and its derivatives. Their ability to exercise these freedoms depends on the viral mechanism preventing proprietary relicensing. Exit means abandoning the GPL ecosystem entirely — switching to proprietary alternatives or permissively-licensed stacks — which carries switching costs in skills, tooling, and community.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, constrained, global).

% Companies that want to incorporate GPL code into proprietary products without releasing source. They bear the cost of either: (a) complying with GPL and releasing proprietary additions (losing competitive secrecy), (b) rewriting GPL components from scratch (engineering cost), or (c) avoiding GPL code entirely (opportunity cost). Their exit is mobile — they can choose other codebases, build alternatives, or negotiate dual-licensing — but the constraint actively suppresses the 'incorporate and close' path.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, mobile, global).

% Developers and organizations who hold copyright on GPL-licensed works and choose the license terms. They set the reciprocity obligation by licensing under GPL. They can relicense their own contributions (but not others') and can dual-license commercially. Their position is strong — they define the constraint — but they are bound by the same terms on others' contributions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% The Free Software Foundation stewards the GPL text, publishes authoritative compliance guidance, and advocates for the freedom framing. They do not hold copyright on most GPL code but define the license's interpretation. Their authority derives from historical lineage (GNU Project) and moral leadership in the free software movement.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, fsf_gnu_project, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, fsf_gnu_project, observer).

% Projects using permissive licenses (MIT, BSD, Apache) that compete for developer mindshare and corporate adoption. They benefit from the GPL's existence as a 'strong copyleft' alternative that makes their permissive licensing look business-friendly, but they also lose potential contributors who prefer strong reciprocity. They observe the constraint's effects without being directly bound by it.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, competitive_oss_projects_mit_bsd, observer,
    organized, generational, analytical, global).

% Legal systems that enforce GPL compliance through copyright law. They provide the enforcement machinery (injunctions, damages) that makes the viral mechanism effective. Their interpretation of 'derivative work' and 'distribution' shapes the constraint's practical reach (e.g., SaaS loophole).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, courts_legal_system, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents proprietary capture of communal code by ensuring that anyone who builds on GPL software must extend the same freedoms to downstream recipients — solving the collective-action problem where individual actors would otherwise enclose shared work for private gain.
% TRANSFER_FUNCTION: Moves control over derivative works from proprietary integrators (who would restrict downstream users) to downstream users (who receive guaranteed freedoms to use, study, modify, and distribute). The transfer is not monetary but capacitative: the freedom to run, inspect, change, and share software.
% ABSENT_VOICES: Proprietary software vendors who would integrate GPL code into closed products without reciprocating; cloud providers who run GPL code as a service without distributing it (the 'SaaS loophole'); companies building 'open core' models that keep value-added features proprietary. These actors are structurally excluded by the license terms — they would object to the viral obligation but cannot participate in the licensing decision for code they did not write.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished overnight, the dominant equilibrium would shift toward proprietary enclosure: companies would incorporate GPL code into closed products, cloud providers would rebrand GPL services as proprietary offerings without source disclosure, and the four freedoms would become contingent on the goodwill of each downstream distributor rather than a legal guarantee. The free software ecosystem would reorganize around permissive licenses or contract-based reciprocity, fundamentally altering the power balance between users and distributors.
% FOUNDING_PROBLEM: The problem of proprietary software enclosing shared code and denying users the freedom to modify, study, and redistribute the software they depend on — exemplified by the historical enclosure of Unix (BSD code absorbed into proprietary Unix variants), the 'embrace, extend, extinguish' pattern, and the general tendency of commercial actors to capture commons for private gain.
% FOUNDING_PROBLEM_CORROBORATION: Historical record: proprietary Unix vendors enclosed BSD-licensed code in the 1980s-90s; Microsoft's documented 'embrace, extend, extinguish' strategy against open standards; contemporary cloud providers (AWS, Azure, GCP) offering managed services built on open source (Redis, Elasticsearch, MongoDB) without contributing improvements upstream, prompting license changes (SSPL, BSL) that the FSF criticizes as non-free. Corroboration comes from outside the GPL beneficiary set: business press documentation of 'strip-mining' open source, academic studies on commons enclosure in software, and the very existence of the AGPL and newer copyleft variants as responses to the SaaS loophole.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35) from this reading's perspective: the reading acknowledges that proprietary integrators bear real costs (foregone enclosure opportunities) but frames this as the necessary price of preventing enclosure, not as rent extraction. Suppression is high (0.75) because the constraint actively blocks the 'incorporate and close' path through copyright enforcement — alternatives collapse for proprietary integrators (accessibility_collapse 0.82). Theater is low (0.15): the mechanism is functional, not performative; compliance activity directly serves the freedom-preservation function. Resistance is moderate (0.55): proprietary vendors resist through lobbying, FUD campaigns, and license avoidance, but the constraint has held for 35+ years.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (proprietary integrators) experiences this as a snare-like restriction — high suppression, active enforcement, constrained alternatives. The beneficiary seat (downstream users) experiences it as a mountain-like guarantee — freedoms that persist regardless of distributor goodwill. The agenda-setter seats (copyright holders, FSF) experience it as a rope they administer. The engine computes this divergence from the structural data; the claimed_type 'rope' reflects the freedom reading's self-understanding, not the payer's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users are structural beneficiaries (d ~ 0.15): the constraint subsidizes their freedoms at the expense of proprietary integrators' enclosure options. Proprietary integrators are structural targets (d ~ 0.85): they bear the full cost of the reciprocity obligation, with mobile but non-trivial exit (rewrite, avoid, dual-license). GPL copyright holders and FSF are agenda-setters with arbitrage-grade exit (they define the terms). The directionality derivation from beneficiary/victim declarations + exit options produces the expected asymmetry: users gain capabilities, proprietary integrators lose enclosure options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of shared code) remains live — evidenced by ongoing SaaS enclosure, 'open core' models, and license changes away from permissive licenses toward stronger copyleft or source-available terms. The constraint has not atrophied; its enforcement has intensified (GPLv3 addressed patent threats and tivoization; AGPL addressed SaaS). Mandatrophy is not resolved — the coordination function is still necessary and the constraint still serves it, though the SaaS loophole represents a genuine drift in effectiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the GPL reciprocity obligation a single constraint with multiple interpretations, or are the freedom/restriction/commons framings structurally distinct constraints with different ε values and classification outcomes?',
    'Compare the ε-authored values and computed per-seat classifications across the three reading stories. If ε differs substantially (e.g., freedom reading ε=0.35, restriction reading ε=0.75), they are distinct constraints per the ε-invariance principle. If ε converges but seat classifications diverge, they are perspectival variants of one constraint.',
    'If distinct constraints, each gets its own story file linked via network.affects_constraints. If one constraint, the engine''s per-seat classification captures the divergence internally. The current corpus practice (BGS decomposition) favors decomposition when ε differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the three GPL readings are one constraint or three (ε-invariance test)').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.75) experienced by proprietary integrators structural (copyright law enforcement) or partially internalized (community norms, reputational pressure, ideological commitment to open source)?',
    'Track suppression trajectory for proprietary integrators who exit the GPL ecosystem: if suppression persists after they switch to permissive-licensed stacks (reputational damage, community hostility), internalized component exists. Compare enforcement rates: legal actions vs. social pressure compliance.',
    'If substantially internalized, effective suppression is higher than legal enforcement alone suggests — the constraint carries its suppression mechanism into the target''s future choices. This would increase χ for the proprietary_integrator seat beyond the structural measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for proprietary integrators').

omega_variable(
    viral_mechanism_necessity,
    'Does preventing proprietary capture structurally require a viral (derivative-work) mechanism, or would a non-viral copyleft (file-level, like MPL) or contract-based reciprocity achieve the same freedom-preservation with less suppression?',
    'Natural experiment: compare outcomes in ecosystems with viral copyleft (GPL), file-level copyleft (MPL, CDDL), and contract-based reciprocity (MongoDB SSPL, Elastic BSL). Measure: rate of proprietary enclosure, downstream user freedom preservation, contributor participation.',
    'If weaker mechanisms suffice, the viral mechanism''s excess suppression is extractive overhead (pushing classification toward tangled_rope). If viral mechanism is necessary, the suppression is the coordination cost (supporting rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viral_mechanism_necessity, empirical, 'Whether viral copyleft is structurally necessary for freedom preservation or extractively excessive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.08).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2007, 0.13).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tr_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.25).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2007, 0.32).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_be_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.65).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t1995, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t2007, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2007, 0.73).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(gpl_reciprocity_obligation__copyleft_as_freedom_reading_su_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.08).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, agpl_network_copyleft).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, mpl_file_level_copyleft).

% DUAL FORMULATION NOTE:
% This story (copyleft_as_freedom_reading) and its siblings (copyleft_as_restriction_reading, copyleft_as_commons_reading) form a constraint family decomposing the gpl_reciprocity_obligation kernel. The freedom reading ε=0.35 (rope claimed); the restriction reading likely authors ε>0.6 (snare/tangled_rope claimed); the commons reading likely authors ε~0.4-0.5 (tangled_rope claimed). They share the same legal text but instantiate different constraints because the ε referent (the standing arrangement) is assessed by different readings' lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
