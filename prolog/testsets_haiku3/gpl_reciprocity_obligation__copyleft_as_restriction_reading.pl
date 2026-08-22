% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft-as-Restriction Reading)
 *   domain: intellectual_property/software_licensing/open_source
 *
 * SUMMARY:
 *   GPL and other viral licenses enforce reciprocity: any software
 *   incorporating GPL-licensed code must itself be distributed under GPL,
 *   preventing proprietary integration without consent from all copyright
 *   holders. This constraint story instantiates the 'copyleft-as-restriction'
 *   reading: viral licensing is framed as constraining business models by
 *   prohibiting proprietary derivatives. Under this reading, the GPL's
 *   primary effect is to prevent proprietary vendors from capturing commons
 *   work, and it does this by extracting (enforcing) openness as the price of
 *   integration. The commons contributors are the targets: they must
 *   open-source or forfeit integration. Proprietary vendors are the
 *   beneficiaries: the restriction prevents their competitors from building
 *   closed-source derivatives, segmenting the software market into GPL
 *   commons and proprietary islands. This reading does NOT assert that
 *   reciprocity is wrong; it simply identifies who bears the cost and who
 *   gains market advantage from the constraint.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: benefit from market segmentation; GPL prevents competitors from closing commons work
 *   - commons_contributors: bear the cost of mandatory openness; constrained in how they monetize contributions
 *   - derivative_work_authors: identity-locked to open-source values; face choice between GPL compliance or rewriting
 *   - GPL leadership (FSF): agenda-setter; enforces license terms through litigation and community norms
 *   - end users: beneficiary but voiceless; GPL ensures visibility but they do not negotiate terms
 *   - proprietary fork operators: strategically benefit by forking before viral point and relicensing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.42).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft-as-Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "intellectual_property/software_licensing/open_source").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '500f8411-c3e9-4030-9132-a4865f511984').
narrative_ontology:cs_kernel_codification('500f8411-c3e9-4030-9132-a4865f511984', fixed_text).
narrative_ontology:cs_authority_grounding('500f8411-c3e9-4030-9132-a4865f511984', extraction).
narrative_ontology:cs_interpretation_layer_present('500f8411-c3e9-4030-9132-a4865f511984').
narrative_ontology:cs_reading_relation('500f8411-c3e9-4030-9132-a4865f511984', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('500f8411-c3e9-4030-9132-a4865f511984', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('500f8411-c3e9-4030-9132-a4865f511984', foundational, proprietary_integration_must_be_prevented).
narrative_ontology:cs_axiom_status(proprietary_integration_must_be_prevented, holdable).
narrative_ontology:cs_axiom_grounding('500f8411-c3e9-4030-9132-a4865f511984', proprietary_integration_must_be_prevented, instrumental).
narrative_ontology:cs_axiom('500f8411-c3e9-4030-9132-a4865f511984', secondary, market_segmentation_is_legitimate_enforcement_consequence).
narrative_ontology:cs_axiom_status(market_segmentation_is_legitimate_enforcement_consequence, holdable).
narrative_ontology:cs_axiom_grounding('500f8411-c3e9-4030-9132-a4865f511984', market_segmentation_is_legitimate_enforcement_consequence, conventional).
narrative_ontology:cs_reference_frame('500f8411-c3e9-4030-9132-a4865f511984', proprietary_commons_enclosure_prevention).
narrative_ontology:cs_drift_state('500f8411-c3e9-4030-9132-a4865f511984', contemporary_permissive_dominance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('500f8411-c3e9-4030-9132-a4865f511984', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_preservationists).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, derivative_work_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_gpl_code).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commercial_service_providers).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, property_rights_are_enforceable_through_licensing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from GPL's prohibition on proprietary integration because it prevents competitors from building closed-source derivatives of their GPL-licensed code. The restriction creates a market segmentation: GPL code remains in the open-source ecosystem where they cannot extract proprietary value, while they can freely build proprietary software on top of non-viral licenses. The constraint enables them to fork GPL projects into proprietary versions without reciprocity obligation as long as they use permissive licenses for new work.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, mobile, global).

% Contribute code to GPL-licensed projects with the intention of advancing shared infrastructure. They bear the cost of the reciprocity obligation: any derivative they create or integrate into proprietary software must also be GPL-licensed, which prevents them from monetizing their work through proprietary distribution or integration with closed ecosystems. They may also lose control over downstream uses if others can fork and relicense under GPL variants.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, biographical, constrained, global).

% Face the reciprocity constraint when building on GPL-licensed foundations: they must open-source their derivative work or risk copyright infringement. Their business model options are constrained to open-source distribution unless they rewrite components to avoid the viral trigger. Identity-locked because many view themselves as part of the open-source community and internalize GPL as a moral commitment, even when economic incentives would favor proprietary integration.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, derivative_work_authors, payer,
    moderate, biographical, identity_locked, global).

% Receive the benefit of GPL reciprocity: all derivatives remain open-source and auditable, preserving the freedoms to inspect, modify, and redistribute. However, they have no seat in licensing decisions; they are beneficiaries by design, not participants. They are excluded from negotiating reciprocity terms and cannot approve exceptions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_gpl_code, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, end_users_of_gpl_code, excluded).

% Operate under the constraint that services built on GPL code must disclose GPL material and offer source access to customers. This prevents them from bundling GPL-licensed components into proprietary service architectures. They can work around the constraint by dual-licensing, hosting-only models (AGPL), or using permissive licenses instead.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commercial_service_providers, payer,
    powerful, generational, constrained, global).

% Sets the GPL terms and enforces them through the FSF and community norms. They adjudicate what constitutes derivative work, license compatibility, and acceptable redistribution. They can modify the license terms (GPL v2, v3) and grant exceptions. Their enforcement is primarily through social pressure and selective lawsuit; violation is common but selective prosecution maintains credibility.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_movement_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Can create closed-source branches of GPL code by taking a snapshot, relicensing under proprietary terms (if they are the copyright holder or have obtained licenses from all contributors), and then forking away. The GPL reciprocity obligation does NOT bind them retroactively, enabling a strategy of dual-licensing or forking to capture value from accumulated commons work.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_fork_operators, beneficiary,
    institutional, generational, mobile, global).

% Projects using permissive licenses (MIT, Apache, BSD) are excluded from the reciprocity constraint entirely, creating an alternative commons model. Permissive-licensed code can be freely integrated into proprietary products without obligation. This exclusion means GPL's restriction operates as a market segmentation force, pushing some innovation toward permissive licenses and commercial integration.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, competing_open_source_models, excluded,
    moderate, generational, constrained, global).

% Treat the GPL as a legal test case for whether copyright can be used to enforce open-source requirements. Patent offices, courts, and legislatures monitor GPL litigation and adoption to understand the legitimacy and enforceability of copyleft constraints. Their decisions on enforceability shape the constraint's persistence.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, intellectual_property_regimes, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures all derivatives of shared code remain visible and auditable by preventing proprietary enclosure of accumulated improvements. Creates a commons-preservation mechanism: if anyone contributes, all benefit from subsequent contributions because no derivative can be hidden.
% TRANSFER_FUNCTION: Transfers the right to extract proprietary value from GPL-licensed code away from individual derivative authors and toward the collective commons. The constraint moves the option to commercialize through proprietary integration from derivative authors to those willing to stay open-source or use permissive alternatives.
% ABSENT_VOICES: End users of GPL code benefit but have no voice in license terms. Downstream maintainers of permissive-licensed projects that could have used GPL see GPL as restrictive and choose alternatives; they are excluded from the copyleft framing. Proprietary vendors initially prohibited from integrating GPL code into their products are structurally absent from the negotiation.
% DISAPPEARANCE_RATIONALE: If GPL reciprocity vanished, proprietary vendors would immediately integrate GPL-licensed components into closed products, accumulated commons improvements would fragment into proprietary branches, and the economic incentive structure would shift toward permissive licensing. The open-source ecosystem's commons-pooling dynamic would collapse into a property-rights model.
% FOUNDING_PROBLEM: Early GPL (v1, 1989) was built to prevent proprietary capture of GNU software improvements: the founding problem was that proprietary vendors could take GNU tools, close them, and sell the result without returning improvements. GPL's reciprocity was the solution.
% FOUNDING_PROBLEM_CORROBORATION: The GPL maintainers attest the founding problem is live: proprietary extraction threats persist and reciprocity deters them. Proprietary vendors counter that the founding problem is solved by market reputation (they can fork and maintain a permissive version to attract contributors). Independent software economists note that GPL's enforcement has become selective (many violations go unprosecuted) and that permissive licensing is ascendant, suggesting the founding problem's urgency has declined.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint transfers the right to monetize through proprietary integration away from derivative authors, creating an asymmetry: commons contributors must open-source or lose integration options, while proprietary vendors can freely build closed products on permissive alternatives and still benefit from GPL's market segmentation. Suppression is moderate (0.42) because the reciprocity obligation is enforced through copyright law (strong legal backing) but violations are common and enforcement is selective—the constraint does not absolutely prevent proprietary integration (technical workarounds, dual-licensing, forks exist), it just raises the cost. Theater is low (0.18) because the constraint's function is transparent: the point IS to enforce openness, not to disguise extraction as something else. Accessibility collapse is moderate (0.51) because alternatives exist (permissive licenses, proprietary forks, dual-licensing), but once the commons is established under GPL, exit for derivative authors becomes costly because they lose the accumulated improvements. Resistance is high (0.72) because proprietary vendors actively lobby against GPL (SPDX, patent licenses, trade secret protection), and derivative authors test the boundaries (linking disputes, AGPL, SaaS loopholes). The measurement series shows extractiveness rising from 0.48 to 0.68 over 35 years as proprietary vendors increasingly depend on GPL commons (Linux, OpenSSL, etc.) while facing stronger downstream reciprocity pressure. Suppression requirement declines over time (0.55 to 0.42) as enforcement becomes more selective and workarounds proliferate, suggesting the constraint's coercive power is degrading even as its extractive effect on commons contributors has solidified.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary-vendor seat and the commons-contributor seat experience this constraint inversely. Vendors see a restriction on their freedom to use open-source code in closed products—GPL is extracting openness as the price of access. Contributors see a protection mechanism—GPL prevents vendors from taking their work and selling it closed. The engine computes both perspectives from the structural data: high d (directionality toward target) for contributors who face reciprocity obligations, low d for vendors who benefit from market segmentation. Neither perspective is 'wrong'; they are structural consequences of the same rule applied to different power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors (institutional power, mobile exit, arbitrage options) have low directionality—they are not trapped by GPL because they can use permissive licenses, fork proprietary variants, or license separately. They are beneficiaries in this reading: the constraint prevents their competitors from freely integrating commons improvements. Commons contributors (moderate power, constrained exit, identity-locked) have high directionality—they face the reciprocity obligation directly; exit means forking into permissive alternatives and losing commons participation. Derivative-work authors sit between: they face the constraint but have identity-locked exit (they internalize GPL as part of their professional identity), raising their effective d. Commercial service providers have constrained exit (cannot easily avoid GPL dependencies) and moderate power, placing them at moderate-to-high d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates a Tangled Rope, not a pure Snare, because the constraint has genuine coordination value (commons preservation) alongside asymmetric extraction (vendors vs. contributors). If the coordination function is considered successful (accumulated improvements, ecosystem stability), the rope persists because both beneficiaries and victims find it valuable to maintain. If the founding problem (proprietary capture of improvements) declines in urgency, the rope risks mandatrophy: the commons-preservation function is no longer needed (because permissive licensing and corporate open-source norms now dominate), but the reciprocity obligation persists due to institutional inertia (FSF authority, legal precedent, community identity). Measurement data showing suppression decline while extractiveness plateaus suggests mandatrophy onset: the constraint is doing less coordination work but still extracting from commons contributors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_erosion,
    'Does selective enforcement of GPL reciprocity obligations gradually erode the constraint''s coercive effect, converting it toward a norm rather than a binding rule?',
    'Comparative analysis of GPL violation patterns, lawsuit outcomes, and community response over time. Track whether violations increase, go unprosecuted, or face weakened penalties.',
    'If enforcement becomes more selective, suppression will continue to decline and the constraint will transition from ''enforced obligation'' toward ''internalized norm'' (higher identity-lock component, lower structural suppression). Type classification may shift from Tangled Rope toward Rope as the coercive component degrades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_erosion, empirical, 'Whether GPL enforcement is becoming weaker over time, suggesting constraint degradation through non-enforcement rather than revision.').

omega_variable(
    permissive_alternative_maturation,
    'As permissive licenses (MIT, Apache) mature and corporate open-source practices normalize, does GPL''s market segmentation advantage persist or erode?',
    'Track adoption rates of permissive vs. viral licenses over time; survey proprietary vendors on licensing strategy; analyze whether commons fragmentation into proprietary branches increases.',
    'If permissive licenses capture more commons work, GPL''s market-segmentation benefit to proprietary vendors declines; extractiveness from commons contributors may drop if exit to permissive alternatives becomes more viable. The Tangled Rope may decompose into separate Rope-like constraints (mutual coordination under each licensing model) rather than persist as a unified extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permissive_alternative_maturation, empirical, 'Whether market evolution is making the GPL constraint less extractive by reducing its monopoly on commons coordination.').

omega_variable(
    foundational_reading_contested,
    'Is the framing of GPL-as-restriction (this reading) a legitimate structural claim, or does it represent a rhetorical inversion of the constraint''s actual function?',
    'Analyze the constraint under the competing readings (freedom_reading, commons_reading) and assess whether the restriction framing is compatible with those readings or whether one reading forecloses the others.',
    'If the restriction framing is compatible with the freedom and commons readings (coexist_with relation), then all three readings are live and the constraint has genuine perspectival ambiguity. If one reading foreclosed another, the constraint would be simpler (one of the readings would collapse). Current evidence suggests coexistence: vendors emphasize restriction, users emphasize freedom, commons advocates emphasize preservation—the same GPL text supports all three readings depending on which causal path is emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_reading_contested, conceptual, 'Whether this reading is a coherent structural perspective on GPL or a rhetorical distortion of its primary function.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.42) primarily structural (legal risk of GPL violation) or internalized (commons contributors have internalized GPL as a moral commitment)?',
    'Comparative study of GPL contributor behavior in jurisdictions with weak IP enforcement vs. strong enforcement; survey contributors on whether they would violate GPL if enforcement vanished; track whether proprietary derivatives emerge more rapidly in low-enforcement regions.',
    'If suppression is primarily internalized, the constraint''s effective suppression would persist even if legal enforcement weakened—the measurement underestimates true persistence. If primarily structural, legal erosion would rapidly cascade into behavioral change. This affects type stability: an internalized-suppression constraint is closer to Rope (self-reinforcing norm) than Tangled Rope (enforced asymmetry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether GPL recipients comply due to legal risk or due to internalized values—diagnostic for understanding constraint persistence mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 35, 0.42).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_license_enforcement).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dual_licensing_business_model).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation decomposes into three structurally distinct constraints, one for each reading. All three share the same kernel (GPL text) but emphasize different structural consequences and have different ε values. This reading (copyleft_as_restriction) emphasizes market-segmentation extraction (ε=0.68). The freedom reading emphasizes user-autonomy preservation (lower ε, different beneficiary set). The commons reading emphasizes institutional commons protection (different victim/beneficiary structure). They are linked via network.affects_constraints; each story authors its own ε independently and the engine determines whether readings coexist or one forecloses another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
