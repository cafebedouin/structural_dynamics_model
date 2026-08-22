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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: GPL Reciprocity Obligation (Freedom-Preservation Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This story instantiates the freedom-preservation reading of the GPL
 *   reciprocity kernel: the viral (copyleft) clause is authored here as the
 *   mechanism that keeps user freedoms (to run, study, modify, redistribute)
 *   durable across every downstream fork, by removing the option to capture a
 *   derivative into a closed proprietary product. The referent for extraction
 *   is the standing reciprocity arrangement as this reading sees it — mild,
 *   because from this reading's own lights the obligation is a low-cost
 *   guarantee that secures a much larger good (a durable commons) rather than
 *   a rent extracted from integrators. Suppression is more substantial: the
 *   reading is candid that alternative licensing paths for a derivative work
 *   are genuinely foreclosed once GPL code is incorporated and distributed —
 *   that suppression is the point, not a side effect, from this reading's
 *   perspective. Sibling readings of the same kernel
 *   (copyleft_as_restriction_reading, copyleft_as_commons_reading) are NOT
 *   part of this story; they are separate constraints with their own ε and
 *   stakeholder sets, linked only via network edges and this commentary note.
 *
 * KEY AGENTS:
 *   - downstream_users: primary beneficiary (powerless/mobile) — receives guaranteed inspectability and modification rights
 *   - software_freedom_movement: agenda_setter (organized/analytical) — drafts, defends, and litigates the reciprocity clause
 *   - proprietary_integrators: primary target (powerful/constrained) — bears the cost of the reciprocity obligation when incorporating GPL code
 *   - original_copyright_holders: beneficiary/agenda_setter (moderate/mobile) — chooses the license and sometimes monetizes via dual-licensing
 *   - permissive_license_advocates: excluded — objects to the obligation itself as a restriction, not incorporated into this reading's terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.62).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Freedom-Preservation Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '8afb4289-d861-4bbb-9b37-c0beee344d40').
narrative_ontology:cs_kernel_codification('8afb4289-d861-4bbb-9b37-c0beee344d40', fixed_text).
narrative_ontology:cs_authority_grounding('8afb4289-d861-4bbb-9b37-c0beee344d40', practice).
narrative_ontology:cs_interpretation_layer_present('8afb4289-d861-4bbb-9b37-c0beee344d40').
narrative_ontology:cs_reading_relation('8afb4289-d861-4bbb-9b37-c0beee344d40', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8afb4289-d861-4bbb-9b37-c0beee344d40', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('8afb4289-d861-4bbb-9b37-c0beee344d40', foundational, user_rights_survive_redistribution).
narrative_ontology:cs_axiom_status(user_rights_survive_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('8afb4289-d861-4bbb-9b37-c0beee344d40', user_rights_survive_redistribution, deontological).
narrative_ontology:cs_axiom('8afb4289-d861-4bbb-9b37-c0beee344d40', secondary, reciprocity_cost_is_justified_by_freedom_gain).
narrative_ontology:cs_axiom_status(reciprocity_cost_is_justified_by_freedom_gain, holdable).
narrative_ontology:cs_axiom_grounding('8afb4289-d861-4bbb-9b37-c0beee344d40', reciprocity_cost_is_justified_by_freedom_gain, instrumental).
narrative_ontology:cs_reference_frame('8afb4289-d861-4bbb-9b37-c0beee344d40', four_freedoms_founding_charter).
narrative_ontology:cs_drift_state('8afb4289-d861-4bbb-9b37-c0beee344d40', post_saas_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8afb4289-d861-4bbb-9b37-c0beee344d40', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_developers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_movement).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, original_copyright_holders).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_developers).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, four_freedoms_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software along with the source code and the same rights the original author had: to run, study, modify, and redistribute. The reciprocity clause guarantees that any modified version they receive also comes with these rights, rather than being captured into a closed derivative they cannot inspect or alter.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, generational, mobile, global).

% Build on GPL-licensed code and gain a large, actively maintained commons to draw from. In exchange they must release their own modifications under the same terms if they distribute the work — a real cost in flexibility, but one that guarantees the next developer downstream gets the same commons they received.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_work_developers, payer).

% Drafts and defends the license text, litigates violations, and frames the reciprocity requirement as the mechanism that keeps user freedom durable across every downstream fork. Treats the viral clause as the load-bearing structural element, not an incidental feature.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_movement, agenda_setter,
    organized, civilizational, analytical, global).

% Would like to fold GPL-licensed components into closed products but cannot without triggering the obligation to release their own source under the same license. Their options are to avoid GPL code entirely, negotiate a separate commercial license from the copyright holder where available, or reimplement the functionality independently — all costly compared to simply incorporating the code freely.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Operate in markets adjacent to GPL software and must architect around it — using permissively licensed or proprietary alternatives, or isolating GPL components behind process boundaries — to avoid the reciprocity trigger. They retain exit via alternative licensing ecosystems, unlike an end user locked into a single product.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, closed_source_vendors, payer,
    powerful, biographical, mobile, global).

% Choose the GPL for their own projects to ensure their labor cannot be captured into a closed proprietary fork. Some also dual-license, selling proprietary exceptions to companies unwilling to comply with reciprocity, capturing revenue precisely because the default obligation is costly to those integrators.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, original_copyright_holders, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_freedom_reading, original_copyright_holders, agenda_setter).

% Argue that maximizing adoption and permitting proprietary derivatives (BSD/MIT-style) grows freedom more effectively than mandatory reciprocity. Within the copyleft-as-freedom framework their position is treated as a category error — freedom-for-code rather than freedom-for-users — so their objection is acknowledged but not incorporated into this reading's terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared commons of freely modifiable software by ensuring that every party who builds on the commons and distributes their work returns their modifications to it — solving the problem that a purely permissive commons erodes as successive forks are captured into closed, non-reciprocating products.
% TRANSFER_FUNCTION: Moves the option to capture derivative works into closed proprietary products away from integrators and vendors, and moves guaranteed inspectability/modifiability rights to every downstream recipient of the software, in perpetuity down the distribution chain.
% ABSENT_VOICES: Permissive-license advocates would object that the obligation itself is a restriction on freedom (freedom to license as one chooses) rather than a guarantee of it; they are acknowledged in the wider ecosystem debate but this reading's own framework treats their objection as addressing a different value (developer freedom vs. user freedom) rather than as a defect in the license.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished, existing GPL codebases would remain distributable but future forks could be closed off at will; over a few product cycles the commons would fragment as competitive advantage accrued to whoever closed their fork first, and the guarantee that downstream users retain the four freedoms would no longer hold structurally — it would depend entirely on each redistributor's discretion.
% FOUNDING_PROBLEM: Early free software distributed under permissive or informal terms was repeatedly captured: companies took community code, improved it, and shipped closed proprietary versions with no obligation to share the improvements back, leaving the original contributors and all downstream users worse off than if the code had never been shared.
% FOUNDING_PROBLEM_CORROBORATION: The software freedom movement attests the capture problem persists, citing ongoing proprietary forks of permissively-licensed projects as evidence. Independent observers outside the movement — antitrust economists studying platform lock-in and academic software-supply-chain researchers — corroborate that proprietary capture of open code remains a documented pattern, though they do not universally agree the GPL's specific viral mechanism is the only or best remedy; some attest a narrower founding problem than the movement's broader framing claims.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.28 at interval end) because, from the freedom-preservation reading's own lights, the reciprocity clause imposes a limited, well-defined cost (share your modifications) in exchange for a large collective benefit (a commons that cannot be captured). Suppression is authored substantially higher (0.62) because the reading is honest that alternative licensing paths are genuinely closed off for anyone who wants to combine GPL code with proprietary code — that is a real, not incidental, restriction of the integrator's options, and it is precisely what secures the freedom claim. Theater ratio stays low and slowly rising (0.10 by interval end) — enforcement (license compliance audits, litigation) is functional, not performative, though an increasing share of movement activity over time goes toward compliance monitoring and legal defense as the ecosystem matures.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (proprietary_integrators) and the beneficiary/agenda-setter seats (software_freedom_movement, downstream_users) will compute structurally different types from the same reciprocity clause: from the beneficiary seats this looks like a rope — a low-coercion coordination mechanism that most participants would choose again. From the integrator seat, the same mandatory-sharing structure reads as a constraint imposed without consent whose cost is not offset by any benefit the integrator wanted. This divergence is the intended object of measurement, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and derivative-work developers are declared beneficiaries: they receive rights and a growing commons at low direct cost, placing them near the beneficiary end of directionality. Proprietary integrators and closed-source vendors are declared victims/payers: the reciprocity obligation directly constrains their business options when touching GPL code, placing them near the target end. Original copyright holders sit close to the beneficiary end but with agenda-setting power — some additionally monetize the asymmetry via dual-licensing, which is a distinct but related capture of value from the same structural leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permissive-license capture into closed proprietary forks) remains empirically live in the wider ecosystem, which is why founding_problem_status is authored as contested rather than dead — the movement and some independent researchers corroborate ongoing capture dynamics, while permissive-license advocates dispute that GPL's specific mechanism is still the necessary or sufficient remedy. Because the founding problem has not gone dead while the reciprocity mechanism persists, this reading does not exhibit the zombie-mandate pattern (status=dead + disappearance=world_rearranges) that would flag mandatrophy; the arrangement's continued restrictiveness tracks a genealogy that at least some outside observers still corroborate as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_restriction_framing_choice,
    'Is the reciprocity obligation better understood as a freedom-preservation guarantee for downstream users, or as a restriction on the licensing choices of anyone who wants to build a business on the code? Both are coherent framings of the identical clause.',
    'No empirical resolution exists — this is a conceptual/preference disagreement about which party''s freedom (the user''s freedom to inspect/modify vs. the integrator''s freedom to license as they choose) the license should be evaluated from. Sibling constraint stories (copyleft_as_restriction_reading, copyleft_as_commons_reading) instantiate the alternative framings as separate, ε-invariant constraints rather than resolving this story''s ε.',
    'Under this reading (freedom-preservation), extraction is authored low and suppression is authored as the necessary price of the guarantee. Under the sibling restriction reading, the same suppression value would be read as evidence of extraction against integrators, likely raising claimed and metric divergence toward tangled_rope or snare. The classification each reading receives depends on which framing is adopted as the story''s own lights — this is exactly the ε-invariance principle at work: two structurally distinct claims sharing one label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_restriction_framing_choice, conceptual, 'Whether the GPL reciprocity clause is a freedom-preservation mechanism or a business-model restriction is a framing choice, not an empirical fact, and is why the kernel is split into three sibling constraint stories rather than one story with a hidden measurement parameter.').

omega_variable(
    dual_licensing_capture,
    'When original copyright holders monetize the reciprocity asymmetry via dual-licensing (selling proprietary exceptions to integrators who cannot comply), does this represent a legitimate return on authorship or a rent extracted from the structural leverage the license itself creates?',
    'Comparative analysis of dual-licensing revenue relative to the counterfactual revenue available under a purely permissive license for the same codebase; interviews with integrators about whether they perceive the commercial license fee as fair value or as leverage-driven extraction.',
    'If dual-licensing revenue substantially exceeds what a permissively-licensed equivalent could capture, it suggests the reciprocity obligation functions partly as a mechanism to create paid escape valves — shifting some of this reading''s classification toward tangled_rope for the subset of relationships involving commercial licensing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_licensing_capture, empirical, 'Whether dual-licensing built on the reciprocity clause is authorship return or leverage-extraction.').

omega_variable(
    user_freedom_measurement_ambiguity,
    'Is ''downstream user freedom'' meaningfully preserved when most end users never read, modify, or redistribute source code, making the guaranteed rights largely theoretical for the median user while very real for the smaller population of developers and integrators?',
    'Survey data on actual exercise of the four freedoms by end users vs. developers; compare rates of source-code access and modification across GPL vs. proprietary equivalents.',
    'If the practical beneficiary population is much narrower than ''downstream users'' broadly construed, the beneficiary declaration should be narrowed to technically capable users/developers, which would somewhat reduce the coordination-function weight of this reading without changing the suppression finding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_freedom_measurement_ambiguity, empirical, 'Whether the declared beneficiary class (downstream users) matches who actually exercises the preserved freedoms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gpl__tr_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(gpl__tr_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 18, 0.08).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl__be_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(gpl__be_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gpl__su_t6, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(gpl__su_t18, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.05).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the gpl_reciprocity_obligation kernel, each a separate ε-invariant constraint per the ε-invariance principle: copyleft_as_freedom_reading (this story — user-freedom framing, beneficiary=downstream users, low authored extraction), copyleft_as_restriction_reading (business-constraint framing, beneficiary/victim roles largely inverted, likely higher authored extraction against integrators), and copyleft_as_commons_reading (ecosystem/institutional framing, beneficiary=the commons as a collective good rather than individual users). All three share identical license text and enforcement mechanism but diverge in beneficiary/victim declaration and in claimed type. Network edges here are declared forward to the sibling constraint_ids; each sibling should reciprocally link back to this one and to the third.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
