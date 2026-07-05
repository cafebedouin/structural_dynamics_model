% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Parliamentary Sovereignty over Basic Law Interpretation
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the parliamentary sovereignty
 *   reading — of the contested basic_law_interpretive_boundary kernel
 *   governing who has final authority to interpret and amend Israel's Basic
 *   Laws. Under this reading, the Knesset is the ultimate sovereign: it can
 *   amend Basic Laws by simple majority and override judicial invalidation of
 *   ordinary legislation, subject only to binding international treaty
 *   obligations. This is a distinct structural claim from the
 *   judicial_supremacy_reading (where court invalidation binds the Knesset)
 *   and the balanced_contestation_reading (where both institutions hold
 *   bounded but mutually constraining authority) — those are separate
 *   constraints with their own ε values, linked here via
 *   network.affects_constraints, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - governing_coalition: primary agenda_setter (institutional/arbitrage) — drafts and benefits from override power
 *   - knesset_majority_bloc: primary beneficiary (organized/mobile) — legislates without judicial constraint
 *   - supreme_court: institutional payer (institutional/constrained) — retains form, loses binding force
 *   - political_minorities and non_jewish_citizens: structural targets (powerless/trapped) — lose the judicial check they relied on
 *   - civil_society_petitioners: excluded voice (moderate/constrained) — heard but not decisive
 *   - international_treaty_partners: analytical observer (institutional/analytical) — retain a lever domestic actors lack
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.55).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Parliamentary Sovereignty over Basic Law Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '96273bb3-9d3a-43fa-b2ef-a5c3bef529a0').
narrative_ontology:cs_kernel_codification('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', distributed).
narrative_ontology:cs_authority_grounding('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', distributed).
narrative_ontology:cs_reading_relation('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', foundational, electoral_mandate_is_sufficient_constitutional_legitimation).
narrative_ontology:cs_axiom_status(electoral_mandate_is_sufficient_constitutional_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', electoral_mandate_is_sufficient_constitutional_legitimation, conventional).
narrative_ontology:cs_axiom('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', foundational, unelected_judicial_body_lacks_standing_to_bind_elected_legislature).
narrative_ontology:cs_axiom_status(unelected_judicial_body_lacks_standing_to_bind_elected_legislature, holdable).
narrative_ontology:cs_axiom_grounding('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', unelected_judicial_body_lacks_standing_to_bind_elected_legislature, deontological).
narrative_ontology:cs_reference_frame('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', founding_era_ambiguous_sovereignty).
narrative_ontology:cs_drift_state('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', post_2023_judicial_reform_crisis, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('96273bb3-9d3a-43fa-b2ef-a5c3bef529a0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_bloc).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, political_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, non_jewish_citizens).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_petitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds simple-majority control of the Knesset and, under this reading, can pass, amend, or override Basic Laws and neutralize judicial invalidation through an override mechanism. It sets the agenda for what counts as constitutional and drafts the override procedures it will be governed by, giving it effective control over its own constraints.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition, agenda_setter,
    institutional, biographical, arbitrage, national).

% Coalition legislators who benefit from the removal of a credible judicial check on majoritarian legislation. Their electoral mandate is treated as sufficient legitimation for policy that might otherwise be struck down as inconsistent with rights-protective Basic Law provisions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_bloc, beneficiary,
    organized, biographical, mobile, national).

% Retains formal power of judicial review but, under this reading, its rulings on Basic Law compatibility can be legislatively overridden by the body whose acts it reviews. It cannot exit the relationship — it continues sitting and issuing rulings, but its rulings carry advisory rather than binding weight against a determined majority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, observer).

% Opposition voters and parties who lack the votes to block majority legislation and lose the judiciary as a secondary check once override power is exercised. Their recourse is confined to future elections, which may be years away and does not undo enacted harms.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, political_minorities, payer,
    powerless, biographical, trapped, national).

% A demographic minority whose equality and status claims have historically relied on judicial interpretation of Basic Law protections (e.g., human dignity and liberty). Under parliamentary sovereignty, protections they depend on become contingent on the preferences of a majority they structurally cannot form.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, non_jewish_citizens, payer,
    powerless, generational, trapped, national).

% NGOs and individuals who petition the Supreme Court against government action. Under this reading their petitions can succeed on the merits and still be nullified by legislative override, so their voice is heard but not decisive — a formal channel without a guaranteed floor.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_petitioners, excluded,
    moderate, biographical, constrained, national).

% States and international bodies bound to Israel by treaty. This reading concedes that Knesset sovereignty does not extend to overriding binding international obligations, so treaty partners retain a lever the domestic constitutional order does not provide to domestic minorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_treaty_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular locus of ultimate authority so that constitutional questions are resolved by an electorally accountable body rather than by an unelected court, avoiding institutional deadlock between two bodies each claiming final say.
% TRANSFER_FUNCTION: Moves final interpretive and amendment authority over Basic Laws from the judiciary to the Knesset majority, and correspondingly moves rights-protection assurance away from minorities and toward whatever coalition currently holds power.
% ABSENT_VOICES: Non-Jewish citizens, unrepresented political minorities, and civil society petitioners whose claims previously found a check in judicial review are structurally present in the political process (they can vote, petition) but absent from any body that can bind the majority against its own legislative will.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over Basic Law interpretation were abandoned overnight in favor of a binding judicial-supremacy reading, previously override-immune legislation would become subject to invalidation, coalition governments would need to build rights-compatible legislation prospectively, and minority groups would regain a credible institutional check — a substantial rearrangement of legislative behavior and minority protection.
% FOUNDING_PROBLEM: Israel lacks a single written constitution; the Basic Laws were enacted incrementally with contested constitutional status, and someone had to be designated as the final word on their meaning and mutability to prevent open-ended institutional conflict between an unelected court and an elected legislature.
% FOUNDING_PROBLEM_CORROBORATION: Coalition legal advisors and sovereignty-reading scholars attest the problem (undefined final authority) remains live and requires legislative primacy to resolve democratically. Independent constitutional scholars, retired justices, and comparative-law observers outside the governing coalition attest that the problem has shifted: the live issue is no longer 'who decides' in the abstract but whether removing the check functions as protection against overreach or as its enablement — corroboration for the 'still live, but now differently framed' status comes from outside the coalition's own legal apparatus.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-rising (0.18 to 0.42) rather than low, reflecting that this reading concentrates constitutional authority in a self-interested body (the Knesset legislating its own limits) even though for the bulk of ordinary majoritarian policy the effective cost imposed on the general population is low — the extraction is concentrated on minorities whose protections depended on the removed check, not diffused across the whole population. Suppression is authored moderate (0.55 at T=20) because the mechanism does not use direct coercion but does foreclose a previously available remedy path for petitioners, which functions as suppression of an exit option rather than suppression of dissent per se. Theater ratio is kept low-moderate (0.28) because the coordination function (resolving who has final say) is genuinely exercised, not merely performed — the Knesset does legislate and does override; the constraint is not empty theater. Resistance is authored high (0.72) reflecting sustained litigation, mass protest, and international commentary contesting this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The governing coalition and knesset_majority_bloc sit near the beneficiary end: they gain unconstrained legislative capacity and control the very override mechanism that binds them, producing near-arbitrage exit from any check. Political minorities and non_jewish_citizens sit near the target end: trapped exit options (they cannot form a majority to reverse the arrangement through ordinary politics) and generational time horizon (this is a status arrangement, not a single-transaction cost) push their derived directionality toward high χ. The Supreme Court occupies an unusual institutional-power/constrained-exit position — it retains formal function but cannot exit the relationship or refuse to be overridden, which is why it is authored as payer rather than agenda_setter under this reading, despite holding institutional-level power in the abstract.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (undefined final constitutional authority in a state without a single written constitution) is authored as contested rather than resolved: the coalition's own legal apparatus treats the problem as still live and requiring this exact fix, while independent scholars outside that apparatus argue the founding problem has been answered procedurally but at the cost of creating a new, unaddressed problem (minority protection against majoritarian override). This divergence is exactly the kind of founding_problem_status mismatch the R5 interview is designed to expose — the disappearance_verdict of world_rearranges combined with a contested status flags the arrangement for further scrutiny rather than certifying it as either settled coordination or pure capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_kernel_disambiguation,
    'Is the correct structural reading of the Basic Law interpretive boundary the parliamentary sovereignty reading (this story), the judicial supremacy reading, or the balanced contestation reading — and is the disagreement resolvable, or is it a live constitutional contest without a fact-of-the-matter answer?',
    'Track which reading the Knesset, Supreme Court, and international bodies actually act on over successive constitutional crises (e.g., override legislation, subsequent judicial responses, and whether either side treats the other''s ruling as binding in practice). Convergent institutional behavior over time would indicate which reading has become operative; continued divergence would indicate the kernel is genuinely contested rather than merely under-theorized.',
    'If institutional practice consistently follows this reading (overrides sustained, judicial rulings treated as advisory), this story''s classification is corroborated. If practice instead follows the judicial_supremacy_reading (overrides struck down or reversed under domestic/international pressure), this story describes an aspirational or transitional claim rather than the operative constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_reading_kernel_disambiguation, conceptual, 'Which kernel reading is operative in practice, and whether the contest is resolvable at all.').

omega_variable(
    minority_protection_displacement,
    'Does removing binding judicial review over Basic Laws genuinely displace minority protection onto the political process, or does it merely relocate protection to informal norms and international pressure that are themselves fragile?',
    'Comparative analysis of minority-rights outcomes before and after override mechanisms are exercised, cross-referenced with international treaty enforcement actions and diplomatic pressure episodes.',
    'If informal/international protections prove robust substitutes, the extractiveness authored here is overstated; if they prove fragile or slow, the extraction concentrated on non_jewish_citizens and political_minorities is understated relative to the true cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_displacement, empirical, 'Whether non-judicial protections adequately substitute for the removed judicial check.').

omega_variable(
    self_binding_paradox,
    'Can a legislative majority that controls the override mechanism ever be meaningfully said to be ''bound'' by Basic Laws at all, or does the override power collapse the distinction between ordinary legislation and constitutional constraint?',
    'Analyze whether any override attempt has ever failed politically (been withdrawn under public or coalition pressure) versus always succeeding once initiated — a pattern of failed overrides would suggest a real (if weak) constraining function persists.',
    'If overrides never fail, the Basic Law layer under this reading is revealed as effectively identical to ordinary legislation, which would push the classification toward pure majoritarian discretion rather than any constitutional constraint at all — strengthening the snare-adjacent reading of victim impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_binding_paradox, conceptual, 'Whether the override mechanism preserves any real constitutional constraint or collapses it entirely.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t4, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(basi_be_t4, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t4, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the basic_law_interpretive_boundary kernel. parliamentary_sovereignty_reading (this file) holds the Knesset as unconstrained final authority; judicial_supremacy_reading holds Supreme Court invalidation as binding on the Knesset; balanced_contestation_reading holds both institutions as bounded but mutually constraining. Each carries a distinct ε: this reading is authored with moderate, minority-concentrated extraction (0.42 at interval end) reflecting near-zero cost to majoritarian policy but substantial cost to structurally powerless minorities; the judicial_supremacy_reading should be authored with its own distinct extraction profile reflecting counter-majoritarian court costs to legislative agendas; the balanced_contestation_reading should be authored as the lowest-extraction of the three, reflecting genuine mutual constraint. Decomposition follows the ε-invariance principle: these are not the same constraint measured three ways, but three structurally distinct constitutional claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
