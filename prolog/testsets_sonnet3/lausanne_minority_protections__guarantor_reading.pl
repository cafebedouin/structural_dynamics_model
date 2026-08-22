% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections as Internationally Supervised Obligation (Guarantor Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the guarantor reading of the Treaty of Lausanne
 *   (1923) minority protection clauses: the claim that these obligations are
 *   not solely subject to Turkish domestic interpretation but are
 *   internationally supervised, invocable through guarantor-state diplomacy
 *   and, in the modern era, through European human rights mechanisms such as
 *   the ECtHR. This is distinct from the expansive reading (which claims
 *   institutional self-administration, property, and clergy formation
 *   guarantees) and the restrictive reading (which confines protection to
 *   individual worship rights and treats institutional questions as purely
 *   domestic). The guarantor reading's distinguishing claim is
 *   procedural/jurisdictional rather than substantive: it is about WHO gets
 *   to adjudicate minority questions, not WHAT substantive protections exist.
 *   Historically the mechanism has functioned more as diplomatic leverage —
 *   invoked selectively, rarely producing binding remedy — than as an
 *   enforceable constraint, which is reflected in the low extractiveness but
 *   rising theater_ratio over the interval.
 *
 * KEY AGENTS:
 *   - ecumenical_patriarchate: Primary beneficiary (moderate/trapped) — depends on external invocation for leverage it cannot generate domestically
 *   - greek_orthodox_minority_istanbul: Beneficiary in name (powerless/trapped) — the community the mechanism is nominally for, with no direct standing
 *   - guarantor_states_diplomatic_corps: Agenda-setter (institutional/mobile) — chooses when to invoke the mechanism, weighted against unrelated interests
 *   - council_of_europe_monitoring_bodies: Agenda-setter/observer (institutional/analytical) — provides the modern legal channel but limited enforcement teeth
 *   - turkish_state_sovereignty_claimants: Payer (institutional/constrained) — bears reputational cost of invocation without facing binding remedy
 *   - legal_historians_treaty_scholars: Analytical observer — assesses whether the mechanism was ever more than gesture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.22).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.28).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections as Internationally Supervised Obligation (Guarantor Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, '477f7d95-dfa8-4764-a333-4de7f67852b4').
narrative_ontology:cs_kernel_codification('477f7d95-dfa8-4764-a333-4de7f67852b4', fixed_text).
narrative_ontology:cs_authority_grounding('477f7d95-dfa8-4764-a333-4de7f67852b4', distributed).
narrative_ontology:cs_reading_relation('477f7d95-dfa8-4764-a333-4de7f67852b4', lausanne_minority_protections__restrictive_reading, influences).
narrative_ontology:cs_reading_relation('477f7d95-dfa8-4764-a333-4de7f67852b4', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('477f7d95-dfa8-4764-a333-4de7f67852b4', foundational, adjudication_locus_is_international).
narrative_ontology:cs_axiom_status(adjudication_locus_is_international, holdable).
narrative_ontology:cs_axiom_grounding('477f7d95-dfa8-4764-a333-4de7f67852b4', adjudication_locus_is_international, conventional).
narrative_ontology:cs_axiom('477f7d95-dfa8-4764-a333-4de7f67852b4', secondary, guarantor_standing_survives_treaty_era).
narrative_ontology:cs_axiom_status(guarantor_standing_survives_treaty_era, holdable).
narrative_ontology:cs_axiom_grounding('477f7d95-dfa8-4764-a333-4de7f67852b4', guarantor_standing_survives_treaty_era, conventional).
narrative_ontology:cs_reference_frame('477f7d95-dfa8-4764-a333-4de7f67852b4', post_1923_guarantor_supervision_regime).
narrative_ontology:cs_drift_state('477f7d95-dfa8-4764-a333-4de7f67852b4', contemporary_eu_accession_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('477f7d95-dfa8-4764-a333-4de7f67852b4', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_istanbul).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states_diplomatic_corps).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, council_of_europe_monitoring_bodies).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state_sovereignty_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Relies on the treaty's minority-protection language as leverage to internationalize disputes over property, seminary closures, and institutional status that it cannot win purely through domestic Turkish courts. Has no exit — its seat and institutional continuity are physically located in Istanbul.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, trapped, national).

% A small, demographically diminished community whose remaining institutional infrastructure depends on whatever protective weight the treaty carries when invoked by outside parties on its behalf. Cannot itself compel enforcement; depends entirely on others choosing to act.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_istanbul, beneficiary,
    powerless, generational, trapped, local).

% The United Kingdom, France, and other Lausanne signatories retain a nominal diplomatic standing to raise minority-treatment issues with Turkey. They choose when and how vigorously to invoke this standing, balancing it against unrelated bilateral interests (trade, NATO cohesion, migration cooperation). Exit is easy for them — the issue is one lever among many and rarely a priority.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states_diplomatic_corps, agenda_setter,
    institutional, generational, mobile, continental).

% The European Court of Human Rights and related bodies can hear cases framed as violations of religious-minority rights, producing judgments that carry moral and reputational weight but limited direct enforcement power over a state that can slow-walk compliance.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, council_of_europe_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, council_of_europe_monitoring_bodies, observer).

% Turkish officials who hold that minority governance is a domestic matter experience the guarantor-reading as external interference dressed as treaty obligation. They bear reputational and diplomatic costs when guarantor states or European bodies raise the issue, and must expend political capital managing or deflecting the resulting pressure, even though no binding enforcement mechanism compels a specific remedy.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state_sovereignty_claimants, payer,
    institutional, generational, constrained, national).

% Would prefer that minority questions be resolved entirely through ordinary domestic adjudication under Turkish constitutional and administrative law. Their interpretive authority is structurally bypassed whenever a claim escalates to guarantor diplomacy or Strasbourg, but they have no voice in whether that escalation happens.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts, excluded,
    institutional, immediate, constrained, national).

% Study the negotiating history and subsequent state practice under Lausanne to assess whether the guarantor mechanism was ever intended, or has ever functioned, as anything beyond diplomatic gesture.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, legal_historians_treaty_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, treaty-anchored channel through which third-party states and international human rights bodies can raise minority-treatment concerns with Turkey without those concerns being dismissed as pure foreign interference — a legal hook rather than a bare political demand.
% TRANSFER_FUNCTION: Moves reputational and diplomatic leverage from Turkey toward the minority community and its external patrons whenever guarantor states or Strasbourg choose to invoke the mechanism; moves almost nothing in the way of binding material remedy, since no enforcement organ can compel specific domestic action.
% ABSENT_VOICES: Domestic Turkish courts and legislators, who would prefer the matter resolved entirely within national law, have no seat in whether or how the guarantor mechanism is invoked; the minority community itself has limited direct standing and depends on guarantor states choosing to act, which they do inconsistently and often for unrelated diplomatic reasons.
% DISAPPEARANCE_RATIONALE: If the guarantor-diplomacy reading vanished, the Ecumenical Patriarchate and the remaining Greek Orthodox community would lose one of their few external levers, and Strasbourg cases might lose a treaty anchor — a real loss for those relying on it. But because the mechanism has rarely produced binding remedies in practice, Turkish domestic policy on minority institutions would likely continue largely as-is; whether the world 'rearranges' or stays the same is precisely what the guarantor and restrictive readings dispute.
% FOUNDING_PROBLEM: In 1923, the departing imperial powers and the new Turkish state needed a formula that let population-exchange-era minority communities remain under some form of external assurance without embedding a standing occupation or mandate — a diplomatic safety valve short of continued great-power tutelage.
% FOUNDING_PROBLEM_CORROBORATION: Guarantor states' own foreign ministries occasionally cite the mechanism when raising cases (e.g., seminary reopening disputes), attesting some residual live function; independent legal historians and Council of Europe rapporteurs, outside both the Turkish state and the minority community, describe the mechanism's practical force as having atrophied into occasional rhetorical invocation rather than a live enforcement channel.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the guarantor mechanism rarely converts into material transfer — it is a jurisdictional/procedural claim, not a substantive entitlement claim, and produces mostly reputational rather than material costs for Turkey. Suppression is moderate-low (0.28): Turkey does not need to actively suppress the mechanism because it lacks binding force; occasional diplomatic friction is absorbed rather than crushed. Theater ratio rises substantially over the interval (0.30 to 0.55) reflecting a mechanism increasingly invoked ceremonially — in EU accession talks, in periodic diplomatic statements — without a corresponding increase in binding effect, which is the classic signature of a scaffold whose transitional purpose (managing the post-imperial transition) has not been formally sunset but has drifted toward performative invocation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Ecumenical Patriarchate and the Greek Orthodox minority are structural beneficiaries in the sense that the guarantor reading, if taken seriously, would strengthen their negotiating position — but their exit options are trapped (their institutional life is physically fixed in Istanbul), so they cannot generate this leverage themselves; they are dependent beneficiaries. Guarantor states benefit from having a lever without cost, since their exit is mobile — they can raise or drop the issue without local consequence. Turkish sovereignty claimants are the payer seat: they bear diplomatic and reputational friction whenever the mechanism is invoked, even absent binding remedy, and their exit is constrained because domestic political audiences expect resistance to perceived foreign interference.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing minority protection in the vacuum of collapsed imperial tutelage — is genuinely contested as live or dead: guarantor states occasionally act as if it is still live (raising specific cases), while the demographic reality (a Greek Orthodox community reduced to a small fraction of its 1923 size) suggests the original problem has been overtaken by demographic attrition rather than resolved by protection. Classifying this as scaffold-with-drifting-theater rather than either mountain (natural/inevitable) or snare (pure extraction) captures that a genuine coordination function (an internationally legible channel for raising concerns) persists structurally even as its practical bite has eroded — this is not the same failure mode as a mechanism built purely to extract, nor is it inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guarantor_mechanism_dormant_or_live,
    'Is the guarantor state diplomatic mechanism a dormant treaty relic invoked only rhetorically, or does it retain genuine capacity to alter Turkish state behavior when actually mobilized?',
    'Trace historical instances of guarantor state intervention (e.g., Halki seminary, property restitution cases) and measure whether Turkish policy outcomes changed measurably following invocation versus cases where no invocation occurred.',
    'If genuinely live, the guarantor reading is better modeled as a weak but real rope/scaffold hybrid; if wholly dormant, the theater_ratio trajectory should be read as terminal and the constraint reclassified nearer piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_mechanism_dormant_or_live, empirical, 'Whether guarantor diplomacy produces measurable behavioral change or only rhetorical friction.').

omega_variable(
    jurisdictional_claim_vs_substantive_reading_independence,
    'Can the guarantor (jurisdictional/procedural) reading be coherently separated from the expansive/restrictive (substantive) readings, or does asserting international adjudicability necessarily import a particular substantive scope?',
    'Analyze ECtHR case law and guarantor-state diplomatic notes to see whether procedural invocations of Lausanne have in practice carried an implicit substantive theory (expansive or restrictive) or have remained genuinely scope-neutral.',
    'If the readings cannot be cleanly separated in practice, treating them as three independent constraints understates their coupling; if they are genuinely separable, the three-story decomposition per the kernel is the correct model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisdictional_claim_vs_substantive_reading_independence, conceptual, 'Whether the procedural (guarantor) axis is truly independent of the substantive (expansive/restrictive) axis of the same kernel.').

omega_variable(
    guarantor_state_motive_purity,
    'When guarantor states invoke the mechanism, is the motivation genuine solicitude for minority welfare, or primarily instrumental leverage in unrelated bilateral disputes (EU accession, migration, trade)?',
    'Correlate timing of guarantor invocations with concurrent unrelated diplomatic disputes between guarantor states and Turkey.',
    'If invocation timing correlates strongly with unrelated leverage needs, the mechanism functions substantially as diplomatic instrumentalization of the minority community rather than protection for its own sake, which would push the classification toward a mechanism whose real beneficiaries are guarantor state interests rather than the minority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_state_motive_purity, empirical, 'Whether guarantor invocation is minority-welfare-driven or opportunistically instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(laus_tr_t20, lausanne_minority_protections__guarantor_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(laus_tr_t40, lausanne_minority_protections__guarantor_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(laus_tr_t60, lausanne_minority_protections__guarantor_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(laus_tr_t80, lausanne_minority_protections__guarantor_reading, theater_ratio, 80, 0.53).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__guarantor_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(laus_be_t20, lausanne_minority_protections__guarantor_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(laus_be_t40, lausanne_minority_protections__guarantor_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(laus_be_t60, lausanne_minority_protections__guarantor_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(laus_be_t80, lausanne_minority_protections__guarantor_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__guarantor_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lausanne_minority_protections__guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'Lausanne minority protections' along the ε-invariance principle: the guarantor_reading addresses WHO adjudicates (procedural/jurisdictional locus), while expansive_reading and restrictive_reading address WHAT is substantively protected (institutional continuity vs. individual worship only). Each carries its own ε: this reading's low extractiveness (0.22) reflects the mechanism's weak binding force as a jurisdictional claim; the expansive reading is expected to show higher extraction (a stronger substantive claim contested by the Turkish state), and the restrictive reading is expected to show the lowest (a claim largely conceded by domestic law). All three are linked here per the network decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
