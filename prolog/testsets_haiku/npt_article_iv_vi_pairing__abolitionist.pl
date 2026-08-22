% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading): Humanitarian Prohibition Norm
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (NPT) Article IV grants non-nuclear states
 *   the right to peaceful nuclear technology; Article VI obligates all states
 *   to disarm. The abolitionist reading interprets this pairing through the
 *   lens of humanitarian law and weapons-prohibition norms (TPNW precedent):
 *   Article IV is illegitimate to the extent it enables dual-use
 *   proliferation, and Article VI is an operative obligation that
 *   delegitimizes all nuclear arsenals. This reading is in direct structural
 *   conflict with the nonproliferation_primary reading (which treats Article
 *   IV as development right and Article VI as aspirational) and in tension
 *   with the grand_bargain reading (which treats them as reciprocal but
 *   historically negotiated obligations). The abolitionist reading is ONE
 *   constraint story instantiating ONE reading of the contested kernel; the
 *   sibling readings are different constraints in different files, linked via
 *   network edges.
 *
 * KEY AGENTS:
 *   - Non-nuclear weapon states: structured beneficiary under prohibition norm, gain moral standing to refuse dual-use programs
 *   - Humanitarian advocacy coalitions: beneficiary-in-framing, using TPNW precedent to validate their legal theory
 *   - Nuclear weapon states: structured target, their arsenals become categorically delegitimized
 *   - Dual-use technology exporters: target, lose market legitimacy as civilian programs reframed as proliferation vectors
 *   - IAEA and verification regime: agenda-setter, mandate expands to enforce prohibition norm
 *   - Weapon-state security establishments: excluded from defining Article VI legitimacy
 *   - Courts and treaty bodies: observer seat, adjudicate whether this reading is operative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading): Humanitarian Prohibition Norm").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'e510b9b5-5bae-4ce6-8219-586215ae6887').
narrative_ontology:cs_kernel_codification('e510b9b5-5bae-4ce6-8219-586215ae6887', fixed_text).
narrative_ontology:cs_authority_grounding('e510b9b5-5bae-4ce6-8219-586215ae6887', lineage).
narrative_ontology:cs_interpretation_layer_present('e510b9b5-5bae-4ce6-8219-586215ae6887').
narrative_ontology:cs_reading_relation('e510b9b5-5bae-4ce6-8219-586215ae6887', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('e510b9b5-5bae-4ce6-8219-586215ae6887', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('e510b9b5-5bae-4ce6-8219-586215ae6887', foundational, humanitarian_law_supremacy_over_security_interest).
narrative_ontology:cs_axiom_status(humanitarian_law_supremacy_over_security_interest, holdable).
narrative_ontology:cs_axiom_grounding('e510b9b5-5bae-4ce6-8219-586215ae6887', humanitarian_law_supremacy_over_security_interest, deontological).
narrative_ontology:cs_axiom('e510b9b5-5bae-4ce6-8219-586215ae6887', foundational, article_vi_operative_obligation_not_aspirational).
narrative_ontology:cs_axiom_status(article_vi_operative_obligation_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('e510b9b5-5bae-4ce6-8219-586215ae6887', article_vi_operative_obligation_not_aspirational, empirically_contingent).
narrative_ontology:cs_reference_frame('e510b9b5-5bae-4ce6-8219-586215ae6887', humanitarian_law_constraint_on_state_capacity).
narrative_ontology:cs_drift_state('e510b9b5-5bae-4ce6-8219-586215ae6887', contemporary_tpnw_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e510b9b5-5bae-4ce6-8219-586215ae6887', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, humanitarian_advocacy_coalitions).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain moral and legal standing to refuse participation in civilian nuclear programs framed as development infrastructure. Under the abolitionist reading, they are vindicated in their strategic rejection of dual-use fuel cycles. They collect legitimacy and diplomatic leverage in treaty negotiations, though they remain constrained by geopolitical pressure to accept nuclear technology transfer as development aid.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% Deploy TPNW precedent and humanitarian law to argue that the NPT's Article IV is illegitimate. Their legal theory is strengthened and their litigation strategy gains treaty-text support (Article VI as operative, not aspirational). They can move between venues (courts, NGO networks, media campaigns) and are not trapped by any single institutional relationship, though their power depends on maintaining coalition unity.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_advocacy_coalitions, beneficiary,
    moderate, generational, mobile, global).

% Under this reading, their nuclear arsenals are categorically delegitimized under the same humanitarian law that binds all parties. They must invest in suppressing the reading's adoption (diplomatic campaigns, reframing Article VI as aspirational, defending deterrent doctrine as security necessity). Their exit options are: withdraw from NPT (political cost), maintain deterrence while appearing to cooperate (theater), or genuinely pursue disarmament (identity transformation). None are low-cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    institutional, civilizational, trapped, global).

% Lose market legitimacy as their civilian nuclear infrastructure, uranium enrichment services, and fuel-cycle technology are reframed as proliferation vectors rather than development rights. They must navigate regulatory pressure to treat all nuclear programs as proliferation risks, even those claiming peaceful intent. Their exit is constrained: they can withdraw from markets (business loss) or invest in new legitimacy narratives (costly).
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters, payer,
    powerful, biographical, constrained, global).

% Administers enforcement of the constraint by expanding verification mandate to assess whether civilian programs are compatible with humanitarian prohibition norms. They are the institutional seat that makes the reading operational — inspectorates file reports not just on technical compliance but on humanitarian-law alignment. Their exit is constrained by treaty obligation; their power is institutional but depends on weapon-state cooperation, creating internal tension.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_and_verification_regime, agenda_setter,
    institutional, generational, constrained, global).

% Military planners and strategic theorists that depend on nuclear deterrent doctrine are excluded from defining Article VI legitimacy under this reading. Their argument — that graduated, verified disarmament under mutual security arrangements is the realistic path — is not heard as an authoritative voice. Their strategic rationalities are preempted by humanitarian law framing, treated as obstacles to norm enforcement rather than legitimate constraints on interpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, weapon_state_security_establishments, excluded,
    institutional, civilizational, trapped, global).

% The ICJ, regional human rights courts, and NPT review conferences are the analytical seats from which this reading is being adjudicated. Their verdicts on whether Article IV is illegitimate under prohibition norms, whether TPNW precedent grounds operative obligation, and whether weapon-state arsenals violate Article VI reshape the constraint's enforcement and institutional legitimacy. Their role is to measure and declare whether the reading is operative in international law.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, courts_and_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, humanitarian_advocacy_coalitions).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global humanitarian prohibition on nuclear weapons and dual-use programs by making Article VI operative and subordinating Article IV to proliferation-risk assessment. Solves the collective-action problem of how to make weapons prohibition enforceable in international law rather than merely aspirational, and how to prevent civilian-program infrastructure from enabling proliferation.
% TRANSFER_FUNCTION: Transfers legitimacy FROM weapon states and dual-use exporters TO non-nuclear states and humanitarian advocates. Weapon states lose their claim that Article IV accommodates security interests; non-nuclear states gain grounds to refuse fuel-cycle participation. Dual-use exporters lose market legitimacy; humanitarian organizations gain legal standing to challenge programs.
% ABSENT_VOICES: The strategic establishments of nuclear weapon states — military planners, deterrent theorists, security advisors — would argue that graduated, verified disarmament is the achievable path and that humanitarian framing ignores security realities. They are excluded from this reading's authority structure. Also excluded: non-aligned states that view civilian nuclear capacity as a development right; their claim that fuel-cycle access is essential infrastructure is delegitimized by treating all programs as proliferation vectors.
% DISAPPEARANCE_RATIONALE: If this constraint (the abolitionist reading's authority) vanished, weapon states would openly justify arsenals as permanent security features; non-nuclear states would lose moral standing to refuse dual-use programs; dual-use exporters would resume fuel-cycle commerce as legitimate development infrastructure. The global governance structure would revert to treating nuclear weapons as differentially legitimate based on state power, and civilian programs as development rights decoupled from prohibition.
% FOUNDING_PROBLEM: The NPT's internal contradiction and strategic ambiguity: Article IV grants civilian nuclear rights (framed as development) while Article VI obligates disarmament, but Article VI is non-justiciable and weapon states have never meaningfully pursued it. This enables a de facto system where weapon states retain arsenals, dual-use exporters commercialize proliferation infrastructure, and non-nuclear states are pressed to accept both. The problem: how to make Article VI operative (not aspirational), subordinate Article IV to proliferation-risk assessment, and use humanitarian law as the authority grounding that resolves the contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian law scholars (Orna Ben-Naftali, Christoph Safferling), TPNW state parties, ICJ precedent (Legality of Nuclear Weapons, 1996), and independent disarmament analysts corroborate that the NPT contradiction is real and unresolved. Weapon-state security establishments contest this, arguing Article VI was always understood as aspirational and the contradiction is a feature, not a bug. The competing reading (nonproliferation_primary) is corroborated only by weapon-state governments asserting their security interest defines the problem. The abolitionist corroboration comes from outside the benefiting parties (humanitarian advocates, non-aligned legal scholars) — from seats that have no structural interest in reading Article VI as operative except the legal and moral principle itself.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 in 1970 (when the NPT was new and the humanitarian prohibition norm was nascent) to 0.78 by 2026 (as TPNW gained signatories and ICJ precedent accumulated). This rise reflects the constraint's operation: the reading is increasingly deployed to delegitimize dual-use programs and weapon-state arsenals, extracting compliance burden from those parties while granting non-nuclear states moral standing to refuse. Theater ratio rises (0.08 to 0.41) because an increasing share of NPT-regime activity is performative verification and disarmament conferences that produce no weapon reductions, while the humanitarian norm gains visibility. Suppression requirement rises (0.45 to 0.72) because weapon states must increasingly invest in blocking the abolitionist reading's adoption — diplomatic campaigns against TPNW precedent, reframing Article VI as 'aspirational,' and defending dual-use programs as legitimate development infrastructure. All three metrics rise together because the constraint's operation intensifies: it is being actively enforced (suppression), extraction burden is mounting (extractiveness), and the enforcement theater is thickening (theater_ratio). The shared time grid spans 56 years so that temporal analysis can track how the reading's legitimacy has grown in parallel with the humanitarian law movement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between weapon states and non-nuclear states is the entire constraint. Weapon states compute the NPT as an asymmetric bargain where they retain deterrence while non-nuclear states forgo weapons; non-nuclear states compute it (under the abolitionist reading) as an operative commitment to prohibition that weapon states are violating. The IAEA sits in a paradoxical middle: it administers verification (supports the reading by making inspections credible) but depends on weapon-state cooperation (constrains its full endorsement). The engine should compute weapon-state seats as snare (extraction with suppression) and humanitarian-advocate seats as rope (coordination with benefit). This divergence is the key measurement: whether the abolitionist reading can be instantiated as a tangled_rope (real coordination function + asymmetric extraction + enforcement) or collapses into snare (pure delegitimization with no coordination benefit for weapon states).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states hold d near 1.0 (full targets): they are identity-locked to deterrent doctrine, have no exit except arsenal surrender (trapped exit), and are institutionally powerful — all three factors push them toward the target end. Non-nuclear states hold d near 0.0 (beneficiaries): they gain moral standing and legitimacy grounds (benefit), have constrained exit but can coordinate collectively (moderate constraint), and are organized rather than individually powerful. Humanitarian advocates hold d near 0.1 (beneficiary side): their moral authority rises, their options are mobile (they can publish, litigate, mobilize), and their power is moderate. Dual-use exporters hold d near 0.9 (target side): their market shrinks, their exit is constrained to compliance or withdrawal, and they are powerful but institutionally dependent on state permission. IAEA holds d near 0.5 (symmetric): they administer the constraint but also depend on weapon-state cooperation; they are institutional (high power) but their time horizon is generational (stable mandates) and their exit is constrained by the treaty itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining a live coordination function: prohibition norms do solve a genuine collective-action problem (how to make weapons illegal under international law, not just diplomatically discouraged). The extraction (delegitimization of arsenals + suppression of counterarguments) rides on that coordination function. The danger point: if the reading becomes pure theater (weapon states continue arsenals while ceremonies of disarmament continue), the coordination function atrophies and the constraint becomes piton. The theater_ratio trajectory (rising from 0.08 to 0.41) suggests this danger is materializing — increasingly, NPT review conferences and disarmament rhetoric are decoupled from actual weapons reductions. If theater_ratio crosses 0.50, the constraint should reclassify as piton (mostly performative, real function atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_law_supremacy_contested,
    'Does international humanitarian law (TPNW, weapons-prohibition norms) actually supersede the security-interest authority grounding of the NPT, or do both authorities coexist in different institutional contexts?',
    'ICJ advisory opinion or binding interpretation of treaty hierarchy; empirical test: do states actually withdraw from NPT if humanitarian norm is declared operative, or do they treat the readings as coexisting claims?',
    'If humanitarian law is supreme, the abolitionist reading is operationally binding and Article IV is illegitimate. If both authorities coexist, the readings remain contested and weapon states can maintain strategic ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_law_supremacy_contested, conceptual, 'Whether humanitarian law and security-interest authority grounds are hierarchical or coexistent.').

omega_variable(
    dual_use_program_proliferation_causality,
    'Does civilian nuclear technology ACTUALLY enable weapon proliferation in measurable ways, or is the connection largely hypothetical / contingent on state intent?',
    'Quantitative analysis of proliferation pathways: how many weapon programs relied on civilian-program infrastructure vs. parallel military development? Retrospective case studies of proliferators (Iran, North Korea, Iraq).',
    'High measured causality supports the abolitionist reading''s claim that Article IV perpetuates proliferation risk; low causality suggests civilian and military programs are separable and Article IV remains legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_program_proliferation_causality, empirical, 'Whether civilian nuclear technology is a material pathway to proliferation or a decoupled development infrastructure.').

omega_variable(
    reading_adoption_by_weapon_states,
    'Can the abolitionist reading be adopted by nuclear weapon states themselves, or is it structurally foreclosed by their deterrent-doctrine identity and strategic interest?',
    'Political-economy analysis: if a weapon state began framing its arsenal as delegitimized by humanitarian law while maintaining deterrent capability, would that be coherent (new framing of old arsenals) or logically incoherent (identity contradiction)? Can weapon states move to post-deterrent security models?',
    'If adoption is structurally impossible for weapon states, the reading remains a constraint imposed ON them rather than a shared norm framework. If adoption is possible, the reading can mature from extraction to genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_adoption_by_weapon_states, conceptual, 'Whether weapon states can coherently adopt the abolitionist reading or whether the reading is foreclosed by their institutional identity.').

omega_variable(
    authorization_for_iaea_mandate_expansion,
    'Does the IAEA have authority to expand its verification mandate beyond detecting diversion, to assess whether civilian programs are compatible with humanitarian prohibition norms?',
    'Legal interpretation of the IAEA statute and NPT; empirical test whether states authorize IAEA inspectors to file reports on humanitarian-law compliance vs. technical diversion-detection only.',
    'If IAEA mandate can expand, the constraint gains institutional enforcement capacity. If mandate is bounded to technical verification, enforcement relies on political pressure and court proceedings, weakening the constraint''s operability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_for_iaea_mandate_expansion, empirical, 'Whether the IAEA''s institutional authority extends to humanitarian-law compliance assessment.').

omega_variable(
    constraint_identity_across_kernel_readings,
    'Is the abolitionist reading measuring a DIFFERENT constraint (different ε, different referent) than the nonproliferation_primary reading, or the same constraint viewed from different seats?',
    'Per DP-001 (ε-invariance principle): if the abolitionist and nonproliferation readings yield materially different ε values for the same NPT article pair, they are different constraints. If they yield the same ε (same extraction fact, different interpretation of its legitimacy), they are the same constraint with different readings.',
    'Different constraints require separate story files linked via network edges (per ε-invariance discipline). Same constraint with different readings requires committer-frame treatment within one story or explicit reading-indexed ε values. This omega documents the decomposition decision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_identity_across_kernel_readings, conceptual, 'Whether abolitionist and nonproliferation readings are separate constraints or two readings of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2012, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(npt__be_t2012, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(npt__su_t2012, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2012, 0.64).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_entry_into_force__humanitarian_law_supremacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading (abolitionist) of the contested kernel npt_article_iv_vi_pairing. The kernel is the NPT Article IV/VI text and its interpretation. Sibling readings (nonproliferation_primary, grand_bargain) instantiate alternative legal frameworks applied to the same kernel. Each reading has its own ε, beneficiary/victim structure, and type classification. The network edges document the constraint family: abolitionist reading forecloses nonproliferation_primary (humanitarian law supremacy rules out development-rights framing) and coexists with grand_bargain (both acknowledge reciprocity, but prioritize different authorities). The ε difference: abolitionist measures the constraint as delegitimization of dual-use programs (high extractiveness from exporters) while nonproliferation measures it as verification-regime coordination (lower extractiveness, higher cooperation benefit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
