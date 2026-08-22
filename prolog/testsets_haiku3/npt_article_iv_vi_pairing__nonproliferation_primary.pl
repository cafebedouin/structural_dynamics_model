% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing (Nonproliferation-Primary Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The NPT Article IV/VI pairing, read through the nonproliferation-primary
 *   lens, creates a permanent two-tier nuclear order: weapon states retain
 *   arsenals and verify only non-weapon states; non-weapon states accept
 *   indefinite restraint in exchange for civilian technology and disarmament
 *   promises that are treated as aspirational rather than binding. This
 *   reading privileges horizontal-proliferation prevention (the founding
 *   security problem) over nuclear disarmament (the stated long-term goal of
 *   Article VI). The constraint is substantially extractive—non-weapon states
 *   bear verification costs and technology restrictions while weapon states
 *   extract the security benefit of a capped proliferation environment and
 *   retain strategic advantage. Authority derives not from treaty language
 *   symmetry but from the structural power of weapon states to define what
 *   verification means, which states are bound, and how non-justiciability of
 *   Article VI is compatible with Article III enforcement.
 *
 * KEY AGENTS:
 *   - Weapon states (USA, Russia, UK, France, China): define verification standards, set IAEA Board agenda, control what non-weapon states can access; maintain arsenals outside disarmament timeline.
 *   - Non-weapon states (majority of signatories, especially non-aligned movement): accept verification, accept technology restrictions, bear opportunity cost of restricted fuel-cycle development.
 *   - IAEA: enforces Article III verification; derives institutional mandate and resources from demonstrating non-weapon-state compliance; has no parallel authority over weapon-state arsenals.
 *   - Civil nuclear aspirants (India, Pakistan, Iran, Egypt, Saudi Arabia, Turkey): caught between energy/development needs and verification constraints; exit from treaty means sanctions and isolation.
 *   - Nuclear technology exporters (Russia, France, Germany, Japan, South Korea): benefit from Article IV legitimacy; their market access is enabled by the constraint.
 *   - Humanitarian disarmament advocates (ICAN, TPNW signatories): excluded from the treaty's authorization structure; contest this reading's core premise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation-Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, 'a430d9a0-347e-42c1-be64-511c844741ae').
narrative_ontology:cs_kernel_codification('a430d9a0-347e-42c1-be64-511c844741ae', formalized).
narrative_ontology:cs_authority_grounding('a430d9a0-347e-42c1-be64-511c844741ae', extraction).
narrative_ontology:cs_interpretation_layer_present('a430d9a0-347e-42c1-be64-511c844741ae').
narrative_ontology:cs_reading_relation('a430d9a0-347e-42c1-be64-511c844741ae', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('a430d9a0-347e-42c1-be64-511c844741ae', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('a430d9a0-347e-42c1-be64-511c844741ae', foundational, horizontal_proliferation_prevention_primary_function).
narrative_ontology:cs_axiom_status(horizontal_proliferation_prevention_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('a430d9a0-347e-42c1-be64-511c844741ae', horizontal_proliferation_prevention_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('a430d9a0-347e-42c1-be64-511c844741ae', foundational, article_vi_aspirational_non_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('a430d9a0-347e-42c1-be64-511c844741ae', article_vi_aspirational_non_justiciable, conventional).
narrative_ontology:cs_axiom('a430d9a0-347e-42c1-be64-511c844741ae', secondary, two_tier_order_structurally_rational).
narrative_ontology:cs_axiom_status(two_tier_order_structurally_rational, holdable).
narrative_ontology:cs_axiom_grounding('a430d9a0-347e-42c1-be64-511c844741ae', two_tier_order_structurally_rational, instrumental).
narrative_ontology:cs_reference_frame('a430d9a0-347e-42c1-be64-511c844741ae', rational_security_hierarchy).
narrative_ontology:cs_drift_state('a430d9a0-347e-42c1-be64-511c844741ae', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a430d9a0-347e-42c1-be64-511c844741ae', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, international_verification_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, civil_nuclear_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_technology_exporters).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, security_dilemma_structural_necessity).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, horizontal_proliferation_risk_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and ratified the NPT; maintain arsenals outside the disarmament timetable; set verification standards through their control of the IAEA Board of Governors and Security Council veto; define which dual-use technologies non-weapon states may access. Benefit from the constraint by excluding horizontal proliferation while retaining strategic advantage. Can exit by reneging or by redefining the treaty through reinterpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, beneficiary).

% Accept restrictions on nuclear fuel cycle autonomy and weapons research under Article III (full-scope IAEA safeguards); receive access to civilian nuclear technology and materials under Article IV. Face indefinite restraint while weapon states' disarmament obligations (Article VI) remain unmet and non-justiciable. Their benefit is conditional access; their cost is perpetual structural subordination in the nuclear order.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, beneficiary).

% States seeking nuclear power to meet energy demands or prestige requirements. Bound by non-weapon-state constraints; face restricted access to enrichment and reprocessing technology justified by proliferation risk; trapped between energy policy needs and treaty-mandated verification regimes. Their nuclear development is structurally dependent on weapon-state authorization through technology transfer agreements and IAEA licensing. Exit would mean national energy security strategies foreclosed or dependent on rival power suppliers.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, civil_nuclear_aspirants, payer,
    moderate, biographical, identity_locked, national).

% The IAEA, through inspections and safeguards verification, enforces Article III compliance. Derives institutional authority and resources from weapon-state demand for horizontal-proliferation assurance. Conducts continuous verification of non-weapon-state civil programs; has no parallel authority over weapon-state arsenals. Its survival and mandate renewal depend on demonstrating non-weapon-state compliance, creating structural asymmetry in what gets inspected.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, international_verification_regime, agenda_setter,
    institutional, generational, analytical, global).

% States with the technical capacity and strategic interest in acquiring nuclear weapons but constrained by the NPT and its verification mechanisms. Structurally excluded from the beneficiary set (weapon-state status is closed); their only routes are either treaty withdrawal (costly in sanctions and isolation) or clandestine programs (risking detection and military intervention). Would advocate for renegotiation of the Article IV/VI pairing if they had voice.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, aspiring_weapon_states, excluded,
    powerful, generational, trapped, national).

% Industries and states supplying civilian nuclear technology, fuel, and equipment to non-weapon states. Benefit from the constraint because Article IV legitimizes their market access (non-weapon states can legally acquire technology under safeguards) while Article III's verification regime certifies that sales won't be diverted to weapons programs, reducing their reputational and legal liability. Access extends globally but is legally bounded.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_technology_exporters, beneficiary,
    powerful, biographical, mobile, global).

% International NGOs, civil society, and some non-weapon-state governments advocating for complete nuclear disarmament and reframing proliferation risk as secondary to weapons-abolition imperatives. Excluded from the treaty's authorization structure (no seat in the IAEA Board, no NPT Review Conference veto). Would demand reinterpretation of Article IV as illegitimate if the current constraint framework were open to amendment.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, humanitarian_and_disarmament_advocates, excluded,
    moderate, biographical, constrained, global).

% Academic and policy analysts measuring horizontal-proliferation risk, treaty compliance, and the logic of the security dilemma. Provide interpretive authority for why the two-tier order (weapon-state exemption + non-weapon-state restraint) is structurally rational. Their framing vindicates the nonproliferation-primary reading by treating disarmament as aspirational rather than mandatory and by centering security-dilemma logic that justifies asymmetry.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, security_studies_epistemic_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents an expansion of nuclear-armed states (horizontal proliferation) by creating a binding non-weapon-state restraint conditional on verification (Article III) and linking it to weapon-state disarmament commitments (Article VI). Solves the collective-action problem where individual states seeking security might acquire weapons, raising the risk profile for all states.
% TRANSFER_FUNCTION: Transfers restraint obligation and technological subordination from weapon states (exempted from disarmament timeline) to non-weapon states (indefinitely bound by verification and restricted fuel-cycle access). Weapon states extract the benefit of a capped proliferation environment while avoiding the cost of their own mandated disarmament. Non-weapon states pay the cost of perpetual verification and technology restriction in exchange for legal civilian nuclear access.
% ABSENT_VOICES: Nuclear-aspiring states with the technical capacity and strategic rationale for acquiring weapons are excluded: they would contest the two-tier order and demand equal security rights under the treaty. Humanitarian disarmament advocates are excluded: they would argue Article VI is a binding disarmament mandate, not an aspirational goal, and that Article IV legitimizes an unjust dual-use proliferation regime. Small non-weapon states without nuclear programs have minimal voice: their interests in non-military security (deterrence, strategic autonomy) are subordinated to the weapon states' nonproliferation preference.
% DISAPPEARANCE_RATIONALE: If the NPT Article IV/VI pairing disappeared, non-weapon states would immediately pursue unrestricted fuel-cycle development (enrichment, reprocessing) and military research programs; weapon states would lose their primary mechanism for restraining horizontal proliferation and would face a multipolar nuclear world within a generation. Regional security competitions (Middle East, East Asia, South Asia) would accelerate toward weapons acquisition. The constraint's disappearance would collapse the post-1968 proliferation ceiling and restructure global power geometry.
% FOUNDING_PROBLEM: After the 1960s diffusion of nuclear technology and the emergence of non-weapon states with advanced technical capacity, the risk of unlimited horizontal proliferation threatened to make nuclear weapons available to dozens of state and non-state actors, destabilizing deterrence balances and increasing the likelihood of nuclear use. The NPT was negotiated to create a permanent, verifiable cap on the number of nuclear-armed states by trading non-weapon-state restraint (Article III) for disarmament promises by weapon states (Article VI) and civilian technology access (Article IV).
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and security-studies analysts attest the proliferation risk remains live and that the two-tier order has successfully capped the number of armed states for 55+ years. Non-weapon states and humanitarian advocates attest the founding problem has been partially displaced by the dual-use proliferation risk (civilian technology enabling covert weapons programs) and by the structural injustice of a permanent non-weapon-state subordination. The IAEA's compliance reports and the pattern of non-weapon-state violations (Iran, North Korea, South Africa) support the live-risk reading; the TPNW's ratification by 99 states since 2017 and the NPT Review Conference failures since 2015 attest the contested status.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics climb steadily across the interval, driven by several dynamics: (1) Extraction rises as Article VI unmet, converting it from negotiated commitment to declarative aspiration—by 2000, the weapon-state breach is undeniable, yet the constraint persists unchanged. (2) Theater rises as the IAEA's verification activity becomes performative: the regime demonstrates 'non-weapon-state compliance' while weapon arsenals grow; by 2015, the regime's theater function (certifying the system works) exceeds its actual constraint function on the main power-holders. (3) Suppression requirement rises as non-weapon-state violations (Iran, North Korea, Syria, Libya) accumulate—more active enforcement machinery is needed to hold the two-tier order in place. The plateau after 2010 reflects a steady state: the constraint is maintained by active suppression and by non-weapon-state cost internalization, but it no longer improves (the founding proliferation problem is capped but the extraction problem is permanent). Theater stays elevated because the constraint's persistence depends partly on theater—the NPT Review Conferences perform consensus on disarmament while disarming weapons-states do not occur.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat, the constraint is legitimate security architecture: it has prevented proliferation, it reflects rational security dilemmas, and Article VI is a long-term aspiration to be pursued only when conditions permit. From the non-weapon-state seat (especially civil nuclear aspirants), the constraint is coercive extraction: the verification burden is asymmetric, technology access is restricted, and Article VI's non-justiciability reveals that the disarmament obligation was always aspirational cover for a permanent two-tier hierarchy. The engine computes per-seat classification from the structural data: weapon states experience this as rope or coordination (bounded verification, mutual security interest); non-weapon states with constrained exit (identity-locked by security doctrine or development strategy) experience this as snare (perpetual restraint, suppressed exit). The perspectival divergence is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states hold high power, arbitrage-grade exit (they can withdraw, reinterpret, or ignore without facing the sanctions non-weapon states would face), and derive benefit from the constraint (capped proliferation, retained strategic advantage). Their directionality is d ≈ 0.15–0.25 (beneficiary end). Non-weapon states hold organized power but constrained exit (withdrawal means regional isolation, economic sanctions, inability to access civilian technology); they pay verification costs and technology restriction costs while receiving access conditional on compliance. Their directionality is d ≈ 0.75–0.85 (target end). Civil nuclear aspirants are even more target-end: identity-locked by development doctrine, they face maximum suppression (international scrutiny of dual-use research) and minimum exit (any weapons research triggers pre-emptive intervention). The overrides reflect weapon-state exit advantage and the asymmetric institutional access (IAEA Board, Security Council veto) that non-weapon states lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint escapes Mandatrophy classification because the founding problem (horizontal proliferation risk) is analytically distinct from the founding solution (disarmament commitment in Article VI). This reading subordinates the disarmament commitment to the proliferation-capping function: Article VI is treated as inspirational scaffolding for a permanent two-tier order, not as a binding mandate whose non-fulfillment delegitimizes the whole structure. The alternative reading (grand_bargain) would see the Article IV/VI pairing as causally linked: non-weapon-state restraint is conditional on weapon-state disarmament progress, and Article VI's non-fulfillment delegitimizes Article IV. Under that reading, the constraint would be Mandatrophy-resolved—the founding disarmament problem is dead (weapon arsenals grow or stagnate), yet the restraint persists, indicating the original trade has been breached. This reading avoids Mandatrophy by severing the causal linkage: Article IV stands on its own nonproliferation logic; Article VI is a separate long-term aspiration. The constraint is Tangled Rope (coordinates non-weapon-state verification while extracting their restraint) rather than a Snare (which would require the founding problem to still be live and the arrangement to be pure coercion). The Mandatrophy gap is real and is the site of the reading contest—the reading author chooses to treat the gap as structural design, not mandate failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI a binding disarmament obligation with an implied timeline, or an aspirational statement of principle whose non-fulfillment does not undermine the treaty''s other provisions?',
    'Formal legal interpretation by the International Court of Justice or a new NPT amendment conference explicitly stating Article VI''s legal status and enforceability. Alternatively, a systematic breach by all weapon states with formal acknowledgment would effectively resolve it as aspirational (de facto non-binding).',
    'If binding with an implied timeline, the constraint becomes Mandatrophy-resolved and the grand_bargain reading gains structural authority. If aspirational, the nonproliferation_primary reading is confirmed and the two-tier order is treated as intentionally permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'The binding status of Article VI determines whether the constraint is a conditional coordination (grand_bargain) or a stable two-tier extraction (nonproliferation_primary).').

omega_variable(
    verification_asymmetry_structural_necessity,
    'Is the asymmetry between Article III verification of non-weapon states and the absence of parallel verification of weapon-state arsenals a structural necessity (weapon states will not accept inspection) or a political choice that could be remedied by treaty amendment?',
    'Negotiation records of the NPT drafting and recent weapons-state statements on verification willingness. A credible weapon-state offer to accept mutual verification would resolve it as political choice; continued refusal despite non-weapon-state demands would confirm structural necessity.',
    'If structural necessity, the asymmetry is defensible as rational security architecture. If political choice, it indicates the constraint is not truly reciprocal coordination but rather imposed hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_structural_necessity, empirical, 'Whether verification asymmetry reflects irreducible security-dilemma logic or deliberate power asymmetry.').

omega_variable(
    dual_use_technology_risk_substitution,
    'As horizontal proliferation has been successfully capped (no new weapon states since 1968 using the NPT), has the constraint''s primary risk shifted from state acquisition to dual-use civilian technology enabling covert programs (Iran, North Korea models), and does this shift render the original justification for Article IV/III obsolete?',
    'Historical analysis of violation patterns (Libya''s AQ Khan network, Iran''s concealed enrichment, North Korea''s pre-disclosure programs) and assessments of whether the constraint''s verification machinery actually prevented any acquisition vs. merely prolonged discovery timelines.',
    'If the founding proliferation problem is solved and new risk is dual-use diversion, the constraint''s mandate may be obsolete (Mandatrophy candidate). The nonproliferation_primary reading''s authority would weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_technology_risk_substitution, empirical, 'Whether the founding proliferation problem remains the primary risk driver or has been supplanted.').

omega_variable(
    weapon_state_exit_symmetry,
    'Do weapon states and non-weapon states have symmetric exit options in practice? Can non-weapon states realistically withdraw, reinterpret, or ignore the constraint as weapon states can?',
    'Counterfactual: a non-weapon state announces withdrawal and full fuel-cycle development; does it face the same costs weapon states would face (diplomatic isolation, sanctions)? Historical precedent: North Korea and Iran provide partial answers; the asymmetric response suggests unequal exit.',
    'If exit is asymmetric (confirmed), the constraint is structurally coercive, not coordinative, from the non-weapon-state seat. The classification tilts toward snare, not tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weapon_state_exit_symmetry, empirical, 'Whether the constraint''s binding force is symmetrically distributed or concentrated on non-weapon states.').

omega_variable(
    reading_contest_axis_location,
    'Which structural element most sharply differentiates this reading (nonproliferation_primary) from its siblings? Is it the binding status of Article VI, the justification for asymmetric verification, the definition of the founding problem, or the temporal horizon of the disarmament obligation?',
    'Textual and genealogical analysis of the three readings'' premises; identifying which premise shift would convert one reading into another.',
    'Locating the axis guides where future empirical or conceptual resolution would matter most. If the axis is Article VI''s binding status, the ICJ opinion matters most. If it is the founding problem definition, historical/risk analysis matters most.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_axis_location, conceptual, 'The structural location of the reading contest in the constraint''s authorization architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1968, 0.18).
narrative_ontology:measurement_basis(npt__tr_t1968, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.24).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.32).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2018, 0.4).
narrative_ontology:measurement_basis(npt__tr_t2018, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement_basis(npt__be_t1968, observed).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(npt__be_t2018, observed).
narrative_ontology:measurement(npt__be_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1968, 0.52).
narrative_ontology:measurement_basis(npt__su_t1968, observed).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(npt__su_t2000, observed).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement_basis(npt__su_t2018, observed).
narrative_ontology:measurement(npt__su_t2026, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(npt__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.14).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_full_scope_safeguards).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, dual_use_technology_control_regimes).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_supplier_group_access_gates).

% DUAL FORMULATION NOTE:
% The NPT Article IV/VI pairing is a contested kernel with three structurally distinct readings. This file (nonproliferation_primary) treats the two-tier order as a permanent, rational security architecture where Article VI is aspirational and Article IV is binding. The grand_bargain reading treats them as reciprocal, with non-weapon-state restraint conditional on weapon-state disarmament progress. The abolitionist reading treats Article VI as a binding disarmament mandate and Article IV as illegitimate proliferation cover. The three readings share the treaty text but have different ε values (this reading: 0.68; grand_bargain would score higher on resistance/contestation; abolitionist would score extraction primarily as illegitimate); they are linked via network.affects_constraints and must be compared via the omega variables and cs_structure declarations, not collapsed into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, institutional, 0.2).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, organized, 0.78).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
