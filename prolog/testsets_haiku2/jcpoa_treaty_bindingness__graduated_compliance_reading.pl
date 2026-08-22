% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Constraint (Scaled Reciprocal Reading)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action, 2015) is a treaty
 *   establishing Iran's nuclear commitments in exchange for sanctions relief.
 *   This constraint story instantiates the GRADUATED-COMPLIANCE READING:
 *   JCPOA as a scaled reciprocal commitment where violations trigger
 *   proportional (not absolute) enforcement responses, and compliance
 *   assessment is continuous and technically mediated by the IAEA. This
 *   reading prioritizes de-escalation and sustained engagement over binary
 *   (compliance/non-compliance) determinations. It is one of three readings
 *   of the contested kernel 'jcpoa_treaty_bindingness': sibling readings
 *   include the binding-multilateral reading (JCPOA as formally binding on
 *   all signatories, voidable only through consensus) and the
 *   transactional-provisional reading (JCPOA as a deal voidable by unilateral
 *   determination of bad faith). The graduated reading inhabits a middle
 *   zone: reciprocal obligations with proportional enforcement, not absolute
 *   binding, and not purely transactional. The story's claim (tangled_rope)
 *   reflects this middle character: genuine coordination (Iran's compliance
 *   caps are real constraints; sanctions relief is real benefit) paired with
 *   asymmetric extraction (signatories retain enforcement discretion; Iran is
 *   monitored continuously). The measurement series shows extraction rising
 *   modestly over 10 years as enforcement disputes accumulated, theater
 *   rising as political pressures made compliance assessments contestable,
 *   and suppression remaining moderate because the constraint depends on
 *   Iran's voluntary participation in inspections, not coercive force.
 *
 * KEY AGENTS:
 *   - Pragmatic diplomacy advocates: benefit from maintained engagement channels and graduated conflict de-escalation
 *   - International trade actors (multinational firms, banking sector): benefit from incremental sanctions relief and market predictability
 *   - Iran's nuclear development program: constrained by enrichment caps and IAEA verification, but with conditional sanctions relief
 *   - Strict enforcement advocates (Israeli, U.S. maximalist security establishment): pay the cost of proportional rather than absolute enforcement; excluded from core decision structure
 *   - Unilateralist state actors (Trump administration example): pay the cost of multilateral dispute resolution rather than unilateral withdrawal freedom
 *   - IAEA verification body: agenda-setter role, administers compliance assessment that scales enforcement
 *   - P5+1 signatories: jointly set compliance thresholds and proportionality formulas; no single actor can unilaterally terminate
 *   - Regional trade-dependent economies (Iraq, Syria, Lebanon): trapped beneficiaries of partial sanctions relief; no governance voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.31).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Constraint (Scaled Reciprocal Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '1bd106a1-3b48-443b-ab46-cfd935a479e1').
narrative_ontology:cs_kernel_codification('1bd106a1-3b48-443b-ab46-cfd935a479e1', fixed_text).
narrative_ontology:cs_authority_grounding('1bd106a1-3b48-443b-ab46-cfd935a479e1', lineage).
narrative_ontology:cs_interpretation_layer_present('1bd106a1-3b48-443b-ab46-cfd935a479e1').
narrative_ontology:cs_reading_relation('1bd106a1-3b48-443b-ab46-cfd935a479e1', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bd106a1-3b48-443b-ab46-cfd935a479e1', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('1bd106a1-3b48-443b-ab46-cfd935a479e1', foundational, scaled_reciprocity_bindingness).
narrative_ontology:cs_axiom_status(scaled_reciprocity_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('1bd106a1-3b48-443b-ab46-cfd935a479e1', scaled_reciprocity_bindingness, deontological).
narrative_ontology:cs_axiom('1bd106a1-3b48-443b-ab46-cfd935a479e1', foundational, technical_compliance_mediation).
narrative_ontology:cs_axiom_status(technical_compliance_mediation, holdable).
narrative_ontology:cs_axiom_grounding('1bd106a1-3b48-443b-ab46-cfd935a479e1', technical_compliance_mediation, instrumental).
narrative_ontology:cs_axiom('1bd106a1-3b48-443b-ab46-cfd935a479e1', secondary, de_escalation_priority).
narrative_ontology:cs_axiom_status(de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('1bd106a1-3b48-443b-ab46-cfd935a479e1', de_escalation_priority, conventional).
narrative_ontology:cs_reference_frame('1bd106a1-3b48-443b-ab46-cfd935a479e1', reciprocal_scaled_obligation_framework).
narrative_ontology:cs_drift_state('1bd106a1-3b48-443b-ab46-cfd935a479e1', post_2018_u_s_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1bd106a1-3b48-443b-ab46-cfd935a479e1', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, international_trade_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_dependent_economies).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_development_constrained).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, strict_enforcement_advocates).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, unilateralist_state_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International negotiators, non-governmental advocacy groups, and states committed to preserving multilateral engagement frameworks. They benefit from the graduated-compliance reading because it prioritizes de-escalation pathways and preserves the possibility of corrective negotiation when violations occur. Their situation is constrained by the requirement that they maintain broad coalition support and cannot unilaterally withdraw from the framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, generational, constrained, global).

% Multinational corporations, banking sectors, and investment firms that benefit from partial sanctions relief and the gradual normalization of economic relations with Iran. Under the graduated reading, sanctions are withdrawn in incremental tranches tied to compliance milestones, creating predictable cycles of market access. Their exit is constrained by regulatory environments but they maintain pricing flexibility and can shift geographic exposure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, international_trade_actors, beneficiary,
    institutional, biographical, mobile, global).

% Iran's nuclear program and hydrocarbon economy operate under graduated caps and inspections tied to compliance tiers. The reading imposes constraints: uranium enrichment ceilings that scale down, IAEA inspection protocols, and contingent sanctions relief. Violation triggers proportional (not total) sanctions re-imposition. Iran's exit options are constrained by the cost of complete isolation versus the burden of compliance monitoring.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_development_constrained, payer,
    powerful, generational, constrained, global).

% States and security communities (notably elements within the Israeli and U.S. government) who view any Iranian nuclear capability as unacceptable and advocate for maximalist enforcement provisions, snap-back sanctions, and no-tolerance violation policies. Under the graduated reading, their preferred zero-tolerance stance is explicitly traded away for de-escalation prioritization. They are excluded from the constraint's core decision structure because their participation would collapse the moderate-compliance framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, strict_enforcement_advocates, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, strict_enforcement_advocates, excluded).

% State actors (e.g., the Trump-era U.S. administration) who view the JCPOA as a bilateral bargain with Iran, not a multilateral commitment, and claim unilateral withdrawal rights. The graduated reading denies this framing by establishing joint signatory proportionality obligations. These actors pay the cost of being bound to multilateral dispute resolution rather than free to act unilaterally; their exit is technically available (treaty withdrawal) but politically costly.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, unilateralist_state_actors, payer,
    institutional, biographical, mobile, global).

% The International Atomic Energy Agency administers compliance verification and generates the technical assessments that trigger proportional sanctions relief/re-imposition cycles. Under this reading, the IAEA occupies a neutral arbiter role: their reports measure compliance severity, which then scales the enforcement response. They carry the burden of maintaining credibility and face pressure from all signatory parties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_body, agenda_setter,
    institutional, generational, constrained, global).

% The permanent Security Council members plus Germany and Iran jointly set the compliance thresholds, dispute-resolution procedures, and proportionality formulas. They administer the graduated response regime: they vote on whether a violation is material enough to trigger partial sanctions re-imposition, and at what scale. Their constraint is mutual: no single signatory can unilaterally close the enforcement loop.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_one_signatories, agenda_setter,
    institutional, generational, constrained, global).

% Iraqi, Syrian, Lebanese, and other regional economies dependent on Iranian energy supplies and trade relationships. They benefit from the graduated reading because partial sanctions relief creates incrementally more trade opportunity. Their exit options are trapped because geographic/geopolitical realities make Iranian relations functionally unavoidable, and they cannot participate in JCPOA governance.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_dependent_economies, beneficiary,
    moderate, biographical, trapped, regional).

% The 2015 pre-deal sanctions architecture (UN Security Council and multilateral secondary sanctions). Under the binding multilateral reading, this coalition retains the power to re-impose all sanctions via 'snapback' vote if Iran violates terminal terms. Under the graduated reading, snapback is replaced by proportional sanctions adjustments, making the pre-existing enforcement coalition structurally less relevant. They are excluded from the constraint's core mechanism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, snap_back_enforcement_coalition, excluded,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, international_trade_actors).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mutual commitment framework for Iran to constrain its nuclear program in exchange for scaled sanctions relief, with graduated enforcement tied to the severity of compliance infractions. The coordination problem solved: how to verify a rival's compliance trajectory while maintaining incentive structure for continued engagement despite verification uncertainties and violation pressures.
% TRANSFER_FUNCTION: Moves progressive sanctions relief (targeted sectors: banking, shipping, oil) from the signatories to Iran, scaled to the IAEA's compliance assessment. Simultaneously moves constraints on Iran's enrichment capacity, inspection access, and weaponizable material stockpiles. The transfer reverses proportionally if violations are detected; total de-escalation is replaced with graduated re-calibration.
% ABSENT_VOICES: Unilateralist political coalitions (Netanyahu administration, Trump-era maximalist hawks, Iranian hardline constituencies opposed to any verification) are structurally excluded because their participation would dissolve the moderate-reciprocity frame. Regional economic actors (Iraq, Syria) would benefit from or suffer effects but have no voice in governance. Civil society actors in Iran and signatory states have limited participation mechanisms.
% DISAPPEARANCE_RATIONALE: If the graduated-compliance constraint disappeared, Iran's nuclear program would likely advance toward weapons capability without international monitoring; regional security competitions would intensify; international trade and banking relationships with Iran would revert to pre-2015 isolation or unilateral state-level bargains instead of coordinated frameworks. The graduated mechanism's absence would force binary choices: either maximum sanctions pressure (pre-2015 state) or full normalization without verification (no constraint case).
% FOUNDING_PROBLEM: Iran's nuclear program development trajectory raised regional security concerns and triggered expanding international sanctions. The founding problem was how to halt weapons-pathway progress while providing Iran with economic incentive to comply and signatories with verification confidence that compliance was real.
% FOUNDING_PROBLEM_CORROBORATION: Pragmatic diplomacy advocates and most international trade communities attest the founding problem was adequately solved under the graduated reading: Iran's enrichment was capped, inspections expanded, weapons-materials reduced. Strict-enforcement advocates and unilateralist actors contest this: they attest the founding problem remains unsolved because Iran retains civilian enrichment capability and therefore retains a pathway to weapons if it chooses. The IAEA's technical testimony supports the 'constraint achieved' reading; regional security establishments remain divided.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.48 (moderate): the graduated reading establishes real constraints on Iran (enrichment caps, verification, stockpile limits) but does not impose maximum suppression because Iran retains some enrichment capacity and the constraint depends on Iran's willing participation in inspections. Suppression measures 0.31 (low-moderate): the constraint's enforcement is technical (IAEA reports) and diplomatic (proportional sanctions adjustments), not coercive or military. There is no external force preventing Iran from exiting; exit is costly but not impossible. Theater measures 0.42 (moderate-high): compliance assessments are technically mediated but politically contested. Both signatories and Iran have incentive to dispute borderline violations, creating cycles of accusation and counter-accusation that sometimes are real compliance concerns and sometimes are political theater. Accessibility-collapse at 0.52 reflects the moderate-difficulty of alternatives: Iran could exit by openly rejecting inspections (costly but possible); signatories could exit via unilateral withdrawal (violating the graduated reading's own premise but technically available). Resistance at 0.68 reflects substantial push-back: strict enforcers contest the graduated frame, unilateralists deny its bindingness, and Iranian hardliners reject verification as a trojan horse for intelligence gathering. The measurements show extractiveness rising initially (years 0–10) as disputes accumulated, then stabilizing (years 10–25) as participants internalized the graduated-compliance framework. Theater rises through the interval as political contests over borderline violations intensified. Suppression remains low because the constraint never developed into coercive enforcement; it remained diplomatic throughout.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (P5+1 signatories, IAEA) perceives the constraint as successful coordination: Iran was brought into a verification regime, enrichment was capped, weapons-pathway materials were reduced. They measure compliance against graduated thresholds and execute proportional responses. The payer seat (Iran) perceives graduated extraction: enrichment caps limit development pathway; verification intrudes on sovereign industrial process; sanctions relief is contingent and reversible. They experience the constraint as a ceiling they operate under, not a coordination problem solved. The strict-enforcement seat perceives the constraint as inadequate: Iran retains enrichment capability and therefore retains a weapons pathway; any enrichment is unacceptable. They experience the graduated reading as a failure of enforcement. These divergent perceptions derive from the structural asymmetry: signatories retain enforcement discretion and verification control; Iran is monitored and constrained; strict enforcers are excluded from governance. The engine computes this divergence per-seat from the structural data. This story does not adjudicate which perception is correct; it establishes the structural condition from which the divergence arises.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the pragmatic-diplomacy advocates is near beneficiary (d ~ 0.15): they collect benefit (sustained engagement channels, negotiation space) without bearing the technical burden of verification or compliance monitoring. Their time horizon is generational and their exit is constrained by coalition politics. Directionality for international trade actors is near beneficiary (d ~ 0.10): they benefit directly from incremental sanctions relief; their time horizon is biographical but their exit options are mobile (they can shift to other markets). Directionality for Iran is substantially higher (d ~ 0.72): Iran bears the core constraint (enrichment caps, verification intrusion, contingent sanctions); its time horizon is generational and exit is constrained by the cost of isolation. Directionality for strict enforcement advocates is near target (d ~ 0.85): they are excluded from governance and forced to accept proportional rather than absolute enforcement; their power is institutional but their exit is constrained by the need to maintain coalition cohesion. Directionality for unilateralist actors is moderate-to-high (d ~ 0.68): they are denied unilateral withdrawal freedom and must operate within multilateral dispute resolution; their power is institutional and exit is mobile (they can technically withdraw but at high political cost). The IAEA sits near symmetric (d ~ 0.50): they bear the burden of maintaining credibility under fire from all parties, but they also exercise substantial agenda-setting power over compliance thresholds. These values are derived from the stakeholder structural declarations (beneficiary/payer roles, power atoms, exit_options); they feed the engine's directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The JCPOA's founding problem — how to halt Iran's weapons-pathway nuclear development while providing economic incentive and verification confidence — was structurally relevant (mid-2010s) when proliferation trajectory and regional security seemed genuinely live concerns. Under the graduated reading, the foundational obligation (Iran's enrichment caps, signatories' verification access) remains engaged continuously. The potential mandatrophy signal: if Iran's enrichment restraint or signatories' commitment to proportional enforcement decays over time while the constraint persists as theater (compliance reports issued, sanctions adjustments made, but neither party genuinely responds to the other's violations), that would indicate mandatrophy — the founding problem solved or superseded, but the constraint maintained for institutional inertia or political cover. The measurement series show theater rising (0.25→0.42 over 25 years), which is consistent with mandatrophy drift. However, the founding problem status is marked 'contested', not 'dead': pragmatic diplomats still claim the founding problem is live (regional nuclear competition, verification uncertainty), while strict enforcers claim it is dead (Iran's restraint is not credible; proliferation pathway is merely delayed). Under the graduated reading, mandatrophy is averted as long as both signatories and Iran retain genuine incentive to adjust behavior proportional to compliance assessments. If that incentive structure breaks (Iran ignores proportional sanctions, signatories ignore proportional relief), the constraint becomes zombie — persisting without substantive function. The commentary flags this: watch for divergence between proportional sanctions re-impositions and actual Iranian enrichment increases. If re-impositions become routine theater (announced but not enforced) while enrichment accelerates unchecked, that signals mandatrophy transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_mechanism_contested,
    'What constitutes ''proportional'' sanctions re-imposition in response to graduated enrichment violations? Is the proportionality formula transparent, technically determined, or politically negotiated?',
    'Examine P5+1 joint commission records and IAEA reports: do signatories justify sanctions adjustments via explicit quantitative thresholds (technical proportionality) or via political negotiation (strategic proportionality)?',
    'If proportionality is technical and transparent, the graduated constraint is operationally sustainable. If proportionality is political, the constraint reverts toward binary (enforce or don''t) under diplomatic pressure, collapsing into the binding-multilateral or transactional readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_mechanism_contested, conceptual, 'Whether graduated proportionality operates via transparent technical thresholds or political negotiation, determining constraint stability.').

omega_variable(
    iran_compliance_verification_credibility,
    'Does Iran actually respond to proportional sanctions adjustments by adjusting enrichment? Or does enrichment follow independent technical/domestic-political drivers regardless of sanctions signals?',
    'Correlate IAEA enrichment reports with sanctions relief timelines: if enrichment caps respond to sanctions cycles, the feedback loop is real; if enrichment follows independent trajectory, the constraint is one-directional (signatories adjust, Iran does not).',
    'A responsive feedback loop sustains the graduated constraint as genuine reciprocal obligation. No correlation would indicate Iran treats the constraint as exogenous pressure, not as reciprocal coordination, shifting the effective reading toward snare (Iran constrained, signatories extract relief).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iran_compliance_verification_credibility, empirical, 'Whether Iran''s enrichment decisions respond to proportional sanctions incentives.').

omega_variable(
    reading_forestalls_unilateral_exit,
    'Does the graduated-compliance reading actually foreclose unilateral withdrawal, or does it merely make withdrawal politically costly while technically available?',
    'Examine U.S. 2018 withdrawal: if the reading is foreclosing, other signatories should have had enforcement recourse (they did not). If the reading merely makes exit costly, observe whether other signatories maintained the constraint despite U.S. non-participation.',
    'If foreclosed: the graduated reading''s bindingness claim holds — unilateral exit is structurally impossible. If merely costly: unilateralist actors retain de facto exit, making the reading aspiration rather than constraint. This determines whether the reading coexists with the transactional reading (both available) or forecloses it (graduated forecloses transactional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_forestalls_unilateral_exit, empirical, 'Whether the graduated reading structurally forecloses unilateral withdrawal or merely makes it politically costly.').

omega_variable(
    sibling_reading_framing_ambiguity,
    'Is the choice between graduated, binding-multilateral, and transactional readings a real structural difference in how the JCPOA constraint operates, or a difference in how legal arguments are framed while the material constraint remains the same?',
    'Test via counterfactual: if all three readings are in force simultaneously (different signatories adopt different readings), do the material outcomes (Iran''s enrichment, sanctions levels, verification access) differ? Or do all three readings produce identical compliance dynamics?',
    'If material outcomes differ, the readings are real distinct constraints (different ε values, different beneficiary/victim structures). If outcomes are identical, the readings are framing disputes layered on one constraint, and the constraint-story should be merged with omegas capturing the framing ambiguity. This is the ε-invariance test for kernel readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_ambiguity, conceptual, 'Whether the three JCPOA readings represent distinct constraints or frame-disputes over one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(jcpo_tr_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement(jcpo_tr_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(jcpo_tr_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(jcpo_be_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(jcpo_be_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(jcpo_be_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(jcpo_su_t15, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(jcpo_su_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 20, 0.31).
narrative_ontology:measurement(jcpo_su_t25, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 25, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_regime__technical_authority_constraint).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_one_governance__joint_decision_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the JCPOA-bindingness kernel. The graduated-compliance reading is distinguished from binding-multilateral (which emphasizes absolute bindingness and consensus modification) and transactional-provisional (which emphasizes unilateral voidability). All three share the same referent (the JCPOA text and its legitimacy structure) but differ in how they classify obligation, enforcement, and exit. The graduated reading's ε=0.48 reflects moderate constraint: real reciprocal obligations, but enforced proportionally rather than absolutely. The binding reading's ε would be higher (stronger constraint, less escape velocity); the transactional reading's ε would be lower (weaker constraint, greater unilateral exit). These are separate constraint stories (files), linked via network.affects_constraints to show their kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
