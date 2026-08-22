% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Practice Norm Complex (Liberal Institutional Reading)
 *   domain: international_relations/international_law
 *
 * SUMMARY:
 *   The liberal institutional reading of the RBIO practice norm complex
 *   frames international humanitarian law and intervention authorization as
 *   universal, consent-based, and revisable through multilateral processes.
 *   From this reading's standpoint, UNSC authorization, multilateral treaty
 *   negotiation, and humanitarian grounds for intervention constitute a
 *   legitimate system for managing state sovereignty while preventing
 *   atrocities. Enforcement selectivity (why some atrocities trigger response
 *   and others do not) is attributed to capacity constraints, geopolitical
 *   complexity, and the practical difficulty of multilateral coordination
 *   rather than to structural extraction or hegemonic bias. This reading
 *   dominates contemporary international law scholarship and is the official
 *   doctrine of intervening state coalitions. However, it coexists with
 *   hegemonic extraction and sovereignty-maximalist readings that contest the
 *   legitimacy claim: the hegemonic reading sees RBIO norms as a frozen
 *   project maintained by P5 veto power; the sovereignty-maximalist reading
 *   sees RBIO norms as pretexts for regime change and sovereignty violation.
 *   The constraint story models THIS reading as a tangled_rope: it genuinely
 *   coordinates humanitarian response and establishes universal standards
 *   (rope component), but it asymmetrically benefits intervening states and
 *   their contractors while imposing costs on targeted populations
 *   (extraction component). The tension between the coordination claim and
 *   the extraction evidence is the contested kernel.
 *
 * KEY AGENTS:
 *   - Intervening state coalitions (P5 members and allies): institutional power, dominate authorization processes, benefit from normative flexibility, exit options available through treaty withdrawal or norm rewriting
 *   - Targeted sovereign states: powerful at global scale but constrained by RBIO enforcement, experience norms as imposed, asymmetric cost-bearing
 *   - Civilian populations under sanctions: powerless, experience sanctions as collective punishment, trapped with no exit or voice
 *   - Humanitarian advocacy networks: organized, benefit from RBIO legitimacy framework, provide moral and legal grounding for interventions
 *   - Non-aligned and medium-power states: excluded from veto authority, experience norms as externally imposed, constrained voice in revision processes
 *   - UNSC permanent members: agenda-setters, hold structural veto authority over enforcement authorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.58).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.41).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Practice Norm Complex (Liberal Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '38b4f484-b5f3-4688-993d-5996071e63cd').
narrative_ontology:cs_kernel_codification('38b4f484-b5f3-4688-993d-5996071e63cd', fixed_text).
narrative_ontology:cs_authority_grounding('38b4f484-b5f3-4688-993d-5996071e63cd', extraction).
narrative_ontology:cs_interpretation_layer_present('38b4f484-b5f3-4688-993d-5996071e63cd').
narrative_ontology:cs_reading_relation('38b4f484-b5f3-4688-993d-5996071e63cd', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('38b4f484-b5f3-4688-993d-5996071e63cd', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('38b4f484-b5f3-4688-993d-5996071e63cd', foundational, multilateral_consensus_legitimates_intervention).
narrative_ontology:cs_axiom_status(multilateral_consensus_legitimates_intervention, holdable).
narrative_ontology:cs_axiom_grounding('38b4f484-b5f3-4688-993d-5996071e63cd', multilateral_consensus_legitimates_intervention, deontological).
narrative_ontology:cs_axiom('38b4f484-b5f3-4688-993d-5996071e63cd', foundational, rbio_norms_are_revisable_through_treaty_amendment).
narrative_ontology:cs_axiom_status(rbio_norms_are_revisable_through_treaty_amendment, holdable).
narrative_ontology:cs_axiom_grounding('38b4f484-b5f3-4688-993d-5996071e63cd', rbio_norms_are_revisable_through_treaty_amendment, empirically_contingent).
narrative_ontology:cs_reference_frame('38b4f484-b5f3-4688-993d-5996071e63cd', post_wwii_consensus_driven_multilateral_authority).
narrative_ontology:cs_drift_state('38b4f484-b5f3-4688-993d-5996071e63cd', contemporary_enforcement_selectivity_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38b4f484-b5f3-4688-993d-5996071e63cd', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_state_coalitions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_advocacy_networks).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_sovereign_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Permanent Security Council members and their allied states set and interpret RBIO norms through multilateral processes they structurally dominate. They authorize interventions when grave atrocities occur or when security interests align with stated humanitarian rationales. They benefit from normative flexibility: the same rules permit their humanitarian interventions (Kosovo, Libya) while constraining others (Syria, Myanmar). They can rewrite norms, withdraw from treaties, or operate outside frameworks with limited cost. They frame enforcement selectivity as resource constraints rather than structural bias.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_state_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% Experience RBIO enforcement (sanctions, intervention authorization, peacekeeping mandates) as imposed constraints on policy autonomy. Even powerful states face asymmetric costs: sanctions disrupt economies, intervention undermines capacity, and enforcement selectivity is contested. They experience enforcement selectivity as evidence that norms serve intervening states' interests rather than universal principles. Withdrawal from the system means losing treaty benefits, trade access, and diplomatic standing, but remaining means accepting unequal treatment.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_sovereign_states, payer,
    powerful, generational, constrained, global).

% Bear direct costs of sanctions (fuel shortage, medicine scarcity, economic collapse) intended to pressure governments toward RBIO compliance. The liberal reading frames sanctions as nonviolent coercion toward norm compliance; populations experience them as collective punishment. They derive speculative benefit if sanctions contribute to ending atrocities, but that benefit is deferred while costs are immediate. They have no seat in authorization processes and cannot exit their government's exposure to the sanction regime.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, beneficiary).

% International human rights organizations, development NGOs, and accountability coalitions benefit from RBIO norms by gaining legitimacy to intervene in state sovereignty. The liberal reading provides their moral and legal foundation: universal norms, consent-based legitimacy, revisable processes. They frame enforcement selectivity as a problem of political will or capacity, not norm design. They can work within or outside RBIO frameworks, shift focus to non-state actors, or advocate for norm revision. RBIO norms are their institutional base.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Collectively hold veto authority over enforcement authorization. In this reading, the veto ensures legitimacy through great-power consensus and prevents abuse. They ratify, interpret, and selectively enforce norms based on stated humanitarian criteria and security interests. They can block authorization against allies and permit it against adversaries. The liberal reading treats veto as structural support for consensus legitimacy; competing readings contest this.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% Experience RBIO norms as externally imposed standards they must navigate without meaningful input to their design. They lack veto authority or agenda-setting capacity. They would argue that consent-based legitimacy is contradicted by P5 structural dominance in treaty drafting, that enforcement selectivity reflects power asymmetry not capacity constraints, and that revisability is theoretical when revision requires P5 consensus. Their voice is present in forums but not determinative.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, non_aligned_states, excluded,
    moderate, generational, constrained, global).

% Provides analytical and normative grounding for the liberal institutional reading: legal scholarship defending multilateral authorization legitimacy, empirical work on humanitarian outcomes, and theoretical work on norm revisability. Different schools (liberal, critical, realist) contest the reading, but academic legitimacy accrues through publications, institutional positions, and influence on legal education. They have no enforcement authority but substantial influence on how states justify their actions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, academic_international_law_community, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, intervening_state_coalitions).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes universal norms for state conduct toward civilians (humanitarian law, atrocity prevention, sanctions architecture) and creates multilateral processes through which enforcement is authorized. Solves the coordination problem of moving beyond unilateral or ad hoc state action toward rule-based international order. Enables shared burden-sharing on humanitarian crises and establishes common standards for legitimate intervention.
% TRANSFER_FUNCTION: Moves policy autonomy and economic resources from targeted states and their civilian populations to intervening state coalitions and humanitarian organizations. Sanctions drain foreign exchange reserves and disrupt trade; intervention authorization legitimizes military action and regime-change operations; sanctions architecture enriches contractors implementing humanitarian aid and sanctions administration. The transfer is justified in this reading as payment for the coordination benefit of universal norms, though targeted states and populations experience it as uncompensated extraction.
% ABSENT_VOICES: States that would benefit from norm revision to constrain great-power intervention (non-aligned states, regional powers), populations under sanctions (no formal representation), and the hegemonic extraction and sovereignty-maximalist readings of the same kernel are structurally absent from the authorization process. They would attest that enforcement selectivity is evidence of extractive intent, not capacity constraint, and that revisability is theoretical when veto authority is concentrated.
% DISAPPEARANCE_RATIONALE: If RBIO norms and their enforcement apparatus vanished, humanitarian response would fragment into unilateral state action and ad hoc coalitions; atrocity prevention would depend on reputational and strategic interests rather than multilateral authorization; sanctions would migrate to bilateral and secondary-market enforcement; and the international legal order would revert to great-power balance-of-power dynamics. Intervening states would lose the legitimacy architecture that permits their actions; humanitarian organizations would lose legal standing; and targeted states would gain unilateral autonomy at the cost of losing treaty protections.
% FOUNDING_PROBLEM: The early post-WWII order needed a mechanism to prevent atrocities and manage state behavior in an anarchic system. The League's failure to constrain fascism, WWII's industrial genocide, and post-WWII decolonization created demand for universal norms that transcend sovereignty claims. RBIO norms emerged as a compromise: sovereignty protected except when grave atrocities occur or multilateral process authorizes intervention.
% FOUNDING_PROBLEM_CORROBORATION: The liberal institutional reading attests the problem is live: ongoing atrocities, failed states, and transnational crises require multilateral response mechanisms. Sovereignty-maximalist states attest the founding problem is dead or overstated: modern international institutions constrain atrocities regardless of RBIO norms; RBIO enforcement is selective and reflects power, not principle. Hegemonic extraction readings attest the founding problem was instrumentally useful for establishing the current order but the problem itself is now managed through the norms for the benefit of established powers. Academic witnesses (international law scholars, human rights researchers) are divided: some support the liberal institutional reading's assessment of ongoing coordination demand, others support critical readings emphasizing norm selectivity.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint asymmetrically benefits intervening states through normative flexibility (they get to define grave atrocities, authorize their preferred interventions, exclude rivals) while imposing costs on targeted populations (sanctions, intervention, regime change). The beneficiaries gain policy autonomy; the victims lose it. Suppression is moderate (0.41) because the constraint persists through normative legitimacy and institutional design rather than through direct coercion — but enforcement selectivity does require suppression of alternative framings (sovereignty-maximalist and hegemonic extraction readings are marginalized in mainstream discourse). Theater is moderate-low (0.33) in this interval because the humanitarian function is partially real (atrocities are documented, some interventions do prevent harms) but increasingly visible as selectivity is recognized: as evidence of selective enforcement grows, the theater ratio should rise. The measurement series shows extractiveness rising from 1945 (when coordination was the dominant need post-WWII) through 2001 (when intervention doctrine solidified post-9/11), then declining slightly as the humanitarian reading faces stronger critical challenge from 2001-2026. Theater rises from 1945-2001 as enforcement selectivity becomes visible, then plateaus as the contradiction is widely acknowledged. Suppression requirement rises initially (as the system matures and requires active suppression of alternative readings) then stabilizes. The shared time grid ensures every metric is authored at every examined time point (1945, 1975, 1990, 2001, 2015, 2026).
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state coalition and humanitarian network seats, the constraint is genuine coordination with acknowledged capacity constraints — enforcement selectivity reflects real-world complexity, not structural bias. From the targeted state and powerless population seats, the same constraint operates as enforced extraction justified by humanitarian rhetoric — enforcement selectivity reveals that the norms serve intervening state interests, not universal principles. From the non-aligned state seat, the constraint is externally imposed despite claims to consent-based legitimacy — participation in forums is theater because veto authority is concentrated. The engine should compute these divergences from the structural data: beneficiaries sit near d=0.0 (subsidized by the constraint's legitimacy benefits); victims sit near d=1.0 (targeted by enforcement); non-aligned states sit near d=0.7-0.8 (constrained by design but not direct targets). The perspectival gap reflects genuine asymmetry, not measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening state coalitions: role=agenda_setter, power=institutional, exit_options=arbitrage. They set the norms, interpret them, authorize enforcement, and can exit through treaty withdrawal or norm rewriting with minimal cost. Their directionality is near the beneficiary end (d~0.15-0.25) because they structure the system to their advantage. Targeted sovereign states: role=payer, power=powerful, exit_options=constrained. They bear the cost of sanctions and enforcement but cannot unilaterally rewrite the norms or exit without losing treaty benefits. Their directionality is near the target end (d~0.7-0.8) because they pay in policy autonomy while beneficiaries gain it. Civilian populations: role=payer (pay in suffering), secondary role=beneficiary (speculative humanitarian benefit), power=powerless, exit_options=trapped. They have no seat in authorization and cannot exit; they are pure targets despite the beneficiary rhetoric. Their directionality is at the target end (d~0.85-0.95). Humanitarian networks: role=beneficiary, power=organized, exit_options=mobile. They gain legitimacy and operational space from RBIO norms; they could shift to non-state action if norms changed. Their directionality is near the beneficiary end (d~0.2-0.35). No directionality overrides are needed — the derivation chain produces the right d from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The liberal institutional reading maintains that the founding problem (atrocity prevention, coordination toward universal standards) is live and that RBIO norms remain the legitimate solution. However, the measurement series and the hegemonic reading both suggest mandatrophy: enforcement selectivity indicates the norms now serve to legitimize intervening state interests more than to prevent atrocities. If founding_problem_status=contested and disappearance_verdict=world_rearranges, the mismatch consumer (R5 gate in the engine) should flag a potential mandatrophy condition: the founding problem may be dead (atrocities occur whether or not RBIO norms exist) but the arrangement persists because intervening states and humanitarian networks benefit from it. The liberal institutional reading resists this conclusion (it asserts the problem is live) but the empirical evidence (rising theater ratio, enforcement selectivity, limited atrocity prevention outcomes) creates room for the mandatrophy hypothesis. This is exactly the kind of structure mandatrophy detection should catch: a claimed coordination solution that is visibly failing at its founding function but persisting due to institutional inertia and beneficiary interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_interpretation,
    'Is enforcement selectivity evidence of extractive intent (hegemonic reading) or of legitimate capacity constraints (liberal institutional reading)?',
    'Counterfactual analysis: if intervening states had comparable capacity and security interests across all cases, would enforcement patterns match the liberal institutional reading''s predictions? Comparison with stated authorization criteria against actual intervention decisions across the full interval.',
    'If selectivity reflects power asymmetry rather than capacity, the constraint reclassifies from tangled_rope (genuine coordination with asymmetric enforcement) to snare (extractive disguised as coordination). The beneficiary/victim relationship inverts: intervening states become pure extractors, humanitarian networks become captured beneficiaries, and targeted populations are the stable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_interpretation, empirical, 'Whether enforcement selectivity reveals structural extraction or legitimate constraint.').

omega_variable(
    consent_revisability_gap,
    'Is the liberal institutional reading''s claim that RBIO norms are ''consent-based and revisable'' structural or rhetorical? Can P5-dominated treaty processes genuinely be revised against P5 preference?',
    'Historical analysis of attempted norm revision: what proportion of proposed norm changes were adopted despite P5 opposition? What proportion required P5 consensus? Do non-P5 states perceive revision pathways as open or foreclosed?',
    'If revision is contingent on P5 consensus and has never succeeded against P5 opposition, the constraint is not truly revisable — it is a frozen hegemonic project masquerading as open process. This supports the hegemonic extraction reading. If revision has occurred against initial P5 opposition or if clear mechanisms exist, the liberal reading''s claim holds structural weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_revisability_gap, empirical, 'Whether consent-based revisability is structural or a normative claim without institutional substrate.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings of the RBIO kernel genuinely coexist as live institutional positions, or does the dominance of the liberal institutional reading functionally foreclose the sovereignty-maximalist reading?',
    'Survey of state policy commitments, legal positions, and institutional behavior: what proportion of states hold the sovereignty-maximalist reading as their official doctrine? Do sovereignty-maximalist positions face systematic exclusion from international forums, or do they coexist alongside liberal positions?',
    'If the sovereignty-maximalist reading is institutionally excluded (countries adopting it are sanctioned, face diplomatic isolation, or are denied participation), then the liberal reading forecloses it despite theoretical coexistence. The constraint''s legitimacy claim weakens because genuine contestation is suppressed. If both readings coexist in institutional practice (different states adopt different readings without systematic exclusion), the liberal reading''s claim to universal consent-based legitimacy gains credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether the three readings coexist as live institutional positions or the liberal reading functionally forecloses alternatives.').

omega_variable(
    civilian_sanction_cost_framing,
    'Is the cost of sanctions to civilian populations (medicine scarcity, fuel shortage, malnutrition) a legitimate price for coordination toward norm compliance (liberal framing) or a form of collective punishment that undermines the humanitarian legitimacy of RBIO norms (critical framing)?',
    'Empirical study of sanction outcomes: do sanctions achieve stated compliance objectives? Do humanitarian benefits (atrocity reduction, regime change toward democratic governance) outweigh documented civilian harms? What is the counterfactual: would the harms occur anyway under the targeted regime, or are they imposed by the sanction regime itself?',
    'If sanctions systematically fail to achieve compliance but do impose measurable civilian harms, the constraint shifts from tangled_rope (coordination with acknowledged asymmetric cost) to snare (extractive mechanism using humanitarian justification). Humanitarian networks shift from beneficiary to captured beneficiary. The victim set expands and the extraction becomes undeniable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_sanction_cost_framing, empirical, 'Whether sanction costs to civilians reflect coordination overhead or unjustified harm.').

omega_variable(
    liberal_vs_hegemonic_axiom_ground,
    'The liberal institutional reading grounds its legitimacy claim in the axiom that multilateral authorization mechanisms are consent-based and revisable. Is this axiom deontological (all state consent is normatively necessary) or instrumental (we choose multilateralism because it works better than alternatives)?',
    'Textual analysis of foundational documents (UN Charter, treaty preambles, scholarly justifications) and state practice: do states justify RBIO norms by appeal to intrinsic right to consent (deontological) or by appeal to coordination benefits (instrumental)? If instrumental, what happens when consensus breaks and instrumental benefits are questioned?',
    'If the axiom is instrumental and empirical evidence shows that multilateral authorization does NOT prevent atrocities or does NOT serve universal interests, the reading''s legitimacy erodes. The hegemonic extraction reading gains explanatory power. If the axiom is deontological, the reading''s legitimacy persists even if empirical outcomes are poor — the commitment is to process, not results. This distinction affects the vulnerability of the constraint to reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_vs_hegemonic_axiom_ground, conceptual, 'Whether liberal institutional legitimacy rests on deontological or instrumental grounds.').

omega_variable(
    non_aligned_state_voice_exclusion,
    'Are non-aligned and medium-power states genuinely excluded from RBIO norm-setting, or do they have meaningful input that the liberal institutional reading underestimates?',
    'Institutional analysis: what percentage of UNSC authorization votes are decided by P5 over non-P5 opposition? What proportion of treaty negotiations are dominated by P5? Do non-P5 states believe they have meaningful voice, and has non-P5 opposition ever blocked a norm revision?',
    'If non-P5 states are systematically outvoted and have never blocked a norm revision, the liberal reading''s claim to ''universal consent-based'' legitimacy is undermined — consent is constrained by institutional design. If non-P5 states have meaningful blocking power or regular success in negotiations, the liberal reading''s claim strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_aligned_state_voice_exclusion, empirical, 'Whether non-P5 states have meaningful voice in RBIO norm revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(rbio_tr_t2001, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(rbio_tr_t2026, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2026, 0.33).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(rbio_be_t2001, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2001, 0.63).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(rbio_be_t2026, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.28).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1975, 0.32).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.36).
narrative_ontology:measurement(rbio_su_t2001, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(rbio_su_t2026, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2026, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.18).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The RBIO practice norm complex kernel admits three structurally distinct readings: liberal institutional (this story), hegemonic extraction (sibling), and sovereignty-maximalist (sibling). They share the same referent (international humanitarian law and intervention authorization regimes) but assess the constraint's legitimacy, beneficiary structure, and enforcement selectivity differently. Each reading instantiates a different constraint with its own ε, beneficiary/victim set, and classification. The three stories are linked via network.affects_constraints to enable cross-reading comparative analysis. The liberal institutional reading claims RBIO norms solve a genuine coordination problem; the hegemonic reading claims they mask extraction; the sovereignty-maximalist reading claims they violate sovereignty. The engine's per-seat classification will show how the constraint type diverges by seat and reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__liberal_institutional_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
