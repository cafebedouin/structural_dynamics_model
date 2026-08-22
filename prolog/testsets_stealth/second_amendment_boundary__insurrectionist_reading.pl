% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Boundary — Insurrectionist Reading (Armed Resistance Capacity Against Tyranny)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Second Amendment kernel: the
 *   insurrectionist reading, under which the right exists to preserve armed
 *   resistance capacity against tyrannical government and individual
 *   possession is instrumental to potential overthrow. Under this reading the
 *   protected domain logically extends toward military-grade arms, state
 *   disarmament efforts are legible as tyranny precursors, and the operative
 *   arrangement distributes deterrent legitimacy to armed citizens while
 *   distributing its costs to the civilian populace and the security
 *   apparatus. The arrangement has a genuine (if heavily contested)
 *   coordination core — republican deterrence theory is a real theory of
 *   collective action with founding-era pedigree — and a substantial,
 *   well-documented extraction layer: civilian mortality, policing
 *   militarization costs, and a commercial market that concentrates pecuniary
 *   gains under a justification that speaks of civic virtue. KEY AGENTS (by
 *   structural relationship): armed_citizen_insurrectionists — primary
 *   beneficiary (organized/identity_locked), holds the deterrent legitimacy
 *   the arrangement confers; firearms_industry — concentrated commercial
 *   beneficiary (institutional/arbitrage), receives the pecuniary proceeds;
 *   insurrectionist_advocacy_networks — agenda_setter and secondary
 *   beneficiary (organized/constrained), maintains the frame and enforces the
 *   boundary; civilian_populace — primary target (moderate/trapped), bears
 *   mortality and security costs without consent; law_enforcement_agencies —
 *   secondary target (institutional/constrained), bears the arms-race costs;
 *   gun_regulation_advocates — excluded voice (moderate/constrained),
 *   pre-classified as confirming the threat; constitutional_scholars —
 *   analytical observer (analytical/analytical).
 *
 * KEY AGENTS:
 *   - armed_citizen_insurrectionists: primary beneficiary (organized/identity_locked) — collects deterrent legitimacy; exit means surrendering a constitutive self-concept
 *   - firearms_industry: concentrated commercial beneficiary (institutional/arbitrage) — receives the pecuniary extraction; can pivot markets if the domain narrows
 *   - insurrectionist_advocacy_networks: agenda_setter with secondary beneficiary position (organized/constrained) — administers the frame, funded by the threat's salience
 *   - civilian_populace: primary target (moderate/trapped) — bears mortality and security costs it did not choose; cannot exit the risk environment
 *   - law_enforcement_agencies: secondary target (institutional/constrained) — absorbs militarization costs; internally split constituency
 *   - gun_regulation_advocates: excluded voice (moderate/constrained) — proposals pre-classified as tyranny precursors
 *   - constitutional_scholars: analytical observer (analytical/analytical) — maps the structure without holding a position in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.62).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Boundary — Insurrectionist Reading (Armed Resistance Capacity Against Tyranny)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'e0675dc7-752c-4424-a9a2-07d8a7ad4dde').
narrative_ontology:cs_kernel_codification('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', fixed_text).
narrative_ontology:cs_authority_grounding('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', lineage).
narrative_ontology:cs_interpretation_layer_present('e0675dc7-752c-4424-a9a2-07d8a7ad4dde').
narrative_ontology:cs_reading_relation('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', foundational, resistance_against_tyranny_is_protected_purpose).
narrative_ontology:cs_axiom_status(resistance_against_tyranny_is_protected_purpose, holdable).
narrative_ontology:cs_axiom_grounding('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', resistance_against_tyranny_is_protected_purpose, deontological).
narrative_ontology:cs_axiom('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', foundational, individual_armament_confers_real_check_capacity).
narrative_ontology:cs_axiom_status(individual_armament_confers_real_check_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', individual_armament_confers_real_check_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', founding_era_armed_citizenry_check).
narrative_ontology:cs_drift_state('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', contemporary_post_heller_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e0675dc7-752c-4424-a9a2-07d8a7ad4dde', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizen_insurrectionists).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilian_populace).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, armed_deterrence_of_tyranny_thesis).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, declaration_right_of_revolution_lineage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and train with firearms expressly as insurance against governmental tyranny, understanding possession as participation in the founding-era compact of the armed sovereign people. Treat registration proposals and military-pattern rifle restrictions as steps toward confiscation, and therefore as confirmation of the threat the arrangement guards against. Leaving the position would mean surrendering a self-concept built around vigilance and readiness, so departure is rare even when the deterrent rationale is challenged.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizen_insurrectionists, beneficiary,
    organized, biographical, identity_locked, national).

% Designs and sells military-pattern semiautomatic rifles and high-capacity platforms whose widest lawful domestic market exists because the anti-tyranny justification keeps them inside the protected domain. Funds litigation and marketing keyed to fear-of-ban purchase cycles. The pecuniary proceeds of the arrangement concentrate here even though its stated justification speaks of citizen virtue. Could pivot product lines or shift to export markets if the domestic protected domain narrowed.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Litigate, lobby, and publish to keep the anti-tyranny purpose at the center of the right's meaning. Recruit members whose dues and activism sustain the network, classify every regulatory proposal as a tyranny precursor, and organize electoral consequences for officials who disagree. Organizational survival depends on the threat remaining salient, which makes de-escalation of the framing structurally unavailable to them.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, insurrectionist_advocacy_networks, beneficiary).

% Bears the mortality and security costs of widespread military-grade armament: mass-casualty events, elevated everyday homicide, and the tax-financed militarization of policing that responds to it. Did not choose to be party to the deterrent arrangement and cannot opt out of the risk environment short of emigration. Internal geographic and political division prevents coordinated response despite aggregate numbers.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilian_populace, payer,
    moderate, biographical, trapped, national).

% Face suspects armed with military-pattern rifles and respond by scaling up equipment, armor, and tactics, absorbing the budgetary and personnel costs of that arms race. Many officers individually sympathize with gun-rights politics, so the institutions bear costs from an arrangement that parts of their own constituency defend, and their institutional voice is split accordingly.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, law_enforcement_agencies, payer,
    institutional, biographical, constrained, national).

% Propose licensing, safe-storage, and assault-platform restrictions. Inside the dominant frame their proposals arrive pre-classified as tyranny precursors rather than policy options, so they enter the conversation only as evidence for the threat. Survivors of firearm violence among them carry moral authority that the frame converts into further proof of the danger justifying the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_regulation_advocates, excluded,
    moderate, biographical, constrained, national).

% Map the textual, historical, and doctrinal structure of the amendment's competing readings, document the gap between founding-era arms parity and the modern arsenal, and publish assessments that neither political coalition is obliged to accept. Hold no enforcement or receipt position in the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed individual armament into a nominally credible deterrent against governmental tyranny, addressing the republican problem of checking a standing army without building a rival standing force. Stated without evaluation of whether the deterrent works.
% TRANSFER_FUNCTION: Moves mortality risk and security costs onto the general civilian populace and law enforcement agencies; moves money (platform sales, membership dues, litigation funding) toward manufacturers and advocacy networks; moves symbolic legitimacy and constitutional standing to armed citizens.
% ABSENT_VOICES: Gun-regulation advocates and survivors of firearm violence are rhetorically admitted but structurally sidelined: the frame pre-classifies their proposals as tyranny precursors, so dissent can be heard only as confirmation of the threat. Communities bearing concentrated violence costs are spoken about more than they speak.
% DISAPPEARANCE_RATIONALE: If the insurrectionist boundary vanished overnight, the protected domain around military-pattern platforms would collapse, regulation would normalize along the lines of peer democracies, manufacturer product lines and advocacy revenue models would reorganize, and the armed-citizen identity community would lose its constitutional warrant — the political economy built on the arrangement would rearrange within years.
% FOUNDING_PROBLEM: Defense against standing armies and remote imperial government: the eighteenth-century problem of a professional army loyal to a distant crown, experienced directly in the Revolution and codified in the English Bill of Rights lineage and the ratification-era Standing Army debates.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era, writing outside any benefiting party, corroborate that the original problem was real and specific: the Standing Army debates, Federalist 46's arithmetic of militia versus federal force, and state ratification-era objections all attest it. Contemporary military analysts outside the advocacy orbit attest that the modern deterrent claim — citizen small arms checking a modern state's air, logistics, and surveillance apparatus — is doubtful at best. No corroborating source outside the benefiting parties attests that the founding problem remains live in its original form; the liveness claim is carried almost entirely by the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the arrangement transfers mortality risk and security costs onto non-consenting parties while its pecuniary proceeds concentrate in an industry seat whose market breadth the justification itself protects. Suppression (0.62) is a raw structural property, unscaled by power or scope: it reflects the active closure of the regulatory alternative — preemption statutes, expanding doctrine, and a framing apparatus that converts every restriction proposal into evidence for the threat — not any scaling arithmetic. Theater ratio (0.58) is elevated because the anti-tyranny function is largely performative in operation: no organized musters, no functional check capacity against modern state force, while rallies, rhetoric, and fear-cycle marketing dominate observable activity; genuine function persists in training, possession, and legal defense, keeping the ratio below piton territory. Accessibility collapse (0.50) is mid-range: alternatives remain visible and partly operable in other jurisdictions and some states, but within the reading's own logic they collapse completely, since any regulation is pre-classified as a tyranny precursor. Resistance (0.60) is substantial and sustained: a mass mobilization movement, litigation, state-level regulation, and scholarly critique meet the arrangement continuously. The measurement series run on ONE shared grid (t=0..60, mapped approximately to 1965-2025: t0 pre-modern movement; t10 the post-Mulford/NRA-hardening era; t30 the 1990s militia movement; t50 the post-Heller and post-Sandy Hook decade; t60 the Bruen era). Base extractiveness rises monotonically as military-pattern platforms proliferate and the casualty ledger accumulates. Theater rises as the deterrent function recedes further from operational reality while symbolic and commercial activity grows. Suppression_requirement rises because enforcement capacity genuinely MATURED over the interval — doctrinal expansion, preemption proliferation, and electoral enforcement of the frame — which is exactly the enforcement-infrastructure dynamic the scalar base_properties.suppression cannot capture; that is why the series is tracked temporally rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the advocacy seat the arrangement is a sacred constitutional guarantee under siege — every loss confirms the threat, every victory is mere survival. From the armed-citizen seat it is an inheritance and an identity. From the civilian seat it is an imposed risk environment: the same boundary that reads as liberty from inside the frame reads as transferred mortality from outside it. From the industry seat it is, functionally, a market protection whose moral justification is someone else's. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Firearms_industry sits nearest the beneficiary pole (arbitrage-grade exit plus direct receipt of proceeds). Armed_citizen_insurrectionists sit near the beneficiary end but slightly inward: identity_lock amplifies their stake beyond their material receipts. Insurrectionist_advocacy_networks derive low d as beneficiaries but their agenda_setter role means they administer rather than merely collect. Civilian_populace sits nearest the target pole: trapped exit plus involuntary exposure places them at the full-target end. Law_enforcement_agencies derive high d as cost-bearers, moderated somewhat by their constrained-but-institutional position and their internally split constituency. No directionality overrides were authored: the derivation chain produces accurate values from the declared structure, and the one candidate (law enforcement's ambivalence) is already captured by their constrained exit and dual constituency rather than requiring a per-power-atom correction that would leak onto other institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking eighteenth-century standing armies — is dead in its original form, and the arrangement persists anyway, sustained by identity fusion on the beneficiary side and commercial reinforcement on the industry side. But the R5 status is authored 'contested', not 'dead', because the parties genuinely dispute liveness: adherents assert tyranny-risk is timeless, and no external authority can adjudicate a counterfactual threat. The mismatch consumer therefore reads contested-status x world_rearranges-verdict: no clean zombie flag fires, correctly, because the arrangement is not inert — it actively enforces, extracts, and mobilizes. The tangled_rope claim is what prevents mislabeling in both directions: calling this a pure rope would erase the documented victim set and the concentrated commercial capture; calling it a pure snare would deny the sincere, historically grounded coordination core that millions of participants actually hold and that gives the arrangement its mobilizing power. The classification holds both truths: genuine coordination function, asymmetric extraction through the same structure, active enforcement required to maintain it. If the modern_deterrence_coherence omega resolves toward negligible capacity, the coordination half hollows out and the structure trends toward pure extraction maintained by identity and commerce — the omega records exactly that transition condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the insurrectionist_reading of the second_amendment_boundary kernel; what changes structurally if a sibling reading governs instead?',
    'Doctrinal adoption analysis: if individual_right_reading becomes the governing frame, the insurrection endpoint drops out (military-grade arms fall outside common-use protection) and the victim set contracts toward ordinary regulatory stakes; if militia_conditioned_reading governs, the right is bounded to organized-militia context, comprehensive regulation becomes permissible, and this constraint''s protected domain dissolves entirely.',
    'Under individual_right_reading, epsilon falls toward ordinary property-protection levels and the armed-citizen beneficiary seat loses its deterrent warrant; under militia_conditioned_reading, the beneficiary set empties and the arrangement converts toward a defunct historical claim — the classification of this story is conditional on this reading holding the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which reading of the Second Amendment kernel governs determines this constraint''s entire beneficiary/victim architecture.').

omega_variable(
    modern_deterrence_coherence,
    'Does distributed small-arm possession confer real check capacity against a modern state''s military, logistics, and surveillance apparatus, or is the deterrent function structurally obsolete?',
    'Comparative military analysis of asymmetric insurgencies, historical study of armed-population checks on state power, and domestic-confrontation scenario modeling conducted outside advocacy sponsorship by either coalition.',
    'If capacity is negligible, the coordination half of the arrangement is hollow and the structure trends toward pure extraction maintained by identity fusion and commerce; if meaningful, the tangled coordination/extraction reading stands and part of the measured cost is the price of a functioning check.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_deterrence_coherence, empirical, 'Whether the arrangement''s coordination function is operationally real or theatrically maintained.').

omega_variable(
    hypothetical_conflict_victim_referent,
    'Are civilians harmed by military-grade arms properly counted as victims OF this constraint, or only of adjacent enforcement failures the constraint merely fails to prevent?',
    'Counterfactual regulatory comparison: casualty profiles under regimes where the insurrectionist premise does not shield military-pattern platforms, holding enforcement capacity and socioeconomic variables constant across jurisdictions.',
    'If the causal chain runs through the protected domain itself, epsilon stays high and the victim declaration stands; if the deaths trace to separable enforcement gaps, the constraint''s extraction drops materially and the classification softens toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypothetical_conflict_victim_referent, conceptual, 'Referent discipline for the victim set: constraint-caused harm versus harm the constraint fails to prevent.').

omega_variable(
    framing_suppression_mechanism,
    'Is the suppression of regulatory alternatives structural (preemption statutes, expanding doctrine, campaign-finance asymmetry) or internalized (sincere adherent belief that regulation equals tyranny), and in what proportion?',
    'Post-doctrinal trajectory analysis: if restriction proposals regain legislative hearing wherever preemption lapses or doctrine narrows, suppression was predominantly structural; if the tyranny-framing persists unchanged regardless of legal environment, it is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure and predicts persistence of the boundary even after legal defeat — raising the floor under the arrangement''s stability independent of enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_suppression_mechanism, empirical, 'Structural versus internalized mechanism maintaining the closure of the regulatory alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__insurrectionist_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__insurrectionist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__insurrectionist_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__insurrectionist_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(seco_tr_t60, second_amendment_boundary__insurrectionist_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(seco_be_t60, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(seco_su_t60, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel. individual_right_reading is the upstream member (judicially entrenched, highest empirical confidence) and influences this reading by supplying the doctrinal vehicle through which the insurrectionist purpose widens the protected domain. This reading exerts structural pressure back on the individual right's scope without foreclosing it — the two are held simultaneously by overlapping coalitions. militia_conditioned_reading is the bounded rival: within any single interpretive framework, the insurrectionist premise (resistance AGAINST the state) and the militia-conditioned premise (arms-bearing bounded to state-organized collective defense) directly contradict, so this reading forecloses that one. Epsilon differs sharply across the family: the individual-right arrangement extracts mainly through market externalities; the insurrectionist arrangement adds the military-grade endpoint and the tyranny-precursor suppression dynamic; the militia-conditioned arrangement, if it governed, would dissolve the protected domain this story measures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
