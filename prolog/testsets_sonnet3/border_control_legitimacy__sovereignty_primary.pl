% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Border Control Authority
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primary reading of the border
 *   control legitimacy kernel: territorial sovereignty is treated as
 *   entailing an essentially unconditional discretion to exclude
 *   non-citizens, and this discretion is presented as constitutive of
 *   statehood rather than as one policy option among several a state might
 *   choose within jurisdictional limits. Under this reading, human rights
 *   obligations toward migrants (non-refoulement, family unity,
 *   proportionality) are external constraints the state has voluntarily
 *   assumed and could in principle unwind, not internal limits on what counts
 *   as legitimate authority in the first place. This produces a stark victim
 *   set — excluded migrants, asylum seekers, stateless persons, and
 *   already-resident undocumented workers — whose claims register only as
 *   petitions to grace. The sibling readings (freedom_of_movement_primary,
 *   jurisdictional_sovereignty) are separate constraints with their own ε and
 *   stakeholder sets; they are not blended into this one.
 *
 * KEY AGENTS:
 *   - state_apparatus: agenda_setter (institutional/analytical) — defines and enforces exclusion discretion
 *   - citizen_polity: beneficiary (organized/mobile) — receives bounded membership goods
 *   - border_enforcement_industry: beneficiary/agenda_setter (organized/arbitrage) — profits from enforcement intensity
 *   - excluded_migrants, asylum_seekers, stateless_persons, undocumented_resident_workers: payer (powerless/trapped) — bear exclusion costs with no standing to contest them under this reading
 *   - international_human_rights_bodies: excluded (institutional/analytical) — articulate constitutive-limit claims that this reading treats as non-binding
 *   - comparative_legal_scholars: observer (analytical) — trace doctrinal genealogy across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.81).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Sovereignty-Primary Reading of Border Control Authority").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '20748743-18c4-40a4-8e44-4ca92b160c6f').
narrative_ontology:cs_kernel_codification('20748743-18c4-40a4-8e44-4ca92b160c6f', distributed).
narrative_ontology:cs_authority_grounding('20748743-18c4-40a4-8e44-4ca92b160c6f', practice).
narrative_ontology:cs_interpretation_layer_present('20748743-18c4-40a4-8e44-4ca92b160c6f').
narrative_ontology:cs_reading_relation('20748743-18c4-40a4-8e44-4ca92b160c6f', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('20748743-18c4-40a4-8e44-4ca92b160c6f', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('20748743-18c4-40a4-8e44-4ca92b160c6f', foundational, exclusion_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('20748743-18c4-40a4-8e44-4ca92b160c6f', exclusion_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('20748743-18c4-40a4-8e44-4ca92b160c6f', secondary, protection_obligations_external_to_sovereign_authority).
narrative_ontology:cs_axiom_status(protection_obligations_external_to_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('20748743-18c4-40a4-8e44-4ca92b160c6f', protection_obligations_external_to_sovereign_authority, conventional).
narrative_ontology:cs_reference_frame('20748743-18c4-40a4-8e44-4ca92b160c6f', westphalian_plenary_power_doctrine).
narrative_ontology:cs_drift_state('20748743-18c4-40a4-8e44-4ca92b160c6f', post_universal_human_rights_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20748743-18c4-40a4-8e44-4ca92b160c6f', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, stateless_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, territorial_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces who may cross and remain within the territory, treating this discretion as inseparable from statehood itself. Builds detention, deportation, and screening infrastructure and justifies each expansion as an expression of sovereign self-definition rather than a policy choice subject to external constraint. Collects legitimacy and administrative capacity from the arrangement, not direct revenue.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Receives the good of a demarcated membership community — control over labor markets, welfare eligibility, and cultural continuity — on the premise that a people's collective self-determination requires the power to exclude. Bears some costs (enforcement taxation, occasional labor shortages) but retains full exit and voting power over the arrangement's terms.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Contractors, technology vendors, and detention operators whose revenue scales directly with enforcement intensity. Lobbies to keep the sovereignty framing intact because any reframing toward jurisdictional balancing or rights-based limits threatens the budget lines the framing sustains.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, agenda_setter).

% Denied entry or presence on the sole ground of non-membership, regardless of the severity of their need or the arbitrariness of the line drawn. Under this reading, their claims register only as petitions to a discretion the state is not obligated to extend, not as rights the state is bound to accommodate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Present protection claims that are processed as exceptions carved out of the state's discretion rather than as independent constraints on it; under sovereignty-primary logic, non-refoulement and asylum obligations are treated as voluntarily assumed treaty commitments the state could in principle withdraw, not as limits constitutive of legitimate authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Belong to no polity whose sovereignty could be asserted on their behalf; the sovereignty-primary frame has no seat for them except as objects of discretionary admission, leaving them permanently outside the class the arrangement is designed to serve.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, stateless_persons, payer,
    powerless, civilizational, trapped, global).

% Already inside the territory, performing labor the citizen economy depends on, yet classified as illegitimate presences subject to removal at the state's discretion. Employers benefit from their labor while the workers carry the full risk of enforcement, unable to invoke rights claims that the sovereignty frame treats as external to the state's authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers, beneficiary).

% Would argue that protection obligations and non-refoulement are constitutive limits on legitimate state authority, not external treaty add-ons the state extends by grace. Under the sovereignty-primary reading, their findings are treated as advisory or as further evidence of encroaching supranational overreach, not as binding constraints on what counts as legitimate exclusion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% Trace how the sovereignty-primary doctrine developed historically (Chinese Exclusion Case, Nishimura Ekiu) and compare it against jurisdictional and rights-based alternatives, without themselves holding power to alter enforcement practice.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible answer to who belongs to a political community and who may be excluded, which enables collective self-governance, resource planning, and a bounded demos capable of democratic deliberation.
% TRANSFER_FUNCTION: Moves the costs of global inequality, displacement, and conflict onto individuals born outside the favored territory, while concentrating the benefits of membership — labor market protection, welfare access, physical security — onto those already inside it; enforcement budgets transfer public funds to a growing detention and surveillance industry.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and stateless persons have no vote in the polity whose discretion determines their fate; international human rights bodies articulate their interests from outside the sovereign decision-making process and are treated as non-binding under this reading.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary doctrine were abandoned as the operative legal premise, exclusion would require independent justification against a baseline right to move or a balancing test — enforcement budgets, detention infrastructure, and the legal doctrine insulating discretionary exclusion from external review would all have to be rebuilt or dismantled; millions currently classified as unauthorized would gain standing to contest their exclusion on grounds the current frame does not recognize.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century states needed a doctrine that would let them control immigration during periods of mass migration, war, and nation-building without external legal bodies second-guessing admission decisions — courts derived 'plenary power' and absolute territorial discretion to insulate these decisions from judicial and international review.
% FOUNDING_PROBLEM_CORROBORATION: States and their courts (invoking Chinese Exclusion Case-style plenary power doctrine) attest the problem — unregulated mass entry threatening self-governance — remains live. Comparative legal scholars and international human rights bodies, external to the states that benefit from the doctrine, attest that the original problem (managing genuinely chaotic mass entry) has been substantially replaced by a bureaucratically manageable regulatory function, and that the absolute-discretion framing now serves mainly to insulate enforcement practice from proportionality and rights review rather than to solve a live coordination problem.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.50→0.68) as plenary-power doctrine has expanded from a narrow immigration-admission rule into a general warrant for detention, expedited removal, and externalized border enforcement (offshore processing, safe-third-country deals) — the same discretion-as-constitutive framing is stretched to cover an increasingly broad enforcement apparatus. Suppression is high and rising (0.55→0.81) because the doctrine's persistence depends on courts and international bodies being denied binding review authority over exclusion decisions — this is a structural, not incidental, feature of the reading. Theater ratio is moderate and rising (0.20→0.42): a genuine coordination function (defining a bounded political community capable of self-governance) is real, but an increasing share of enforcement activity — security theater at ports of entry, symbolic wall construction, high-profile but low-yield interior raids — serves demonstrative rather than functional exclusion purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and citizen polity sit near the beneficiary end: the state collects legitimacy and administrative capacity, the polity collects bounded membership goods, and both retain analytical or mobile exit relative to the constraint (the polity can vote to change the policy; the state defines the policy). The border enforcement industry benefits materially and lobbies to entrench the framing, giving it a secondary agenda-setting role despite lacking sovereign authority itself. Excluded migrants, asylum seekers, and stateless persons sit at the full-target end — trapped exit, no standing within the frame, civilizational-scale exposure for the stateless in particular. Undocumented resident workers occupy an unusual dual position: they are economically integrated (indirect beneficiaries of being employed) while being the direct payers of enforcement risk, which the secondary_role field captures without collapsing the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing genuinely destabilizing, unregulated mass entry during nation-building and wartime periods — is largely dead as a practical matter in most contemporary contexts (modern states possess biometric tracking, visa systems, and processing capacity the doctrine's originators lacked), yet the absolute-discretion doctrine persists at full strength and is expanding rather than sunsetting. This is close to a mandatrophy pattern: a doctrine whose founding justification has substantially resolved but which continues to expand its footprint (from admission control into interior enforcement, offshore deals, and detention) by treating its own persistence as self-justifying under the 'constitutive of statehood' framing. Classifying this as tangled_rope rather than snare or piton preserves the fact that a genuine coordination function (bounded self-governing political community) still operates alongside the extraction — collapsing the two would either whitewash the victim set (calling it pure rope) or deny the doctrine any real coordination content (calling it pure snare), when in fact both are structurally present and mutually reinforcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_external_limit_framing,
    'Is the discretion to exclude genuinely constitutive of sovereignty as such (such that human rights obligations are external, defeasible add-ons), or is sovereignty better understood as jurisdictional authority that human rights law has always internally limited, making the ''absolute discretion'' claim a historically contingent doctrinal overreach rather than a conceptual truth about statehood?',
    'Comparative doctrinal history across jurisdictions that have moved from plenary-power-style discretion toward proportionality review (e.g., shifts in some regional human rights court jurisprudence) would show whether the constitutive claim survives sustained legal contestation or is progressively hollowed out — convergence toward jurisdictional_sovereignty-style balancing across jurisdictions would undermine the sovereignty_primary premise''s claim to conceptual necessity.',
    'If sovereignty_primary''s core premise is conceptually contingent rather than necessary, its foreclosure claim against freedom_of_movement_primary weakens substantially, and the reading itself becomes harder to sustain as anything more than a historically dominant but not uniquely correct interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_external_limit_framing, conceptual, 'Whether absolute exclusion discretion is conceptually constitutive of sovereignty or a contingent, contestable doctrinal choice.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original problem the plenary-power doctrine was built to solve (chaotic, unmanageable mass entry threatening basic state functions) been substantially resolved by modern administrative and biometric capacity, such that the doctrine''s continued expansion serves enforcement-industry and bureaucratic-legitimacy interests rather than the founding coordination problem?',
    'Compare enforcement-doctrine scope and intensity against measures of actual administrative capacity to manage entry (visa systems, biometric tracking, processing throughput) over time; a widening gap between capacity and doctrinal scope would support the obsolescence reading.',
    'If the founding problem is substantially dead, the case for treating this as mandatrophy (tangled_rope drifting toward snare) strengthens; if the founding problem remains live (e.g., under genuine crisis conditions of mass displacement), the coordination function is better preserved as still-load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether administrative capacity has overtaken the original justification for absolute discretionary exclusion authority.').

omega_variable(
    sibling_reading_kernel_disagreement_locus,
    'Where exactly does the disagreement between sovereignty_primary and its siblings live — is it a disagreement about facts (does border closure actually serve self-determination goals), about the concept of sovereignty (what sovereignty analytically entails), or about values (how to weigh collective self-determination against individual movement rights when they conflict)?',
    'This is inherently a conceptual/normative dispute rather than an empirically resolvable one; the corpus can only document that the three readings occupy genuinely different premises (constitutive claim vs. rights-primacy vs. jurisdictional-balancing) rather than converge on a shared factual disagreement.',
    'Locating the disagreement as conceptual/normative (rather than empirical) means no future evidence resolves the kernel contest — the three readings will persist as coexisting live positions rather than one being vindicated by data, which is why coexists_with rather than forecloses is the primary relation to the jurisdictional_sovereignty sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_disagreement_locus, conceptual, 'Whether the kernel dispute is factual, conceptual, or normative — bearing on whether it is ever empirically resolvable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__sovereignty_primary, theater_ratio, 8, 0.26).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__sovereignty_primary, theater_ratio, 16, 0.31).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__sovereignty_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__sovereignty_primary, theater_ratio, 32, 0.39).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__sovereignty_primary, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__sovereignty_primary, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__sovereignty_primary, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__sovereignty_primary, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__sovereignty_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__sovereignty_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__sovereignty_primary, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__sovereignty_primary, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_control_legitimacy kernel. sovereignty_primary treats absolute exclusion discretion as constitutive of statehood (this story, ε≈0.68, tangled_rope). jurisdictional_sovereignty treats sovereignty as jurisdictional authority requiring balancing against protection obligations and labor needs (a distinct, less extractive constraint — likely a lower-ε tangled_rope or scaffold given its built-in balancing requirement). freedom_of_movement_primary treats movement as a fundamental right sovereignty does not override, and would author a very different beneficiary/victim structure entirely (excluded migrants would shift toward the beneficiary/vindicated side, and the enforcement apparatus itself would likely read as the primary extraction mechanism, closer to snare). These are not the same constraint measured three ways — they are three constraints sharing a contested kernel, each with its own stable ε and stakeholder set, linked here for contamination-propagation and cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
