% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Absolute Sovereign Discretion to Exclude Non-Citizens
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the
 *   border_control_legitimacy kernel: the claim that territorial sovereignty
 *   entails absolute, essentially non-justiciable discretion to exclude
 *   non-citizens, and that this discretion is constitutive of statehood
 *   rather than a policy choice balanced against competing claims. Under this
 *   reading, human rights obligations (non-refoulement, due process for
 *   asylum claims) are treated as external moral or treaty appeals to
 *   sovereign grace rather than constraints internal to what makes the
 *   exercise of authority legitimate. This produces a distinct victim set —
 *   excluded migrants, asylum seekers, and undocumented residents who cannot
 *   invoke rights-based or labor-balancing tests against exclusion — and
 *   justifies an enforcement apparatus (detention, deportation
 *   infrastructure, border surveillance) as sovereignty defense rather than
 *   as one policy instrument among several. This is a single, clean reading
 *   with its own stable epsilon; the sibling readings
 *   (freedom_of_movement_primary, jurisdictional_sovereignty) are separate
 *   constraint stories with their own beneficiary/victim structures and are
 *   not blended in here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.62).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.81).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Absolute Sovereign Discretion to Exclude Non-Citizens").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '74f1d502-425f-40bc-949c-7bb81d2005ab').
narrative_ontology:cs_kernel_codification('74f1d502-425f-40bc-949c-7bb81d2005ab', distributed).
narrative_ontology:cs_authority_grounding('74f1d502-425f-40bc-949c-7bb81d2005ab', practice).
narrative_ontology:cs_interpretation_layer_present('74f1d502-425f-40bc-949c-7bb81d2005ab').
narrative_ontology:cs_reading_relation('74f1d502-425f-40bc-949c-7bb81d2005ab', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('74f1d502-425f-40bc-949c-7bb81d2005ab', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('74f1d502-425f-40bc-949c-7bb81d2005ab', foundational, exclusion_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('74f1d502-425f-40bc-949c-7bb81d2005ab', exclusion_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('74f1d502-425f-40bc-949c-7bb81d2005ab', foundational, human_rights_as_external_limit_not_internal_constraint).
narrative_ontology:cs_axiom_status(human_rights_as_external_limit_not_internal_constraint, holdable).
narrative_ontology:cs_axiom_grounding('74f1d502-425f-40bc-949c-7bb81d2005ab', human_rights_as_external_limit_not_internal_constraint, conventional).
narrative_ontology:cs_reference_frame('74f1d502-425f-40bc-949c-7bb81d2005ab', westphalian_plenary_power_doctrine).
narrative_ontology:cs_drift_state('74f1d502-425f-40bc-949c-7bb81d2005ab', post_refugee_convention_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('74f1d502-425f-40bc-949c-7bb81d2005ab', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, receiving_state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers_at_borders).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_state_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces immigration law under the claim that exclusion authority is inherent to statehood, not a policy choice subject to external balancing. Sets detention, deportation, and admission criteria; funds and directs the enforcement apparatus; frames any international human-rights constraint as an optional treaty commitment rather than a limit on legitimate authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, receiving_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from reduced labor market competition and preserved wage floors in sectors exposed to migrant labor. Can lobby for tighter or looser enforcement depending on sectoral interest; bear essentially no direct cost of the exclusion apparatus and retain full citizenship-based exit options domestically and internationally.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_labor_incumbents, beneficiary,
    organized, biographical, mobile, national).

% Private contractors, detention operators, and surveillance technology vendors whose revenue scales directly with enforcement intensity. Lobby to maintain and expand the sovereignty-primary framing because it forecloses judicial or treaty-based limits that would shrink their contract base.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, agenda_setter).

% Denied entry, detained, or deported under discretion the receiving state claims is absolute and non-justiciable. Have no standing within the receiving state's legal order until admitted, and the sovereignty-primary framing is precisely what denies them a forum to contest the decision on grounds other than the state's own domestic law.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Present protection claims that, under the sovereignty-primary reading, are treated as external humanitarian appeals to sovereign discretion rather than as claims the state's authority is constituted to answer. Face pushback, prolonged detention, or return to danger; their only recourse is the same state apparatus asserting the discretion against them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers_at_borders, payer,
    powerless, immediate, trapped, global).

% Already inside the territory, performing labor the economy depends on, but hold no legal claim against removal because the sovereignty-primary frame treats their presence as tolerated discretion rather than entitlement. Cannot appeal to labor contribution or duration of residence as a constraint on the state's exclusion authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Issue findings and treaty interpretations holding that non-refoulement and due process constrain exclusion authority. Under this reading their findings are treated as external, non-binding moral commentary rather than constitutive limits on legitimate state authority, so their institutional voice has no enforceable purchase on the constraint.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% Study how sovereignty doctrine has been invoked historically to justify exclusion regimes, tracing the doctrine's genealogy from Chae Chan Ping-era plenary power jurisprudence to contemporary border policy, and comparing it against jurisdictional and rights-based framings.
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
% COORDINATION_FUNCTION: Provides a state with a clear, self-contained rule for who may enter and remain, avoiding the coordination costs of case-by-case adjudication against external standards and allowing rapid, unilateral response to migration flows.
% TRANSFER_FUNCTION: Moves protection, labor-market access, and physical safety away from non-citizens seeking entry or residence and toward the receiving state's incumbent citizens and the enforcement apparatus tasked with excluding them; also transfers adjudicative authority away from international bodies and toward the excluding state itself.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and undocumented residents have no standing within the very legal order whose discretion is at issue; international human rights bodies speak but are structurally positioned as external commentators rather than co-authorities, so their objections are heard without being binding.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary doctrine were abandoned overnight, exclusion decisions would become justiciable against external human-rights and jurisdictional-balancing standards, enforcement budgets and detention capacity would face new legal exposure, and admission criteria would need to answer to labor-need and protection-obligation tests rather than to unilateral state discretion alone — the entire enforcement-plus-adjudication architecture would have to be rebuilt around a different legitimacy premise.
% FOUNDING_PROBLEM: Nascent and consolidating states needed a doctrine that would let courts and other states defer to a state's control of its own territory without perpetual external second-guessing, in an era when territorial control was itself contested and fragile.
% FOUNDING_PROBLEM_CORROBORATION: The receiving state apparatus and enforcement industry attest the doctrine remains necessary against unauthorized mass migration and security threats. Comparative legal scholars and international human rights bodies — outside the beneficiary set — attest that the plenary-power version of the doctrine has been substantially decoupled from any live threat to territorial control in consolidated states, and instead now functions primarily to insulate exclusion decisions from judicial and treaty-based review; that reading is corroborated by dissenting judicial opinions within the receiving states' own court systems, not only by external critics.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is substantial (0.62) but not extreme: the doctrine does perform a real coordination function (a state does need some rule for admission, and unlimited case-by-case external adjudication has genuine costs), but the 'absolute discretion' framing goes well beyond what is needed to solve that coordination problem, transferring costs asymmetrically onto excluded non-citizens who have no standing to contest the decision. Suppression is high and rising (0.58 to 0.81) because the doctrine's core mechanism is precisely the foreclosure of external and internal legal avenues — detention, summary removal, and denial of judicial review are suppression instruments, not incidental effects. Theater ratio is comparatively low and only modestly rising (0.14 to 0.28): most of the enforcement apparatus performs its stated function (excluding people) rather than merely performing exclusion theatrically, though performative elements (high-visibility enforcement operations, symbolic wall construction) have grown as domestic political returns to visible enforcement have increased.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state's seat, the arrangement reads as a mountain-adjacent claim: an irreducible feature of what statehood IS, not a policy this or any state chose. From the excluded migrant's seat, the identical rule reads as an actively enforced, coercively maintained extraction of safety, opportunity, and legal voice — a tangled rope wearing mountain clothing. The engine's per-seat computation is expected to surface exactly this divergence: the powerful/institutional agenda-setter seat likely computes closer to coordination, the powerless/trapped payer seats closer to extraction, without either seat's computation being privileged as 'the' answer.
 *
 * DIRECTIONALITY LOGIC:
 *   The receiving state apparatus sits at the pure agenda-setter end: it writes the discretion, administers it, and answers to no external adjudicator under this reading. Citizen labor incumbents and the enforcement industry are structural beneficiaries with low derived d — wage protection and contract revenue flow to them without them bearing enforcement costs. Excluded migrants, asylum seekers, and undocumented residents sit at the target end with the highest derived d: trapped exit options, powerless power atom, and explicit victim declaration compound to push effective extraction toward the full-target pole. International human rights bodies are institutional but structurally excluded from adjudicative purchase under this reading — their institutional power does not translate into constraint-specific leverage, which is itself the point of the sovereignty-primary framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding perpetual external second-guessing of fragile territorial control) was live when interstate territorial control was itself contested. In consolidated modern states the underlying problem this doctrine addresses (uncertain territorial control) is largely dead, yet the plenary discretion the doctrine grants has not narrowed — if anything it has hardened (rising suppression_requirement series) even as the original justification weakened. This is the signature the mandatrophy analysis is built to catch: a founding-problem-status of 'contested' paired with a disappearance_verdict of 'world_rearranges' flags exactly the zombie-mandate pattern, distinguishable from genuine ongoing coordination need only by looking outside the beneficiary set for corroboration, which this story does via dissenting domestic judicial opinions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_kernel_reading_selection,
    'Is ''border control is constitutive of statehood'' a defensible reading of territorial sovereignty, or does it smuggle in a much stronger claim (absolute, non-justiciable discretion) than the jurisdictional concept of sovereignty actually requires?',
    'Comparative doctrinal analysis: does removing unbounded exclusion discretion (adopting the jurisdictional_sovereignty reading instead) leave a state''s territorial sovereignty otherwise intact in every other domain (taxation, criminal jurisdiction, resource control)? If yes, the sovereignty_primary reading''s constitutive claim is not load-bearing and is doing extractive rather than definitional work.',
    'If sovereignty is fully intact without absolute exclusion discretion, the sovereignty_primary reading''s naturalizing claim (that this is what statehood inherently requires) collapses, and the constraint is better read as tangled_rope or even snare rather than as approaching a mountain-like necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_kernel_reading_selection, conceptual, 'Whether absolute exclusion discretion is genuinely constitutive of sovereignty or an extractive overlay on jurisdictional authority.').

omega_variable(
    human_rights_external_or_constitutive,
    'Are non-refoulement and due process obligations best understood as external limits imposed on sovereign authority from outside (the sovereignty_primary framing), or as constitutive of what makes the exercise of that authority legitimate in the first place (closer to the jurisdictional_sovereignty and freedom_of_movement_primary framings)?',
    'Track whether domestic courts within states that formally hold the sovereignty_primary doctrine nonetheless increasingly treat rights obligations as internal constitutional constraints (via incorporation of international law into domestic constitutional review) rather than external treaty appeals — a trend would indicate the external framing is eroding even within its own tradition.',
    'If courts increasingly treat these obligations as internal/constitutive, the sovereignty_primary reading''s core axiom is being overridden from within its own legal tradition, which is exactly the axiom_overriding drift this reading''s cs_structure should register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_external_or_constitutive, empirical, 'Whether the external-limit framing of human rights obligations is stable or eroding within the sovereignty_primary tradition itself.').

omega_variable(
    sibling_reading_disagreement_location,
    'Where exactly do the three kernel readings diverge — is it (a) whether sovereignty is jurisdictional authority at all, (b) whether that authority extends to exclusion, or (c) whether exclusion authority is bounded by external protection/consent tests?',
    'Structural comparison of the three sibling stories'' axioms: sovereignty_primary and jurisdictional_sovereignty agree sovereignty is jurisdictional authority (disagreement is on (b)/(c)); freedom_of_movement_primary disagrees at a more fundamental level, denying that sovereignty entails any exclusion authority over movement as a human right.',
    'Locating the disagreement precisely determines which sibling relation is correct: sovereignty_primary and freedom_of_movement_primary directly contradict each other''s core premise (forecloses), while sovereignty_primary and jurisdictional_sovereignty disagree on the scope/boundedness of the same underlying authority claim (coexists_with, since both remain live positions among different courts and scholars).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_location, conceptual, 'Precise location of the kernel disagreement across the three sibling readings, used to fix reading_relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__sovereignty_primary, theater_ratio, 8, 0.17).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__sovereignty_primary, theater_ratio, 16, 0.2).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__sovereignty_primary, theater_ratio, 24, 0.23).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__sovereignty_primary, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__sovereignty_primary, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__sovereignty_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__sovereignty_primary, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__sovereignty_primary, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__sovereignty_primary, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__sovereignty_primary, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__sovereignty_primary, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__sovereignty_primary, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, asylum_adjudication_deference_doctrine).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the border_control_legitimacy kernel. sovereignty_primary claims absolute, non-justiciable exclusion discretion as constitutive of statehood (this file); jurisdictional_sovereignty claims real jurisdictional authority bounded by protection/labor/consent balancing tests (separate file); freedom_of_movement_primary denies that sovereignty entails exclusion authority at all, treating movement as a fundamental right (separate file). The three share no single epsilon value — each reading produces a structurally distinct beneficiary/victim configuration and a distinct classification, linked here rather than merged into one measurement-dependent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
