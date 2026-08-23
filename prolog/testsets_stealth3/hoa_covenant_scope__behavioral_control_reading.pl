% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral-Scope Enforcement (Behavioral-Control Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   A recorded declaration of covenants, conditions and restrictions governs
 *   a planned community: an elected board runs architectural review, hears
 *   complaints, fines deviations, and can lien lots. This file instantiates
 *   ONE reading of the contested kernel hoa_covenant_scope — the
 *   behavioral_control_reading: the covenant's operative function is
 *   enforcing aesthetic uniformity and behavioral conformity as a
 *   property-value-maximization strategy. Per the epsilon-invariance rule,
 *   this reading is authored as a clean constraint with a single stable
 *   epsilon (0.46) over the standing arrangement — the covenant regime as
 *   practiced — assessed by this reading's own lights. The sibling readings,
 *   hoa_covenant_scope__coordination_reading (genuine externality
 *   coordination) and hoa_covenant_scope__extraction_reading (revenue
 *   generation and board power consolidation), are separate constraint
 *   stories with their own epsilon values, victim sets, and classifications;
 *   they are linked through network.affects_constraints, and the contest
 *   among them is recorded in the omega variables rather than hedged inside
 *   this constraint's metrics. KEY AGENTS (by structural relationship): -
 *   covenant_board: agenda-setter (organized/constrained) — adopts,
 *   interprets, and enforces the behavioral standard; levies fines and liens
 *   - conformist_majority: primary beneficiary (organized/constrained) —
 *   holds conforming lots, captures the uniform-street premium, votes the
 *   standard into place - board_aligned_homeowners: secondary beneficiary
 *   (moderate/constrained) — receives selective leniency and procedural
 *   advantage - nonconformist_homeowners: primary target
 *   (moderate/constrained) — bears fines, compliance costs, and suppressed
 *   property choices - marginal_aesthetic_households: primary target, least
 *   mobile (powerless/trapped) — bears compounding fines and lien/foreclosure
 *   exposure - political_expression_households: primary target on the speech
 *   dimension (moderate/constrained) — bears fines for signs, flags, and
 *   displays - renters_in_community: excluded seat (powerless/mobile) — bound
 *   by the rules through leases with no vote - state_courts: analytical
 *   observer (institutional/analytical) — adjudicates scope disputes across
 *   many communities
 *
 * KEY AGENTS:
 *   - covenant_board: agenda-setter (organized/constrained) — adopts, interprets, and enforces the behavioral standard
 *   - conformist_majority: primary beneficiary (organized/constrained) — captures the uniform-street premium and votes the standard into place
 *   - board_aligned_homeowners: secondary beneficiary (moderate/constrained) — selective leniency and procedural advantage
 *   - nonconformist_homeowners: primary target (moderate/constrained) — fines, forced compliance, suppressed property choices
 *   - marginal_aesthetic_households: primary target, least mobile (powerless/trapped) — fine/lien spiral on the primary asset
 *   - political_expression_households: primary target on the speech dimension (moderate/constrained) — fines for signs, flags, displays
 *   - renters_in_community: excluded seat (powerless/mobile) — bound by rules, no vote
 *   - state_courts: analytical observer (institutional/analytical) — sees the cross-association pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.46).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.62).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral-Scope Enforcement (Behavioral-Control Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, 'e48a1062-1c18-4f60-bf0b-001d6a89aaa8').
narrative_ontology:cs_kernel_codification('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', fixed_text).
narrative_ontology:cs_authority_grounding('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', lineage).
narrative_ontology:cs_interpretation_layer_present('e48a1062-1c18-4f60-bf0b-001d6a89aaa8').
narrative_ontology:cs_reading_relation('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', foundational, aesthetic_uniformity_protects_property_values).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_protects_property_values, holdable).
narrative_ontology:cs_axiom_grounding('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', aesthetic_uniformity_protects_property_values, empirically_contingent).
narrative_ontology:cs_axiom('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', foundational, purchased_consent_extends_to_behavioral_scope).
narrative_ontology:cs_axiom_status(purchased_consent_extends_to_behavioral_scope, holdable).
narrative_ontology:cs_axiom_grounding('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', purchased_consent_extends_to_behavioral_scope, conventional).
narrative_ontology:cs_reference_frame('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', uniform_aesthetic_character_as_protected_asset).
narrative_ontology:cs_drift_state('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', contemporary_statutory_carveout_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e48a1062-1c18-4f60-bf0b-001d6a89aaa8', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_households).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, political_expression_households).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, property_value_maximization_doctrine).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, covenant_running_with_land_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected volunteer board that adopts and amends the community's rules, runs architectural review, hears violations, levies fines, and can attach liens to delinquent lots. Its members are also homeowners subject to the same rules; they typically serve a few years, and their workload and legal exposure rise with every rule they add. Resigning ends the role but not their residence under the rules.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, covenant_board, agenda_setter,
    organized, biographical, constrained, local).

% Households whose homes and habits already match the community standard. They receive no violation notices, face no review, and enjoy the street appearance they prefer; their lots appraise under the uniform-street premium the standard is said to protect. They vote in board elections and can sell into the same market the standard supports; moving is costly but ordinary.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, constrained, local).

% Households close to the board socially or through service — past officers, committee members, and their allies. They receive faster approvals, informal warnings instead of fines, and a voice in which complaints get pursued. Their standing inside the community depends on the rules staying active; selling would end that standing along with the residence.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Households whose property choices depart from the standard — paint colors, fencing, gardens, holiday displays, vehicle parking. They receive violation notices, pay fines, repaint or remove at their own cost, and attend hearings to defend choices the standard treats as defects. They can comply, litigate a single dispute, or sell; the covenant runs with the land, so a buyer inherits the same rules, and a home with a violation history sells at a discount.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    moderate, biographical, constrained, local).

% Households at the edge of their budgets — fixed incomes, job loss, divorce — whose upkeep slips below the standard for reasons of money rather than taste. Fines accumulate faster than they can pay, liens threaten the primary asset they own, and neither a move nor a lawyer is within reach; compliance itself costs more than they can spend.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_households, payer,
    powerless, biographical, trapped, local).

% Households that display yard signs, flags, religious symbols, or seasonal decorations expressing political or religious identity. Rules cap sign size, duration, and content, and fines attach to display; several states have begun carving these displays out of enforceable rules. Their options are removing the display, paying the fine, or litigating under the newer statutes.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, political_expression_households, payer,
    moderate, biographical, constrained, local).

% Tenants in covenant-governed homes. Leases pass the rules through to them — parking, guests, decor, noise — but they hold no vote in board elections, cannot attend most hearings as members, and bear fines passed through by landlords. They can leave at lease end without selling anything, which makes their exposure short and their voice zero.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, renters_in_community, excluded,
    powerless, biographical, mobile, local).

% State courts adjudicate covenant-enforcement disputes across many communities: they interpret the recorded instruments, apply statutory carve-outs for signs and flags, police selective enforcement, and set the doctrine that determines how far behavioral rules can reach. They see the pattern across hundreds of associations that no single household can see.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, state_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a single community-wide standard of property appearance and conduct: one shared expectation of what a lot, home, and household may look like, adopted once and applied to every household, so that appearance and behavior decisions are pre-committed rather than negotiated neighbor by neighbor.
% TRANSFER_FUNCTION: Moves fine revenue, forced compliance spending (repainting, landscaping, removal of displays), and expressive latitude from households whose choices depart from the standard toward the appraised values of conforming households and the board's enforcement discretion; fines collected fund the enforcement machinery itself.
% ABSENT_VOICES: Renters bound by the rules through leases but holding no vote; prospective buyers whose preferences the standard screens out before they ever see a listing; households priced out of compliance who cannot attend hearings or fund appeals — all would object to the behavioral scope, and none is in the governance conversation.
% DISAPPEARANCE_RATIONALE: If the behavioral rules and their enforcement machinery vanished overnight, paint colors, fences, gardens, signs, and flags would diversify within a season, the architectural review docket and fine schedule would empty, board elections would lose their enforcement stakes, and appraisals would reprice the street from a uniform-product premium toward ordinary market variance; households currently fined would redirect compliance spending.
% FOUNDING_PROBLEM: The covenant's behavioral scope was built to keep the community's appearance and population 'desirable' so lot values would hold: originally through racially restrictive clauses, and after courts struck those down, through aesthetic and conduct standards that carried the same protective function in facially neutral form.
% FOUNDING_PROBLEM_CORROBORATION: The exclusionary genealogy is corroborated outside the benefiting parties by the case-law record itself (state and federal decisions on racially restrictive covenants and their migration into aesthetic standards) and by land-use historians. Whether uniformity still protects values is disputed: industry-sponsored surveys attest the premium, while independent hedonic studies attribute much of it to self-selection; no disinterested party attests the behavioral-control function as currently practiced, which this reading treats as signal rather than gap.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 (moderate, within the expected 0.35-0.50 band): the behavioral scope reaches paint, landscaping, fencing, vehicles, decorations, signage, and flags, and its enforcement moves real money (fines, forced repainting) and real freedom (suppressed use and display of one's own lot) away from nonconforming households — while the same machinery also processes ordinary maintenance, which this reading reads as the intake that makes the conformity function durable rather than as a separate function. Suppression is 0.62 and structural: fines escalate, liens attach to the primary asset, attorney-fee provisions deter single-household litigation, and the covenant runs with the land so selling does not exit it; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater_ratio is 0.30: 'community character' hearings and architectural review are partly ratification of predetermined taste judgments, but the machinery genuinely enforces, so performance remains a minority share. Accessibility_collapse is 0.48: alternatives (comply, sell at a covenant-bound price, litigate one dispute, campaign for statutory carve-outs) remain partly open. Resistance is 0.55: selective-enforcement litigation, speech carve-out statutes, board-election contests, and press coverage of fine spirals. The measurement series share one grid (t=0..36 by 6) so every tracked metric is authored at every examined time point; enforcement intensity oscillates seasonally with complaint waves, but the decade-scale trend in the record is monotonic intensification, so a monotonic grid rather than a cyclical one is honest here. Coalition note: marginal-aesthetic households are individually powerless, but the record shows coalitions work — statutory flag and sign protections were won by exactly such cross-household campaigns, which is why their exit is authored as trapped while their resistance potential is real.
 *
 * PERSPECTIVAL GAP:
 *   From the conformist-majority seat the arrangement computes as a low-burden taste standard they already satisfy and vote for; from the payer seats — nonconforming, marginal, and expression-displaying households — the same rules compute as a machine that converts deviation into fines and the primary asset into collateral. The board seat sees administration; the excluded renter seat sees rules without voice. The engine computes these per-seat classifications from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The conformist majority and board-aligned households sit at the beneficiary end of d: they pay no enforcement costs, capture the appraised premium, and in the aligned case receive procedural leniency. The three payer groups sit at the target end with graded intensity: nonconforming homeowners (moderate power, constrained exit) bear the standard enforcement burden; marginal-aesthetic households (powerless, trapped — the fine/lien cycle consumes the asset they cannot sell) sit nearest the full-target end; political-expression households bear the speech-suppression slice. Renters are excluded rather than coordinated — bound by the rules with no seat, which the derivation reads as target-side with zero voice. The board is the agenda-setter: it administers the machinery and its aligned constituents collect, but its members are also homeowners under the rules, so its structural position sits between the beneficiary and administrator poles. No directionality overrides are needed: the beneficiary and victim declarations plus exit options produce the intended d values for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim prevents the coordination cover from absorbing the analysis: the arrangement's persistence requires active enforcement against identifiable payers, not merely the solving of a shared problem. The R5 mismatch check runs clean here only because the founding problem's status is authored as contested rather than dead: the value-protection rationale is still asserted by the beneficiaries and still litigated outside them, so the arrangement is not yet a zombie mandate — but if independent appraisal evidence settled against the uniformity premium, status would flip to dead while world_rearranges held, flagging capture. The sibling extraction_reading file carries the fine-proliferation and board-power dynamics that this reading treats as downstream; keeping them in a separate file is what stops this story from double-counting the revenue mechanism as conformity enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (behavioral_control_reading) of the kernel hoa_covenant_scope: would instantiating the coordination_reading or extraction_reading instead change the structural facts, and where exactly is the disagreement located?',
    'Read the sibling stories (hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading) against the same violation docket: if the docket is dominated by externality-relevant maintenance, the coordination reading gains; if by fine proliferation and selectively prosecuted complaints, the extraction reading gains; if by taste, lifestyle, and expression policing, this reading holds.',
    'Under the coordination reading the victim set empties and the arrangement classifies as a low-burden coordination device; under the extraction reading the capturer seat moves to the board and fine revenue becomes the primary flow. This file''s snare classification stands only under the behavioral-control function attribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the hoa_covenant_scope kernel the enforcement record actually instantiates; the disagreement is located in the covenant''s dominant function and consequently in its victim set and gain flow.').

omega_variable(
    uniformity_value_premium_empirics,
    'Does aesthetic uniformity actually produce a durable property-value premium, or is the observed premium an artifact of self-selection into covenant communities?',
    'Hedonic pricing studies controlling for self-selection and community demographics, ideally exploiting quasi-random covenant adoption or jurisdictional boundary discontinuities.',
    'If no durable premium exists, the value-maximization justification collapses, effective extractiveness rises toward the pure-conformity end, and the founding problem''s status flips from contested to dead while the arrangement persists; if the premium is real, part of the payer burden prices as an amenity the market will pay for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_value_premium_empirics, empirical, 'Whether the uniformity-premium premise of the behavioral scope survives selection-controlled appraisal analysis.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) structural (escalating fines, liens on the primary asset, attorney-fee shifting, covenant runs with the land) or internalized (households self-censor displays and property choices before any enforcement contact)?',
    'Post-exit trajectory: compare display and maintenance choices of households that sold out of covenant communities into uncovenanted ones; persistence of self-censorship after exit indicates internalization.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure — the standard travels with the household — and the behavioral scope reaches further than its enforcement docket shows; the omega variable, not the scalar, carries this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized share of the covenant''s suppressive force.').

omega_variable(
    behavioral_scope_boundary,
    'Is there a principled boundary between externality-relevant conduct and pure-taste enforcement within the recorded standards, or is the boundary whatever the architectural committee says it is on a given complaint?',
    'Cross-association code comparison plus docket analysis: if materially identical instruments produce widely divergent violation mixes under different boards, the boundary is discretionary; if violation mixes converge, the recorded text binds.',
    'If the boundary is discretionary, the measured extractiveness is a property of board discretion rather than of the instrument, and scope creep — not the recorded covenant — is the operative engine; classification would then track the discretion structure rather than the text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_scope_boundary, conceptual, 'Whether the behavioral scope''s reach is fixed by the recorded standards or set by board discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_behavioral_control_tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t0, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t6, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t6, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t12, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t12, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t18, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t18, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t24, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t30, observed).
narrative_ontology:measurement(hoa_behavioral_control_tr_t36, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement_basis(hoa_behavioral_control_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(hoa_behavioral_control_be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t0, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t6, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t6, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t12, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t12, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t18, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 18, 0.41).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t18, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.43).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t24, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t30, observed).
narrative_ontology:measurement(hoa_behavioral_control_be_t36, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 36, 0.46).
narrative_ontology:measurement_basis(hoa_behavioral_control_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa_behavioral_control_su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t0, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t6, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t6, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t12, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t12, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t18, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t18, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t24, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t30, observed).
narrative_ontology:measurement(hoa_behavioral_control_su_t36, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement_basis(hoa_behavioral_control_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the HOA covenant' conflates three structurally distinct claims about one recorded instrument: genuine externality coordination (hoa_covenant_scope__coordination_reading), behavioral conformity enforcement as value strategy (this file), and fine-based revenue and board-power consolidation (hoa_covenant_scope__extraction_reading). Per the epsilon-invariance principle each is a separate story with its own epsilon, beneficiaries, victims, and claimed type, linked here through affects_constraints. The behavioral reading sits upstream of the extraction reading: every behavioral rule added enlarges the violation surface on which fine and selective-enforcement machinery feeds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
