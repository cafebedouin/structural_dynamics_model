% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Absolute Sovereignty Norm — Sovereignty-Maximalist Reading of the RBIO Practice Norm Complex
 *   domain: international relations/international law/political economy
 *
 * SUMMARY:
 *   The sovereignty-maximalist reading of the rules-based international order
 *   holds three commitments as one: state sovereignty is absolute; order
 *   norms are legitimate only when they protect sovereignty against external
 *   interference; humanitarian exceptions are pretexts for regime change.
 *   This story instantiates that reading as a standing arrangement of
 *   international practice — the Westphalian-Charter non-intervention
 *   baseline defended without exception, no legitimate intervention authority
 *   beyond self-defense, conditionality tolerable only where the target can
 *   exit without cost. The arrangement has a genuine coordination face: it is
 *   the protection small and middle states explicitly demand, the reason the
 *   great powers do not routinely rescue, raid, or reorganize one another's
 *   internal affairs, and the settlement that closed the era in which
 *   cross-border force answered to religion, dynasty, or imperial mission. It
 *   also has an extraction face the reading's own frame cannot register: the
 *   same baseline seals repressive governments against external
 *   accountability and leaves their populations without institutional
 *   recourse — by design, since the reading's own content removes every
 *   external channel on their behalf. The claim/metric gap is deliberate and
 *   is the point of the story: the reading presents the norm as an absolute,
 *   natural feature of international order (a mountain claim), while the
 *   authored metrics describe substantial, actively enforced extraction whose
 *   burden falls on the people the norm's consent fiction speaks for. This is
 *   intentional false-summit authoring: the beneficiary declarations are
 *   present so the engine can evaluate the naturality claim against the
 *   beneficiary structure. KEY AGENTS (by structural relationship): -
 *   authoritarian_regimes: primary beneficiary (organized/identity_locked) —
 *   the absolute reading is their shield; internal repression stays off the
 *   international table - sovereignty_shielding_great_powers: agenda-setter
 *   and secondary beneficiary (institutional/arbitrage) — enforce the
 *   baseline by veto while exempting their own projection case by case -
 *   small_state_governments: secondary beneficiary (moderate/constrained) —
 *   genuine protection against great-power predation; defend the baseline in
 *   bloc politics - populations_under_repressive_governments: primary target
 *   (powerless/trapped) — bear the arrangement's cost: no rescue, no
 *   leverage, no institutional address -
 *   dissidents_and_civil_society_in_closed_states: target (powerless/trapped)
 *   — external ties reclassified as interference and used against them -
 *   liberal_democratic_governments: dual-positioned payer/beneficiary
 *   (powerful/mobile) — forfeit the humanitarian toolkit, receive immunity;
 *   compliance largely willing - humanitarian_intervention_advocates:
 *   excluded (organized/constrained) — present in discourse, pre-classified
 *   as pretext, absent from decision - international_law_scholarship:
 *   analytical observer — maps the consent fiction and the claim/practice gap
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: primary beneficiary (organized/identity_locked) — shielded from external accountability; identity fused with the sovereignty claim
 *   - sovereignty_shielding_great_powers: agenda-setter and secondary beneficiary (institutional/arbitrage) — veto enforcement plus case-by-case self-exemption
 *   - small_state_governments: secondary beneficiary (moderate/constrained) — real protection value, cannot enforce alone, bundled with the shield given to repressors
 *   - populations_under_repressive_governments: primary target (powerless/trapped) — every external channel foreclosed by the reading's own content
 *   - dissidents_and_civil_society_in_closed_states: target (powerless/trapped) — external support reclassified as interference
 *   - liberal_democratic_governments: dual-positioned payer/beneficiary (powerful/mobile) — forfeit the humanitarian toolkit, receive immunity, comply willingly
 *   - humanitarian_intervention_advocates: excluded (organized/constrained) — R2P coalition with no operative pathway
 *   - international_law_scholarship: analytical observer (analytical/analytical) — documents the consent fiction and selective practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, mountain).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Absolute Sovereignty Norm — Sovereignty-Maximalist Reading of the RBIO Practice Norm Complex").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international relations/international law/political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).
domain_priors:emerges_naturally(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '55a745cd-bac3-4378-abae-d7b6485e08c6').
narrative_ontology:cs_kernel_codification('55a745cd-bac3-4378-abae-d7b6485e08c6', fixed_text).
narrative_ontology:cs_authority_grounding('55a745cd-bac3-4378-abae-d7b6485e08c6', lineage).
narrative_ontology:cs_interpretation_layer_present('55a745cd-bac3-4378-abae-d7b6485e08c6').
narrative_ontology:cs_reading_relation('55a745cd-bac3-4378-abae-d7b6485e08c6', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('55a745cd-bac3-4378-abae-d7b6485e08c6', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('55a745cd-bac3-4378-abae-d7b6485e08c6', foundational, no_intervention_authority_beyond_self_defense).
narrative_ontology:cs_axiom_status(no_intervention_authority_beyond_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('55a745cd-bac3-4378-abae-d7b6485e08c6', no_intervention_authority_beyond_self_defense, conventional).
narrative_ontology:cs_axiom('55a745cd-bac3-4378-abae-d7b6485e08c6', foundational, humanitarian_exceptions_categorically_pretextual).
narrative_ontology:cs_axiom_status(humanitarian_exceptions_categorically_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('55a745cd-bac3-4378-abae-d7b6485e08c6', humanitarian_exceptions_categorically_pretextual, empirically_contingent).
narrative_ontology:cs_axiom('55a745cd-bac3-4378-abae-d7b6485e08c6', secondary, conditionality_requires_costless_exit).
narrative_ontology:cs_axiom_status(conditionality_requires_costless_exit, holdable).
narrative_ontology:cs_axiom_grounding('55a745cd-bac3-4378-abae-d7b6485e08c6', conditionality_requires_costless_exit, instrumental).
narrative_ontology:cs_reference_frame('55a745cd-bac3-4378-abae-d7b6485e08c6', westphalian_charter_sovereign_equality).
narrative_ontology:cs_drift_state('55a745cd-bac3-4378-abae-d7b6485e08c6', post_libya_backlash_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55a745cd-bac3-4378-abae-d7b6485e08c6', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_state_governments).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_shielding_great_powers).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, dissidents_and_civil_society_in_closed_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_governments).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_non_intervention_principle).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, domestic_jurisdiction_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern through repression and rely on the absolute reading of non-intervention to keep sanctions, diplomatic pressure, and rescue operations for their own populations off the international table. They invoke sovereign inviolability in UN blocs and treaty bodies whenever their internal conduct is raised. Their legitimacy narrative is fused with the sovereignty claim itself — conceding any external accountability role would require dismantling the regime's self-description — so forgoing the shield is not a live option from where they stand.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    organized, generational, identity_locked, global).

% Hold veto power in the Security Council and lead the diplomatic blocs that defend the non-intervention baseline. They block authorization of humanitarian operations that would set precedents against themselves or their clients, while retaining practical freedom to project force into weaker states when their interests require, framing each use case by case. The arrangement costs them little and protects both their internal conduct and their clients.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_shielding_great_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_shielding_great_powers, beneficiary).

% Lack the military weight to deter great-power intervention on their own and rely on the non-intervention baseline as their main protection against predation by larger neighbors. They defend the reading in the General Assembly and regional bodies. Their protection is real but bundled with the same rule that shields repressive governments elsewhere; they cannot accept the shield while waiving it for others, and they cannot enforce it alone.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, small_state_governments, beneficiary,
    moderate, generational, constrained, national).

% Live under governments that jail, displace, or starve them, with the absolute reading foreclosing every external channel: no intervention to stop atrocities, no sanctions leverage argued on their behalf, no external protection force. Their governments claim to consent on their behalf. Emigration is the only individual way out and is itself controlled by the state; collective recourse has no institutional address.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, biographical, trapped, national).

% Organize opposition inside closed states and seek funding, sanctions pressure, and diplomatic advocacy from abroad. Under the absolute reading, every such link is reclassified as foreign interference, which their governments use to justify arrests and shutdowns. Their international allies cannot act without confirming the interference charge, so support arrives late, deniably, or not at all.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, dissidents_and_civil_society_in_closed_states, payer,
    powerless, biographical, trapped, national).

% Accept the non-intervention baseline and receive its immunity for their own conduct, but forfeit the humanitarian toolkit — no-notice rescue operations, coercive conditionality, protective intervention — that their publics and parliaments sometimes demand. They can act unilaterally when they judge it necessary, as over Kosovo, at the cost of legal isolation and precedents they cannot control. Their compliance is largely willing, which keeps the enforcement burden on the arrangement's defenders low.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_governments, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_governments, beneficiary).

% R2P coalitions, atrocity-prevention organizations, and the interventionist wings of foreign ministries argue for a legitimate, guarded humanitarian exception. The reading pre-classifies their claims as regime-change pretexts, so their proposals reach Security Council discussion only to be vetoed, and their doctrine has no operative pathway. They remain in the discourse but out of the decision.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_advocates, excluded,
    organized, generational, constrained, global).

% Map the doctrinal structure: the Charter text, the consent practice by which governments speak for the governed, and the gap between the absolute claim and selective state practice. They document how the reading's holders behave when the shield and their projection interests conflict, and they publish the counterfactuals — Rwanda, Kosovo, Libya — that both sides of the dispute cite.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of universal interventionism: without a shared non-interference baseline, every state's internal affairs are a standing invitation to rivals' force, and the system reproduces the pre-1945 pattern of serial intervention, proxy war, and great-power collision. The baseline lets each government rely on not being targeted if it does not target, and lets small states exist without great-power permission.
% TRANSFER_FUNCTION: Moves immunity from external accountability to incumbent governments — concentrated where repression is heaviest, since those regimes have the most to shield — and moves the cost of that immunity to the governed populations of shielded states, in the form of forgone rescue, forgone leverage, and the criminalization of their external ties. Legitimacy flows to sovereignty-invoking states; the price of silence is paid by the people the silence covers.
% ABSENT_VOICES: The populations the reading's consent practice speaks for: dissidents and atrocity survivors inside closed states have no seat in the General Assembly caucuses, Security Council consultations, or treaty-body negotiations where the absolute reading is defended — their governments occupy the state's seat and consent on their behalf. Humanitarian-intervention advocates are present in discourse but excluded from decision, their claims pre-classified as pretext. Where they are: inside the sealed states, and outside the voting rooms.
% DISAPPEARANCE_RATIONALE: Intervention decisions would proliferate immediately and in both directions: rescue operations against ongoing atrocities that the veto now blocks, and predatory interventions into weak states that the baseline now deters. Alliance structures, UN Charter architecture, regime-survival expectations, and the bloc politics of the General Assembly all presuppose the baseline; its overnight removal would force every state to re-price its security, producing a period of great-power friction before any new equilibrium.
% FOUNDING_PROBLEM: Serial cross-border force justified by religion, dynasty, civilization, or imperial interest — the European wars of religion, the imperial partition era, and finally the world wars — plus, later, the vulnerability of newly decolonized states to their former rulers. The Westphalian tradition and then the UN Charter built non-intervention to remove the justifications for interstate war by placing internal affairs off-limits.
% FOUNDING_PROBLEM_CORROBORATION: Charter drafting records and diplomatic historians corroborate the founding problem from outside the current beneficiary core: the small and middle states at San Francisco demanded non-intervention protection against great-power predation, and the decolonization-era diplomatic record shows new states seeking the same shield. International legal scholarship (the observer seat) attests the genealogy. What the parties dispute is whether that problem still binds: the reading's holders attest it is live, while atrocity-prevention scholarship and the R2P coalition attest the founding problem has partially mutated — interstate predation receded while the shield now protects perpetrators of internal atrocity. No attesting source is fully outside every beneficiary set, since all states gain some protection value; the corroboration that matters is that the genealogy is attested from seats independent of the authoritarian-beneficiary core.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rbio_practice_norm_complex__sovereignty_maximalist_reading),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the arrangement's cost is concentrated and structural: a large fraction of humanity lives under governments the baseline shields, and the reading's own content forecloses every external channel on those populations' behalf; the offsetting coordination value is real but accrues at the state level, not to the people paying. Suppression is 0.62 and predominantly structural — the channels are institutionally foreclosed (veto, bloc discipline, the interference reclassification) rather than internally enforced, though the reading cultivates an internalized component: anti-imperial socialization under which foreclosed recourse feels like protection; the consent_fiction omega carries that ambiguity. Theater is 0.58 and rising: a growing share of the reading's operation is selective invocation — annexation and projection conducted while invoking inviolability — which makes the shield's defense increasingly performative. Accessibility collapse is 0.55: alternatives (a guarded humanitarian exception, R2P, conditionality) were practiced within living memory and remain conceivable, but each application that was gamed is used to foreclose the next, so the alternative space is closing without being conceptually eliminated. Resistance is 0.55: a live counter-coalition contests the reading and is currently losing. The temporal series run on one shared eight-point grid. The extractiveness series shows one full cycle over the interval: the exception window of the 1990s-2000s opened recourse (extraction dips to 0.50 around the R2P moment), then the gamed Libya application closed it (extraction climbs to 0.68) — and the cycle itself functions as an extraction mechanism, because each abused exception becomes the reading's strongest argument for absoluteness. Suppression rises through the contest years, peaks after Libya, then eases as the reading re-consolidates and needs less active defense. Theater rises monotonically throughout: the rhetorical use of the reading decouples from its order function continuously.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same structure. From the trapped-population seat the arrangement is a sealed room: full-target position, no exit, no institutional address — the reading's design removes their recourse and their governments consent on their behalf. From the small-state seat it is genuine protection they explicitly demand and cannot enforce alone. From the authoritarian-beneficiary seat it is a subsidy fused with regime identity — forgoing it would dissolve the regime's self-description, so the seat is locked to the arrangement it feeds on. From the great-power seat it is cheap arbitrage: veto-enforced against others' interventions, case-by-case waived for their own. The reading's holders see only the protection face and author the arrangement as mutual restraint; the declared victim structure forces the engine to compute the sealed-room experience the frame cannot register. Coalition check: the victim seats' natural coalition path — cross-border dissident solidarity, externally funded civil society — is foreclosed by the same structure, since external ties are the interference charge; powerless victims therefore cannot convert numbers into leverage inside this arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (authoritarian regimes, small states, great powers) derive low directionality; the victim declarations (trapped populations, dissidents) derive near-full-target position, with trapped exit holding them at the target end rather than letting mobility damp it. The great powers' agenda-setter role does not raise their d: they bear almost none of the arrangement's cost, and their arbitrage-grade exit keeps them at the beneficiary end despite running its enforcement. One override is authored: the powerful atom is set to 0.45 for the liberal-democratic seat, which is genuinely dual-positioned (forfeits the humanitarian toolkit, receives immunity, complies willingly) but appears in neither the beneficiary nor victim arrays, so the structural derivation cannot see its dual position and would substitute the powerful-atom fallback — misreading willing compliance as extraction. 0.45 places the seat near symmetric, which is what its situation describes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — serial cross-border force justified from outside — is still partially live, so this is not a dead-mandate case; founding_problem_status is authored 'contested' because the parties genuinely dispute whether interstate predation or internal atrocity is now the binding danger. The mandate has, however, morphed: the arrangement built to stop states preying on states now operates principally to stop outsiders reaching populations their own states prey on. The status-contested × world-rearranges combination flags that morph without overclaiming mandate death, and it cross-checks against the theater trajectory: as the order function recedes, the selective-shield function carries the arrangement and the rhetoric decouples accordingly. The classification discipline prevents both mislabels: reading the arrangement as pure cooperation erases the sealed-room seats; reading it as pure extraction erases the small-state protection that keeps it load-bearing and the great-power enforcement that would not be paid for a pure racket. The false-summit evaluation is the point of the story: the reading claims naturality; the declared beneficiaries let the engine test that claim against the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_shield,
    'Is the absolute sovereignty norm a genuine structural feature of the international system, or a constructed legal-political arrangement whose absoluteness serves identifiable regimes?',
    'Comparative-historical analysis of the norm''s construction (Westphalian settlement, Charter drafting, decolonization) combined with counterfactual analysis of the periods when exceptions were operative; the false-summit signature evaluates the beneficiary-declared mountain claim directly.',
    'If constructed with the declared beneficiary structure, the mountain claim is a false summit and the constraint reclassifies toward coordination-plus-extraction; if genuinely structural, the reading''s self-presentation stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_shield, conceptual, 'Whether sovereignty absoluteness is natural law of the international system or a constructed shield with beneficiaries.').

omega_variable(
    consent_fiction_of_state_unit,
    'Does state consent to the non-intervention norm represent the consent of the governed, or does treating the state as a unitary consenter launder the repression of the people it consents for?',
    'Comparative analysis of consent attribution: exit and expression data from closed states, post-transition truth-telling processes, and the doctrinal treatment of governments-in-exile versus de facto regimes.',
    'If consent is fictional for repressive states, the legitimacy chain of the reading breaks at the unit boundary, the victim declaration dominates classification, and the reading''s own consent-based logic turns against its absoluteness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_fiction_of_state_unit, conceptual, 'Whether the state-as-consenter practice validates the norm for trapped populations.').

omega_variable(
    pretext_universality,
    'Are humanitarian exceptions categorically pretextual as the reading holds, or do some post-Cold War interventions satisfy humanitarian intent despite mixed motives?',
    'Systematic coding of post-Cold War interventions (Kosovo, Sierra Leone, East Timor, Libya, humanitarian corridors) against ex ante intent, mandate language, and ex post outcomes, with adversarial review by both maximalist and atrocity-prevention scholars.',
    'If any exception is genuine, the reading''s categorical foreclosure is overbroad by its own logic, and the trapped populations'' forgone recourse is not the price of order but the cost of the reading''s overclaim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_universality, empirical, 'Whether the categorical pretext claim survives systematic case analysis.').

omega_variable(
    selective_invocation_instrumentality,
    'Do the reading''s great-power holders hold sovereignty absoluteness as conviction, or invoke it instrumentally — shielding their own conduct while projecting into weaker states?',
    'Coding of voting and behavior consistency: how sovereignty-invoking powers treat others'' sovereignty when their projection interests engage (annexations, interventions among bloc members, border disputes).',
    'If instrumental, the theater ratio understates the performative share, and the arrangement functions as cover for great-power spheres rather than an order principle — the arbitraging seat''s classification diverges further from the small-state beneficiary seat''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_instrumentality, empirical, 'Conviction versus instrumentality in the reading''s enforcement seats.').

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the sovereignty_maximalist_reading of the rbio_practice_norm_complex kernel; the liberal_institutional_reading and the hegemonic_extraction_reading instantiate different constraints from the same kernel. Where exactly is the disagreement located, and how would adopting a sibling reading change this constraint''s structure?',
    'Per-seat classification comparison across the three sibling stories on the shared referent: the disagreement lives in the status of humanitarian exceptions (categorically pretextual / legitimate capacity-limited functions / evidence of extractive intent) and in the party structure each reading declares.',
    'Classification of this constraint is reading-indexed; the liberal sibling would name small states and atrocity victims as the relevant parties and author lower extraction, while the hegemonic sibling would name the veto-wielding powers as captor — the three stories together locate the kernel''s contest structurally rather than rhetorically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the RBIO kernel; disagreement located in exception status and party structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_sovmax_reading_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t0, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t5, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t10, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t15, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t20, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t25, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t30, observed).
narrative_ontology:measurement(rbio_sovmax_reading_tr_t35, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement_basis(rbio_sovmax_reading_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(rbio_sovmax_reading_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t0, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t5, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t10, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t15, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t20, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t25, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t30, observed).
narrative_ontology:measurement(rbio_sovmax_reading_be_t35, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(rbio_sovmax_reading_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_sovmax_reading_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t0, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t5, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t10, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t15, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t20, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t25, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t30, observed).
narrative_ontology:measurement(rbio_sovmax_reading_su_t35, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(rbio_sovmax_reading_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one member of a three-reading constraint family decomposing the rbio_practice_norm_complex kernel. The colloquial label 'the rules-based international order' conflates three structurally distinct claims about the same norm complex: the liberal institutional claim (legitimate, consent-based, revisable, capacity-limited), the hegemonic extraction claim (frozen by the veto, extractive by design), and the sovereignty maximalist claim (legitimate only as sovereignty protection; exceptions are pretext). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification. They are linked because they contest the same kernel and cite one another in practice: the maximalist reading's foreclosure of the liberal reading's exception authority, and its coexistence with the hegemonic reading's structural critique, are the family's load-bearing edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
