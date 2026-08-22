% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Welfare Reading of Animal Moral Status (Regulated Humane Use)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the welfare reading of the animal moral status
 *   kernel: animals are sentient beings whose suffering is a legitimate moral
 *   concern, and the arrangement's job is to regulate the METHODS of use —
 *   confinement dimensions, transport duration, slaughter technique — while
 *   leaving the fact of use itself unquestioned. The claim is tangled_rope:
 *   there is a genuine coordination function (a shared, auditable,
 *   method-focused standard reduces certain specific harms and lets
 *   industries, regulators, and consumers coordinate around a legible floor),
 *   and there is asymmetric extraction (the animals bear the entire cost of
 *   confinement, pain, and death inherent to the use itself, while human
 *   parties capture legitimacy, revenue, and moral comfort). The rising
 *   theater_ratio over the interval reflects welfare-label proliferation
 *   outpacing verified improvement in animal outcomes — certification
 *   becoming increasingly a reassurance signal rather than a binding
 *   constraint on the total harm experienced.
 *
 * KEY AGENTS:
 *   - welfare_certification_organizations: sets and administers the standard, captures legitimacy and institutional purpose
 *   - regulated_animal_industries: bears compliance cost, captures social license and continued market access — the gain_flow seat
 *   - consuming_public: benefits from reduced moral friction at purchase
 *   - animals_in_regulated_use_systems: bears the entire underlying cost of confinement, pain, and death; the victim set for THIS reading is animals suffering within nominally humane use, not animals subjected to use per se
 *   - abolitionist_advocates: excluded from standard-setting; their objection to use-per-se is not on the agenda this reading recognizes
 *   - animal_welfare_scientists: analytical observer whose findings are selectively cited
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.42).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.38).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Welfare Reading of Animal Moral Status (Regulated Humane Use)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '5073c2cb-838c-4d03-9c28-bfdee30921de').
narrative_ontology:cs_kernel_codification('5073c2cb-838c-4d03-9c28-bfdee30921de', distributed).
narrative_ontology:cs_authority_grounding('5073c2cb-838c-4d03-9c28-bfdee30921de', practice).
narrative_ontology:cs_interpretation_layer_present('5073c2cb-838c-4d03-9c28-bfdee30921de').
narrative_ontology:cs_reading_relation('5073c2cb-838c-4d03-9c28-bfdee30921de', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('5073c2cb-838c-4d03-9c28-bfdee30921de', animal_moral_status__abolitionist_reading, influences).
narrative_ontology:cs_axiom('5073c2cb-838c-4d03-9c28-bfdee30921de', foundational, sentience_grounds_suffering_relevant_moral_status).
narrative_ontology:cs_axiom_status(sentience_grounds_suffering_relevant_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('5073c2cb-838c-4d03-9c28-bfdee30921de', sentience_grounds_suffering_relevant_moral_status, deontological).
narrative_ontology:cs_axiom('5073c2cb-838c-4d03-9c28-bfdee30921de', foundational, regulated_use_is_morally_permissible_independent_of_use_itself).
narrative_ontology:cs_axiom_status(regulated_use_is_morally_permissible_independent_of_use_itself, holdable).
narrative_ontology:cs_axiom_grounding('5073c2cb-838c-4d03-9c28-bfdee30921de', regulated_use_is_morally_permissible_independent_of_use_itself, conventional).
narrative_ontology:cs_reference_frame('5073c2cb-838c-4d03-9c28-bfdee30921de', post_cruelty_statute_regulatory_consensus).
narrative_ontology:cs_drift_state('5073c2cb-838c-4d03-9c28-bfdee30921de', contemporary_animal_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5073c2cb-838c-4d03-9c28-bfdee30921de', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_certification_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consuming_public).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, cruelty_wrongness_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, regulated_use_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and audits humane-treatment standards (space allowances, slaughter methods, transport conditions), issues certification labels, and lobbies for welfare statutes. Draws funding, membership dues, and public legitimacy from being the recognized arbiter of what counts as acceptable treatment. Has no exit from the arrangement because its institutional identity is constituted by administering it — but is never the one bearing the underlying cost of use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_certification_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_certification_organizations, beneficiary).

% Farms, laboratories, and entertainment operations adopt welfare certifications and comply with minimum-standard regulations. Compliance costs money but purchases a durable social license to continue using animals at scale; certification is often cheaper than the reputational and legal risk of remaining uncertified. Can relocate operations or shift compliance burden to less-regulated jurisdictions.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_industries, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_animal_industries, payer).

% Buys welfare-labeled products and services with reduced moral discomfort, without needing to examine what 'humane' actually entails in practice or to give up animal-derived goods. The label discharges the ethical question at the point of purchase.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consuming_public, beneficiary,
    organized, biographical, constrained, national).

% Are bred, confined, transported, and killed within systems that meet the welfare threshold but still impose confinement, pain, and death as routine features of the use itself. The welfare standard reduces some specific harms (e.g. cage size, stunning method) while leaving the underlying use — and the animal's total lack of exit — completely intact. Cannot consent, cannot leave, cannot be represented except through the very institutions that profit from certifying their treatment.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use_systems, payer,
    powerless, immediate, trapped, national).

% Argue that welfare certification legitimizes and stabilizes the very use-systems that constitute the violation, and that 'humane' framing is a public-relations solvent for continued exploitation. Are treated as fringe or unrealistic within welfare-standard rulemaking processes, where their objection to use-per-se is structurally not on the agenda.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, civilizational, constrained, national).

% Measure stress hormones, behavioral indicators, and pain responses to inform standards; their findings are cited selectively by certifiers and industry to justify existing practice more often than to force costly revision.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_animal_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, auditable floor for how animals may be treated within systems of use, allowing industries, regulators, and consumers to coordinate around a common minimum standard rather than each actor negotiating treatment terms individually or not at all.
% TRANSFER_FUNCTION: Moves moral and reputational risk away from industries and consumers and onto a certification apparatus; moves a portion of industry revenue into certification fees and compliance costs; the residual and largest cost — confinement, pain, and death inherent to the use itself — remains with the animals and is not transferred to any human party.
% ABSENT_VOICES: Abolitionist advocates who reject the premise that use can be made acceptable through method regulation are structurally outside the standard-setting process; the animals themselves have no voice in any part of the process and are represented only by proxies who have an institutional stake in the arrangement's continuation.
% DISAPPEARANCE_RATIONALE: If welfare certification and its enforcing statutes vanished, industries would lose their social license and legal cover, consumer trust in animal products would fracture without a legible reassurance mechanism, and welfare organizations would lose their institutional purpose — the underlying use-systems would likely continue under looser constraint, but the coordination structure that stabilizes public acceptance of them would collapse and require rebuilding or replacement.
% FOUNDING_PROBLEM: Documented cruelty in industrial animal use (extreme confinement, inhumane slaughter, visible suffering) generated public outrage and reputational/legal risk that threatened to destabilize animal-use industries; welfare regulation was built to address the cruelty without addressing the use.
% FOUNDING_PROBLEM_CORROBORATION: Welfare organizations and regulated industries attest the founding problem (unregulated cruelty) has been substantially addressed and the arrangement now functions as intended. Independent animal welfare scientists and abolitionist advocates, from outside the certifying and profiting parties, attest that documented suffering persists within certified systems at levels the label does not disclose to consumers, and that the founding problem was reframed rather than solved — from 'is use acceptable' to 'is this specific method of use acceptable,' a narrower question that the arrangement was built to make consumers stop asking.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits in the low-to-moderate band the welfare reading structurally implies: the standard demonstrably reduces SOME specific harms (which is real coordination, not zero), but leaves the dominant harm — confinement and death as routine features of use — completely untouched, which is why extraction is moderate rather than low. Suppression (0.38) is moderate: animals cannot exit or object by definition (trapped, powerless), but the suppression is a background structural fact of the use-system rather than an actively intensifying enforcement campaign against a resisting population. Theater ratio (0.48) is elevated and rising because welfare labeling functions increasingly as a legitimacy signal for consumers rather than as a tight binding constraint on producer behavior — labels proliferate faster than audited, enforced improvement. Accessibility collapse (0.55) reflects that once a consumer accepts the welfare framing, the alternative framing (that any use is the violation) becomes hard to access — the welfare label discharges the ethical question and forecloses further inquiry for most purchasers. Resistance (0.5) is moderate: abolitionist advocacy is real and organized but structurally excluded from the standard-setting process itself, so it registers as external pressure rather than internal contestation.
 *
 * PERSPECTIVAL GAP:
 *   From welfare_certification_organizations' seat, the arrangement is straightforward, working coordination: cruelty is being reduced, standards are rising, the system does what it says. From animals_in_regulated_use_systems' position (to the extent a position can be attributed to a non-language-using, non-consenting party), the same structure delivers confinement and death regardless of whether the specific method used to inflict them meets an audited threshold. The engine should compute these divergently from the structural data — powerless/trapped/immediate for the animals versus organized/arbitrage/generational for the certifiers — without either seat's authored account overriding the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare certification organizations sit near the beneficiary end: they administer the standard and derive their institutional purpose and funding from its continued existence, but they do not bear the underlying cost of use — hence agenda_setter + secondary beneficiary. Regulated industries are dual-positioned: they pay compliance costs (real, sometimes substantial) but purchase something worth more than the cost — durable social license — so their directionality remains beneficiary-leaning despite the payer role. The consuming public benefits from reduced moral friction with essentially no direct cost. Animals in regulated-use systems are the sole full-target seat: powerless, trapped, immediate time horizon (their claim is measured in the span of a single confined life), bearing the entire residual harm the welfare standard does not address. This is the reading's key structural move: animals are IN the victim set for cruelty (excess suffering beyond the regulated floor) but NOT in the victim set for use itself — the welfare reading treats use as a settled background condition, which is precisely what distinguishes it from the abolitionist reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated, visible cruelty generating public outrage) has been substantially addressed by the welfare apparatus — this is a real accomplishment and the arrangement is not empty theater. But the founding problem has also been quietly re-scoped: from 'is animal use acceptable' to 'is this specific method of animal use acceptable,' and the welfare apparatus's institutional survival now depends on the narrower question remaining the only one asked. This is why founding_problem_status is authored as contested rather than dead: the original narrow problem (egregious, undisguised cruelty) is genuinely diminished; but if the founding problem is understood at the broader level the abolitionist reading operates at, it was never addressed and the arrangement's persistence forecloses rather than solves it. The tangled_rope classification (rather than snare) preserves this: there is real coordination value in reducing specific documented harms, which a pure extraction story would deny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_committer_structure,
    'This constraint is one reading (welfare_reading) of the contested animal_moral_status kernel; the property_reading and abolitionist_reading are separate constraints instantiating the same kernel differently. What would each sibling reading change structurally?',
    'Compare the three constraint files'' beneficiary/victim sets and ε values directly: property_reading removes animals from the victim set entirely (no moral standing to violate); abolitionist_reading extends the victim set to cover ALL use, not just cruelty within use, and reads the welfare apparatus itself as the extraction mechanism rather than as mitigation.',
    'The disagreement between readings is located specifically at the boundary of the victim set: does it include only animals suffering excess/non-compliant treatment (welfare_reading), no animals at all (property_reading), or all animals under any use (abolitionist_reading)? This is a values disagreement about moral status, not a factual disagreement resolvable by better welfare science.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_committer_structure, conceptual, 'Documents the three-way kernel split and where each reading draws the victim-set boundary.').

omega_variable(
    welfare_floor_as_legitimation_mechanism,
    'Does the welfare standard function primarily to reduce actual animal suffering, or primarily to produce public and legal legitimacy for continued use regardless of the suffering reduction achieved?',
    'Compare longitudinal animal welfare science metrics (stress hormones, behavioral indicators, mortality/morbidity rates) against certification adoption rates and consumer trust surveys over the same period; divergence between improving trust metrics and flat/worsening welfare-science metrics would indicate legitimation without corresponding harm reduction.',
    'If legitimation outpaces actual harm reduction, the theater_ratio should be authored higher and the tangled_rope classification''s coordination component should be weighted lower relative to its extraction component in future revisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_floor_as_legitimation_mechanism, empirical, 'Whether certification tracks welfare science or outpaces it as pure legitimation.').

omega_variable(
    cs_framing_regulatory_vs_moral_legitimacy,
    'Should this constraint be framed as the regulatory apparatus (statutes, certifying bodies, audit regimes) or as the underlying moral legitimacy claim the apparatus depends on (''use accompanied by minimized suffering is morally acceptable'')? The obvious framing is the regulatory institution; the less obvious framing is the moral doctrine layered above it that the institution exists to make credible.',
    'Trace whether removing the regulatory apparatus (statutes repealed, certifiers disbanded) would also collapse the moral legitimacy claim, or whether the claim would persist and simply seek a new institutional vehicle. If the claim outlives any particular institutional form, the moral-doctrine framing is the more structurally fundamental one.',
    'Under the regulatory-apparatus framing, this constraint is a tangled_rope tied closely to specific enforceable statutes (as authored). Under the moral-doctrine framing, it would be closer to a self-enforcing, diffuse-epistemic commitment system with no single administering authority, and cs_structure.authority_grounding would shift from institutional practice/extraction toward diffuse_epistemic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_regulatory_vs_moral_legitimacy, conceptual, 'Whether the constraint is better modeled as institutional apparatus or as the moral doctrine the apparatus serves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__welfare_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__welfare_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__welfare_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__welfare_reading, theater_ratio, 32, 0.46).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__welfare_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__welfare_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__welfare_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__welfare_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__welfare_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__welfare_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__welfare_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__welfare_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Sibling of animal_moral_status__property_reading and animal_moral_status__abolitionist_reading — three readings of the single animal_moral_status kernel, each instantiated as its own constraint with its own ε per the ε-invariance principle. The welfare_reading occupies the structural middle: it shares with property_reading the acceptance of use-as-legitimate, but shares with abolitionist_reading the acceptance of sentience and cruelty-wrongness. Do not average or blend ε across the three files; each is a distinct constraint with a distinct beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
