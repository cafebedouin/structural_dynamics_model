% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Christology as Enforced Imperial-Ecclesiastical Orthodoxy
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Nicene Christological
 *   kernel: the homoousios ('same substance') formula as it came to be
 *   enforced as imperial-ecclesiastical orthodoxy from Nicaea (325) through
 *   Constantinople (381) and beyond. The sibling reading — homoiousios
 *   ('similar substance'), preserving greater ontological distinction between
 *   Father and Son — is a separate constraint (homoiousios_reading) and is
 *   NOT evaluated here; per the ε-invariance principle, the two readings have
 *   different beneficiary/victim structures and different extraction profiles
 *   and must not be averaged into one story. This story's ε is assessed by
 *   the homoousios reading's own lights, against the standing arrangement of
 *   enforced doctrinal uniformity that reading produced — not against any
 *   endorsed alternative.
 *
 * KEY AGENTS:
 *   - nicene_imperial_episcopate: agenda_setter (institutional/arbitrage) — drafts and enforces the creed
 *   - alexandrian_theological_faction: beneficiary (organized/mobile) — gains doctrinal primacy
 *   - constantinian_imperial_authority: beneficiary/agenda_setter (institutional/arbitrage) — enforces uniformity for political consolidation
 *   - gothic_arian_communities: payer (moderate/constrained) — branded heretical, churches confiscated
 *   - north_african_homoian_congregations: payer (powerless/trapped) — clergy exiled, property seized
 *   - eastern_homoiousian_bishops: payer/excluded (organized/constrained) — outvoted middle position
 *   - later_church_historians: observer (analytical/analytical) — assesses theological vs. political drivers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.71).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.8).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Christology as Enforced Imperial-Ecclesiastical Orthodoxy").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '1e2b2efe-f729-47fa-841a-5e8c43bf7a8f').
narrative_ontology:cs_kernel_codification('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', formalized).
narrative_ontology:cs_authority_grounding('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', lineage).
narrative_ontology:cs_interpretation_layer_present('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f').
narrative_ontology:cs_reading_relation('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', foundational, father_son_identical_essence).
narrative_ontology:cs_axiom_status(father_son_identical_essence, holdable).
narrative_ontology:cs_axiom_grounding('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', father_son_identical_essence, deontological).
narrative_ontology:cs_axiom('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', secondary, single_formula_required_for_valid_communion).
narrative_ontology:cs_axiom_status(single_formula_required_for_valid_communion, holdable).
narrative_ontology:cs_axiom_grounding('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', single_formula_required_for_valid_communion, conventional).
narrative_ontology:cs_reference_frame('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', apostolic_monotheistic_transmission).
narrative_ontology:cs_drift_state('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', post_theodosian_state_establishment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1e2b2efe-f729-47fa-841a-5e8c43bf7a8f', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_imperial_episcopate).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, alexandrian_theological_faction).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, constantinian_imperial_authority).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, eastern_homoiousian_bishops).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, lay_believers_general).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, lay_believers_general).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, trinitarian_consubstantiality_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, single_divine_essence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops aligned with the homoousios formula who, backed by imperial convening power after Nicaea (325) and its later reaffirmation at Constantinople (381), draft the creed's language, control conciliar votes, and issue anathemas against dissenting formulations. They administer sees, control basilica property, and can depose rival bishops through imperial decree.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_imperial_episcopate, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The Athanasian party whose Christological formula becomes the enforced standard. They gain doctrinal primacy, patronage networks, and the ability to characterize theological rivals as heretics subject to exile, securing their see's long-term institutional authority in the wider church.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, alexandrian_theological_faction, beneficiary,
    organized, generational, mobile, regional).

% The imperial state that convenes and enforces the councils, using doctrinal uniformity as a tool of political consolidation across a fractious empire. Benefits from a single enforceable creed that can be backed by civil penalties (exile, property confiscation, loss of legal standing) against dissenting clergy and congregations.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, constantinian_imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, constantinian_imperial_authority, agenda_setter).

% Gothic and other Germanic Christian communities who received an earlier, homoian-influenced missionary Christianity. Under the enforced homoousios standard they are branded heretical, their clergy delegitimized, their churches subject to confiscation or reconsecration, and their communities pressured toward reconversion or marginalization as imperial and later successor-kingdom politics shift.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_communities, payer,
    moderate, generational, constrained, regional).

% Local congregations in North Africa holding non-Nicene Christological views who face episcopal deposition, exile of their clergy by imperial edict, and forced property transfer to Nicene-aligned bishops. They have no meaningful appeal outside the imperial-conciliar apparatus that produced the ruling against them.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations, payer,
    powerless, biographical, trapped, regional).

% Bishops who held the homoiousios ('similar substance') position, seeking a middle path preserving some ontological distinction between Father and Son to guard against modalism. Their formula loses conciliar favor; many are pressured to sign the homoousian formula or face deposition, and their theological reasoning is retroactively read as proto-heresy despite having been a mainstream position among many eastern sees.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, eastern_homoiousian_bishops, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, eastern_homoiousian_bishops, excluded).

% Ordinary congregants receive a doctrinally unified, portable liturgical and creedal identity that coordinates worship, catechesis, and communion across a vast empire. They also bear costs when their local bishop or region falls on the losing side of a conciliar ruling — sudden reclassification of their sacraments, clergy, or community as heretical.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, lay_believers_general, beneficiary,
    powerless, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, lay_believers_general, payer).

% Scholars examining the councils' proceedings, imperial correspondence, and regional records to assess how much of the homoousios formula's triumph reflects theological argument versus imperial political consolidation and faction politics.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, nicene_imperial_episcopate).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, transmissible Christological formula that lets a geographically vast and linguistically diverse church coordinate worship, catechesis, and communion recognition — resolving genuine ambiguity in earlier looser formulations about Christ's relationship to the Father.
% TRANSFER_FUNCTION: Moves doctrinal authority, ecclesiastical property, clerical office, and communal legitimacy from communities and clergy holding non-Nicene formulations toward the Nicene-aligned episcopate and the imperial authority that backs it; also transfers reputational status from 'orthodox' to 'heretic' by conciliar fiat.
% ABSENT_VOICES: Gothic and other Germanic Arian communities, and many North African congregations, were converted or catechized before the homoousios standard hardened and had no seat at Nicaea or Constantinople; homoiousian bishops who sought a middle formula were present at some councils but were structurally outvoted and later retroactively cast as heretical without their fuller theological reasoning being preserved on equal terms.
% DISAPPEARANCE_RATIONALE: If the enforced homoousios standard vanished, the array of Christological formulas current in the fourth century (homoian, homoiousian, anomoean, and others) would likely persist as live regional variants rather than being resolved into a single imperial orthodoxy; ecclesiastical property currently held by Nicene sees under confiscation rulings would revert to contested status, and communion boundaries between regions would reorganize around local theological consensus rather than conciliar anathema.
% FOUNDING_PROBLEM: Fourth-century Christian communities held genuinely divergent views on Christ's ontological relationship to the Father (ranging from strict subordinationism to full identity of essence), producing schism risk, competing liturgical practices, and no agreed criterion for communion; the councils were convened to resolve this doctrinal fragmentation and, not incidentally, to give the newly Christianized empire a single legible religious authority structure.
% FOUNDING_PROBLEM_CORROBORATION: Nicene-aligned church historians (Eusebius, later Socrates Scholasticus) attest the problem was primarily theological and resolved by sound argument at the councils. Independent secular historians of late antiquity and comparative historians of the Arian successor kingdoms (Goths, Vandals) attest that the 'resolution' tracked imperial political consolidation as much as theological consensus, and that homoian/Arian Christianity remained a stable, functioning tradition among Germanic peoples for over two centuries after Nicaea — evidence outside the benefiting Nicene party that the founding problem was not cleanly settled by argument alone.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.42 at Nicaea to 0.71 by Chalcedon as the formula moves from a contested conciliar proposition to an imperially-backed test of orthodoxy with civil penalties attached. Suppression rises faster still (0.45 to 0.80) because the mechanism securing the formula's dominance is increasingly coercive — anathema, deposition, exile, and confiscation — rather than persuasion; this tracks the historical shift from Nicaea (a contested vote among bishops, weakly enforced for decades, with a substantial Arian/homoian revival under Constantius II) to Theodosius's Edict of Thessalonica (380) and beyond, when Nicene Christianity becomes the enforced imperial standard. Theater ratio is comparatively low and rises only modestly (0.15 to 0.28): the theological argumentation is largely substantive engagement by both sides, not empty performance, though conciliar proceedings increasingly ritualize a predetermined outcome as imperial backing solidifies.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the Nicene episcopate and imperial authority, the homoousios formula is a genuine coordination achievement: it resolves real ontological ambiguity and unifies a fracturing church under one creed. From the seat of Gothic Arian communities, North African homoian congregations, and homoiousian bishops, the same formula operates as an extraction and suppression mechanism — their theological tradition is redefined as heresy by a vote they did not shape, with real costs in property, office, and communal legitimacy. The engine computes these as structurally different experiences of the same arrangement; this divergence is not resolved by picking a 'true' reading but is the object the classification is measuring.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Alexandrian faction, imperial authority, Nicene episcopate) sit near the low-d end: they set the terms, collect doctrinal and institutional authority, and retain mobile or arbitrage-grade exit (they can relocate the locus of authority, e.g., between Rome, Alexandria, Constantinople). Victims (Gothic Arians, North African congregations, homoiousian bishops) sit near the high-d end: their exit options are constrained or trapped — leaving communion means losing legal standing, property, and often physical safety in the post-Theodosian legal environment. Lay believers are genuinely dual-positioned: real coordination benefit (a stable, portable creed) combined with real exposure to confiscation/reclassification risk if their region falls on the losing side of a ruling.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the genuine coordination function the homoousios formula served — a large, diverse church did need SOME resolution to the Christological ambiguity that earlier vaguer formulations left open, and unresolved doctrinal fragmentation carried real schism costs. Calling this pure extraction would erase that real coordination problem. But calling it a pure rope would erase the documented coercive machinery — anathema, exile, forced property transfer — that the formula's persistence depended on once imperial power backed it. The tangled_rope classification holds both: real coordination function, riding on top of asymmetric extraction from specific named victim communities, sustained by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Is this constraint''s high extraction/suppression profile a property of the homoousios formula itself, or of the specific historical enforcement apparatus (imperial backing after Theodosius) that happened to attach to it — such that the homoiousios_reading, had IT won imperial backing instead, would show a comparably high extraction profile?',
    'Compare against periods and regions where homoian/homoiousian formulas held imperial or royal backing (e.g., under Constantius II, or in the Gothic and Vandal successor kingdoms) to see whether THEIR enforcement apparatus produced comparably high suppression against Nicene minorities. If so, the extraction is a property of imperially-backed doctrinal enforcement as a mechanism, not of this specific Christological content.',
    'If extraction tracks ''whichever reading holds state power'' rather than the homoousios content specifically, this reframes the constraint as fundamentally about the fusion of doctrinal authority with imperial coercion rather than about Trinitarian theology per se — though the ε value authored here remains specific to THIS reading''s actual historical operation and is not retroactively averaged with the counterfactual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether high extraction is intrinsic to the homoousios content or to state-backed doctrinal enforcement generally, evidenced by comparable coercion under homoian state backing in Gothic/Vandal kingdoms.').

omega_variable(
    theological_argument_vs_political_consolidation,
    'To what extent did the homoousios formula prevail because of the theological cogency of the Cappadocian/Athanasian arguments versus the political utility of a single enforceable creed for imperial consolidation under Constantine, Theodosius, and their successors?',
    'Comparative analysis of conciliar voting patterns against documented imperial pressure (exile threats, packing of councils, timing relative to imperial political needs) versus the independent theological literature''s assessment of argumentative merit.',
    'If political consolidation dominates, this strengthens the tangled_rope reading (real but secondary coordination function riding on primarily political extraction); if theological argument dominates and the coercive apparatus was a later addition rather than the mechanism of the formula''s initial acceptance, the early-period ε should be read as lower than the later-period ε, which the measurement series already partially reflects (0.42 at 325 rising to 0.71 by 451).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_argument_vs_political_consolidation, empirical, 'Relative weight of theological argument versus imperial political consolidation in the formula''s historical triumph.').

omega_variable(
    naturalness_of_doctrinal_uniformity_claim,
    'Is ''the church requires one settled Christological formula for valid communion'' a genuine coordination necessity (analogous to a natural law of large-scale religious organization) or a constructed claim that benefits whichever faction controls the settling mechanism?',
    'Cross-tradition comparison: examine whether religious traditions that tolerate greater Christological/doctrinal pluralism (e.g., some strands of later Anglican comprehensiveness, or contemporary ecumenical bodies) suffer the schism/coordination-failure costs the Nicene party predicted, or whether pluralism is sustainable without the predicted collapse.',
    'If doctrinal pluralism proves sustainable elsewhere without catastrophic coordination failure, the ''we need ONE formula'' premise looks more like a constructed justification for centralizing authority than a structural necessity — sharpening the extraction reading. If pluralism reliably produces the schism and communion-breakdown the Nicene party feared, the coordination function is more genuinely load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_doctrinal_uniformity_claim, conceptual, 'Whether doctrinal uniformity is a structural necessity for large religious institutions or a constructed rationale serving centralized authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(nice_tr_t350, nicene_christological_kernel__homoousios_reading, theater_ratio, 350, 0.2).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.22).
narrative_ontology:measurement(nice_tr_t400, nicene_christological_kernel__homoousios_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(nice_tr_t425, nicene_christological_kernel__homoousios_reading, theater_ratio, 425, 0.27).
narrative_ontology:measurement(nice_tr_t451, nicene_christological_kernel__homoousios_reading, theater_ratio, 451, 0.28).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement(nice_be_t350, nicene_christological_kernel__homoousios_reading, base_extractiveness, 350, 0.5).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.63).
narrative_ontology:measurement(nice_be_t400, nicene_christological_kernel__homoousios_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(nice_be_t425, nicene_christological_kernel__homoousios_reading, base_extractiveness, 425, 0.7).
narrative_ontology:measurement(nice_be_t451, nicene_christological_kernel__homoousios_reading, base_extractiveness, 451, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement(nice_su_t350, nicene_christological_kernel__homoousios_reading, suppression_requirement, 350, 0.55).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.68).
narrative_ontology:measurement(nice_su_t400, nicene_christological_kernel__homoousios_reading, suppression_requirement, 400, 0.74).
narrative_ontology:measurement(nice_su_t425, nicene_christological_kernel__homoousios_reading, suppression_requirement, 425, 0.78).
narrative_ontology:measurement(nice_su_t451, nicene_christological_kernel__homoousios_reading, suppression_requirement, 451, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, chalcedonian_definition_kernel).

% DUAL FORMULATION NOTE:
% This story is one of two constraint files decomposing the natural-language label 'the Arian controversy / Nicene settlement' into structurally distinct kernel readings, per the ε-invariance principle. homoousios_reading (this file) authors the 'same substance' formula as it came to be enforced with rising imperial coercion (ε rising 0.42→0.71 over 325–451). homoiousios_reading authors the 'similar substance' middle formula as held by bishops who were structurally outvoted and later marginalized — a different beneficiary/victim structure and a different ε trajectory (predominantly a payer/excluded position rather than a beneficiary/agenda-setter position). The two files must not be merged or averaged; they are linked here via affects_constraints. reading_relations declares forecloses because, within a single conciliar canon, a Church cannot simultaneously affirm 'identical essence' and 'merely similar essence' as its official Christology — the sibling was structurally displaced from mainstream imperial orthodoxy once this reading prevailed, though it persisted as a live minority and Germanic/Gothic tradition for centuries outside imperial center control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
