% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: Strict Geographic Reading of Maritime Feature Entitlement (UNCLOS Art. 121)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   maritime sovereignty from insular features: which pieces of land in the
 *   ocean generate territorial sea and exclusive economic zone. Three
 *   readings compete. This file is the strict_geographic_reading: only
 *   features naturally formed and above water at high tide qualify as
 *   islands; dredged reefs, poured concrete, and reclaimed platforms remain
 *   legally inert for zone-generation no matter what is built on them. The
 *   reading was decisively operationalized by the 2016 South China Sea
 *   arbitration, which classified major contested features as rocks and held
 *   that construction does not upgrade them. Its coordination face is real: a
 *   fixed natural reference prevents every reef from becoming a
 *   two-hundred-nautical-mile claim and keeps chokepoints negotiable. Its
 *   asymmetric face is equally real: the costs concentrate on states trying
 *   to overcome unfavorable geography through engineering, while the gains
 *   flow to actors whose fleets and trade already ride on open water — and
 *   the principal beneficiary supplies most of the enforcement. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (expansive_construction_reading, hybrid_effective_control_reading) are
 *   separate constraint stories with their own epsilon, beneficiaries, and
 *   victims; they are linked, not merged, here. The claim and the metrics are
 *   independent authored facts: I claim tangled_rope because I judge the
 *   structure to combine genuine coordination with enforced asymmetric
 *   bearing of costs; the metrics describe the arrangement's actual operation
 *   as I read the record.
 *
 * KEY AGENTS:
 *   - - blue_water_naval_powers: Primary beneficiary (powerful/mobile) — gains unimpeded transit; supplies most enforcement
 *   - - global_trading_states: Secondary beneficiary (organized/mobile) — gains predictable passage for commerce
 *   - - natural_feature_claimant_states: Secondary beneficiary (moderate/constrained) — holds zones shielded from rivals' construction
 *   - - low_lying_atoll_states: Dual-positioned beneficiary/payer (moderate/constrained) — protected today, existentially exposed to the same test tomorrow
 *   - - artificial_island_construction_states: Primary target (powerful/identity_locked) — bears denial of engineered claims; responds with defiance
 *   - - geographically_disadvantaged_coastal_states: Secondary target (moderate/constrained) — capped by geography, unable to engineer around the rule
 *   - - arbitral_tribunal_system: Agenda-setter (institutional/analytical) — administers interpretation without compliance power
 *   - - international_legal_academy: Analytical observer (institutional/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.6).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "Strict Geographic Reading of Maritime Feature Entitlement (UNCLOS Art. 121)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'aed7e486-e580-4ba4-9e89-a465bfea8bcf').
narrative_ontology:cs_kernel_codification('aed7e486-e580-4ba4-9e89-a465bfea8bcf', fixed_text).
narrative_ontology:cs_authority_grounding('aed7e486-e580-4ba4-9e89-a465bfea8bcf', lineage).
narrative_ontology:cs_interpretation_layer_present('aed7e486-e580-4ba4-9e89-a465bfea8bcf').
narrative_ontology:cs_reading_relation('aed7e486-e580-4ba4-9e89-a465bfea8bcf', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('aed7e486-e580-4ba4-9e89-a465bfea8bcf', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('aed7e486-e580-4ba4-9e89-a465bfea8bcf', foundational, entitlement_tracks_natural_formation).
narrative_ontology:cs_axiom_status(entitlement_tracks_natural_formation, holdable).
narrative_ontology:cs_axiom_grounding('aed7e486-e580-4ba4-9e89-a465bfea8bcf', entitlement_tracks_natural_formation, conventional).
narrative_ontology:cs_axiom('aed7e486-e580-4ba4-9e89-a465bfea8bcf', secondary, artificial_works_legally_inert_for_zone_generation).
narrative_ontology:cs_axiom_status(artificial_works_legally_inert_for_zone_generation, holdable).
narrative_ontology:cs_axiom_grounding('aed7e486-e580-4ba4-9e89-a465bfea8bcf', artificial_works_legally_inert_for_zone_generation, conventional).
narrative_ontology:cs_reference_frame('aed7e486-e580-4ba4-9e89-a465bfea8bcf', natural_formation_entitlement_framework).
narrative_ontology:cs_drift_state('aed7e486-e580-4ba4-9e89-a465bfea8bcf', post_2016_award_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aed7e486-e580-4ba4-9e89-a465bfea8bcf', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, global_trading_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, natural_feature_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, low_lying_atoll_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_construction_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, geographically_disadvantaged_coastal_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, low_lying_atoll_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, unclos_article_121_paragraph_3).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, high_tide_elevation_qualifying_test).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__strict_geographic_reading, scs_arbitration_2016_feature_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate global fleets that transit straits, run exercises, and patrol lanes passing waters adjacent to disputed features. Constructed platforms generate no surrounding zones under this reading, so passage needs no feature-by-feature negotiation. They supply most of the practical weight behind adverse rulings — presence operations, diplomatic protest, combined exercises — and can redirect deployments wherever costs bite.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers, beneficiary,
    powerful, generational, mobile, global).

% Move imports, exports, and energy through chokepoints near contested reefs. They contribute little to enforcement but gain predictable passage; rerouting around an enclosed lane is possible but expensive, so they press for the reading's maintenance in diplomatic forums and free-rider on the naval powers' presence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, global_trading_states, beneficiary,
    organized, generational, mobile, global).

% Hold maritime zones anchored to genuinely formed islands. The reading shields their entitlements from neighbors' dredging projects, but they cannot reinforce their own marginal features without inviting the same denial they invoke against rivals; their position is fixed by geography they inherited, and their leverage is coalition voting in treaty bodies.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, natural_feature_claimant_states, beneficiary,
    moderate, generational, constrained, regional).

% Sovereign states whose entire exclusive zones hang on narrow atolls barely clearing the tide line. Today the natural-formation test secures their waters against larger neighbors; erosion and rising seas threaten to take the features themselves below the qualifying line, and they campaign internationally to freeze baselines before the test that protects them turns on them. There is no exit from their own geography.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, low_lying_atoll_states, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, low_lying_atoll_states, payer).

% Dredge and pour reefs into fortified installations with runways, garrisons, and shelters, presenting the works as administration of historic territory. Billions in capital and regime prestige are sunk into the features; domestic politics treat any admission that the works confer nothing as national humiliation, so reversal is not a live option. Adverse rulings are answered with accelerated building and rejection of the adjudicating body's authority.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_construction_states, payer,
    powerful, generational, identity_locked, regional).

% Front coastlines dotted with rocks and sandbars that dry at low tide. The reading caps their reachable jurisdiction at a fraction of what neighbors with proper islands command, and no engineering budget changes the outcome. Their recourse is bloc diplomacy in treaty-review forums, where they are routinely outvoted by the very grouping the rule favors.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, geographically_disadvantaged_coastal_states, payer,
    moderate, biographical, constrained, regional).

% Interprets the treaty's feature-classification language when parties submit disputes and when commissions review extended-shelf claims. Its 2016 award fixed the high-tide natural-formation test as operative law in the leading dispute. It commands no fleet and collects no revenue; its rulings bind only where the addressed party accepts them or other powers make refusal costly.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, arbitral_tribunal_system, agenda_setter,
    institutional, civilizational, analytical, global).

% Law-of-the-sea scholars, commentaries, and professional associations tracing how the feature clauses were negotiated, publishing interpretations, and training the judges and diplomats who staff tribunals and ministries. They hold no enforcement power and gain nothing material from any reading prevailing; their stake is doctrinal coherence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_legal_academy, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, blue_water_naval_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single natural reference for maritime entitlement: without it, every reef becomes a potential two-hundred-nautical-mile claim, every dredging season a sovereignty escalation, and every chokepoint a patchwork of bilateral corridors negotiated under duress. The strict test makes zones predictable and bounds claim inflation.
% TRANSFER_FUNCTION: Moves jurisdictional space — and the fisheries, hydrocarbon, and transit value attached to it — away from states attempting to manufacture entitlement through construction, toward holders of naturally formed features and toward the open-sea user community, whose access construction-generated zones would have enclosed.
% ABSENT_VOICES: The construction states sat out the room that fixed the reading's operative form: the lead claimant refused participation in the 2016 arbitration, so its core objection — that the tribunal exceeded its mandate and that the rocks clause was never meant to extinguish historic position — was voiced only from outside. Geographically disadvantaged coastal states were numerically marginal in the original negotiations. Future generations facing a rising tide line, who will inherit either frozen or vanishing baselines, had no seat at all.
% DISAPPEARANCE_RATIONALE: If the strict test vanished overnight, construction races would escalate immediately: every capable littoral state would pour concrete on its reefs, zones would proliferate around engineered platforms, chokepoints would close behind newly generated territorial seas, and naval and commercial traffic would need constant negotiation for passage. Claims, deployments, and resource access across three oceans would reorganize around whatever had been built most recently.
% FOUNDING_PROBLEM: Mid-century technology made offshore oil and fisheries valuable enough that every rock mattered; the 1958 and 1982 conventions needed a line between full islands and bare rocks so that genuine islands could carry zones without letting every islet swallow the ocean and starve the high-seas balance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the UNCLOS III negotiating record shows the rocks clause was inserted at the insistence of mid-sized states fearing great-power claim inflation, not at the beneficiaries' urging; independent law-of-the-sea commentary treats the feature-classification problem as unresolved and worsening; and the construction states themselves, while rejecting the tribunal's authority, concede in their own diplomatic notes that a distinction between islands and rocks exists in the treaty text. No party attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 because the rule moves jurisdictional value asymmetrically: fisheries, hydrocarbon access, and strategic depth that engineering-built features would otherwise anchor are permanently allocated away from the states that built them, toward holders of natural features and the open-sea user community. It is a boundary rule rather than a recurring levy, which keeps it below snare-range, but the transferred value is large and irreversible. Suppression at 0.60 is a raw structural property, unscaled by power or scope: the alternative strategy (claim through construction) is foreclosed by legal interpretation, diplomatic isolation of defiant claimants, and naval presence signaling — yet incompletely, because the enforcing coalition lacks coercion monopoly over the strongest target, as the post-2016 record shows. Theater at 0.31 captures a real but growing performative layer: protest notes, symbolic freedom-of-navigation passes, and tribunal proceedings the addressed party simply declines to attend. Accessibility_collapse at 0.52: within the reading's own frame the construction alternative collapses almost completely once understood, but the physical alternative persists and the sibling readings keep it institutionally alive, so collapse stalls just past midpoint. Resistance at 0.70 reflects open great-power defiance, accelerated building after adverse rulings, and rival readings institutionalized in claimant diplomacy. All three temporal series run on one shared eight-point grid (1982–2024); trajectories are monotonic, driven by construction technology maturing, stake appreciation as offshore resources grew, and the 2016 award sharpening denial from background assumption into explicit ruling — enforcement-capacity buildup is traced by the suppression series, which is why it is authored despite the static-picture scalar rule.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by design. From the construction-state seat the arrangement is great-power lock-in: a rule that freezes its exclusion at the moment its engineering finally matured, administered by tribunals it never consented to ad hoc, backed by the fleets of its rivals. From the naval-power seat the same structure is neutral geography faithfully recorded: nature made the features, the rule merely declines to let concrete lie about it. The tribunal seat experiences rule-maintenance duty without compliance power — every award is an expenditure of institutional credibility it cannot replenish. The atoll-state seat holds the sharpest internal split: the test that guards its zones today is the test that will strip them if the tide line climbs. Same-power divergence is visible between the two powerful seats: the naval beneficiary exits anywhere (mobile), the construction payer is fused to its claims by regime legitimacy and domestic nationalism (identity-locked) — identical global standing, opposite structural relationships, differentiated by role and exit rather than by power atom.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: blue_water_naval_powers (declared beneficiary, mobile exit) derive near the full-beneficiary end — the rule subsidizes their operating environment and they can redeploy at will; global_trading_states similar with slightly higher d since rerouting trade is costly but possible; natural_feature_claimant_states mid-low, protected but unable to strengthen their own marginal features; low_lying_atoll_states low today, with the sea-level omega flagging a possible inversion. Victim declarations drive the targets: artificial_island_construction_states derive near the full-target end — identity-locked exit amplifies their exposure since they cannot abandon sunk prestige; geographically_disadvantaged_coastal_states high but somewhat lower-stakes, their loss chronic rather than acute. The tribunal derives near-symmetric: it administers without collecting. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate the same-power seats (mobile naval beneficiary versus identity-locked construction payer), which is the case overrides exist for, and the derivation handles it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping genuine islands entitled without letting every islet swallow the ocean — is live, indeed more acute than at drafting, so no mandatrophy is declared and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The classification guards against two mislabelings. Reading the rule as a mountain ('geography is destiny', entitlement as natural fact) would launder a maintained convention into inevitability and hide its beneficiaries behind geology; the rule requires tribunals, protest diplomacy, and naval signaling to hold, which no mountain does. Reading it as a snare would erase the genuine coordination function — predictability of zones and protection of the commons — that most parties affirmatively want. The live risk is piton-decay: if the compliance gap widens until the strict test is affirmed everywhere and obeyed nowhere, the reading persists as ceremonial citation while construction facts on the water decide outcomes; the theater_ratio series is the early-warning instrument for exactly that trajectory, and the tribunal_compliance_gap omega names the observation that would confirm it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (strict_geographic_reading) of the unclos_maritime_sovereignty kernel; if the expansive_construction_reading or hybrid_effective_control_reading consolidated as operative law, would the beneficiary and victim structure invert?',
    'Crystallization of contrary state practice, an authoritative ICJ advisory opinion on feature classification, or a UNCLOS review conference amending Article 121.',
    'Under the expansive reading the construction states become the benefiting seats and naval powers bear the costs; the whole three-story family''s classifications shift, and this file''s epsilon no longer describes the operative arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the maritime-sovereignty kernel prevails determines who pays and who gains.').

omega_variable(
    sea_level_baseline_instability,
    'Does the high-tide natural-formation test survive sea-level rise that submerges naturally formed features — do drowned atolls lose the zones they currently generate?',
    'Track state practice and continental-shelf commission decisions on atoll-state submissions: whether baselines are frozen at deposit or ambulatory with the tide line.',
    'If entitlements evaporate with the features, the low_lying_atoll_states seat flips from beneficiary to victim and the strict reading splits into frozen-baseline and ambulatory variants — likely a further decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sea_level_baseline_instability, empirical, 'Whether nature''s withdrawal of features strips nature-derived entitlements.').

omega_variable(
    tribunal_compliance_gap,
    'Does adjudicated interpretation produce effective operation of the strict test when the largest addressed party refuses compliance and no enforcer compels it?',
    'Longitudinal tracking of construction pace, claim behavior, and diplomatic costs after the 2016 award: does refusal carry escalating price, or does the ruling fade into citation-only status?',
    'Sustained costless noncompliance drives theater_ratio upward and pushes the reading toward ceremonial maintenance — a rule affirmed in words and ignored in fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_compliance_gap, empirical, 'Whether the strict reading operates or merely performs without a compliance mechanism.').

omega_variable(
    rock_island_capacity_threshold,
    'What minimum of sustained human habitation or economic life separates an entitled island from a bare rock under the strict test?',
    'Accumulated case law and shelf-commission determinations on individual features (Okinotorishima-type cases foremost).',
    'Fixes how many mid-sized features lose zone-generation; shifts the burden of the rule between claimant sets and changes the size of the paying population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rock_island_capacity_threshold, conceptual, 'The strict reading inherits an unresolved capacity threshold that determines its reach.').

omega_variable(
    geographic_destiny_vs_constructed_convention,
    'Is the strict reading a discovered fact of geography (entitlement simply is where nature puts land) or a political allocation dressed in geographic costume?',
    'Comparative study of regimes that allocate maritime entitlement by human works (historic-title doctrines, reclamation practice in other legal orders) against the negotiating record of the feature clauses.',
    'If constructed, the rule''s apparent immunity to revision weakens and pressure toward reclassification as a maintained interest-structure rises; if treated as natural, its beneficiaries shelter behind inevitability while enforcement costs stay externalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_destiny_vs_constructed_convention, conceptual, 'Natural-law framing versus constructed-convention framing of the qualifying test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.15).
narrative_ontology:measurement_basis(uncl_tr_t1982, observed).
narrative_ontology:measurement(uncl_tr_t1990, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(uncl_tr_t1990, observed).
narrative_ontology:measurement(uncl_tr_t1998, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement_basis(uncl_tr_t1998, observed).
narrative_ontology:measurement(uncl_tr_t2006, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t2006, observed).
narrative_ontology:measurement(uncl_tr_t2012, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement_basis(uncl_tr_t2012, observed).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t2016, observed).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement_basis(uncl_tr_t2020, observed).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(uncl_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.38).
narrative_ontology:measurement_basis(uncl_be_t1982, observed).
narrative_ontology:measurement(uncl_be_t1990, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1990, 0.41).
narrative_ontology:measurement_basis(uncl_be_t1990, observed).
narrative_ontology:measurement(uncl_be_t1998, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1998, 0.44).
narrative_ontology:measurement_basis(uncl_be_t1998, observed).
narrative_ontology:measurement(uncl_be_t2006, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2006, 0.47).
narrative_ontology:measurement_basis(uncl_be_t2006, observed).
narrative_ontology:measurement(uncl_be_t2012, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2012, 0.51).
narrative_ontology:measurement_basis(uncl_be_t2012, observed).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2016, 0.57).
narrative_ontology:measurement_basis(uncl_be_t2016, observed).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(uncl_be_t2020, observed).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(uncl_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.35).
narrative_ontology:measurement_basis(uncl_su_t1982, observed).
narrative_ontology:measurement(uncl_su_t1990, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(uncl_su_t1990, observed).
narrative_ontology:measurement(uncl_su_t1998, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement_basis(uncl_su_t1998, observed).
narrative_ontology:measurement(uncl_su_t2006, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2006, 0.46).
narrative_ontology:measurement_basis(uncl_su_t2006, observed).
narrative_ontology:measurement(uncl_su_t2012, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement_basis(uncl_su_t2012, observed).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2016, 0.58).
narrative_ontology:measurement_basis(uncl_su_t2016, observed).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.59).
narrative_ontology:measurement_basis(uncl_su_t2020, observed).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(uncl_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'maritime sovereignty from islands' decomposes per the epsilon-invariance principle into three structurally distinct claims — strict_geographic_reading (this file), expansive_construction_reading, and hybrid_effective_control_reading. Each carries its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observer-relative, which the chi formula forbids. This file is the upstream member: it is the textually grounded, highest-confidence reading that both siblings define themselves against — the 2016 award it operationalizes is cited by expansive holders as the position to reject and by hybrid holders as the baseline to soften. Edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
