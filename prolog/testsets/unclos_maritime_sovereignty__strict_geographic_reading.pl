% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Strict Geographic Reading (Natural Islands Only)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   The United Nations Convention on the Law of the Sea (UNCLOS) Part V
 *   establishes that 'islands' generate territorial seas and exclusive
 *   economic zones (EEZs), while 'rocks' and 'artificial structures' do not.
 *   The strict geographic reading of Article 121(1) holds that ONLY naturally
 *   formed features that remain above water at high tide qualify as islands;
 *   artificial construction—no matter how permanent, occupied, or
 *   economically developed—does not alter legal status. This reading is
 *   contested. Expansionist coastal states (particularly in Southeast Asia
 *   and the South China Sea) argue that artificial islands, especially those
 *   built on submerged features and occupied with permanent infrastructure,
 *   should generate territorial waters through effective control. Naval
 *   powers and non-claimant states benefit from the strict reading because it
 *   prevents rapid territorial expansion through engineering and preserves
 *   international waters. The constraint is CLAIMED as tangled_rope (it
 *   coordinates a global rule and creates asymmetric extraction for
 *   expansionist states) and authored metrics describe a substantially
 *   enforced, coercively maintained arrangement—which the engine measures
 *   independently of the claim.
 *
 * KEY AGENTS:
 *   - Naval powers (USA, UK, France, Russia): benefit from constraining expansionist coastal-state sovereignty; maintain freedom of navigation through preserved international waters; high institutional power, low exit (committed to the reading via force if needed)
 *   - Expansionist coastal states (China, Philippines, Vietnam, others): lose potential EEZ/territorial waters from artificial island construction; medium institutional power, trapped exit (UNCLOS binds them, but they dispute interpretation)
 *   - Island-adjacent developing nations: trapped by the reading; cannot use artificial construction to claim resource zones; low-to-moderate power, no exit short of UNCLOS renegotiation
 *   - International Court of Justice + arbitral tribunals: agenda-setters who enforce the reading through case law; high institutional power, analytical exit
 *   - Freedom of navigation advocates (shipping interests, NGOs): diffuse beneficiary; constrained exit (depend on naval power enforcement)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Maritime Sovereignty: Strict Geographic Reading (Natural Islands Only)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'f225df15-02c4-4476-96d1-67d684d0ee59').
narrative_ontology:cs_kernel_codification('f225df15-02c4-4476-96d1-67d684d0ee59', fixed_text).
narrative_ontology:cs_authority_grounding('f225df15-02c4-4476-96d1-67d684d0ee59', extraction).
narrative_ontology:cs_interpretation_layer_present('f225df15-02c4-4476-96d1-67d684d0ee59').
narrative_ontology:cs_reading_relation('f225df15-02c4-4476-96d1-67d684d0ee59', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('f225df15-02c4-4476-96d1-67d684d0ee59', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('f225df15-02c4-4476-96d1-67d684d0ee59', foundational, natural_origin_is_necessary_condition).
narrative_ontology:cs_axiom_status(natural_origin_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('f225df15-02c4-4476-96d1-67d684d0ee59', natural_origin_is_necessary_condition, conventional).
narrative_ontology:cs_axiom('f225df15-02c4-4476-96d1-67d684d0ee59', foundational, artificial_construction_cannot_confer_sovereignty).
narrative_ontology:cs_axiom_status(artificial_construction_cannot_confer_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f225df15-02c4-4476-96d1-67d684d0ee59', artificial_construction_cannot_confer_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('f225df15-02c4-4476-96d1-67d684d0ee59', textual_natural_artificial_boundary).
narrative_ontology:cs_drift_state('f225df15-02c4-4476-96d1-67d684d0ee59', contemporary_artificial_island_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f225df15-02c4-4476-96d1-67d684d0ee59', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_advocates).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, island_adjacent_developing_nations).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_construction_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, United Kingdom, France, Russia, and other major naval actors benefit from the strict geographic reading because it constrains the territorial ambitions of coastal rivals and preserves global freedom of navigation. The reading keeps international waters open and prevents strategic chokepoints from being closed through artificial island expansion. Their ability to project power globally depends on access to international waters; the strict reading is a force multiplier for global reach. They can exit the reading only by withdrawing from UNCLOS entirely, which would sacrifice other maritime protections they rely on and damage their position as norm-setters.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    institutional, generational, analytical, global).

% China, the Philippines, Vietnam, and other coastal states with submerged features or low-tide elevations they hoped to activate into sovereign territory bear the primary cost of the strict reading. It denies them a pathway to sovereignty expansion through artificial island construction on their continental shelves. They have invested in dredging, reclamation, and infrastructure on features they believed (under the expansive or hybrid readings) would generate territorial waters. The constraint traps them: exiting UNCLOS would sacrifice other maritime rights (natural-island EEZs, fishing zones, continental shelf resources). They resist through construction projects and legal counter-arguments, but enforcement by naval powers prevents the constraint from weakening.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    institutional, generational, constrained, regional).

% States like Japan, South Korea, and smaller island nations without active territorial expansion programs benefit from the strict reading because it prevents rivals from suddenly closing off waters through engineering. The rule stabilizes their own maritime boundaries and guarantees they cannot be surprised by a competitor's artificial island project. They have analytical exit (they could withdraw from UNCLOS, but they rely on its protections for their own islands and have no interest in exiting). The constraint aligns their interests with naval powers.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states, beneficiary,
    institutional, generational, analytical, global).

% International shipping interests, merchant marine associations, maritime NGOs, and port authorities benefit from the strict reading because it preserves the extent of international waters and keeps navigation routes open. Every kilometer of territorial sea foreclosed by artificial island expansion increases shipping costs and risks. They have constrained exit: they depend on naval powers to enforce the reading and on the rule itself to function. They cannot withdraw from UNCLOS; they can only advocate for stronger enforcement.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_advocates, beneficiary,
    organized, biographical, constrained, global).

% Smaller coastal nations in regions without natural islands (Maldives, Bangladesh, some Pacific atolls, and island-poor sections of Africa) are trapped by the strict reading. They lack natural features that would generate EEZs and cannot use artificial construction to fill the gap. The reading locks them into a subordinate position: they have minimal maritime jurisdiction, cannot claim extensive EEZs, and cannot expand resource access through engineering. Their exit is trapped: UNCLOS binds them, but withdrawal would forfeit all maritime protections. They bear the cost of being geography-poor and unable to remedy it.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, island_adjacent_developing_nations, payer,
    moderate, biographical, trapped, regional).

% Private and state-backed investors in artificial island and reclamation projects face stranded assets and regulatory uncertainty under the strict reading. Dredging companies, real estate developers, and sovereign wealth funds that funded projects in the South China Sea and elsewhere have seen their investments deliver only limited benefits: artificial islands can host ports and facilities but gain no territorial seas or EEZs. These investors have mobile exit: they can redirect capital to natural-island development, domestic reclamation (where it generates onshore value but no maritime sovereignty), or other sectors. The constraint reduces the return on artificial island investment and suppresses the sector.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_construction_investors, payer,
    moderate, biographical, mobile, regional).

% The ICJ and international arbitral tribunals (especially the Permanent Court of Arbitration) enforce the strict reading through high-profile case law. The 2016 PCA ruling in Philippines v. China, the ongoing ITLOS cases, and ICJ advisory opinions all reinforce the interpretation that artificial islands do not generate territorial seas. These bodies decide disputes when they arise; their authority is backed by state cooperation and naval power enforcement. They have analytical exit: they could change their interpretive stance, but that would require overturning precedent and would undermine the stability of international law. They are committed to the strict reading.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_court_of_justice, agenda_setter,
    institutional, generational, analytical, global).

% ASEAN, the Association of Small Island States, and regional maritime organizations would advocate for the hybrid or expansive readings if they had a formal voice in UNCLOS adjudication. They argue that effective occupation should count for something and that artificial features built with permanent infrastructure should eventually gain recognition. They are excluded from the formal legal interpretation process: they have no seat at the ICJ and cannot initiate cases that would force reconsideration. Their dissent is recorded in diplomatic forums but does not alter enforcement. They are trapped in the system and must accept the interpretations the ICJ imposes.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, coastal_state_forums, excluded,
    organized, generational, trapped, regional).

% Environmental and marine conservation groups are excluded from the UNCLOS sovereignty debate even though artificial island construction causes significant ecosystem damage (coral destruction, dredging impacts, habitat loss). They oppose both the strict reading (which doesn't address environmental costs of natural-island development as a substitute) and the expansive reading (which would incentivize more construction). They have constrained exit: they can lobby for environmental protections in other forums (UNCLOS Part XII on marine environment, regional agreements, etc.) but cannot change the maritime sovereignty interpretation from within those venues. Their voice is heard but peripheral.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, environmental_organizations, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, globally applicable rule for maritime island definitions and the rights they generate. The rule is universally understood: only naturally formed features above water at high tide qualify as islands generating territorial seas and EEZs. This coordination solves the problem of ambiguity—every party knows what counts as an island, can navigate by that standard, and can build their maritime strategy on stable definitions rather than case-by-case disputes. The rule enables adjudication: when disputes arise, courts have a clear criterion to apply. Without a coordination function, every feature would be a contested claim and maritime law would collapse into power negotiations.
% TRANSFER_FUNCTION: Transfers potential sovereignty—specifically, the right to claim territorial seas and EEZs on submerged features and low-tide elevations—from expansionist coastal states to naval powers and non-claimant states. Coastal states lose the option to expand jurisdiction through artificial construction. Naval powers and non-claimant states gain the assurance that rivals cannot rapidly expand territorial claims and close off waters. Non-claimant states also gain the benefit of a stable map: their own maritime boundaries are fixed to natural geography and cannot be destabilized by engineering elsewhere. Shipping interests gain the benefit of preserved international waters.
% ABSENT_VOICES: Island-adjacent developing nations, ASEAN consensus bodies, and private investors in artificial island infrastructure would object to the strict reading if they had a seat at the table. They would argue for the hybrid or expansive reading to permit artificial features to generate at least limited sovereignty or to mature into claims through effective control. They are not in the room when the ICJ interprets UNCLOS; they are parties only if they initiate cases, which requires resources and strategic alignment with major powers. Environmental organizations would also object if their concerns (coastal ecosystem protection) were part of the sovereignty debate, but they are relegated to separate UNCLOS provisions on marine environment. The strategic exclusion of coastal state forums from adjudicatory processes is a feature, not a bug, of the constraint's design: naval powers benefit from making the rules in forums where expansionist states have limited voice.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading disappeared—either replaced by the expansive reading, the hybrid reading, or no consistent reading at all—the geopolitical map would reorganize within a decade. Expansionist coastal states would immediately accelerate artificial island construction and claim territorial waters on their projects. Chokepoints like the South China Sea, Strait of Hormuz surroundings, and Indian Ocean passages would narrow as territorial claims extended. Navigation costs and risks would rise for shipping. Naval powers would face new strategic competition in waters they previously dominated. EEZ boundaries would shift, affecting fishing rights and resource access. The constraint's disappearance would trigger a cascade of claims, counter-claims, and military posturing. Conversely, if the constraint strengthened (even the strict reading fortified by ICJ endorsement), artificial island projects would decline and the strategic landscape would stabilize further—though the stability would depend on naval power enforcement, not voluntary compliance.
% FOUNDING_PROBLEM: In the 1970s, when UNCLOS was being drafted, large-scale artificial island construction was technically difficult and expensive. The framers needed to define what counts as an island (and thus generates maritime rights) to prevent a loophole where states could engineer sovereignty. The natural/artificial distinction was chosen as the criterion because it appeared stable, universally observable, and resistant to manipulation. A nation either does or does not construct a feature; it cannot retroactively make a constructed feature 'natural.' The distinction also aligned with the principle that rights should flow from geographic givens, not from engineering. The founding problem was real: what is an island in maritime law, and who decides?
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and the ICJ attest the founding problem is still live: modern dredging and reclamation technology make artificial island construction feasible and economically rational for coastal states, so the loophole remains real and needs closing. The 2016 PCA ruling against China explicitly cited the need to maintain the distinction to prevent boundless expansion. Independent legal scholar Yoshifumi Tanaka (in 'The International Law of the Sea,' 2nd ed.) supports the strict reading as the only principled alternative to case-by-case effective-control assessments. Expansionist coastal states and regional development advocates (the Asian Development Bank, ASEAN statements) attest the founding problem is obsolete: UNCLOS Article 60 already addresses artificial structures in EEZs, so the text acknowledges them; effective occupation is a settled principle in customary international law, so the distinction between natural and artificial is anachronistic when both can be occupied and administered. Scholar Clyde Sanger ('Ordering the Oceans,' 1987) documented that the natural/artificial line was always pragmatic, not principled, and that it has become functionally incoherent where artificial islands are permanent, inhabited, and economically integrated. No external corroboration exists that settles the contest: international law scholars are divided, and state practice is contested (China building while the PCA rules against it). The reading persists because naval powers enforce it, not because consensus has validated the founding problem's solution.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.62) because the strict reading denies expansionist states a legitimate pathway to sovereignty expansion that the underlying geography (submerged features, low-tide elevations) would otherwise provide under effective-control readings. The extraction is passive—coastal states lose what they might otherwise claim—rather than active wealth transfer, but the deprivation is real. Suppression is high (0.71) because enforcement depends on naval power willingness to patrol, challenge artificial island activities, and defend the interpretation against dissent. The ICJ rulings (most notably the 2016 Permanent Court of Arbitration award against China) represent active suppression of competing interpretations. Theater ratio is low-moderate (0.28): some rhetorical emphasis on the natural/artificial distinction's elegance and clarity, but the core function is enforcement, not performance. Accessibility collapse is high (0.78) because once the constraint is understood—that artificial construction confers no sovereignty—the alternatives (effective occupation, protracted de facto control, building alliances) are substantially foreclosed for isolated states. Resistance is high (0.72) because expansionist states actively resist the constraint through construction projects (South China Sea islands, Maldives reclamation, others) and through legal counter-arguments at forums where they can voice them, even if they cannot overturn the interpretation. The measurement series show extractiveness rising from 1982–2015 as artificial construction technology matured and the stakes increased, then plateauing after 2015 (the South China Sea arbitration verdict solidified enforcement). Suppression requirement rose similarly, reflecting the increasing enforcement burden as more artificial islands were built and naval patrols intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power perspective (USA, UK, France), the constraint is a stabilizing coordination mechanism that preserves the balance of power and prevents an arms race. From the expansionist coastal state perspective (China, Philippines), the constraint is an unjust extraction of potential sovereignty that locks them into a subordinate position relative to historically hegemonic powers. The ICJ sits analytically and interprets the law; its position is neither truly beneficiary nor victim—it administers the constraint and depends on the backing of naval powers to enforce it against dissent. The engine should compute different types at different seats: beneficiary seats see rope-like coordination; victim seats see snare-like extraction with no real exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers: directionality is low (d ≈ 0.15–0.25), because they benefit from the constraint without bearing its costs. The constraint subsidizes their strategic position. They have arbitrage-grade exit (they could abandon UNCLOS if needed, though costily). Expansionist coastal states: directionality is high (d ≈ 0.75–0.85), because they bear the primary cost (loss of potential EEZ) and cannot exit without violating a treaty they are bound to. Their exit is identity-locked: abandoning UNCLOS entirely would forfeit other protections they rely on (12 NM territorial sea for natural features, EEZ over natural islands, fishing rights). Island-adjacent developing nations: directionality is high (d ≈ 0.80), same reasoning. Non-claimant states: directionality is moderate (d ≈ 0.45–0.55), because they are beneficiaries but also bound by the same rule—they cannot expand through artificial construction either, though they are less motivated to. No directionality overrides are needed; the derivation chain produces the right values from beneficiary/victim declarations and exit analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real in 1982: the framers needed to distinguish islands from installations to prevent unbounded territorial expansion. By 2026, the problem is contested. Technology has made artificial island construction feasible and economically rational for some coastal states. The constraint persists not because it solves an urgent coordination problem—the problem it solved is partly solved (agreements exist on what islands are, at least among most parties)—but because it redistributes strategic advantage. Naval powers enforce the strict reading to prevent rivals from closing off chokepoints and extending territorial claims. The constraint exhibits hallmarks of mandatrophy (the original mandate is obsolete; persistence depends on unequal enforcement power), but it is not pure piton because the coordination function (a stable, universally understood rule for maritime boundaries) is still real, even if subordinated to strategic extraction. The tangled-rope classification captures this: genuine coordination (everyone knows the rule, can navigate by it) layered with asymmetric extraction (expansionists lose what they might otherwise claim). No party benefits enough to maintain it voluntarily; enforcement depends on naval power and ICJ backing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_artificial_boundary_ambiguity,
    'Is the natural/artificial distinction in Article 121(1) a stable, principled boundary, or does it collapse when artificial features become permanent, inhabited, economically integrated, and indistinguishable from modified natural features?',
    'Long-term natural experiment: track whether states that have built extensively (China, UAE, Maldives, Singapore) eventually convince the international community that their artificial features are functionally islands, or whether the boundary hardens despite functional equivalence. Examine ICJ and arbitral case law over 20+ years to detect shifts in how ''artificial'' is defined (is dredging and modification of a natural feature artificial, or does the base have to be submerged?).',
    'If the boundary collapses or softens, the strict reading''s structural foundation erodes and the hybrid or expansive reading becomes more plausible. The constraint''s extractiveness would decrease for coastal states (they would gain some sovereignty as their features mature). If the boundary hardens despite functional pressure, the reading is more stable but faces increasing delegitimacy as artificial features become indistinguishable from islands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_artificial_boundary_ambiguity, conceptual, 'Whether the natural/artificial distinction is principled or merely conventional.').

omega_variable(
    enforcement_sustainability,
    'Can naval powers sustain suppression of the strict reading indefinitely, or does enforcement fatigue, power transitions, or alliance shifts undermine it?',
    'Monitor naval patrol frequencies, naval incident rates, and diplomatic friction over artificial islands in the South China Sea and Indian Ocean. Track ICJ caseload and dispute resolution outcomes. Assess whether rising naval powers (India, others) maintain the strict reading or shift toward effective-control interpretations as their strategic interests diverge from Western navies.',
    'If enforcement weakens or naval powers'' interests diverge, the strict reading loses its backing and effectively becomes a weaker constraint. The expansionist reading gains credibility. If enforcement intensifies (more patrols, more aggressive challenges to artificial islands), the constraint''s suppression increases further but faces delegitimacy and alliance strain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Whether the constraint''s enforcement can be sustained as naval power configurations shift.').

omega_variable(
    reading_foreclosure_mechanism,
    'Does the strict reading FORECLOSE the expansive reading by logical contradiction, or do they COEXIST as competing interpretations held by different state coalitions?',
    'Examine whether a state could coherently hold both the strict reading (natural features only) and the expansive reading (artificial features can gain sovereignty through effective control) in its own legal framework, or whether adoption of one position necessarily entails rejection of the other. The test: can a single adjudicatory body (like the ICJ) rule that natural islands generate full EEZ while artificial islands can mature into territorial claims, or would that be logically incoherent?',
    'If the readings foreclose each other (logically incompatible), the kernel exhibits genuine conflict and one must eventually win—the constraint''s terminal state is determined. If they coexist (different states hold different readings without logical contradiction), the kernel is stable as a multi-party dispute and the constraint persists as a contested arrangement. This affects whether mandatrophy is eventual (foreclosure) or chronic (coexistence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Whether the strict and expansive readings are logically incompatible or empirically divided.').

omega_variable(
    suppression_internalization,
    'Do island-adjacent coastal states accept the strict reading''s authority because they internalize its legitimacy, or do they accept it only because of external enforcement by naval powers?',
    'Track diplomatic statements, legal briefs, and domestic political discourse from expansionist states over time. If they gradually frame artificial island projects as legitimate despite the strict reading (i.e., they continue construction while explicitly dissenting from the interpretation), suppression is purely structural. If they internalize the reading and self-police their ambitions, suppression has become partially internalized.',
    'If suppression is purely structural and dependent on external enforcement, the constraint is vulnerable to power transitions and withdrawal of naval power backing. If suppression is internalized, the constraint has deeper legitimacy and persistence. A shift from structural to internalized suppression would actually increase the constraint''s effective stability even if enforcement intensity declined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural (external enforcement) or internalized (accepted as legitimate).').

omega_variable(
    sibling_reading_stability,
    'As technology advances and artificial island construction becomes cheaper and more capable, do the sibling readings (expansive and hybrid) become more structurally compelling, or does the strict reading''s clarity advantage overcome functional pressure?',
    'Monitor the evolution of cost curves for artificial island construction and the economic returns (resource access, strategic depth) that would accrue under competing readings. Track the ratio of artificial islands actually built (indicating economic viability) to the number of potential construction sites (indicating constraint-driven suppression of projects). If the ratio rises, the constraint is weakening; if it remains low, the constraint is stable.',
    'Rising cost-benefit ratios for artificial islands under expansive/hybrid readings would increase pressure on the strict reading and boost the salience of sibling readings. The constraint would face growing resistance and delegitimacy. The terminal state would shift toward hybrid or expansive readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_stability, empirical, 'Whether economic and technological change strengthens or weakens the strict reading relative to alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement_basis(uncl_tr_t1982, observed).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement_basis(uncl_tr_t1995, observed).
narrative_ontology:measurement(uncl_tr_t2007, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2007, 0.22).
narrative_ontology:measurement_basis(uncl_tr_t2007, observed).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t2015, observed).
narrative_ontology:measurement(uncl_tr_t2021, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t2021, observed).
narrative_ontology:measurement(uncl_tr_t2026, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement_basis(uncl_be_t1982, observed).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(uncl_be_t1995, observed).
narrative_ontology:measurement(uncl_be_t2007, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2007, 0.52).
narrative_ontology:measurement_basis(uncl_be_t2007, observed).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(uncl_be_t2015, observed).
narrative_ontology:measurement(uncl_be_t2021, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(uncl_be_t2021, observed).
narrative_ontology:measurement(uncl_be_t2026, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(uncl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement_basis(uncl_su_t1982, observed).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(uncl_su_t1995, observed).
narrative_ontology:measurement(uncl_su_t2007, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2007, 0.66).
narrative_ontology:measurement_basis(uncl_su_t2007, observed).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement_basis(uncl_su_t2015, observed).
narrative_ontology:measurement(uncl_su_t2021, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement_basis(uncl_su_t2021, observed).
narrative_ontology:measurement(uncl_su_t2026, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(uncl_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.18).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_territorial_claims).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, arctic_maritime_jurisdiction).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_state_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (unclos_maritime_sovereignty). The kernel is UNCLOS Part V's definition of maritime islands and the rights they generate. Three structurally distinct constraints instantiate three competing readings: strict_geographic_reading (this file), expansive_construction_reading (sibling), and hybrid_effective_control_reading (sibling). Each reading has different ε values, different victim/beneficiary structures, and different terminal states. They are NOT the same constraint measured differently—they are different claims about what UNCLOS text permits. The strict reading claims only natural features qualify (ε = 0.62, extraction from expansionist states denied of potential sovereignty). The expansive reading claims artificial features on submerged bases qualify if occupied (ε would be ~0.35 from the expansionist perspective, only modest extraction). The hybrid reading claims both paths exist with different thresholds (ε ≈ 0.50, moderate extraction from all parties). The readings compete in state practice and adjudication; all three remain live interpretations as of 2026. Network links route contamination propagation: if one reading gains adherents, the others' operational stability changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
