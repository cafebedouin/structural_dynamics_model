% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Persistence via Incumbent Beneficiary Defense
 *   domain: technology/standards/industrial_history
 *
 * SUMMARY:
 *   The QWERTY keyboard layout has persisted for over 140 years despite
 *   documented evidence that alternative layouts (Dvorak, Colemak) offer
 *   ergonomic and efficiency advantages. Under THIS READING
 *   (incumbent_preservation_reading), the persistence is explained by active
 *   defense from beneficiary sets — manufacturers, trained workers, and
 *   institutions whose capital is invested in QWERTY — who collectively
 *   maintain the standard against alternatives. This reading frames the
 *   constraint as a Tangled Rope: a genuine coordination function (unified
 *   layout) coupled with asymmetric extraction (innovations suppressed,
 *   ergonomic improvements blocked, alternatives locked out). The sibling
 *   reading (lapsed_alternatives_reading) attributes persistence to
 *   coordination value alone — alternatives simply fail to reach critical
 *   mass naturally. These readings are NOT compatible within a single
 *   institutional framework; one holds that beneficiaries actively defend the
 *   standard, the other holds that the standard persists passively because
 *   alternatives cannot overcome adoption thresholds. This JSON instantiates
 *   the incumbent_preservation_reading only.
 *
 * KEY AGENTS:
 *   - mechanical_typewriter_manufacturers: Organized institutional power; actively defend QWERTY through industry coordination, patent strategies, and market dominance. Beneficiary — their capital is locked into QWERTY production.
 *   - trained_typist_cohort: Organized power; benefit from QWERTY dominance because their skills transfer across machines. Their resistance to alternatives is self-interest, not passive coordination.
 *   - typing_instruction_institutions: Moderate power; benefit from standardized curriculum. They actively defend QWERTY by teaching it as THE standard and excluding alternatives from training.
 *   - keyboard_equipment_vendors: Organized institutional power; benefit from manufacturing standardization. They actively suppress alternative-keyboard manufacturers through supply-chain control.
 *   - alternative_keyboard_adopters: Powerless; systematically excluded by network effects and market structure. They bear the cost of inefficient layouts.
 *   - ergonomic_innovation_developers: Moderate power; victims of structural suppression. Their innovations are blocked not by explicit prohibition but by economic incentive misalignment created by the standard's lock-in.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Persistence via Incumbent Beneficiary Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology/standards/industrial_history").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, 'e6b0d3f7-d007-4d71-8050-5a6b12d384ef').
narrative_ontology:cs_kernel_codification('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', formalized).
narrative_ontology:cs_authority_grounding('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', extraction).
narrative_ontology:cs_reading_relation('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', foundational, incumbent_beneficiaries_actively_defend_standard).
narrative_ontology:cs_axiom_status(incumbent_beneficiaries_actively_defend_standard, holdable).
narrative_ontology:cs_axiom_grounding('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', incumbent_beneficiaries_actively_defend_standard, empirically_contingent).
narrative_ontology:cs_axiom('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', secondary, network_effects_amplify_intentional_suppression).
narrative_ontology:cs_axiom_status(network_effects_amplify_intentional_suppression, holdable).
narrative_ontology:cs_axiom_grounding('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', network_effects_amplify_intentional_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', coordinated_industry_standard_serving_collective_interest).
narrative_ontology:cs_drift_state('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', late_twentieth_century_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e6b0d3f7-d007-4d71-8050-5a6b12d384ef', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, mechanical_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typist_cohort).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_instruction_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_equipment_vendors).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomic_innovation_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, end_users_general).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, end_users_general).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant manufacturers (Underwood, Royal, Remington) who collectively established QWERTY as the standard. They actively maintain the standard through industry associations, patent strategies, and market dominance, preventing alternative keyboard layouts from gaining traction. Their capital investments in manufacturing infrastructure, trained sales force, and brand identity are locked into QWERTY production. They benefit from the standard's lock-in because competitors cannot easily shift to alternative layouts and gain market share.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, mechanical_typewriter_manufacturers, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, mechanical_typewriter_manufacturers, beneficiary).

% Millions of trained typists whose muscle memory and professional identity are bound to QWERTY. They benefit from the standard because their skills transfer across machines, employers demand QWERTY, and retraining on alternatives would be costly and career-disruptive. They resist alternatives because switching imposes immediate productivity loss and professional risk. Their skills hold value precisely because of the standard's dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typist_cohort, beneficiary,
    organized, biographical, constrained, global).

% Schools, business colleges, and vocational training programs that teach QWERTY typing. They benefit from standardization because curriculum development and instructor training are amortized across large student populations. Switching to alternative layouts would require rewriting curricula, retraining instructors, and rebuilding relationships with equipment vendors. They actively defend QWERTY by teaching it as THE standard, creating path dependence in the next generation of workers.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_instruction_institutions, beneficiary,
    moderate, generational, constrained, regional).

% Companies that manufacture keyboards and keyboard-related office equipment. They benefit from QWERTY standardization because they can manufacture at scale, reduce inventory complexity, and avoid the coordination problem of supporting multiple layouts. They actively support the standard through industry participation and resist alternative-keyboard manufacturers' market entry through competitive pricing and supply-chain advantages.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_equipment_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Early adopters and efficiency-seeking users who tried alternative layouts (Dvorak, Colemak, etc.). They face network effects that punish adoption: machines rented or purchased come pre-set to QWERTY, repair services only know QWERTY, rental agencies refuse to special-order alternatives, and employers reject job applicants trained on non-standard layouts. Their exit option would be learning QWERTY anyway to function in a QWERTY-dominant world, making the alternative investment sunk and wasted.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_adopters, excluded).

% Innovators and researchers developing ergonomically superior keyboard layouts (reduced finger travel, better load distribution, addressing repetitive strain injury). They are systematically excluded from commercialization: manufacturers will not produce non-standard layouts, office environments will not adopt them, users will not invest in learning them because of QWERTY's network effects, and insurance companies underwriting workplace ergonomics do not recognize non-standard layouts in liability calculations. Their innovations are suppressed by structural rather than explicit barriers.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomic_innovation_developers, payer,
    moderate, biographical, constrained, global).

% Ordinary users who benefit from a single, standardized layout across all machines they encounter (home, office, rental, public terminals) without needing to learn multiple systems. They also pay implicitly through reduced typing efficiency compared to layouts optimized for English or their native language, repetitive strain injury rates higher than would occur under ergonomic alternatives, and slower typing speeds than physically possible with better-designed layouts. Their coordination benefit is real but asymmetrically distributed: trained professionals benefit more; casual users bear the inefficiency cost.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, end_users_general, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, end_users_general, payer).

% The economic system itself that exhibits lock-in dynamics. No single actor directs the persistence; rather, manufacturers, users, institutions, and infrastructure suppliers are all mutually reinforcing the standard. This is a structural observer position that can measure the collective effect without being a party to its maintenance.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, office_equipment_market, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, mechanical_typewriter_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universal keyboard layout so that training in one location transfers across machines and geographies; employers can hire trained typists without layout retraining; equipment manufacturers can standardize on one input configuration; users encounter consistent interfaces across machines. This solves the genuine coordination problem of avoiding a proliferation of incompatible layouts that would fragment the trained workforce and fragment equipment manufacturing.
% TRANSFER_FUNCTION: Transfers value from alternative-keyboard seekers and ergonomic innovators to incumbent manufacturers, trained typist cohorts (whose skills retain value), and institutions that have invested in QWERTY curriculum. The arrangement moves: time (retraining burden avoided by incumbents, imposed on late adopters); efficiency (improvements that would flow from better layouts are captured as monopoly rents); and access to innovation (ergonomic improvements are suppressed because they threaten the incumbent's capital base).
% ABSENT_VOICES: Potential users of alternative layouts and ergonomic innovations are structurally excluded — they cannot participate in the standard-setting conversation because they have not yet adopted or cannot adopt without sacrificing network benefits. Occupational health researchers and repetitive strain injury victims would argue for ergonomic alternatives but are not represented in the office equipment market's decision-making bodies. Labor unions, which might advocate for worker health outcomes, are absent from keyboard standardization discussions.
% DISAPPEARANCE_RATIONALE: If the QWERTY-preservation mechanism vanished and keyboard layouts were allowed to diverge, the office equipment market would rapidly consolidate around one or two ergonomically superior layouts within 15–20 years (the generational retraining window). Manufacturers would shift production, training institutions would update curricula, and the current trained typist advantage would erode. The dominant alternative would likely be Dvorak, Colemak, or a descendant of these. The entire organizational infrastructure supporting QWERTY persistence would become obsolete — the constraint is not natural law, it is a maintained arrangement whose persistence depends on active beneficiary defense.
% FOUNDING_PROBLEM: In the late 19th century, typewriter manufacturers competed on keyboard layout, causing fragmentation that made trained typists unable to transfer between machines and made equipment procurement chaotic for offices. QWERTY was adopted (not because it is optimal, but because Underwood dominated the market and used QWERTY) and became self-reinforcing: training was standardized on it, users locked their skills into it, and manufacturers coordinated production around it. The founding problem was real: standardization solved a genuine coordination failure in the typewriter market.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians (Paul David, economic historians of standardization) attest that the founding problem — manufacturing fragmentation and typist retraining burden — was solved by the early 20th century and has remained solved. The manufacturers and training institutions attest the problem is 'eternal' (users always need a standard), but they are the direct beneficiaries of continuation. Labor economists and ergonomic researchers attest the problem that NOW requires solution is the lock-in trap, not fragmentation risk — the original problem became obsolete once QWERTY achieved dominance.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the standard captures efficiency gains that would flow to users if alternatives could compete; suppression is higher still (0.72) because the constraint requires active defense — manufacturers must maintain market dominance, institutions must exclude alternatives from training, and equipment networks must suppress non-standard options. The measurement series shows BOTH extractiveness and suppression rising over the interval: extractiveness increases from 0.38 to 0.68 as the constraint's scope expands from mechanical typewriters into electric typewriters and then digital keyboards — each new technology platform is locked into QWERTY, expanding the set of victims. Suppression rises in parallel (0.42 to 0.72) as incumbents invest more heavily in defensive mechanisms — patent disputes over alternative layouts, aggressive marketing against Dvorak, exclusion of Colemak from hardware manufacturers. Theater rises most sharply (0.08 to 0.41) because early defense was functional (manufacturers genuinely needed to coordinate), but by mid-interval, defense becomes increasingly performative — the coordination problem is solved, but the standard persists through rhetoric about 'user familiarity' and 'proven reliability' rather than genuine network benefits. All metrics share one time grid (0, 12, 25, 50, 75, 100) with all metrics authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (manufacturers) and the payer seat (alternative adopters and innovators) should compute very differently. From the manufacturer's position, QWERTY is a coordination achievement they built and maintain — a rope. From the alternative-adopter's position, the same standard operates as enforced extraction — a snare. The engine computes this divergence from the directionality chain: manufacturers are low-d (beneficiary), alternative adopters are high-d (trapped and victimized). The claimed_type is tangled_rope because BOTH the coordination function AND the asymmetric extraction are structurally present; the engine will likely compute snare-from-victim-seat and rope-from-beneficiary-seat, revealing the asymmetry the claim names.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: manufactured-typist-cohort, typing-institutions, and equipment-vendors all explicitly collect benefits (career value, curriculum stability, manufacturing scale). Their exit_options are constrained-to-arbitrage (they can shift to alternatives but face sunk costs), and their power is organized-to-institutional. These factors derive low d, approaching beneficiary territory. Victim derivation: alternative-adopters and innovation-developers are explicitly victimized (efficiency suppressed, innovation blocked). Their exit_options are trapped (they cannot use alternatives without professional penalty) or constrained (innovators face market-access barriers). Their power is powerless-to-moderate. These factors derive high d, approaching target territory. The trained-typist-cohort sits ambiguously — they are classified as beneficiary in the stakeholder layer, but from the engine's perspective they are semi-captured by the standard (their skills carry value partly because QWERTY dominates; if alternatives proliferated, their skills would be less distinctive). The directionality derivation will likely place them near d=0.4, slightly beneficiary-side but closer to symmetric than pure beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandate-obsolescence: the founding problem (manufacturing fragmentation and typist retraining chaos) is DEAD — solved in the 1920s when QWERTY achieved dominance. Yet the constraint persists at high extractiveness and high theater. The beneficiaries (manufacturers, trained workers, institutions) now defend it not because the coordination problem requires solution, but because their capital base depends on the standard. The theater_ratio rise (from 0.08 to 0.41) is the signature: early enforcement was functional (genuine coordination work), but mid-to-late enforcement is increasingly theatrical — arguments invoke 'user familiarity' (true but not a founding problem), 'proven reliability' (true but not a founding problem), and 'disruption costs' (true but artificially inflated by the beneficiaries' own lock-in strategy). The founding_problem_status=dead + disappearance_verdict=world_rearranges mismatch confirms the mandate is obsolete. The classification prevents misreading this as a rope (genuine coordination) when it has become primarily extraction masked by coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_network_effect,
    'Is QWERTY''s persistence driven by active, conscious defense from beneficiary sets (manufacturers, trained workers, institutions), or does it persist passively because the network effects of coordination simply overwhelm alternatives even without organized opposition?',
    'Historical evidence of manufacturer coordination and anti-alternative campaigns (patent disputes, market exclusion); testimony from alternative-layout advocates regarding barriers encountered; longitudinal data comparing suppression intensity over time against adoption curves of alternatives. If suppression is constant and alternatives fail to adopt, the network-effect hypothesis is more plausible; if suppression rises when alternatives threaten adoption, the active-defense hypothesis is confirmed.',
    'If active defense is confirmed, the constraint is correctly classified as Tangled Rope with high suppression; if passive, it might reclassify toward Rope or Piton (depending on whether beneficiaries still profit from the standard or are merely maintaining inherited inertia). Under this reading, we assume active defense is the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_passive_network_effect, empirical, 'Whether QWERTY persistence is maintained by active beneficiary defense or passive network-effect coordination failure.').

omega_variable(
    ergonomic_suppression_mechanism_structural_vs_intentional,
    'Are ergonomic innovations (Dvorak, Colemak, etc.) suppressed by intentional market actions from beneficiaries, or are they suppressed by structural (economic) factors that emerge even without organized opposition — manufacturing cannot rationalize production of minority layouts, users cannot economically justify learning alternatives, institutions cannot amortize curriculum development across small populations?',
    'Counterfactual: if manufacturers had been willing to produce alternative layouts at scale, would institutions and users have adopted them despite coordination costs? Evidence from niche markets (Dvorak adoption in specialized communities, Colemak adoption by programmer subcultures) to test whether alternatives can gain traction when structural barriers are lower; oral history from innovators regarding obstacles encountered (intentional rejection by manufacturers vs. economic infeasibility).',
    'If suppression is primarily structural and impersonal, the extraction might be lower than authored (beneficiaries benefit from the standard, but they are not actively victimizing innovators — the victimization is a side effect). If suppression is intentional (manufacturers actively preventing alternative production, institutions actively excluding alternatives from curricula), the extraction is correctly authored as high. This reading assumes mixed mechanism: both structural (economic infeasibility) and intentional (active market dominance defense), with the intentional component as the primary suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_suppression_mechanism_structural_vs_intentional, empirical, 'Whether ergonomic innovation suppression is structural-economic or intentionally-enforced by beneficiary actions.').

omega_variable(
    beneficiary_conscious_vs_unconscious_defense,
    'Do the beneficiary sets (manufacturers, trained typists, institutions) consciously coordinate to defend QWERTY, or do they each act in self-interest unaware of the collective defensive effect? Is QWERTY defense an organized conspiracy or an emergent property of independent rational actors?',
    'Documentary evidence of industry associations'' decisions and statements regarding alternative layouts; interviews with decision-makers regarding conscious defense strategies; comparison of deliberate market-suppression actions (patent litigation, exclusive supplier agreements) vs. incidental side effects of profit-maximization (manufacturing scale economies).',
    'If defense is unconscious/emergent, the constraint is a Tangled Rope with lower intentional extraction and higher structural extraction — it is still extractive but not conspiratorial. If defense is conscious and coordinated, the classification holds as stated with intentional suppression. This reading is agnostic on the consciousness question; the extraction and suppression metrics remain the same either way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_conscious_vs_unconscious_defense, conceptual, 'Whether QWERTY defense is an organized strategy or an emergent property of separate actors'' self-interest.').

omega_variable(
    beneficiary_reading_vs_lapsed_alternatives_reading_foreclosure,
    'Does the incumbent-preservation reading FORECLOSE the lapsed-alternatives reading, or do both readings coexist as different parties'' accounts of the same phenomenon?',
    'Logical analysis: the incumbent-preservation account (active defense) is compatible with the lapsed-alternatives account (network effects) only if BOTH are true simultaneously — benefits actively defend AND alternatives fail to reach critical mass. The readings foreclose each other only if one denies the other''s core premise. The incumbent reading asserts active defense; the lapsed reading implicitly denies it (it does not mention defense). If the lapsed reading were to state ''network effects occur despite active opposition from beneficiaries,'' the readings would coexist; as written, the lapsed reading treats adoption failure as natural, not as a response to opposition.',
    'The declared reading_relation in cs_structure.reading_relations is ''coexists_with'' because both readings are live in the literature and held by different scholarly factions. However, the core premises are in tension — if both were validated empirically, they would be unified into a single account (''active defense happens AND network effects amplify it''). For now, they coexist as competing interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_reading_vs_lapsed_alternatives_reading_foreclosure, conceptual, 'Logical relationship between the incumbent-preservation reading and the lapsed-alternatives reading of the QWERTY persistence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_incumbent_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_tr_t12, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_tr_t25, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t25, observed).
narrative_ontology:measurement(qwerty_incumbent_tr_t50, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t50, observed).
narrative_ontology:measurement(qwerty_incumbent_tr_t75, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t75, observed).
narrative_ontology:measurement(qwerty_incumbent_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(qwerty_incumbent_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwerty_incumbent_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_be_t12, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_be_t25, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t25, observed).
narrative_ontology:measurement(qwerty_incumbent_be_t50, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t50, observed).
narrative_ontology:measurement(qwerty_incumbent_be_t75, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t75, observed).
narrative_ontology:measurement(qwerty_incumbent_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(qwerty_incumbent_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_incumbent_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t0, observed).
narrative_ontology:measurement(qwerty_incumbent_su_t12, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t12, observed).
narrative_ontology:measurement(qwerty_incumbent_su_t25, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t25, observed).
narrative_ontology:measurement(qwerty_incumbent_su_t50, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t50, observed).
narrative_ontology:measurement(qwerty_incumbent_su_t75, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 75, 0.71).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t75, observed).
narrative_ontology:measurement(qwerty_incumbent_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(qwerty_incumbent_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.15).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into two structurally distinct readings: (1) incumbent_preservation_reading (this file) — standards persist via active beneficiary defense, Tangled Rope with high extraction; (2) lapsed_alternatives_reading — standards persist via coordination value, alternatives fail to reach critical mass, Rope with minimal extraction. The epsilon values diverge because the readings disagree on the MECHANISM of persistence: the incumbent reading includes defensive suppression costs in epsilon, while the lapsed reading does not. The two readings are linked: adoption failure in the lapsed reading is partly explained by suppression in the incumbent reading. The incumbent reading influences the lapsed reading because if beneficiaries are actively defending the standard, then alternatives face artificially high adoption barriers, amplifying the coordination-failure explanation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
