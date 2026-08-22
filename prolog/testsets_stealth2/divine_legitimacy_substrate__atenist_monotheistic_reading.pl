% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Reading of Divine Legitimacy: Pharaonic Interpretive Monopoly
 *   domain: religious/political/economic (ancient Egypt, Amarna period)
 *
 * SUMMARY:
 *   During the Amarna interval (modeled here as regnal years 0-17), the royal
 *   house proclaimed that divine legitimacy flowed solely through the king's
 *   revelation of the Aten as the exclusive deity, that all other gods were
 *   false, and that the king alone stood between the god and humanity. The
 *   established temples were closed or starved of offerings, the name of Amun
 *   was chiseled out of monuments including royal ones, a purpose-built
 *   capital at Akhetaten absorbed labor and revenue, and a new cult
 *   establishment owed its entire existence to royal favor. This story
 *   instantiates ONE reading of the divine_legitimacy_substrate kernel - the
 *   atenist_monotheistic_reading - as a clean, epsilon-invariant constraint.
 *   The epsilon referent is the standing Atenist arrangement itself as it
 *   actually operated, not the Amun arrangement it displaced and not any
 *   idealized form of the doctrine. The sibling readings
 *   (amun_polytheistic_reading, folk_syncretistic_reading) are separate
 *   constraints with their own epsilon, beneficiary structures, and
 *   classifications, linked through network.affects_constraints. Claim and
 *   metrics are authored independently: I claim snare because the
 *   arrangement's persistence depended on coercion and the suppression of
 *   alternatives, with identifiable victims and a thin coordination story;
 *   the metrics describe its actual operation without being tuned to that
 *   claim.
 *
 * KEY AGENTS:
 *   - pharaonic_crown: agenda-setter and principal collector (institutional/arbitrage) - proclaims the doctrine, closes rival temples, receives redirected revenue and labor
 *   - aten_cult_establishment: secondary beneficiary (organized/identity_locked) - holds office, estates, and tombs that exist only inside the new order
 *   - amun_priesthood: principal target (organized/trapped) - wealthiest cult dispossessed, its god's name erased, institutions surviving in disuse
 *   - traditional_temple_establishments: secondary targets (moderate/constrained) - Ptah, Osiris, Hathor, and provincial cults cut off from revenue and recognition
 *   - village_household_practitioners: diffuse targets (powerless/trapped) - household and village multiplicity condemned officially, continued covertly
 *   - amarna_laborers: labor-flow participants (powerless/trapped, ration-benefiting) - built and populated the new capital under quota and surveillance
 *   - levantine_vassal_rulers: excluded outsiders (moderate/mobile) - client kings invoking their own gods in diplomatic correspondence, outside the enforcement perimeter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.74).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.85).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Reading of Divine Legitimacy: Pharaonic Interpretive Monopoly").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political/economic (ancient Egypt, Amarna period)").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '36adf3f6-cce6-4985-aec0-d085e234d1a9').
narrative_ontology:cs_kernel_codification('36adf3f6-cce6-4985-aec0-d085e234d1a9', fixed_text).
narrative_ontology:cs_authority_grounding('36adf3f6-cce6-4985-aec0-d085e234d1a9', extraction).
narrative_ontology:cs_reading_relation('36adf3f6-cce6-4985-aec0-d085e234d1a9', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('36adf3f6-cce6-4985-aec0-d085e234d1a9', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('36adf3f6-cce6-4985-aec0-d085e234d1a9', foundational, aten_exclusive_deity).
narrative_ontology:cs_axiom_status(aten_exclusive_deity, holdable).
narrative_ontology:cs_axiom_grounding('36adf3f6-cce6-4985-aec0-d085e234d1a9', aten_exclusive_deity, theological).
narrative_ontology:cs_axiom('36adf3f6-cce6-4985-aec0-d085e234d1a9', foundational, pharaoh_sole_divine_intermediary).
narrative_ontology:cs_axiom_status(pharaoh_sole_divine_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('36adf3f6-cce6-4985-aec0-d085e234d1a9', pharaoh_sole_divine_intermediary, theological).
narrative_ontology:cs_reference_frame('36adf3f6-cce6-4985-aec0-d085e234d1a9', akhenaten_sole_revelation_order).
narrative_ontology:cs_drift_state('36adf3f6-cce6-4985-aec0-d085e234d1a9', late_amarna_succession_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36adf3f6-cce6-4985-aec0-d085e234d1a9', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_crown).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_cult_establishment).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_establishments).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, village_household_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_laborers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_laborers).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_sole_deity_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_sole_intermediation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules as the living intermediary of the sole god, having taken a throne name honoring the Aten and moved the court to a purpose-built capital. Proclaims the doctrine in hymns and boundary stelae, appoints the new cult's officials, orders rival temples closed and rival gods' names removed from monuments down to funerary inscriptions. Receives the revenues, offering streams, and labor previously routed to the established temples. The army, treasury, and recording apparatus answer to the palace personally; no internal body can check or reverse the program while the king lives.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% New priesthood, stewards, treasurers, and administrators raised up around the royal city and the great Aten temple. Hold office, land grants, rations, and rock-cut tombs bestowed by the king; many carry personal names reshaped to honor the sole god. Their titles, property, and burial places exist only inside the new order - if it ends, they lose standing, income, and prepared tombs at a stroke. Service is continuous and visible; distance from the palace is suspicion.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_cult_establishment, beneficiary,
    organized, biographical, identity_locked, regional).

% Custodians of the wealthiest god's cult at Thebes, stripped of temples, estates, offerings, and staff. Their god's name is chiseled out of monuments, including royal ones, and honoring it becomes dangerous; workshops erase it even from commissioned pieces. They retain scribal learning, kin networks, and popular attachment, but hold no legal standing or income during the interval. Their only available strategy is endurance - maintaining memory and personnel quietly until the enforcement structure changes hands.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Cults of Ptah at Memphis, of Osiris, Hathor, and the provincial gods, closed or starved of offerings, staff, and legal recognition. Less singled out than Amun but equally cut off from revenue. Some priests and scribes drift into royal service at Akhetaten; others disperse to villages. Their buildings stand, their endowments lapse, and their festivals cease.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_establishments, payer,
    moderate, generational, constrained, national).

% Farmers, artisans, and villagers along the Nile whose household shrines, amulets, and festivals invoke many gods. Official doctrine condemns their practice, but enforcement reaches villages thinly; they continue old observances indoors, bury their dead with traditional protective figures, and hand the old gods' names to their children. Exposure is episodic - a zealous official, a requisition, a visit - rather than constant, and leaving the land is not a real option.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, village_household_practitioners, payer,
    powerless, biographical, trapped, local).

% Workforce conscripted or drawn to build and inhabit the new capital: stonecutters, tomb crews, brickmakers, bakers, soldiers. Receive standard rations, housing rows, and recorded wages - steadier provision than many villages offer - while carrying accelerated construction quotas under close administrative surveillance, with work-group leaders answerable for output. Leaving means forfeiting rations and returning to taxed village life.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_laborers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_laborers, beneficiary).

% Client kings of Levant city-states under Egyptian overlordship. Correspond with the court in cuneiform, requesting gold and troops while invoking their own gods - Baal, Ishtar, Shamash - by name in their letters. The exclusive-cult doctrine is never extended to them; they sit outside its enforcement perimeter, hedge between powers, and petition over the king's head when aid fails.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, levantine_vassal_rulers, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_crown).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single unified state cult with one legitimate interpreter: standardized royal ritual, a reformed calendar ordered around the sun's daily course, and concentrated mobilization of labor and material for one sacred city and its temples.
% TRANSFER_FUNCTION: Moves temple revenues, offering streams, priestly offices, and corvee labor from the established cult establishments - above all Amun's at Thebes - to the royal house and the new Aten establishment at Akhetaten; moves interpretive authority over the divine from distributed priesthoods to the king alone.
% ABSENT_VOICES: The dispossessed Amun clergy could not speak without inviting destruction of their names and tombs; their silence in the surviving record is enforced, not consenting. Village practitioners left no textual voice at all - their persistence is visible only in archaeology. Levantine vassals invoked their own gods through diplomatic channels that carried no standing inside the Egyptian framework. The apparent unanimity of hymns, tomb inscriptions, and court art reflects a record produced entirely inside the beneficiary circle.
% DISAPPEARANCE_RATIONALE: When enforcement lapsed, the world visibly rearranged: the court abandoned Akhetaten and returned to Thebes and Memphis, the closed temples reopened and resumed offerings, erased names were recarved, the new capital's administration dissolved, and the Aten establishment's holders lost office and standing. Arrangements - capital location, temple funding, clerical careers, tomb preparation - demonstrably depended on the arrangement's continuation.
% FOUNDING_PROBLEM: The Amun priesthood's accumulated wealth and autonomous authority rivaled the crown: temple estates, granaries, and oracular influence mediated the king's legitimacy and constrained royal fiscal freedom. The arrangement was built to dissolve independent sacred power centers and route all divine authority and temple revenue through the royal house alone.
% FOUNDING_PROBLEM_CORROBORATION: Tutankhamun's Restoration Stela - composed by the restoring regime, outside the Atenist beneficiary set - attests that the temples of the gods had fallen into neglect and their offerings had ceased, corroborating both the founding problem's framing and the arrangement's failure to resolve it. Ramesside and later records show the Amun establishment restored and ultimately dominant over the throne. Vassal correspondence in the Amarna archive attests that the exclusive-cult claim never governed the empire's periphery. No source outside the beneficiary set attests the founding problem as solved.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.74 at interval end) because the arrangement moved temple revenues, offering streams, priestly offices, and corvee labor wholesale from the established cults to the crown and the new Aten establishment, with the rate and destination set unilaterally by the king. Suppression is higher still (0.85) because persistence depended on active destruction of alternatives: temple closures, name erasure extending to the founder's own father's cartouches, persecution of the Amun clergy, and prohibition of plural deities even in private names. Theater ratio is moderate (0.32): the liturgical production was real - the Great Hymn to the Aten, open-air sun temples, a reformed ritual calendar - but an increasing share of activity served to display the king's unique access rather than to conduct a cult that needed it. Accessibility collapse is 0.68: official alternatives were eliminated thoroughly, but covert household practice persisted, keeping the figure below natural-law range. Resistance is 0.5: within the interval, resistance was muted by danger (passive non-compliance, quiet continuation, elite silence); its decisive form arrived only after the enforcer died. The three temporal series share one seven-point grid (years 0, 3, 6, 9, 12, 15, 17) so every metric is authored at every examined time point. Suppression_requirement is tracked deliberately: the story traces an enforcement-machinery build-up - closures and revenue seizure in the early years, culminating in the year-9 proscription campaign that extended erasure to tombs and compound theonyms - holding at a high plateau before late-reign strain. Extractiveness peaks mid-interval and eases slightly at the end as enforcement capacity frays and flows leak; theater rises monotonically as ritual increasingly performs royal uniqueness. The identity_coordination declaration carries a gaming risk flagged in the guidance: 'this is who we are - the Aten's people' framing could dress collection as belonging; the coupling profile here (collection concentrated on dispossessed organized cults at national scope while the collector sits at arbitrage-grade exit) is the nonsensical-coupling signature, and the declaration is made so the engine tests it, not to excuse it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the crown's position the arrangement is legitimate self-order: the king expresses what the god revealed to him alone, and the redistribution of temple wealth is the correction of an imbalance. From the Aten establishment's position it is livelihood and standing fused with doctrine - their names, tombs, and titles exist only inside it, so their computed classification is colored by identity-lock even as they collect. From the Amun clergy's position it is dispossession awaiting reversal: organized, learned, popularly attached, legally annihilated. From the village practitioners' position it is a distant decree that rarely reaches their courtyards - high nominal d, low realized pressure. From the vassal rulers' position it barely exists. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown sits nearest the beneficiary pole: it declares the doctrine, administers enforcement, and collects the flows, bearing essentially none of the costs (d near 0.0, reinforced by arbitrage-grade exit - it restructured the entire cultic landscape at will). The Aten establishment also collects, but its identity_locked exit pulls its effective position somewhat toward the target side: it gains while remaining unable to leave. The Amun priesthood and traditional temple establishments sit near the full-target pole (d near 1.0): they bear the transfers directly, and their trapped exits amplify effective extraction. Village practitioners carry high directional exposure but at local scope with thin enforcement contact, so the engine's scope scaling tempers their realized burden. Laborers bear the labor flow while receiving rations - a mixed position resolved by their secondary beneficiary role. Vassal rulers are outside the arrangement's operative scope entirely. No directionality overrides are needed: the beneficiary/victim declarations plus exit options produce the correct d values for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabelings. First, the arrangement's unity-and-purity rhetoric could dress confiscation as coordination - claiming rope or tangled_rope would require a genuine collective-action solution with net beneficiaries, and the record shows the 'coordination' (a unified cult under one interpreter) was imposed on parties who were not asked and who reversed it the moment enforcement lapsed. Second, the arrangement's rapid collapse should not be misread as mere intellectual failure: collapse-on-enforcer-death is the signature of coercion-dependent persistence, not of a bad idea that lost a fair argument. The genealogy interview is coherent rather than zombified: the founding problem (rival sacral power constraining the crown) was live when the arrangement was built, remains live after it (the restored Amun establishment eventually overshadowed the throne itself), and the disappearance verdict is world_rearranges - so the dead-mandate-plus-world_rearranges mismatch flag does not fire. The arrangement died before its mandate did; that ordering, not theatrical maintenance, is what separates this case from a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (atenist_monotheistic_reading) of the divine_legitimacy_substrate kernel; what would change structurally if the amun_polytheistic_reading or the folk_syncretistic_reading were instantiated instead?',
    'Cross-read the sibling stories'' beneficiary/victim sets and epsilon values: the amun reading concentrates collection in priestly colleges with the crown as a constrained payer; the folk reading distributes practice across households with minimal capture. Comparing computed per-seat types across the three files locates the disagreement structurally.',
    'Under the amun reading, temple establishments become beneficiaries and the crown becomes a target seat; under the folk reading, elite-level collection largely vanishes and the arrangement approaches low-overhead local coordination. Victim sets and epsilon are reading-relative, not topic-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: reading-indexed classification of the divine-legitimacy kernel; sibling readings instantiate different constraints.').

omega_variable(
    folk_practice_penetration_depth,
    'How far did the prohibition on other gods actually reach household and village religious practice?',
    'Settlement archaeology: household shrine assemblages, amulet and figurine continuities, and onomastic evidence (personal names honoring Amun, Hathor, Thoth) at Amarna and provincial sites, contrasted with the official monument record.',
    'Broad covert persistence lowers effective accessibility_collapse and narrows the arrangement''s operative reach to official and elite surfaces; near-total penetration would support a higher suppression reading and a more totalizing classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_practice_penetration_depth, empirical, 'Depth of enforcement penetration into non-elite practice versus official-record compliance.').

omega_variable(
    conviction_vs_consolidation,
    'Was exclusive Atenism primarily a theological program or an instrument for breaking independent priestly power?',
    'Sequence and targeting analysis: whether fiscal measures against temple estates preceded or followed doctrinal proclamation; comparative treatment of politically weighty cults versus harmless ones; depth of the king''s personal investment in the doctrine''s content.',
    'A consolidation-first reading strengthens the snare classification (the unity doctrine functioning as cover); a conviction-first reading preserves a residual genuine-coordination component, shading the arrangement toward hybrid coordination-and-collection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conviction_vs_consolidation, conceptual, 'Motivational ambiguity between sincere theological reform and power consolidation.').

omega_variable(
    succession_durability_counterfactual,
    'Could the arrangement have stabilized under a second enforcing reign, or was it inherently bound to its founder?',
    'Succession record and comparanda: coregency evidence, Smenkhkare''s brief tenure, the speed of restoration under Tutankhamun, and comparison with theocracies that survived a founding enforcer.',
    'If inherently founder-bound, the arrangement reads as personal-rule collection dependent on one enforcer; if stabilizable, its suppression and collection profiles describe a potentially durable theocratic centralization, changing counterfactual severity assessments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(succession_durability_counterfactual, empirical, 'Whether the arrangement''s coercive dependence was contingent on founder mortality or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.13).
narrative_ontology:measurement_basis(divi_tr_t3, observed).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(divi_tr_t6, observed).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(divi_tr_t9, observed).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(divi_tr_t12, observed).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement_basis(divi_tr_t15, observed).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.32).
narrative_ontology:measurement_basis(divi_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.61).
narrative_ontology:measurement_basis(divi_be_t3, observed).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement_basis(divi_be_t6, observed).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.77).
narrative_ontology:measurement_basis(divi_be_t9, observed).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement_basis(divi_be_t12, observed).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(divi_be_t15, observed).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.74).
narrative_ontology:measurement_basis(divi_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement_basis(divi_su_t3, observed).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement_basis(divi_su_t6, observed).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.85).
narrative_ontology:measurement_basis(divi_su_t9, observed).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement_basis(divi_su_t12, observed).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(divi_su_t15, observed).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.85).
narrative_ontology:measurement_basis(divi_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the divine_legitimacy_substrate kernel. The colloquial label 'Akhenaten's religious revolution' conflates three structurally distinct arrangements: the atenist reading (this file - collection concentrated on dispossessed cults, enforcement-heavy, epsilon high), the amun_polytheistic_reading (the established order the atenist reading attacked - priestly colleges as collectors, the crown as constrained payer, lower epsilon from the crown's seat), and the folk_syncretistic_reading (distributed household practice, minimal capture, minimal enforcement overhead). The amun reading is upstream: it is the prior arrangement whose revenues and legitimacy the atenist reading seized. The atenist reading exerts foreclosure pressure on both siblings during the interval - successfully (temporarily) against the organized Amun establishment, unsuccessfully against household practice. Epsilon differs across the family because the beneficiary/victim structures differ; averaging over readings would erase exactly the asymmetry the corpus exists to measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
