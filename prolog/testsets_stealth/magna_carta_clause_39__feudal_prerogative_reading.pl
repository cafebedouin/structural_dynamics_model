% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 — Feudal Prerogative Reading (Procedural Rights Within Established Hierarchy)
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal_prerogative_reading of Magna Carta
 *   Clause 39: the clause as a sworn compact between the crown and the armed
 *   elite of 1215 England, preserving narrow procedural protections for 'free
 *   men' — a restricted legal class comprising barons, knights, and other
 *   free tenants, roughly a tenth to a fifth of the population — while
 *   leaving the feudal hierarchy, royal prerogative over the unfree majority,
 *   and the king's fundamental position wholly intact. On this reading the
 *   clause bounds HOW royal power touches free men (no seizure, imprisonment,
 *   dispossession, outlawry, or exile except by lawful judgment of his equals
 *   or the law of the land) without abolishing or democratizing that power.
 *   The interval runs from the 1215 grant (t=0) to the Confirmatio Cartarum
 *   of 1297 (t=82), when the charter entered the statute roll. CONSTRAINT
 *   FAMILY NOTE (epsilon decomposition): this is one of three linked readings
 *   of the same kernel text, decomposed per the epsilon-invariance principle
 *   because the colloquial label 'Clause 39' covers structurally distinct
 *   claims. THIS reading authors epsilon = 0.30: a class-restricted elite
 *   compact with low extraction against traditional authority, whose
 *   recognized injured class (those wronged when the guarantee is breached)
 *   is elite peers only. The sibling liberal_due_process_reading authors a
 *   universal protected class and correspondingly high extraction against
 *   state power; the sibling originalist_limitation_reading narrows the
 *   clause to specific documented 1215 abuses, shrinking both beneficiary set
 *   and standing scope. Each reading is a separate file with its own epsilon,
 *   beneficiaries, and classification; they are linked via
 *   network.affects_constraints, not averaged here. KEY AGENTS (by structural
 *   relationship): - the_english_crown: Agenda-setting authority and
 *   dual-positioned party (institutional/constrained) — grants, swears, and
 *   administers the procedural settlement; surrenders unilateral seizure of
 *   free men while collecting stability, continued feudal revenue, and
 *   legitimacy - magna_carta_baronage: Primary beneficiary
 *   (organized/constrained) — secures persons, lands, wards, and heirs
 *   against arbitrary royal fiscal-military predation; enforcement seat in
 *   1215, judicial consumers thereafter - free_tenants_and_knights: Secondary
 *   beneficiary (moderate/constrained) — receive the same textual protection
 *   with far less leverage to invoke it - unfree_villein_majority: Excluded
 *   party (powerless/trapped) — outside the 'free man' category; bound by the
 *   law's discipline without its procedural shield; no seat in the bargain -
 *   papal_curia: External validator (institutional/analytical) — quashed the
 *   1215 grant and reshaped enforcement conditions without joining the
 *   bargain - royal_justices_and_chancery: Administrative beneficiary
 *   (organized/constrained) — staff the courts through which lawful judgment
 *   and the law of the land actually run
 *
 * KEY AGENTS:
 *   - the_english_crown: agenda_setter + beneficiary + bearer of bounded-prerogative cost (institutional/constrained) — administers the settlement it swore to; its surrendered arbitrariness over free men is the settlement's principal price
 *   - magna_carta_baronage: primary beneficiary (organized/constrained) — the class whose revolt produced the grant and whose persons and tenures the guarantee shields; held the clause 61 enforcement council in 1215
 *   - free_tenants_and_knights: secondary beneficiary (moderate/constrained) — covered by the text, reliant on county courts and later writs, lacking baronial leverage
 *   - unfree_villein_majority: excluded party (powerless/trapped) — the demographic majority, legally bound to manors, outside the protected class the reading recognizes
 *   - papal_curia: external validator (institutional/analytical) — Innocent III's annulment of 1215 demonstrates that enforcement conditions were set partly outside the realm
 *   - royal_justices_and_chancery: administrative beneficiary (organized/constrained) — careers, fees, and institutional standing flow through the procedural order they operate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.3).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 — Feudal Prerogative Reading (Procedural Rights Within Established Hierarchy)").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '45eba8f6-3be6-4b28-9983-297c02f7d1f4').
narrative_ontology:cs_kernel_codification('45eba8f6-3be6-4b28-9983-297c02f7d1f4', fixed_text).
narrative_ontology:cs_authority_grounding('45eba8f6-3be6-4b28-9983-297c02f7d1f4', lineage).
narrative_ontology:cs_interpretation_layer_present('45eba8f6-3be6-4b28-9983-297c02f7d1f4').
narrative_ontology:cs_reading_relation('45eba8f6-3be6-4b28-9983-297c02f7d1f4', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('45eba8f6-3be6-4b28-9983-297c02f7d1f4', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('45eba8f6-3be6-4b28-9983-297c02f7d1f4', foundational, protected_class_restricted_to_free_men).
narrative_ontology:cs_axiom_status(protected_class_restricted_to_free_men, holdable).
narrative_ontology:cs_axiom_grounding('45eba8f6-3be6-4b28-9983-297c02f7d1f4', protected_class_restricted_to_free_men, conventional).
narrative_ontology:cs_axiom('45eba8f6-3be6-4b28-9983-297c02f7d1f4', foundational, royal_power_law_bounded_not_abolished).
narrative_ontology:cs_axiom_status(royal_power_law_bounded_not_abolished, holdable).
narrative_ontology:cs_axiom_grounding('45eba8f6-3be6-4b28-9983-297c02f7d1f4', royal_power_law_bounded_not_abolished, conventional).
narrative_ontology:cs_reference_frame('45eba8f6-3be6-4b28-9983-297c02f7d1f4', sworn_feudal_compact_hierarchy).
narrative_ontology:cs_drift_state('45eba8f6-3be6-4b28-9983-297c02f7d1f4', confirmatio_cartarum_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('45eba8f6-3be6-4b28-9983-297c02f7d1f4', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_baronage).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_and_knights).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, the_english_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, the_english_crown).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, royal_justices_and_chancery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants and swears the charter, staffs the chancery and benches through which lawful judgment and the law of the land operate, and collects feudal incidents, reliefs, and scutage under the settled rules. Surrenders the power to seize, imprison, dispossess, outlaw, or exile a free man except through judgment of his equals or the law of the land. Attempted exit — John's repudiation — brought papal annulment, civil war, and defeat; every later attempt to push past the settled limits raised the price of the next. Collects the end of insurrection, restored legitimacy, and continued revenue.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, the_english_crown, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, the_english_crown, beneficiary).

% Held arms, lands, and castles across the realm and financed the crown through grants and service. Secured protection of persons, tenures, wards, and heirs against arbitrary royal predation, plus judgment by their own equals. Paid for the settlement with a money grant, renewed fealty, and — in 1215 — the clause 61 council of twenty-five that could distrain the king. Cannot leave the realm's legal order; rebellion is the alternative channel, and they used it when the settlement broke.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_baronage, beneficiary,
    organized, generational, constrained, national).

% Lesser free men holding by knight service or free tenure. Covered by the same textual guarantee as the magnates but with a fraction of the leverage: they rely on county courts, local juries, and eventually the writ system to make the protection real. Their protection arrives secondhand, through machinery the magnates negotiated.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_and_knights, beneficiary,
    moderate, biographical, constrained, national).

% The demographic majority of England, legally bound to their manors and lords. Outside the category of free man, they stand before the law's discipline — its courts, its forfeitures, its obligations — without the procedural shield the settlement extends to free men. No seat in the bargain, no standing to invoke it, and no path out of villeinage. Their exclusion marks the boundary of the protected class.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_villein_majority, excluded,
    powerless, generational, trapped, national).

% External spiritual authority with suzerainty claims over the English crown. Quashed the 1215 grant as extorted and shameful, absolving the king from his oath — an intervention that removed the settlement's religious sanction and helped precipitate war. Takes positions on the settlement's validity from outside the realm's legal order without being party to the crown–baronage bargain.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, papal_curia, observer,
    institutional, generational, analytical, continental).

% The professional administrators and judges who issue writs, convene juries, and run the processes the settlement names. Their offices, fees, and institutional standing depend on the procedural order functioning; they convert the sworn promise into operating practice, and their growing routine handling of the guarantee is why enforcement intensity fell over the interval.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_justices_and_chancery, beneficiary,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_baronage).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts crown–free-man disputes from extra-judicial seizure and private war into adjudicable process: no taking, imprisonment, dispossession, outlawry, or exile of a free man except by lawful judgment of his equals or the law of the land, administered through royal courts. Solves the recurring collective-action problem of intermittent civil war between the crown and its armed elite over arbitrary fiscal-military predation.
% TRANSFER_FUNCTION: Moves procedural security — protection of person, tenure, wardship, and heir — to free men, barons foremost, paid for by the crown's surrender of unilateral prerogative over that class. Reciprocally moves money grants and renewed fealty from the baronage to the crown. Moves nothing to the unfree majority, who stand outside the transfer entirely.
% ABSENT_VOICES: The unfree villein majority — most of the population — would object to a settlement that defines legal protection as a class privilege; they are absent because they lack standing in any forum the bargain recognizes, bound to their manors. Radical later readers who would universalize the guarantee are likewise absent from the 1215 table. The unanimity of the bargain is real among its parties and constitutively silent about everyone beneath it.
% DISAPPEARANCE_RATIONALE: If the guarantee vanished overnight, baronial security of tenure and person collapses back onto the king's pleasure; the crown faces immediate renewed insurrection, as the 1215–17 war already demonstrated; the nascent writ system loses its anchor; and crown–elite dispute resolution reverts to extra-judicial seizure and armed reprisal. Every named party's arrangements depend on it — the villein majority least, and only negatively.
% FOUNDING_PROBLEM: King John's arbitrary fiscal-military predation against his own elite: abusive scutage levies, disseisin of baronial lands, imprisonment of rivals without judgment, and exploitation of wardships and reliefs — grievances that drove the baronial revolt and the armed negotiation of 1215.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary monastic chroniclers outside the beneficiary set — Roger of Wendover and Matthew Paris — attest both the specificity of John's abuses and their resolution by the 1217 settlement. The papal register records the curia's independent (hostile) account of the grant's circumstances. Modern constitutional historians — McKechnie, Holt, Turner — writing with no stake in the feudal bargain corroborate that the particular 1215 grievances were remedied while the procedural arrangement persisted and generalized. The crown's own reissue preambles attest persistence but are inside the beneficiary set and carry less weight.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.30 at interval end) because on this reading the settlement's principal cost falls narrowly and modestly: the crown's surrender of unilateral action against free men, in exchange for which it collected an end to insurrection, continued feudal incidents, and restored legitimacy. No party is heavily harvested through the structure; the unfree majority is excluded from it rather than drained by it (see omega class_exclusion_extraction_status for the contestable residue of that claim). Suppression (0.28) is the settlement's coercive face — the barring of royal force outside process — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater ratio (0.20) reflects a function that genuinely ran: writs issued, judgments of equals convened, disseisin was reversed — with a growing ceremonial fringe as reissues became instruments of royal grace (1225) and taxation bargaining (1297). Accessibility collapse (0.48) is moderate: extra-judicial seizure and private war remained conceivable alternatives that kings intermittently attempted, so understanding the settlement narrowed but did not erase the option space. Resistance (0.45) is substantial: John's repudiation, the papal annulment, the First Barons' War of 1215–17, and recurring friction under Henry III and Edward I — sustained, sometimes armed, ultimately accommodated. The temporal series share one grid (t = 0, 14, 27, 41, 55, 68, 82 years from 1215): base_extractiveness drifts gently up as the crown's conceded ground hardened into settled limits; theater creeps up with the ceremonial politics of reissue; suppression_requirement FALLS steeply from war-backed compulsion (clause 61 distraint, 0.55) toward routine judicial normalization (0.28) — the enforcement story this series exists to trace, since the settlement's coercive infrastructure visibly changed character over the interval even as its scalar suppression stayed modest.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the crown's seat the arrangement is a sworn compact it administers: costly at the margin (bounded prerogative over free men), stabilizing in the whole — a near-symmetric experience with a slight net gain, which is why crowned successors reissued it voluntarily in 1225 and bargained with it in 1297. From the baronial seat the same structure is protection purchased with money grants and renewed fealty: a clear net gain, enforced first by their own council and later consumed through the courts. From the villein position the identical legal order presents a third face: the law of the land disciplines everyone while shielding only free men — neither party to the bargain nor protected by it. The engine computes these divergent per-seat classifications from the structural data (roles, power, exit options, the crown's dual beneficiary/victim declaration); nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the baronage collects the principal yield (security of person, tenure, wardship); free tenants collect the same textual protection secondhand; the crown collects the stabilization dividend. The crown is deliberately dual-declared — listed among beneficiaries for the stabilization dividend AND among victims for the bounded-prerogative cost — because both flows are real and material, and the derivation chain should see both rather than a sanitized single position. The unfree majority is intentionally NOT declared a victim: their structural relation to this settlement is exclusion (absence of standing and protection), not payment through it; whether that exclusion constitutes extraction through the structure is precisely the open question routed to omega class_exclusion_extraction_status, and resolving it toward extraction would move this story toward a hybrid classification with villeins as victims. No directionality overrides are authored: the dual crown declaration encodes the crown's two-sided position directly, and the remaining seats derive cleanly from their declared roles and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — King John's specific predation: scutage abuse, disseisin of baronial lands, arbitrary imprisonment, exploitation of wardships — was dead within a decade of the grant; John was dead, the specific abuses remedied, the crisis resolved by 1217 and sealed by the 1225 reissue. Yet the arrangement persists and the world demonstrably rearranges around it. The classification apparatus prevents two opposite mislabels here. Against mislabeling as pure extraction: the coordination function is genuine and primary — converting crown–elite disputes from extra-judicial seizure and civil war into adjudicable process — and the extraction riding on it is thin. Against mislabeling as inertial remnant: theater stays low (0.20) because royal courts actually run the process the clause names; the dead founding problem paired with a live world-rearranging function resolves as function-succession, not zombie maintenance. The founding_problem_status=dead × disappearance_verdict=world_rearranges mismatch is therefore expected to fire the capture/zombie cross-check and resolve CLEAN against the computed low-theater path. The residual watch-item is late-period ceremonial inflation: if charter invocations in later centuries become predominantly theatrical while the feudal frame itself dissolves, this reading's story would drift toward degradation and should be re-measured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_exclusion_extraction_status,
    'Does the settlement''s class-bound operation — shielding free men while helping constitute a two-tier legality that left villeins subject to the law''s discipline without its procedural protections — count as extraction through the structure, or merely as scope-limitation of an elite bargain?',
    'Comparative legal-historical analysis of whether villein disability was load-bearing for the crown–baronage bargain (i.e., protecting free men required defining the unfree as outside) or incidental to it; examine whether common-law procedure''s free/unfree boundary tightened in direct dependence on the clause''s operation.',
    'If the exclusion is load-bearing, the unfree majority should be added as victims, extractiveness revised upward, and the classification moved toward a hybrid coordination/extraction type with active enforcement doing real distributive work; if incidental, the current low-extraction coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_exclusion_extraction_status, conceptual, 'Whether class exclusion constitutes extraction through the settlement or mere scope limitation.').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of kernel magna_carta_clause_39 — the feudal_prerogative_reading. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the liberal_due_process_reading would universalize the protected class and raise extraction against state authority sharply; the originalist_limitation_reading would narrow the arrangement to documented 1215 abuses with minimal standing scope. The disagreement lives in the referent of ''free man'' and in whether the clause bounds a general power or remedies specific abuses.',
    'Adopting a sibling''s referent changes the victim set, the beneficiary set, and epsilon wholesale — the three readings are different constraints over one text, and cross-reading comparison is valid only at the family level via network links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    enforcement_normalization_vs_decay,
    'Does the falling suppression_requirement series (0.55 to 0.28) record genuine normalization — the settlement becoming accepted law that courts routinely apply — or enforcement decay that masks continued royal arbitrariness between crisis windows?',
    'Compare crown compliance with the procedural guarantee in peacetime windows against crisis windows (renewed war, taxation emergencies) across the interval; if compliance holds in peacetime and lapses only under fiscal stress, normalization is real.',
    'If decay rather than normalization, the settlement''s stability is overstated, the low suppression reading flatters it, and the coordination certification rests on shakier ground; if normalization holds, the falling series confirms a settlement maturing into ordinary law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_normalization_vs_decay, empirical, 'Whether declining enforcement intensity reflects acceptance or erosion.').

omega_variable(
    crown_net_position_counterfactual,
    'Is the crown genuinely a net beneficiary of the settlement (stabilization dividend exceeding bounded-prerogative cost), or did it bear net costs it would have revoked outright absent baronial and papal pressure?',
    'Counterfactual analysis of the reissue politics: why Henry III reissued in 1225 and why Edward I confirmed in 1297 — voluntary consolidation versus compelled bargaining (the 1297 confirmation rode a taxation crisis). Voluntary reissuance patterns support net-benefit; purely compelled episodes support net-cost.',
    'If the crown''s position is net-cost, its derived directionality shifts toward the target end, effective extraction against traditional authority rises above the authored low value, and the coordination reading strains; if net-benefit holds, the dual declaration and low epsilon stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_net_position_counterfactual, empirical, 'Whether the crown''s dual position nets positive or negative across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 82).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_feudal_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mc39_feudal_tr_t14, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 14, 0.11).
narrative_ontology:measurement(mc39_feudal_tr_t27, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 27, 0.13).
narrative_ontology:measurement(mc39_feudal_tr_t41, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 41, 0.15).
narrative_ontology:measurement(mc39_feudal_tr_t55, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 55, 0.16).
narrative_ontology:measurement(mc39_feudal_tr_t68, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 68, 0.18).
narrative_ontology:measurement(mc39_feudal_tr_t82, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 82, 0.2).

% Extraction over time
narrative_ontology:measurement(mc39_feudal_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mc39_feudal_be_t14, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 14, 0.23).
narrative_ontology:measurement(mc39_feudal_be_t27, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 27, 0.25).
narrative_ontology:measurement(mc39_feudal_be_t41, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 41, 0.26).
narrative_ontology:measurement(mc39_feudal_be_t55, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 55, 0.28).
narrative_ontology:measurement(mc39_feudal_be_t68, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 68, 0.29).
narrative_ontology:measurement(mc39_feudal_be_t82, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 82, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(mc39_feudal_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mc39_feudal_su_t14, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 14, 0.47).
narrative_ontology:measurement(mc39_feudal_su_t27, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 27, 0.41).
narrative_ontology:measurement(mc39_feudal_su_t41, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 41, 0.36).
narrative_ontology:measurement(mc39_feudal_su_t55, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 55, 0.32).
narrative_ontology:measurement(mc39_feudal_su_t68, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 68, 0.3).
narrative_ontology:measurement(mc39_feudal_su_t82, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 82, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of kernel magna_carta_clause_39 per the epsilon-invariance principle: the colloquial label 'Clause 39' conflates three structurally distinct claims. This file (feudal_prerogative_reading) authors epsilon 0.30 over a class-restricted elite compact with a restricted recognized injured class (elite peers) and low extraction against traditional authority. The liberal_due_process_reading authors a universal protected class and high extraction against state power; the originalist_limitation_reading authors an abuse-specific remedy of minimal standing scope. Upstream/downstream structure: this reading's establishment of the class-restricted original referent supplies the evidentiary baseline that shapes the originalist sibling's scope claims (influences), while coexisting with the liberal sibling as rival live interpretations. All three files link one another via network.affects_constraints; no reading averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
