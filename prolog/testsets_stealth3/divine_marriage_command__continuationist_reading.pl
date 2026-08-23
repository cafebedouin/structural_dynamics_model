% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuing Validity of Plural Marriage Command (Prudential-Suspension Reading)
 *   domain: religious/political-theology
 *
 * SUMMARY:
 *   Within communities holding the continuationist reading, plural marriage
 *   remains a divinely commanded, eternally valid covenant; the 1890
 *   Manifesto is a prudential suspension extracted by federal coercion, not a
 *   rescission, and practitioners retain full theological legitimacy. The
 *   standing arrangement under contest — the ε referent for this reading — is
 *   the underground covenant regime that regime sustains:
 *   council-administered sealings, waiting lists, expulsions, and the
 *   readiness posture maintained under legal prohibition. It is NOT the
 *   reading's endorsed alternative (open lawful practice), which would drive
 *   ε toward zero for every sympathetic seat. ε is reading-indexed: authored
 *   from the continuationist seat, which frankly acknowledges heavy
 *   cost-bearing (prosecution exposure, sacrifice, hierarchy) while denying
 *   those costs constitute wrongful extraction; the authored value encodes
 *   that acknowledgment without adopting the reading's apologetic blanket
 *   denial. The claim and the metrics are independent: claimed_type is my
 *   structural judgment that the arrangement combines genuine
 *   persecuted-minority coordination with dominant asymmetric extraction; the
 *   metrics describe its actual operation. KEY AGENTS (by structural
 *   relationship): - priesthood_council_elders: Agenda-setting principal
 *   (institutional/identity_locked) — authorizes all sealings, assigns
 *   spouses, collects tithing flows - senior_plural_husbands: Primary
 *   beneficiary (moderate/identity_locked) — receives the marriage allocation
 *   - plural_wives_and_sealed_girls: Primary target (powerless/trapped) —
 *   bears assignment, reassignment threat, total exit cost -
 *   expelled_adolescent_boys: Target (powerless/constrained) — removed to
 *   rebalance marriage supply - unauthorized_waiting_men: Target with
 *   deferred promised benefit (powerless/identity_locked) -
 *   rank_and_file_believers: Incidental beneficiary and cost-bearer
 *   (moderate/identity_locked) - federal_state_prosecutors: External coercive
 *   pressure, episodic (institutional/analytical) -
 *   mainline_church_authorities: Institutional repudiator; defines the exit
 *   destination (institutional/analytical)
 *
 * KEY AGENTS:
 *   - priesthood_council_elders: agenda-setter (institutional/identity_locked) — runs the sealing system and collects its flows
 *   - senior_plural_husbands: beneficiary (moderate/identity_locked) — holds allocated plural households
 *   - plural_wives_and_sealed_girls: payer (powerless/trapped) — assigned marriages, reassignment sanction, total exit cost
 *   - expelled_adolescent_boys: payer (powerless/constrained) — expelled to rebalance bride supply
 *   - unauthorized_waiting_men: payer with secondary beneficiary position (powerless/identity_locked) — celibate, tithe, await authorization
 *   - rank_and_file_believers: beneficiary with secondary payer position (moderate/identity_locked) — meaning and cohesion against prosecution exposure and contribution demands
 *   - federal_state_prosecutors: external pressure seat (institutional/analytical) — episodic enforcement campaigns
 *   - mainline_church_authorities: institutional repudiator (institutional/analytical) — defines the alternative members would exit toward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.76).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.74).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuing Validity of Plural Marriage Command (Prudential-Suspension Reading)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political-theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '1545b71e-23d9-4b75-ae79-19cec341cb7a').
narrative_ontology:cs_kernel_codification('1545b71e-23d9-4b75-ae79-19cec341cb7a', fixed_text).
narrative_ontology:cs_authority_grounding('1545b71e-23d9-4b75-ae79-19cec341cb7a', lineage).
narrative_ontology:cs_interpretation_layer_present('1545b71e-23d9-4b75-ae79-19cec341cb7a').
narrative_ontology:cs_reading_relation('1545b71e-23d9-4b75-ae79-19cec341cb7a', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('1545b71e-23d9-4b75-ae79-19cec341cb7a', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('1545b71e-23d9-4b75-ae79-19cec341cb7a', foundational, manifesto_carries_no_doctrinal_force).
narrative_ontology:cs_axiom_status(manifesto_carries_no_doctrinal_force, holdable).
narrative_ontology:cs_axiom_grounding('1545b71e-23d9-4b75-ae79-19cec341cb7a', manifesto_carries_no_doctrinal_force, theological).
narrative_ontology:cs_axiom('1545b71e-23d9-4b75-ae79-19cec341cb7a', foundational, plurality_remains_condition_of_exaltation).
narrative_ontology:cs_axiom_status(plurality_remains_condition_of_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('1545b71e-23d9-4b75-ae79-19cec341cb7a', plurality_remains_condition_of_exaltation, theological).
narrative_ontology:cs_reference_frame('1545b71e-23d9-4b75-ae79-19cec341cb7a', pre_manifesto_open_plural_command).
narrative_ontology:cs_drift_state('1545b71e-23d9-4b75-ae79-19cec341cb7a', contemporary_fragmented_splinter_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('1545b71e-23d9-4b75-ae79-19cec341cb7a', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, priesthood_council_elders).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, senior_plural_husbands).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_and_sealed_girls).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, expelled_adolescent_boys).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, unauthorized_waiting_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, unauthorized_waiting_men).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, rank_and_file_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior men in continuationist priesthood councils who authorize every sealing, assign spouses, adjudicate member worthiness, and direct tithing, housing, and labor. Their office exists only inside the claim that the original command never lost force; abandoning that claim ends their authority, their status, and their livelihood.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, priesthood_council_elders, agenda_setter,
    institutional, generational, identity_locked, regional).

% Men granted additional sealed wives by council authorization. Household size and wife count measure standing in the community. Leaving would mean walking away from sealed children and spouses and, in the reading's terms, forfeiting exaltation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, senior_plural_husbands, beneficiary,
    moderate, biographical, identity_locked, regional).

% Women and girls sealed into marriages, frequently by council assignment and in some groups as minors. Children, housing, kinship, and salvation are all bound to remaining; wife reassignment hangs over dissent as a sanction. The outside world is unfamiliar, stigmatized, and reachable mainly by losing everything at once.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_and_sealed_girls, payer,
    powerless, biographical, trapped, local).

% Boys removed from their communities in their mid-teens, formally for infractions and practically to reduce competition for brides. They are dropped into nearby towns without money, completed schooling, or social networks, carrying the community's condemnation with them.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, expelled_adolescent_boys, payer,
    powerless, immediate, constrained, local).

% Adult baptized men not yet authorized to marry. They remain celibate, tithe, and accept assignments on the strength of a promised future sealing that the council controls. The promise is the product they are paying for; the deferment is the price they pay now.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, unauthorized_waiting_men, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, unauthorized_waiting_men, beneficiary).

% Ordinary members who draw meaning, cohesion, and assurance of exaltation from keeping the covenant the parent church abandoned. They also carry prosecution exposure, economic contribution, and the knowledge that leaving severs the entire kinship web and, in the reading's terms, the eternal family.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, rank_and_file_believers, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, rank_and_file_believers, payer).

% State attorneys general and federal officials enforcing bigamy, unlawful cohabitation, and child-abuse statutes. Enforcement runs in campaigns separated by long lulls: the 1953 Short Creek raid, the 2008 YFZ ranch raid, the Warren Jeffs prosecution, alternating with decades of pragmatic non-enforcement and, after 2020 in Utah, downgraded charging.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_state_prosecutors, observer,
    institutional, biographical, analytical, national).

% The Salt Lake City church hierarchy, which repudiated new plural marriages in 1890 and hardened that position in 1904, and today disciplines continuationists by excommunication. It defines the destination that exit from the continuationist communities leads to, and its repudiation is the fact the continuationist reading exists to reinterpret.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainline_church_authorities, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, priesthood_council_elders).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a proscribed covenant practice across generations of legal suppression: organizing secret sealings, mutual protection against prosecution, resource pooling, and communal identity for a minority whose parent institution surrendered the practice.
% TRANSFER_FUNCTION: Moves marriageable women and household labor upward toward council-authorized senior men; moves tithing, consecration, and unpaid service upward to the councils; moves legal-risk exposure downward onto ordinary practitioners; moves deference upward as the currency in which future authorization is purchased.
% ABSENT_VOICES: Women and girls inside the sealing system deliberate nowhere; expelled boys are physically removed before objection is possible; former members who could testify to coercion are dismissed as apostates; the conversation is confined to councils of senior men.
% DISAPPEARANCE_RATIONALE: If the continuing-validity commitment vanished overnight, the councils lose the object of their authority, sealings and waiting lists dissolve, the authorization economy disappears, and the communities merge into the mainline church or scatter into surrounding towns. The wider world would barely register the change; the rearrangement is concentrated entirely among the named parties.
% FOUNDING_PROBLEM: A two-layer genealogy: first, the 1840s revelation culminating in the 1852 public declaration established plural marriage as a commanded covenant and a condition of highest exaltation. Second, after 1890, the problem became how to remain faithful to a command the parent institution suspended under confiscation, imprisonment, and disincorporation pressure — how to keep a still-valid command alive without an institution willing to practice it openly.
% FOUNDING_PROBLEM_CORROBORATION: Historians and courts outside the benefiting parties corroborate the duress genealogy: the Edmunds-Tucker confiscations, the Reynolds v. United States line, and the stated motives of the 1890 and 1904 proclamations all support that the suspension answered state pressure. Scholars further corroborate the sincerity and demographic reality of continuationist practice. No external source attests the command's continuing divine validity — on that half, the only attestation comes from the continuationist councils themselves, who are the benefiting parties, and the story records that plainly.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76) because the scarcest goods — marriageable women and authorization itself — are allocated by the same council that collects tithing, and because the heaviest costs (legal jeopardy, celibacy, expulsion) fall on those with the least say. Suppression (0.74) is the blend of a persistent internal enforcement floor (excommunication, shunning, geographic isolation, information control) with episodic external campaigns; the suppression_requirement series traces the combined enforcement intensity, which is why it oscillates around a lower mean than the scalar, which weights the always-on internal machinery. Theater is moderate and rising (0.42 at interval end): sealings, households, and community functions are real work, but as open practice recedes, a growing share of activity is symbolic readiness maintenance — waiting lists, worthiness interviews, profession-of-obedience rituals for a practice that cannot lawfully occur. Accessibility collapse (0.62): exit exists de jure and is collapsed de facto by kinship economics and belief, though defections do occur, which keeps this below mountain-range values. Resistance (0.45): internal dissent is crushed quickly, but lost-boy lawsuits, trust litigation, and state prosecution constitute real, recurring opposition.
 *   
 *   CYCLICAL DYNAMICS: all three series run on one shared eight-point grid (1890, 1904, 1935, 1953, 1968, 1991, 2008, 2026), so every metric is authored at every examined time point. The suppression series shows at least two full enforcement cycles: the federal campaign of 1887-1890, a quiet accommodation trough in the 1930s-60s interrupted by the 1953 Short Creek raid spike, a long lull, and the 2008 YFZ/Jeffs spike followed by post-2020 Utah decriminalization relaxation. The cycle is not noise: each crackdown purges moderates, elevates hardliners, and validates the persecution narrative that binds members tighter, after which extraction ratchets up (see the 1953 and 2008 extractiveness steps). The oscillation is partially an extraction mechanism — intermittent reinforcement — not merely an external disturbance. base_properties were sampled at the 2026 phase: post-spike relaxation with elevated internal control retained.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the council seat, the arrangement is faithful stewardship of a command under siege — coordination it administers at personal legal risk. From the wife and girl seats, the same structure is fate: marriage by assignment, mobility of wives as sanction, no exit that does not cost children, salvation, and community at once. Waiting men experience promise-deferral; senior husbands experience reward; same nominal standing as male believers, radically different positions. INTER-INSTITUTIONAL DYNAMICS: federal and state prosecutors meet the arrangement as serial crime, the mainline church meets it as schism and apostasy, and the councils meet the prosecutors as the very duress that proves the reading — external pressure is metabolized as confirmation rather than deterrence, which is why suppression campaigns correlate with consolidation rather than compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   The council elders sit at the beneficiary pole (d near 0): they set the rules, allocate the scarce good, and collect the flows. Senior husbands sit low (d ~0.15): recipients of the allocation. Rank-and-file believers are genuinely dual-positioned — incidental beneficiaries of meaning and cohesion who also carry prosecution risk and contribution demands — which is why they carry a secondary payer role rather than a directionality override; the engine blends the positions from the dual declaration. Unauthorized waiting men derive high d (~0.75): currently targeted, compensated only by a council-controlled promise. Wives, sealed girls, and expelled boys sit nearest the full-target pole (d ~0.95), amplified by trapped exits. Prosecutors and mainline authorities are observer seats: they shape the arrangement's environment but collect nothing from it and pay nothing into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetric misreadings. A sympathetic religious-liberty framing reads pure coordination — a persecuted people keeping a covenant — and misses that the same structure allocates women, expels boys, and sells authorization promises. A prosecutorial framing reads pure predation and misses the real coordination function: sustaining a proscribed practice across four generations genuinely solves a collective-action problem for a besieged minority, which is why suppression campaigns alone have never dissolved it. On obsolescence: the founding problem remains live from inside the reading (the command is held valid, so fidelity remains owed), so no zombie flag is asserted; but the prudential-suspension structure builds a permanent limbo that feeds theater — readiness rituals for a practice that cannot openly occur. If new sealings ever cease entirely while councils persist on legitimacy claims alone, the arrangement drifts piton-ward: administrators maintaining performance without function. The theater_ratio series is the tripwire for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_divine_marriage_command_kernel,
    'This constraint is the continuationist_reading instantiation of the divine_marriage_command kernel; what structural facts change under the sibling readings?',
    'Compile and compare the sibling stories: divine_marriage_command__substitutionist_reading (monogamy doctrinally required; Manifesto as superseding revelation) and divine_marriage_command__coercion_visibility_reading (legitimacy deriving from institutional survival necessity). Compare victim sets, beneficiary sets, epsilon, and computed types across the three files linked by network edges.',
    'Under the substitutionist sibling the practitioner-victim set empties and extraction relocates to whoever enforces the new monogamy requirement; under the coercion-visibility sibling the leadership''s gains reframe as survival compensation rather than rent, lowering attributed extraction. The disagreement between readings is located in exactly two structural elements: the doctrinal status of the 1890 Manifesto (rescission vs. suspension vs. supersession) and the source of theological legitimacy (unchanged command vs. survival necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_divine_marriage_command_kernel, conceptual, 'Committer-frame indexicality: one reading of a three-reading kernel; classification is reading-relative by construction.').

omega_variable(
    doctrine_vs_council_abuse_attribution,
    'How much of the observed marriage-allocation extraction is attributable to the continuationist reading itself, versus to pathologies of particular centralized councils (most visibly the Warren Jeffs-era FLDS)?',
    'Comparative study across continuationist bodies — Apostolic United Brethren, independent fundamentalist groups, post-Jeffs FLDS fragments — correlating council centralization and leader concentration with coerced-sealing rates, minor-bride rates, and expulsion rates.',
    'If extraction tracks centralized councils rather than the doctrinal commitment, epsilon attributable to the reading-level constraint falls materially and the classification softens toward coordination-with-abuse-overlay; if abuse rates hold across decentralized bodies, the extraction is structural to the arrangement and the current values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_council_abuse_attribution, empirical, 'Attribution of measured extraction between the theological reading and specific leadership pathologies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (geographic isolation, poverty, legal jeopardy, information control) or internalized (eternal-family identity fusion, damnation expectations, trained helplessness that persist after physical barriers are gone)?',
    'Post-exit suppression trajectory of former members: if fear of spiritual consequences, loyalty conflicts, and identity dissolution persist for years after physical exit becomes safe, the internalized share dominates; rapid normalization after exit indicates structural dominance.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the constraint with them after exit, raising effective extraction for formerly trapped seats and supporting higher sustained chi than the structural data alone implies. Rough authored split for this arrangement: roughly forty percent structural, sixty percent internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized composition of suppression in the sealing-system population.').

omega_variable(
    raid_consolidation_cycle,
    'Do external enforcement campaigns function as intermittent reinforcement that consolidates internal extraction, rather than reducing it?',
    'Compare internal-control indicators (wife-reassignment events, adolescent expulsions, tithing and consecration demands, purge volumes) in windows before and after the 1953 Short Creek raid and the 2008 YFZ raid and Jeffs prosecution.',
    'If crackdowns reliably amplify internal extraction, external suppression policy is counterproductive against this arrangement and effective extraction peaks immediately post-raid — the 1953 and 2008 extractiveness steps in the measurement series are then causal, not coincidental, and the cyclical pattern is confirmed as an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(raid_consolidation_cycle, empirical, 'Whether the state-crackdown cycle operates as intermittent reinforcement consolidating internal hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement(divi_tr_t1904, divine_marriage_command__continuationist_reading, theater_ratio, 1904, 0.31).
narrative_ontology:measurement(divi_tr_t1935, divine_marriage_command__continuationist_reading, theater_ratio, 1935, 0.28).
narrative_ontology:measurement(divi_tr_t1953, divine_marriage_command__continuationist_reading, theater_ratio, 1953, 0.41).
narrative_ontology:measurement(divi_tr_t1968, divine_marriage_command__continuationist_reading, theater_ratio, 1968, 0.34).
narrative_ontology:measurement(divi_tr_t1991, divine_marriage_command__continuationist_reading, theater_ratio, 1991, 0.29).
narrative_ontology:measurement(divi_tr_t2008, divine_marriage_command__continuationist_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(divi_tr_t2026, divine_marriage_command__continuationist_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(divi_be_t1904, divine_marriage_command__continuationist_reading, base_extractiveness, 1904, 0.58).
narrative_ontology:measurement(divi_be_t1935, divine_marriage_command__continuationist_reading, base_extractiveness, 1935, 0.62).
narrative_ontology:measurement(divi_be_t1953, divine_marriage_command__continuationist_reading, base_extractiveness, 1953, 0.68).
narrative_ontology:measurement(divi_be_t1968, divine_marriage_command__continuationist_reading, base_extractiveness, 1968, 0.63).
narrative_ontology:measurement(divi_be_t1991, divine_marriage_command__continuationist_reading, base_extractiveness, 1991, 0.7).
narrative_ontology:measurement(divi_be_t2008, divine_marriage_command__continuationist_reading, base_extractiveness, 2008, 0.8).
narrative_ontology:measurement(divi_be_t2026, divine_marriage_command__continuationist_reading, base_extractiveness, 2026, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(divi_su_t1904, divine_marriage_command__continuationist_reading, suppression_requirement, 1904, 0.71).
narrative_ontology:measurement(divi_su_t1935, divine_marriage_command__continuationist_reading, suppression_requirement, 1935, 0.58).
narrative_ontology:measurement(divi_su_t1953, divine_marriage_command__continuationist_reading, suppression_requirement, 1953, 0.86).
narrative_ontology:measurement(divi_su_t1968, divine_marriage_command__continuationist_reading, suppression_requirement, 1968, 0.49).
narrative_ontology:measurement(divi_su_t1991, divine_marriage_command__continuationist_reading, suppression_requirement, 1991, 0.56).
narrative_ontology:measurement(divi_su_t2008, divine_marriage_command__continuationist_reading, suppression_requirement, 2008, 0.89).
narrative_ontology:measurement(divi_su_t2026, divine_marriage_command__continuationist_reading, suppression_requirement, 2026, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'polygamy after the Manifesto' decomposes into three structurally distinct constraints — three readings of the single kernel divine_marriage_command (the 1843 revelation canonized as Doctrine and Covenants 132 together with the 1890/1904 proclamations). Each reading yields its own epsilon, beneficiary/victim structure, and classification: this continuationist story keeps the original command valid and locates all illegality outside the doctrine; the substitutionist sibling makes monogamy doctrinally required and relocates extraction accordingly; the coercion-visibility sibling re-grounds legitimacy in survival necessity. The readings disagree on two structural elements only — the Manifesto's doctrinal force and the source of theological legitimacy — and every other difference between the stories follows from those. This file links both siblings via affects_constraints; the continuationist reading is structurally upstream of the coercion-visibility reading (its insistence on continued practice is what creates the survival crisis that reading manages) and logically incompatible with the substitutionist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
